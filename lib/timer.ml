open Std_internal

module Mutex = Mutex0

type interval =
  | INone
  | INormal of Span.t
  | IRandom of Span.t * float

type status = Activated | Deactivating | Deactivated

(* Mutex [mtx] must be held when modifying [status] and [events].  Such
   modifications require that the timer thread and other threads potentially
   waiting on the condition variable be woken up.  Hence the condition
   variable always needs to be broadcast thereafter.  [wrap_update] makes
   this safe and easy.  The timer thread may also wake up while waiting on
   the condition variable when the timer expires. *)
type t =
  {
    mutable status : status;
    events : event Heap.t;
    mtx : Mutex.t;
    cnd : Condition.t;
    mutable now : Time.t;
  }

and event =
  {
    mutable time : Time.t;
    mutable interval : interval;
    handler : event -> Time.t -> unit;
    timer : t;
    mutable t_event_opt : event Heap.heap_el option;
  }

let run_timer timer =
  let mtx = timer.mtx in
  let rec handle_events () =
    (* Assumes that mutex is held *)
    match timer.status with
    | Deactivating ->
        timer.status <- Deactivated;
        Condition.broadcast timer.cnd;
        Mutex.unlock mtx
    | Deactivated -> assert false  (* impossible *)
    | Activated ->
      match Heap.top_heap_el timer.events with
      | None ->
          Condition.wait timer.cnd mtx;
          timer.now <- Time.now ();
          handle_events ()
      | Some event ->
          let ev = Heap.heap_el_get_el event in
          let sched_time = ev.time in
          let now = timer.now in
          if Time.(<) now sched_time then begin
            (* Event in the future, wait until then or until signal *)
            ignore (Core_condition.timedwait timer.cnd mtx sched_time : bool);
            timer.now <- Time.now ()
          end else begin
            (* Update event on the heap as necessary *)
            begin match ev.interval with
            | INone -> Heap.remove event
            | INormal span ->
                ev.time <- Time.add now span;
                Heap.update event ev
            | IRandom (span, max_ratio) ->
                let p2 = Random.float 2.0 in
                let p = p2 -. 1. in
                let confusion = Span.scale span (max_ratio *. p) in
                ev.time <- Time.add (Time.add now span) confusion;
                Heap.update event ev
            end;
            Mutex.unlock mtx;
            begin
              try ev.handler ev now
              with e ->
               eprintf "Timer.run: Exception in event handler: %s\n%!"
                 (Exn.to_string e)
            end;
            Mutex.lock mtx
          end;
          handle_events ()
  in
  Mutex.lock mtx;
  handle_events ()

let create ?(min_size = 1000) () =
  let events =
    Heap.create ~min_size (fun ev1 ev2 -> Time.compare ev1.time ev2.time)
  in
  let timer =
    {
      status = Activated;
      events = events;
      mtx = Mutex.create ();
      cnd = Condition.create ();
      now = Time.now ();
    }
  in
  ignore (Thread.create run_timer timer);
  timer

let size timer =
  Mutex.critical_section timer.mtx ~f:(fun () -> Heap.length timer.events)

let deactivate timer =
  Mutex.critical_section timer.mtx ~f:(fun () ->
    let rec wait () = Condition.wait timer.cnd timer.mtx; check ()
    and check () =
      match timer.status with
      | Activated ->
          timer.status <- Deactivating;
          Condition.broadcast timer.cnd;
          wait ()
      | Deactivating -> wait ()
      | Deactivated -> ()
    in
    check ())

let check_span loc span =
  if Span.(<) span Span.zero then
    invalid_arg (sprintf "Timer.%s: span < 0" loc)

let get_interval_param loc randomize = function
  | None -> INone
  | Some span ->
      check_span loc span;
      match randomize with
      | None -> INormal span
      | Some max_ratio ->
          if max_ratio < 0. || 1. < max_ratio then
            invalid_arg (
              sprintf "Timer.%s: max_ratio not in range [0.0, 1.0]" loc);
          IRandom (span, max_ratio)

(* Makes sure that the timer thread gets signaled only if the element
   at the top of the heap requires earlier wakeups *)
let wrap_update timer ~f =
  Mutex.critical_section timer.mtx ~f:(fun () ->
    let top_before_f = Heap.top timer.events in
    let res = f () in
    let top_after_f = Heap.top timer.events in
    match top_after_f with
    | None -> res (* Nothing on the queue, so no wake-up *)
    | Some top_after_f ->
        match top_before_f with
        | None ->
            (* Heap was empty and is now not, so wake up immediately *)
            Condition.broadcast timer.cnd;
            res
        | Some top_before_f ->
            if Time.(<=) top_before_f.time top_after_f.time then res
            else (
              (* Earlier event time at top: we have to wake up *)
              Condition.broadcast timer.cnd;
              res))


let add_abs timer handler ?randomize ?interval time =
  let interval = get_interval_param "add_abs" randomize interval in
  wrap_update timer
    ~f:(fun () ->
       if timer.status <> Activated then
         failwith "Timer.add_abs: timer deactivated";
       let event =
         {
           time = time;
           interval = interval;
           handler = handler;
           timer = timer;
           t_event_opt = None;
         }
       in
       let t_event = Heap.push timer.events event in
       event.t_event_opt <- Some t_event;
       event)

let add t handler ?randomize ?interval span =
  let time = Time.add (Time.now ()) span in
  check_span "add" span;
  add_abs t handler ?randomize ?interval time

let remove { timer; t_event_opt; _ } =
  match t_event_opt with
  | Some t_event ->
      wrap_update timer ~f:(fun () ->
        if timer.status <> Activated then
          failwith "Timer.remove: timer deactivated";
        if Heap.heap_el_is_valid t_event then Heap.remove t_event)
  | None -> assert false  (* impossible *)

let reschedule ({ timer; _ } as ev) ?randomize ?interval span =
  match ev.t_event_opt with
  | Some t_event ->
      let loc = "reschedule" in
      check_span loc span;
      let interval = get_interval_param loc randomize interval in
      wrap_update timer ~f:(fun () ->
        if timer.status <> Activated then
          failwith "Timer.reschedule: timer deactivated";
        if Heap.heap_el_is_valid t_event then (
          ev.time <- Time.add ev.time span;
          ev.interval <- interval;
          Heap.update t_event ev)
        else failwith "Timer.reschedule: event not scheduled")
  | None -> assert false  (* impossible *)

let get_timer ev = ev.timer
let get_event_time ev = ev.time
let get_event_interval ev = ev.interval
let is_activated timer = timer.status = Activated
