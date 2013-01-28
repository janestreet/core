open Core.Std

open OUnit;;

let raises_exception f =
  try ignore (f ()); false
  with _ -> true

let null_handler _ _ = ()
let exception_handler _ _ = failwith "printing this exception does not indicate a bug"

let test =
  "timer" >:::
    [ "abusive user" >::
        (fun () ->
           "add" @?
             begin
               let timer = Timer.create () in
               raises_exception
                 (fun () -> Timer.add timer null_handler (sec (-1.)))
             end;
           "remove twice" @?
             begin
               let timer = Timer.create () in
               let ev = Timer.add timer null_handler (sec 0.1) in
               Timer.remove ev;
               Timer.remove ev;
               true
             end;
           "malicious handler" @?
             begin
               let timer = Timer.create () in
               let _ev = Timer.add timer exception_handler (sec 0.1) in
               Time.pause (sec 0.3);
               true
             end;
        );
      "ordering of events" >::
        (fun () ->
           let timer = Timer.create () in
           let delays = Array.init 100 ~f:(fun i -> (float i)/.100.) in
           Array.permute delays;
           let queue = Squeue.create 100 in
           let add_handler _ time = Squeue.push queue (Time.to_float time) in
           Array.iter delays
             ~f:(fun delay ->
                   ignore (Timer.add timer add_handler (sec delay))
                );
           Time.pause (sec 1.2);
           let last = ref (-1.) in
           while Squeue.length queue > 0 do
             let next = Squeue.pop queue in
             "in order" @? (next >=. !last);
             last := next
           done
        );
      "deactivation" >::
        (fun () ->
           let timer = Timer.create () in
           "initially activated" @? Timer.is_activated timer;
           let dont_call_handler _ _ = ("handler called stupidly" @? false) in
           ignore (Timer.add timer dont_call_handler (sec 0.25));
           Timer.deactivate timer;
           "can be deactivated" @? not (Timer.is_activated timer);
           Time.pause (sec 0.5)
        )
    ]
