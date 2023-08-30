(** This module extends {{!Base.Printf}[Base.Printf]}. *)

open! Import

include Base.Printf (** @open *)

let eprintf = Stdio.Out_channel.eprintf
let fprintf = Stdio.Out_channel.fprintf
let kfprintf = Stdio.Out_channel.kfprintf
let printf = Stdio.Out_channel.printf

(** print to stderr; exit 1 *)
let exitf fmt =
  ksprintf
    (fun s () ->
      eprintf "%s\n%!" s;
      exit 1)
    fmt
;;

type printf = { printf : 'a. ('a, Buffer.t, unit) format -> 'a }

let collect_to_string f =
  let buf = Buffer.create 64 in
  let done_ = ref false in
  let printf fmt =
    kbprintf
      (fun buf ->
        if !done_
        then (
          Buffer.reset buf;
          raise_s [%message "[printf] used after [collect_to_string] returned"]))
      buf
      fmt
  in
  f { printf };
  done_ := true;
  let output = Buffer.contents buf in
  Buffer.reset buf;
  output
;;
