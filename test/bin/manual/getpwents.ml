open Core

(* This test is vulnerable to someone changing the user database at exactly
   the wrong time. Unfortunately there doesn't seem to be a good way
   around that -- there's some LDAP stuff involved that we can't easily mock up in
   a test environment.

   We tried just running it as an automated test anyway and crossing our fingers, but it
   happens often enough to cause spurious hydra failures. So we stopped running the test
   automatically altogether. *)
let test_getpwents_in_thread () =
  let inside_thread_ref = ref [] in
  let thread =
    Thread.create (fun () -> inside_thread_ref := Unix.Passwd.getpwents ()) ()
  in
  let outside_thread = Unix.Passwd.getpwents () |> List.sort ~compare in
  Thread.join thread;
  let inside_thread = !inside_thread_ref |> List.sort ~compare in
  [%test_eq: Unix.Passwd.t list]
    ~message:"Either locking in getpwents failed, or the user database just \
              changed mid-test and you should retry."
    outside_thread inside_thread
