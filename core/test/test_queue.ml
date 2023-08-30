open! Core

let%test_module _ =
  (module (
             struct
               include Base.Queue
               (* Base.Queue is tested separately. Here we test the additions from Core. *)

               open Core.Queue

               type nonrec 'a t = 'a t [@@deriving bin_io]

               let binary_search = binary_search
               let binary_search_segmented = binary_search_segmented

               (* Tested where instantiated using [Test_binary_searchable.Make1_and_test] *)

               module Stable = struct
                 module V1 = Stable.V1

                 include Stable_unit_test.Make (struct
                   type nonrec t = int V1.t [@@deriving sexp, bin_io, compare]

                   let equal = [%compare.equal: t]

                   let tests =
                     let manipulated = Queue.of_list [ 0; 3; 6; 1 ] in
                     ignore (Queue.dequeue_exn manipulated : int);
                     ignore (Queue.dequeue_exn manipulated : int);
                     Queue.enqueue manipulated 4;
                     [ Queue.of_list [], "()", "\000"
                     ; Queue.of_list [ 1; 2; 6; 4 ], "(1 2 6 4)", "\004\001\002\006\004"
                     ; manipulated, "(6 1 4)", "\003\006\001\004"
                     ]
                   ;;
                 end)
               end
             end
             (* This signature is here to remind us to update the unit tests whenever we
      change [Queue]. *) :
               module type of Queue))
;;
