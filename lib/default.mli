
(* [Default] is used to create new types for specifying default values for optional
   arguments, and having the type checker enforce the default.

   For example, to represent optional boolean arguments with default values [true] and
   [false], in bool.mli we have:

     module True_  : Default.S with type real = t
     module False_ : Default.S with type real = t

   In bool.ml we create the modules using [Default.create], specifying the default value
   as the argument:

     module  True_ = (val Default.create true  : Default.S with type real = t)
     module False_ = (val Default.create false : Default.S with type real = t)

   Here is how to specify and define a function that uses optional arguments with
   enforced defaults:

     let f
       :  ?x:Bool. True_.default
       -> ?y:Bool.False_.default
       -> unit
       -> bool * bool
       = fun ?(x = Bool.True_.default) ?(y = Bool.False_.default) () ->
       let x = (x :> bool) in
       let y = (y :> bool) in
       x, y
     ;;

   And here is how to call [f], using the override [!!] prefix operator, which is defined
   in Core to be [Default.override].

     let () =
       assert ((true , false) = f ());
       assert ((false, false) = f ~x:!!false ());
       assert ((false, true ) = f ~x:!!false ~y:!!true ());
     ;;

   For more discussion, see:

   http://ocaml.janestreet.com/?q=node/96 *)

type ('real, 'phantom) t = private 'real

val override : 'real -> ('real, _) t

module type S = sig
  type phantom
  type real
  type default = (real, phantom) t
  val default : default
end

val create : 'a -> (module S with type real = 'a)

