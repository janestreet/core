(** purely functional command line parsing *)

open Std_internal

(** composable command-line specifications *)
module Spec : sig

  (** {1 command parameters} *)

  (** specification of an individual parameter to the command's main function *)
  type 'a param

  (** a hard-coded parameter *)
  val const : 'a -> 'a param

  (** parameter transformation *)
  val map : 'a param -> f:('a -> 'b) -> 'b param

  (** {2 various internal values} *)

  val help : string Lazy.t param (** the help text for the command *)
  val path : string list param   (** the subcommand path of the command *)
  val args : string list param   (** the arguments passed to the command *)

  (** {1 command specifications} *)

  (** composable command-line specifications *)
  type ('main_in, 'main_out) t
  (**
      Ultimately one forms a base command by combining a spec of type [('main, unit) t]
      with a main function of type ['main]; see the [basic] function below.  Combinators
      in this library incrementally build up the type of main according to what
      command-line parameters it expects, so the resulting type of [main] is something
      like:

      [arg1 -> ... -> argN -> unit]

      It may help to think of [('a, 'b) t] as a function space ['a -> 'b] embellished with
      information about:

      {ul {- how to parse command line}
          {- what the command does and how to call it}
          {- how to auto-complete a partial command line}}

      One can view a value of type [('main_in, 'main_out) t] as function that transforms a
      main function from type ['main_in] to ['main_out], typically by supplying some
      arguments.  E.g. a value of type [Spec.t] might have type:

     {[
       (arg1 -> ... -> argN -> 'r, 'r) Spec.t
     ]}

      Such a value can transform a main function of type [arg1 -> ... -> argN -> 'r] by
      supplying it argument values of type [arg1], ..., [argn], leaving a main function
      whose type is ['r].  In the end, [Command.basic] takes a completed spec where ['r =
      unit], and hence whose type looks like:

      {[
        (arg1 -> ... -> argN -> unit, unit) Spec.t
     ]}

      A value of this type can fully apply a main function of type [arg1 -> ... -> argN
      -> unit] to all its arguments.

      The view of [('main_in, main_out) Spec.t] as a function from ['main_in] to
      ['main_out] is directly reflected by the [step] function, whose type is:

      {[
        val step : ('m1 -> 'm2) -> ('m1, 'm2) t
     ]}
  *)

  (** [spec1 ++ spec2 ++ ... ++ specN] composes spec1 through specN.

      For example, if [spec_a] and [spec_b] have types:

      {[
        spec_a: (a1 -> ... -> aN -> 'ra, 'ra) Spec.t
        spec_b: (b1 -> ... -> bM -> 'rb, 'rb) Spec.t
      ]}

      then [spec_a ++ spec_b] has the following type:

      {[
        (a1 -> ... -> aN -> b1 -> ... -> bM -> 'rb, 'rb) Spec.t
      ]}

      So, [spec_a ++ spec_b] transforms a main function it by first supplying [spec_a]'s
      arguments of type [a1], ..., [aN], and then supplying [spec_b]'s arguments of type
      [b1], ..., [bm].

      One can understand [++] as function composition by thinking of the type of specs
      as concrete function types, representing the transformation of a main function:

      {[
        spec_a: \/ra. (a1 -> ... -> aN -> 'ra) -> 'ra
        spec_b: \/rb. (b1 -> ... -> bM -> 'rb) -> 'rb
      ]}

      Under this interpretation, the composition of [spec_a] and [spec_b] has type:

      {[
        spec_a ++ spec_b : \/rc. (a1 -> ... -> aN -> b1 -> ... -> bM -> 'rc) -> 'rc
      ]}

      And the implementation is just function composition:

      {[
        sa ++ sb = fun main -> sb (sa main)
      ]}
  *)

  (** the empty command-line spec *)
  val empty : ('m, 'm) t

  (** command-line spec composition *)
  val (++) : ('m1, 'm2) t -> ('m2, 'm3) t -> ('m1, 'm3) t

  (** add a rightmost parameter onto the type of main *)
  val (+>) : ('m1, 'a -> 'm2) t -> 'a param -> ('m1, 'm2) t

  (** add a leftmost parameter onto the type of main *)
  val (+<) : ('m1, 'm2) t -> 'a param -> ('a -> 'm1, 'm2) t
    (** this function should only be used as a workaround in situations where the
        order of composition is at odds with the order of anonymous arguments due
        to factoring out some common spec *)

  (** combinator for patching up how parameters are obtained or presented *)
  val step : ('m1 -> 'm2) -> ('m1, 'm2) t
  (** Here are a couple examples of some of its many uses
      {ul
        {li {i introducing labeled arguments}
            {v step (fun m v -> m ~foo:v)
               +> flag "-foo" no_arg : (foo:bool -> 'm, 'm) t v}}
        {li {i prompting for missing values}
            {v step (fun m user -> match user with
                 | Some user -> m user
                 | None -> print_string "enter username: "; m (read_line ()))
               +> flag "-user" (optional string) ~doc:"USER to frobnicate"
               : (string -> 'm, 'm) t v}}
      }

      A use of [step] might look something like:

      {[
        step (fun main -> let ... in main x1 ... xN) : (arg1 -> ... -> argN -> 'r, 'r) t
      ]}

      Thus, [step] allows one to write arbitrary code to decide how to transform a main
      function.  As a simple example:

      {[
        step (fun main -> main 13.) : (float -> 'r, 'r) t
      ]}

      This spec is identical to [const 13.]; it transforms a main function by supplying
      it with a single float argument, [13.].  As another example:

      {[
        step (fun m v -> m ~foo:v) : (foo:'foo -> 'r, 'foo -> 'r) t
      ]}

      This spec transforms a main function that requires a labeled argument into
      a main function that requires the argument unlabeled, making it easily composable
      with other spec combinators. *)

  (** combinator for defining a class of commands with common behavior *)
  val wrap : (run:('m1 -> 'r1) -> main:'m2 -> 'r2) -> ('m1, 'r1) t -> ('m2, 'r2) t
  (** Here are two examples of command classes defined using [wrap]
      {ul
        {li {i print top-level exceptions to stderr}
            {v wrap (fun ~run ~main ->
                 Exn.handle_uncaught ~exit:true (fun () -> run main)
               ) : ('m, unit) t -> ('m, unit) t
             v}}
        {li {i iterate over lines from stdin}
            {v wrap (fun ~run ~main ->
                 In_channel.iter_lines stdin ~f:(fun line -> run (main line))
               ) : ('m, unit) t -> (string -> 'm, unit) t
             v}}
      }
  *)

  (** {1 argument types} *)

  module Arg_type : sig
    type 'a t (** the type of a command line argument *)

    (** an argument type includes information about how to parse values of that type from
        the command line, and (optionally) how to auto-complete partial arguments of that
        type via bash's programmable TAB-completion.  In addition to the argument prefix,
        autocompletion also has access to any previously parsed arguments in the form of a
        heterogeneous map into which previously parsed arguments may register themselves by
        providing a Univ_map.Key using the ~key argument to [create].

        If the [of_string] function raises an exception, command line parsing will be
        aborted and the exception propagated up to top-level and printed along with
        command-line help.
    *)
    val create
      :  ?complete:(Univ_map.t -> part:string -> string list)
      -> ?key:'a Univ_map.Multi.Key.t
      -> (string -> 'a)
      -> 'a t

    (** an auto-completing Arg_type over a finite set of values *)
    val of_map : ?key:'a Univ_map.Multi.Key.t -> 'a String.Map.t -> 'a t

    (** convenience wrapper for [of_map].  Raises on duplicate keys *)
    val of_alist_exn : ?key:'a Univ_map.Multi.Key.t -> (string * 'a) list -> 'a t
  end

  val string    : string Arg_type.t
  val int       : int    Arg_type.t
  val float     : float  Arg_type.t
  val bool      : bool   Arg_type.t
  val date      : Date.t Arg_type.t
  val time_span : Span.t Arg_type.t
  val file      : string Arg_type.t (* with bash autocompletion *)


  (** {1 flag specifications} *)

  type 'a flag (** a flag specification *)

  (** [flag name spec ~doc] specifies a command that, among other things, takes a flag
      named [name] on its command line.  [doc] indicates the meaning of the flag.

      NOTE: the [doc] for a flag which takes an argument should be of the form [arg_name ^
      " " ^ description] where [arg_name] describes the argument and [description]
      describes the meaning of the flag.

      NOTE: flag names (including aliases) containing underscores will
            be rejected.  Use dashes instead.
  *)
  val flag : ?aliases:string list -> string -> 'a flag -> doc:string -> 'a param

  (** required flags must be passed exactly once *)
  val required : 'a Arg_type.t -> 'a flag

  (** optional flags may be passed at most once *)
  val optional : 'a Arg_type.t -> 'a option flag

  (** [optional_with_default] flags may be passed at most once, and
      default to a given value *)
  val optional_with_default : 'a -> 'a Arg_type.t -> 'a flag

  (** [listed] flags may be passed zero or more times *)
  val listed : 'a Arg_type.t -> 'a list flag

  (** [no_arg] flags may be passed at most once.  The boolean returned
      is true iff the flag is passed on the command line *)
  val no_arg : bool flag

  (** [no_arg_register ~key ~value] is like [no_arg], but associates [value]
      with [key] in the in the auto-completion environment *)
  val no_arg_register : key:'a Univ_map.With_default.Key.t -> value:'a -> bool flag

  (** [no_arg_abort ~exit] is like [no_arg], but aborts command-line parsing
      by calling [exit].  This flag type is useful for "help"-style flags that
      just print something and exit. *)
  val no_arg_abort : exit:(unit -> never_returns) -> unit flag

  (** [escape] flags may be passed at most once.  They cause the command
      line parser to abort and pass through all remaining command line
      arguments as the value of the flag. *)
  val escape : string list option flag

  (** [flags_of_args_exn args] creates a spec from [Arg.t]s, for compatibility with
      ocaml's base libraries.  Fails if it encounters an arg that cannot be converted.

      NOTE: There is a difference in side effect ordering between [Arg] and [Command].  In
      the [Arg] module, flag handling functions embedded in [Arg.t] values will be run in
      the order that flags are passed on the command line.  In the [Command] module, using
      [flags_of_args_exn flags], they are evaluated in the order that the [Arg.t] values
      appear in [flags].  *)
  val flags_of_args_exn : Core_arg.t list -> ('a, 'a) t

  (** {1 anonymous argument specifications} *)

  type 'a anons (** a specification of some number of anonymous arguments *)

  (** [anon spec] specifies a command that, among other things, takes
      the anonymous arguments specified by [spec]. *)
  val anon : 'a anons -> 'a param

  (** [(name %: typ)] specifies a required anonymous argument of type [typ].
      The [name] is mentioned in the generated help for the command. *)
  val (%:) : string -> 'a Arg_type.t -> 'a anons

  (** [sequence anons] specifies a sequence of anonymous arguments.  An exception
      will be raised if [anons] matches anything other than a fixed number of
      anonymous arguments  *)
  val sequence : 'a anons -> 'a list anons

  (** [(maybe anons)] indicates that some anonymous arguments are optional *)
  val maybe : 'a anons -> 'a option anons

  (** [(maybe_with_default default anons)] indicates an optional anonymous
      argument with a default value *)
  val maybe_with_default : 'a -> 'a anons -> 'a anons

  (** [t2], [t3], and [t4] each concatenate multiple anonymous argument
      specs into a single one. The purpose of these combinators is to allow
      for optional sequences of anonymous arguments.  Consider a command with
      usage:

      {v
        main.exe FOO [BAR BAZ]
       v}

      where the second and third anonymous arguments must either both
      be there or both not be there.  This can be expressed as:

      {[
        t2 ("FOO" %: foo) (maybe (t2 ("BAR" %: bar) ("BAZ" %: baz)))]
       ]}

      Sequences of 5 or more anonymous arguments can be built up using
      nested tuples:

      {[
        maybe (t3 a b (t3 c d e))
      ]}
  *)

  val t2 : 'a anons -> 'b anons -> ('a * 'b) anons

  val t3 : 'a anons -> 'b anons -> 'c anons -> ('a * 'b * 'c) anons

  val t4 : 'a anons -> 'b anons -> 'c anons -> 'd anons -> ('a * 'b * 'c * 'd) anons

end

type t (** commands which can be combined into a hierarchy of subcommands *)

(** [basic ~summary ?readme spec main] is a basic command that executes a function [main]
    which is passed parameters parsed from the command line according to [spec]. [summary]
    is to contain a short one-line description of its behavior.  [readme] is to contain
    any longer description of its behavior that will go on that commands' help screen. *)
val basic
  :  summary:string
  -> ?readme:(unit -> string)
  -> ('main, unit -> unit) Spec.t
  -> 'main
  -> t

(** [group ~summary subcommand_alist] is a compound command with named
    subcommands, as found in [subcommand_alist].  [summary] is to contain
    a short one-line description of the command group.  [readme] is to
    contain any longer description of its behavior that will go on that
    command's help screen.

    NOTE: subcommand names containing underscores will be rejected.  Use dashes instead.
*)
val group : summary:string -> ?readme:(unit -> string) -> (string * t) list -> t

(** Run a command against [Sys.argv], or [argv] if it is specified.

    [extend] can be used to add extra command line arguments to basic subcommands of the
    command.  [extend] will be passed the (fully expanded) path to a command, and its
    output will be appended to the list of arguments being processed.  For example,
    suppose a program like this is compiled into [exe]:

      {[
        let bar = Command.basic ...
        let foo = Command.group ~summary:... ["bar", bar]
        let main = Command.group ~summary:... ["foo", foo]
        Command.run ~extend:(fun _ -> ["-baz"]) main
      ]}

    Then if a user ran [exe f b], [extend] would be passed [["foo"; "bar"]] and ["-baz"]
    would be appended to the command line for processing by [bar].  This can be used to
    add a default flags section to a user config file.
*)
val run
  :  ?version:string
  -> ?build_info:string
  -> ?argv:string list
  -> ?extend:(string list -> string list)
  -> t
  -> unit

(** [Deprecated] should be used only by [Core_extended.Deprecated_command].  At some point
    it will go away. *)
module Deprecated : sig
  module Spec : sig
    val no_arg : hook:(unit -> unit) -> bool Spec.flag
    val escape : hook:(string list -> unit) -> string list option Spec.flag
    val ad_hoc : usage_arg:string -> string list Spec.anons
  end

  val summary : t -> string

  val help_recursive
    :  cmd:string
    -> with_flags:bool
    -> expand_dots:bool
    -> t
    -> string
    -> (string * string) list

  val run
    :  t
    -> cmd:string
    -> args:string list
    -> is_help:bool
    -> is_help_rec:bool
    -> is_help_rec_flags:bool
    -> is_expand_dots:bool
    -> unit

  val get_flag_names : t ->  string list
  val version : string
  val build_info : string
end
