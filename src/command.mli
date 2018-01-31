(** Purely functional command line parsing.

    Here is a simple example:

    {[
      let () =
        let open Command.Let_syntax in
        Command.basic
          ~summary:"cook eggs"
          [%map_open
            let num_eggs =
              flag "num-eggs" (required int) ~doc:"COUNT cook this many eggs"
            and style =
              flag "style" (required (Arg_type.create Egg_style.of_string))
                ~doc:"OVER-EASY|SUNNY-SIDE-UP style of eggs"
            and recipient =
              anon ("recipient" %: string)
            in
            fun () ->
              (* TODO: implement egg-cooking in ocaml *)
              failwith "no eggs today"
          ]
        |> Command.run
    ]}

    {b Note}: {{!Core.Command.Param}[Command.Param]} has replaced
    {{!Core.Command.Spec}[Command.Spec] (DEPRECATED)} and should be used in all new code.
*)

open! Import
open Import_time

(** Argument types. *)
module Arg_type : sig
  type 'a t (** The type of a command line argument. *)

  (** An argument type includes information about how to parse values of that type from
      the command line, and (optionally) how to autocomplete partial arguments of that
      type via bash's programmable tab-completion. In addition to the argument prefix,
      autocompletion also has access to any previously parsed arguments in the form of a
      heterogeneous map into which previously parsed arguments may register themselves by
      providing a [Univ_map.Key] using the [~key] argument to [create].

      If the [of_string] function raises an exception, command line parsing will be
      aborted and the exception propagated up to top-level and printed along with
      command-line help. *)
  val create
    :  ?complete:(Univ_map.t -> part:string -> string list)
    -> ?key:'a Univ_map.Multi.Key.t
    -> (string -> 'a)
    -> 'a t

  (** Transforms the result of a [t] using [f]. *)
  val map : ?key:'b Univ_map.Multi.Key.t -> 'a t -> f:('a -> 'b) -> 'b t

  (** An auto-completing [Arg_type] over a finite set of values. *)
  val of_map : ?key:'a Univ_map.Multi.Key.t -> 'a String.Map.t -> 'a t

  (** Convenience wrapper for [of_map]. Raises on duplicate keys. *)
  val of_alist_exn : ?key:'a Univ_map.Multi.Key.t -> (string * 'a) list -> 'a t

  (** [file] defines an [Arg_type.t] that completes in the same way as
      [Command.Spec.file], but perhaps with a different type than [string] or with an
      autocompletion key. *)
  val file
    :  ?key:'a Univ_map.Multi.Key.t
    -> (string -> 'a)
    -> 'a t

  (** [comma_separated t] accepts comma-separated lists of arguments parsed by [t].  If
      [strip_whitespace = true], whitespace is stripped from each comma-separated string
      before it is parsed by [t].  The empty string (or just whitespace, if
      [strip_whitespace = true]) results in an empty list.  If [unique_values = true] no
      autocompletion will be offered for arguments already supplied in the fragment to
      complete. *)
  val comma_separated
    :  ?key              : 'a list Univ_map.Multi.Key.t
    -> ?strip_whitespace : bool (** default: [false] *)
    -> ?unique_values    : bool (** default: [false] *)
    -> 'a t
    -> 'a list t

  (** Values to include in other namespaces. *)
  module Export : sig

    val string             : string             t

    (** Beware that an anonymous argument of type [int] cannot be specified as negative,
        as it is ambiguous whether -1 is a negative number or a flag.  (The same applies
        to [float], [time_span], etc.)  You can use the special built-in "-anon" flag to
        force a string starting with a hyphen to be interpreted as an anonymous argument
        rather than as a flag, or you can just make it a parameter to a flag to avoid the
        issue. *)
    val int                : int                t
    val char               : char               t
    val float              : float              t
    val bool               : bool               t
    val date               : Date.t             t
    val percent            : Percent.t          t
    val time               : Time.t             t

    (** Requires a time zone. *)
    val time_ofday         : Time.Ofday.Zoned.t t

    (** For when the time zone is implied. *)
    val time_ofday_unzoned : Time.Ofday.t       t

    val time_zone          : Time.Zone.t        t
    val time_span          : Time.Span.t        t

    (** Uses bash autocompletion. *)
    val file               : string             t

    val host_and_port      : Host_and_port.t    t
    val ip_address         : Unix.inet_addr     t
    val sexp               : Sexp.t             t

    val sexp_conv          : (Sexp.t -> 'a) -> 'a t
  end
end

(** Command-line flag specifications. *)
module Flag : sig
  type 'a t

  (** Required flags must be passed exactly once. *)
  val required : 'a Arg_type.t -> 'a t

  (** Optional flags may be passed at most once. *)
  val optional : 'a Arg_type.t -> 'a option t

  (** [optional_with_default] flags may be passed at most once, and default to a given
      value. *)
  val optional_with_default : 'a -> 'a Arg_type.t -> 'a t

  (** [listed] flags may be passed zero or more times. *)
  val listed : 'a Arg_type.t -> 'a list t

  (** [one_or_more] flags must be passed one or more times. *)
  val one_or_more : 'a Arg_type.t -> ('a * 'a list) t

  (** [no_arg] flags may be passed at most once.  The boolean returned is true iff the
      flag is passed on the command line. *)
  val no_arg : bool t

  (** [no_arg_register ~key ~value] is like [no_arg], but associates [value] with [key] in
      the autocomplete environment. *)
  val no_arg_register : key:'a Univ_map.With_default.Key.t -> value:'a -> bool t

  (** [no_arg_abort ~exit] is like [no_arg], but aborts command-line parsing by calling
      [exit].  This flag type is useful for "help"-style flags that just print something
      and exit. *)
  val no_arg_abort : exit:(unit -> never_returns) -> unit t

  (** [escape] flags may be passed at most once.  They cause the command line parser to
      abort and pass through all remaining command line arguments as the value of the
      flag.

      A standard choice of flag name to use with [escape] is ["--"]. *)
  val escape : string list option t
end

(** Anonymous command-line argument specification. *)
module Anons : sig

  type +'a t (** A specification of some number of anonymous arguments. *)

  (** [(name %: typ)] specifies a required anonymous argument of type [typ].

      The [name] must not be surrounded by whitespace; if it is, an exn will be raised.

      If the [name] is surrounded by a special character pair (<>, \{\}, \[\] or (),)
      [name] will remain as-is, otherwise, [name] will be uppercased.

      In the situation where [name] is only prefixed or only suffixed by one of the
      special character pairs, or different pairs are used (e.g., "<ARG\]"), an exn will
      be raised.

      The (possibly transformed) [name] is mentioned in the generated help for the
      command. *)
  val ( %: ) : string -> 'a Arg_type.t -> 'a t

  (** [sequence anons] specifies a sequence of anonymous arguments.  An exception will be
      raised if [anons] matches anything other than a fixed number of anonymous arguments.
  *)
  val sequence : 'a t -> 'a list t

  (** [non_empty_sequence_as_pair anons] and [non_empty_sequence_as_list anons] are like
      [sequence anons] except that an exception will be raised if there is not at least
      one anonymous argument given. *)
  val non_empty_sequence_as_pair : 'a t -> ('a * 'a list) t
  val non_empty_sequence_as_list : 'a t ->       'a list  t

  (** [(maybe anons)] indicates that some anonymous arguments are optional. *)
  val maybe : 'a t -> 'a option t

  (** [(maybe_with_default default anons)] indicates an optional anonymous argument with a
      default value. *)
  val maybe_with_default : 'a -> 'a t -> 'a t

  (** [t2], [t3], and [t4] each concatenate multiple anonymous argument specs into a
      single one. The purpose of these combinators is to allow for optional sequences of
      anonymous arguments.  Consider a command with usage:

      {v
        main.exe FOO [BAR BAZ]
       v}

      where the second and third anonymous arguments must either both be there or both not
      be there.  This can be expressed as:

      {[
        t2 ("FOO" %: foo) (maybe (t2 ("BAR" %: bar) ("BAZ" %: baz)))]
      ]}

      Sequences of 5 or more anonymous arguments can be built up using
      nested tuples:

      {[
        maybe (t3 a b (t3 c d e))
      ]}
  *)

  val t2 : 'a t -> 'b t -> ('a * 'b) t

  val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
end


(** Command-line parameter specification.

    This module replaces {{!Core.Command.Spec}[Command.Spec]}, and should be used in all
    new code.  Its types and compositional rules are much easier to understand. *)
module Param : sig
  module type S = sig

    type +'a t

    (** [Command.Param] is intended to be used with the [[%map_open]] syntax defined in
        [ppx_let], like so:
        {[
          let command =
            Command.basic ~summary:"..."
              [%map_open
                let count  = anon ("COUNT" %: int)
                and port   = flag "port" (optional int) ~doc:"N listen on this port"
                and person = person_param
                in
                (* ... Command-line validation code, if any, goes here ... *)
                fun () ->
                  (* The body of the command *)
                  do_stuff count port person
              ]
        ]}

        One can also use [[%map_open]] to define composite command line parameters, like
        [person_param] in the previous snippet:
        {[
          type person = { name : string; age : int }

          let person_param : person Command.Param.t =
            [%map_open
              let name = flag "name" (required string) ~doc:"X name of the person"
              and age  = flag "age"  (required int)    ~doc:"N how many years old"
              in
              {name; age}
            ]
        ]}

        The right-hand sides of [[%map_open]] definitions have [Command.Param] in scope.

        Alternatively, you can say:

        {[
          let open Foo.Let_syntax in
          [%map_open
            let x ...
          ]
        ]}

        if [Foo] follows the same conventions as [Command.Param].

        See example/command/main.ml for more examples.
    *)
    include Applicative.S with type 'a t := 'a t

    (** {2 Various internal values} *)

    val help : string Lazy.t t (** The help text for the command. *)

    val path : string list   t (** The subcommand path of the command. *)

    val args : string list   t (** The arguments passed to the command. *)

    (** [flag name spec ~doc] specifies a command that, among other things, takes a flag
        named [name] on its command line.  [doc] indicates the meaning of the flag.

        All flags must have a dash at the beginning of the name.  If [name] is not
        prefixed by "-", it will be normalized to ["-" ^ name].

        Unless [full_flag_required] is used, one doesn't have to pass [name] exactly on
        the command line, but only an unambiguous prefix of [name] (i.e., a prefix which
        is not a prefix of any other flag's name).

        NOTE: the [doc] for a flag which takes an argument should be of the form
        [arg_name ^ " " ^ description] where [arg_name] describes the argument and
        [description] describes the meaning of the flag.

        NOTE: flag names (including aliases) containing underscores will be rejected.
        Use dashes instead.

        NOTE: "-" by itself is an invalid flag name and will be rejected.
    *)
    val flag
      :  ?aliases            : string list
      -> ?full_flag_required : unit
      -> string
      -> 'a Flag.t
      -> doc : string
      -> 'a t

    (** [flag_optional_with_default_doc name arg_type sexp_of_default ~default ~doc] is a
        shortcut for [flag], where:
        + The [Flag.t] is [optional_with_default default arg_type]
        + The [doc] is passed through with an explanation of what the default value
        appended. *)
    val flag_optional_with_default_doc
      :  ?aliases            : string list
      -> ?full_flag_required : unit
      -> string
      -> 'a Arg_type.t
      -> ('a -> Sexp.t)
      -> default:'a
      -> doc : string
      -> 'a t

    (** [anon spec] specifies a command that, among other things, takes the anonymous
        arguments specified by [spec]. *)
    val anon : 'a Anons.t -> 'a t

    (** [choose_one clauses ~if_nothing_chosen] expresses a sum type.  It raises if more
        than one of [clauses] is [Some _].  When [if_nothing_chosen = `Raise], it also
        raises if none of [clauses] is [Some _]. *)
    val choose_one
      :  'a option t list
      -> if_nothing_chosen:[ `Default_to of 'a | `Raise ]
      -> 'a t
  end

  include S (** @open *)

  (** Values included for convenience so you can specify all command line parameters
      inside a single local open of [Param]. *)

  module Arg_type : module type of Arg_type with type 'a t = 'a Arg_type.t
  include module type of Arg_type.Export
  include module type of Flag  with type 'a t := 'a Flag.t
  include module type of Anons with type 'a t := 'a Anons.t
end

module Let_syntax : sig

  type 'a t (** Substituted below. *)

  val return : 'a -> 'a t

  module Let_syntax : sig
    type 'a t (** Substituted below. *)

    val return : 'a -> 'a t
    val map    : 'a t -> f:('a -> 'b) -> 'b t
    val both   : 'a t -> 'b t -> ('a * 'b) t
    module Open_on_rhs = Param
  end with type 'a t := 'a Param.t
end with type 'a t := 'a Param.t

(** The old interface for command-line specifications -- {b Do Not Use}.

    This interface should not be used. See the {{!Core.Command.Param}[Param]} module for
    the new way to do things. *)
module Spec : sig

  (** {2 Command parameters} *)

  (** Specification of an individual parameter to the command's main function. *)
  type 'a param = 'a Param.t
  include Param.S with type 'a t := 'a param

  (** Superceded by [return], preserved for backwards compatibility. *)
  val const : 'a -> 'a param

  (** Superceded by [both], preserved for backwards compatibility. *)
  val pair : 'a param -> 'b param -> ('a * 'b) param

  (** {2 Command specifications} *)

  (** Composable command-line specifications. *)
  type (-'main_in, +'main_out) t
  (** Ultimately one forms a basic command by combining a spec of type [('main, unit ->
      unit) t] with a main function of type ['main]; see the [basic] function below.
      Combinators in this library incrementally build up the type of main according to
      what command-line parameters it expects, so the resulting type of [main] is
      something like:

      [arg1 -> ... -> argN -> unit -> unit]

      It may help to think of [('a, 'b) t] as a function space ['a -> 'b] embellished with
      information about:

      {ul {- how to parse the command line}
      {- what the command does and how to call it}
      {- how to autocomplete a partial command line}}

      One can view a value of type [('main_in, 'main_out) t] as a function that transforms
      a main function from type ['main_in] to ['main_out], typically by supplying some
      arguments.  E.g., a value of type [Spec.t] might have type:

      {[
        (arg1 -> ... -> argN -> 'r, 'r) Spec.t
      ]}

      Such a value can transform a main function of type [arg1 -> ... -> argN -> 'r] by
      supplying it argument values of type [arg1], ..., [argn], leaving a main function
      whose type is ['r].  In the end, [Command.basic] takes a completed spec where
      ['r = unit -> unit], and hence whose type looks like:

      {[
        (arg1 -> ... -> argN -> unit -> unit, unit -> unit) Spec.t
      ]}

      A value of this type can fully apply a main function of type [arg1 -> ... -> argN ->
      unit -> unit] to all its arguments.

      The final unit argument allows the implementation to distinguish between the phases
      of (1) parsing the command line and (2) running the body of the command.  Exceptions
      raised in phase (1) lead to a help message being displayed alongside the exception.
      Exceptions raised in phase (2) are displayed without any command line help.

      The view of [('main_in, main_out) Spec.t] as a function from ['main_in] to
      ['main_out] is directly reflected by the [step] function, whose type is:

      {[
        val step : ('m1 -> 'm2) -> ('m1, 'm2) t
      ]}
  *)

  (** [spec1 ++ spec2 ++ ... ++ specN] composes [spec1] through [specN].

      For example, if [spec_a] and [spec_b] have types:

      {[
        spec_a: (a1 -> ... -> aN -> 'ra, 'ra) Spec.t;
        spec_b: (b1 -> ... -> bM -> 'rb, 'rb) Spec.t
      ]}

      then [spec_a ++ spec_b] has the following type:

      {[
        (a1 -> ... -> aN -> b1 -> ... -> bM -> 'rb, 'rb) Spec.t
      ]}

      So, [spec_a ++ spec_b] transforms a main function by first supplying [spec_a]'s
      arguments of type [a1], ..., [aN], and then supplying [spec_b]'s arguments of type
      [b1], ..., [bm].

      One can understand [++] as function composition by thinking of the type of specs
      as concrete function types, representing the transformation of a main function:

      {[
        spec_a: \/ra. (a1 -> ... -> aN -> 'ra) -> 'ra;
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

  (** The empty command-line spec. *)
  val empty : ('m, 'm) t

  (** Command-line spec composition. *)
  val (++) : ('m1, 'm2) t -> ('m2, 'm3) t -> ('m1, 'm3) t

  (** Adds a rightmost parameter onto the type of main. *)
  val (+>) : ('m1, 'a -> 'm2) t -> 'a param -> ('m1, 'm2) t

  (** Adds a leftmost parameter onto the type of main.

      This function should only be used as a workaround in situations where the order of
      composition is at odds with the order of anonymous arguments because you're
      factoring out some common spec. *)
  val (+<) : ('m1, 'm2) t -> 'a param -> ('a -> 'm1, 'm2) t

  (** Combinator for patching up how parameters are obtained or presented.

      Here are a couple examples of some of its many uses:
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
      with other spec combinators.

  *)
  val step : ('m1 -> 'm2) -> ('m1, 'm2) t

  (** Combinator for defining a class of commands with common behavior.

      Here are two examples of command classes defined using [wrap]:
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
  val wrap : (run:('m1 -> 'r1) -> main:'m2 -> 'r2) -> ('m1, 'r1) t -> ('m2, 'r2) t

  module Arg_type : module type of Arg_type with type 'a t = 'a Arg_type.t

  include module type of Arg_type.Export


  (** A flag specification. *)
  type 'a flag = 'a Flag.t
  include module type of Flag with type 'a t := 'a flag

  (** [map_flag flag ~f] transforms the parsed result of [flag] by applying [f]. *)
  val map_flag : 'a flag -> f:('a -> 'b) -> 'b flag

  (** [flags_of_args_exn args] creates a spec from [Caml.Arg.t]s, for compatibility with
      OCaml's base libraries.  Fails if it encounters an arg that cannot be converted.

      NOTE: There is a difference in side effect ordering between [Caml.Arg] and
      [Command].  In the [Arg] module, flag handling functions embedded in [Caml.Arg.t]
      values will be run in the order that flags are passed on the command line.  In the
      [Command] module, using [flags_of_args_exn flags], they are evaluated in the order
      that the [Caml.Arg.t] values appear in [flags].  *)
  val flags_of_args_exn : Core_kernel.Arg.t list -> ('a, 'a) t

  (** A specification of some number of anonymous arguments. *)
  type 'a anons = 'a Anons.t
  include module type of Anons with type 'a t := 'a anons

  (** [map_anons anons ~f] transforms the parsed result of [anons] by applying [f]. *)
  val map_anons : 'a anons -> f:('a -> 'b) -> 'b anons

  (** Conversions to and from new-style [Param] command line specifications. *)

  val to_param : ('a, 'r) t -> 'a -> 'r Param.t
  val of_param : 'r Param.t -> ('r -> 'm, 'm) t
end

type t (** Commands which can be combined into a hierarchy of subcommands. *)

type ('main, 'result) basic_spec_command
  =  summary : string
  -> ?readme : (unit -> string)
  -> ('main, unit -> 'result) Spec.t
  -> 'main
  -> t

(** [basic_spec ~summary ?readme spec main] is a basic command that executes a function
    [main] which is passed parameters parsed from the command line according to [spec].
    [summary] is to contain a short one-line description of its behavior.  [readme] is to
    contain any longer description of its behavior that will go on that command's help
    screen. *)
val basic_spec : ('main, unit) basic_spec_command

type 'result basic_command
  =  summary : string
  -> ?readme : (unit -> string)
  -> (unit -> 'result) Param.t
  -> t

(** Same general behavior as [basic_spec], but takes a command line specification built up
    using [Params] instead of [Spec]. *)
val basic : unit basic_command

(** [group ~summary subcommand_alist] is a compound command with named subcommands, as
    found in [subcommand_alist].  [summary] is to contain a short one-line description of
    the command group.  [readme] is to contain any longer description of its behavior that
    will go on that command's help screen.

    NOTE: subcommand names containing underscores will be rejected; use dashes instead.

    [body] is called when no additional arguments are passed -- in particular, when no
    subcommand is passed.  Its [path] argument is the subcommand path by which the group
    command was reached. *)
val group
  :  summary                    : string
  -> ?readme                    : (unit -> string)
  -> ?preserve_subcommand_order : unit
  -> ?body                      : (path:string list -> unit)
  -> (string * t) list
  -> t

(** [lazy_group] is the same as [group], except that the list of subcommands may be
    generated lazily. *)
val lazy_group
  :  summary                    : string
  -> ?readme                    : (unit -> string)
  -> ?preserve_subcommand_order : unit
  -> ?body                      : (path:string list -> unit)
  -> (string * t) list Lazy.t
  -> t

(** [exec ~summary ~path_to_exe] runs [exec] on the executable at [path_to_exe]. If
    [path_to_exe] is [`Absolute path] then [path] is executed without any further
    qualification.  If it is [`Relative_to_me path] then [Filename.dirname
    Sys.executable_name ^ "/" ^ path] is executed instead.  All of the usual caveats about
    [Sys.executable_name] apply: specifically, it may only return an absolute path in
    Linux.  On other operating systems it will return [Sys.argv.(0)].  If it is
    [`Relative_to_argv0 path] then [Sys.argv.(0) ^ "/" ^ path] is executed.

    The [child_subcommand] argument allows referencing a subcommand one or more levels
    below the top-level of the child executable. It should {e not} be used to pass flags
    or anonymous arguments to the child.

    Care has been taken to support nesting multiple executables built with Command.  In
    particular, recursive help and autocompletion should work as expected.

    NOTE: Non-Command executables can be used with this function but will still be
    executed when [help -recursive] is called or autocompletion is attempted (despite the
    fact that neither will be particularly helpful in this case).  This means that if you
    have a shell script called "reboot-everything.sh" that takes no arguments and reboots
    everything no matter how it is called, you shouldn't use it with [exec].

    Additionally, no loop detection is attempted, so if you nest an executable within
    itself, [help -recursive] and autocompletion will hang forever (although actually
    running the subcommand will work). *)
val exec
  :  summary           : string
  -> ?readme           : (unit -> string)
  -> ?child_subcommand : string list
  -> path_to_exe       :
       [ `Absolute          of string
       | `Relative_to_argv0 of string
       | `Relative_to_me    of string
       ]
  -> unit
  -> t

(** [of_lazy thunk] constructs a lazy command that is forced only when necessary to run it
    or extract its shape. *)
val of_lazy : t Lazy.t -> t

(** Extracts the summary string for a command. *)
val summary : t -> string

module Shape : sig

  module Flag_info : sig

    type t = {
      name : string;
      doc : string;
      aliases : string list;
    } [@@deriving bin_io, compare, fields, sexp]

  end

  module Base_info : sig

    type grammar =
      | Zero
      | One of string
      | Many of grammar
      | Maybe of grammar
      | Concat of grammar list
      | Ad_hoc of string
    [@@deriving bin_io, compare, sexp]

    type anons =
      | Usage of string (** When exec'ing an older binary whose help sexp doesn't expose the grammar. *)
      | Grammar of grammar
    [@@deriving bin_io, compare, sexp]

    type t = {
      summary : string;
      readme  : string sexp_option;
      anons   : anons;
      flags   : Flag_info.t list;
    } [@@deriving bin_io, compare, fields, sexp]

  end

  module Group_info : sig

    type 'a t = {
      summary     : string;
      readme      : string sexp_option;
      subcommands : (string, 'a) List.Assoc.t Lazy.t;
    } [@@deriving bin_io, compare, fields, sexp]

    val map : 'a t -> f:('a -> 'b) -> 'b t

  end

  module Exec_info : sig

    type t = {
      summary          : string;
      readme           : string sexp_option;
      working_dir      : string;
      path_to_exe      : string;
      child_subcommand : string list;
    } [@@deriving bin_io, compare, fields, sexp]

  end

  type t =
    | Basic of Base_info.t
    | Group of t Group_info.t
    | Exec of Exec_info.t * (unit -> t)
    | Lazy of t Lazy.t

  (** Fully forced shapes are comparable and serializable. *)
  module Fully_forced : sig
    type shape = t

    type t =
      | Basic of Base_info.t
      | Group of t Group_info.t
      | Exec of Exec_info.t * t
    [@@deriving bin_io, compare, sexp]

    val create : shape -> t

  end with type shape := t

end

(** Exposes the shape of a command. *)
val shape : t -> Shape.t

(** Runs a command against [Sys.argv], or [argv] if it is specified.

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
  :  ?version    : string
  -> ?build_info : string
  -> ?argv       : string list
  -> ?extend     : (string list -> string list)
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
    :  cmd         : string
    -> with_flags  : bool
    -> expand_dots : bool
    -> t
    -> string
    -> (string * string) list

  val run
    :  t
    -> cmd               : string
    -> args              : string list
    -> is_help           : bool
    -> is_help_rec       : bool
    -> is_help_rec_flags : bool
    -> is_expand_dots    : bool
    -> unit

  val get_flag_names : t ->  string list
end
