(** Purely functional command line parsing.

    Here is a simple example:

    {[
      let () =
        let open Command.Let_syntax in
        Command.basic
          ~summary:"cook eggs"
          (let%map_open num_eggs =
             flag "num-eggs" (required int) ~doc:"COUNT cook this many eggs"
           and style =
             flag "style" (required (Arg_type.create Egg_style.of_string))
               ~doc:"OVER-EASY|SUNNY-SIDE-UP style of eggs"
           and recipient =
             anon ("recipient" %: string)
           in
           fun () ->
             (* TODO: implement egg-cooking in ocaml *)
             failwith "no eggs today")
        |> Command.run
    ]}

    {b Note}: {{!Core.Command.Param}[Command.Param]} has replaced
    {{!Core.Command.Spec}[Command.Spec] (DEPRECATED)} and should be used in all new
    code. *)

open! Base
open! Import

type env =
  [ `Replace of (string * string) list
  | `Extend of (string * string) list
  | `Override of (string * string option) list
  | `Replace_raw of string list
  ]

module type Version_util = sig
  module Time : sig
    type t [@@deriving sexp_of]
  end

  val version_list : string list
  val reprint_build_info : (Time.t -> Sexp.t) -> string
end

(** [For_unix] is the subset of Core's interface that [Command] needs, in particular to
    implement the [shape] and [run] functions.  [Core.Private.Command] is a functor
    taking a module matching [For_unix] and is applied in Core to construct
    [Core.Command].  We use a functor in this way so that [Command]'s internal data
    types can remain hidden. *)
module type For_unix = sig
  type env_var

  module Version_util : Version_util

  module Pid : sig
    type t

    val to_int : t -> int
  end

  module Signal : sig
    type t
  end

  module Thread : sig
    type t

    val create
      :  on_uncaught_exn:[ `Kill_whole_process | `Print_to_stderr ]
      -> ('a -> unit)
      -> 'a
      -> t

    val join : t -> unit
  end

  module Unix : sig
    module File_descr : sig
      type t
    end

    val getpid : unit -> Pid.t
    val close : ?restart:bool -> File_descr.t -> unit
    val in_channel_of_descr : File_descr.t -> In_channel.t
    val putenv : key:env_var -> data:string -> unit
    val unsetenv : env_var -> unit
    val unsafe_getenv : env_var -> string option

    type env :=
      [ `Replace of (env_var * string) list
      | `Extend of (env_var * string) list
      | `Override of (env_var * string option) list
      | `Replace_raw of string list
      ]

    val exec
      :  prog:string
      -> argv:string list
      -> ?use_path:bool
      -> ?env:env
      -> unit
      -> Nothing.t

    module Process_info : sig
      type t =
        { pid : Pid.t
        ; stdin : File_descr.t
        ; stdout : File_descr.t
        ; stderr : File_descr.t
        }
    end

    val create_process_env
      :  ?working_dir:string
      -> ?prog_search_path:string list
      -> ?argv0:string
      -> prog:string
      -> args:string list
      -> env:env
      -> unit
      -> Process_info.t

    val wait : Pid.t -> unit
  end
end

module type Enumerable_sexpable = sig
  type t [@@deriving enumerate, sexp_of]
end

module type Enumerable_stringable = sig
  type t [@@deriving enumerate]

  val to_string : t -> string
end

module type Command = sig
  module type Enumerable_sexpable = Enumerable_sexpable
  module type Enumerable_stringable = Enumerable_stringable

  (** Specifications for command-line auto-completion. *)
  module Auto_complete : sig
    (** In addition to the argument prefix, an auto-completion spec has access to any
        previously parsed arguments in the form of a heterogeneous map into which those
        arguments may register themselves by providing a [Univ_map.Multi.Key] using the
        [~key] argument to [Arg_type.create]. *)
    type t = Univ_map.t -> part:string -> string list

    module For_escape : sig
      (** For the [escape] flag, it's more useful if the auto-completion spec has access
          to all past arguments after the flag, hence the [part:string list] argument. *)
      type t = Univ_map.t -> part:string list -> string list
    end
  end

  (** Argument types. *)
  module Arg_type : sig
    (** The type of a command line argument. *)
    type 'a t

    (** An argument type includes information about how to parse values of that type from
        the command line, and (optionally) how to autocomplete partial arguments of that
        type via bash's programmable tab-completion.

        If the [of_string] function raises an exception, command line parsing will be
        aborted and the exception propagated up to top-level and printed along with
        command-line help. *)
    val create
      :  ?complete:Auto_complete.t
      -> ?key:'a Univ_map.Multi.Key.t
      -> (string -> 'a)
      -> 'a t

    (** Apply the parse function to the given string the same way it would apply to an
        argument, catching any exceptions thrown. *)
    val parse : 'a t -> string -> 'a Or_error.t

    (** Transforms the result of a [t] using [f]. *)
    val map : ?key:'b Univ_map.Multi.Key.t -> 'a t -> f:('a -> 'b) -> 'b t

    (** Defers construction of the arg type until it is needed. *)
    val of_lazy : ?key:'a Univ_map.Multi.Key.t -> 'a t Lazy.t -> 'a t

    (** An auto-completing [Arg_type] over a finite set of values. *)
    val of_map
      :  ?accept_unique_prefixes:bool
           (** Defaults to [true]. Automatically parses a prefix of a valid key if it's
          unambiguous. *)
      -> ?case_sensitive:bool
           (** Defaults to [true]. If [false], map keys must all be distinct when lowercased.*)
      -> ?list_values_in_help:bool
           (** Defaults to [true]. If you set it to false the accepted values won't be listed
          in the command help. *)
      -> ?auto_complete:Auto_complete.t
           (** Defaults to bash completion on the string prefix. This allows users to specify
          arbitrary auto-completion for [t]. *)
      -> ?key:'a Univ_map.Multi.Key.t
      -> 'a Map.M(String).t
      -> 'a t

    (** Convenience wrapper for [of_map]. Raises on duplicate keys. *)
    val of_alist_exn
      :  ?accept_unique_prefixes:bool
      -> ?case_sensitive:bool
           (** Defaults to [true]. If [false], map keys must all be distinct when lowercased.*)
      -> ?list_values_in_help:bool (** default: true *)
      -> ?auto_complete:Auto_complete.t
      -> ?key:'a Univ_map.Multi.Key.t
      -> (string * 'a) list
      -> 'a t

    (** Convenience wrapper for [of_alist_exn] to use with [ppx_enumerate] using
        [to_string]. Raises on duplicate [to_string]ed values. *)
    val enumerated
      :  ?accept_unique_prefixes:bool
      -> ?case_sensitive:bool
           (** Defaults to [true]. If [false], map keys must all be distinct when lowercased.*)
      -> ?list_values_in_help:bool (** default: true *)
      -> ?auto_complete:Auto_complete.t
      -> ?key:'a Univ_map.Multi.Key.t
      -> (module Enumerable_stringable with type t = 'a)
      -> 'a t

    (** Convenience wrapper for [of_alist_exn] to use with [ppx_enumerate] using
        [sexp_of_t] to turn the value into a string. Raises on duplicate [to_string]ed
        values. *)
    val enumerated_sexpable
      :  ?accept_unique_prefixes:bool
      -> ?case_sensitive:bool
           (** Defaults to [true]. If [false], map keys must all be distinct when lowercased.*)
      -> ?list_values_in_help:bool (** default: true *)
      -> ?auto_complete:Auto_complete.t
      -> ?key:'a Univ_map.Multi.Key.t
      -> (module Enumerable_sexpable with type t = 'a)
      -> 'a t

    (** [comma_separated t] accepts comma-separated lists of arguments parsed by [t].

        If [strip_whitespace = true], whitespace is stripped from each comma-separated
        string before it is parsed by [t].

        If [allow_empty = true] then the empty string (or just whitespace, if
        [strip_whitespace = true]) results in an empty list, and if [allow_empty = false]
        then the empty string will fail to parse. (Note that there is currently no way for
        [comma_separated] to produce a list whose only element is the empty string.)

        If [unique_values = true] no autocompletion will be offered for arguments already
        supplied in the fragment to complete. *)
    val comma_separated
      :  ?allow_empty:bool (** default: [false] *)
      -> ?key:'a list Univ_map.Multi.Key.t
      -> ?strip_whitespace:bool (** default: [false] *)
      -> ?unique_values:bool (** default: [false] *)
      -> 'a t
      -> 'a list t

    (** Values to include in other namespaces. *)
    module Export : sig
      val string : string t

      (** Beware that an anonymous argument of type [int] cannot be specified as negative,
          as it is ambiguous whether -1 is a negative number or a flag.  (The same applies
          to [float], [time_span], etc.)  You can use the special built-in "-anon" flag to
          force a string starting with a hyphen to be interpreted as an anonymous argument
          rather than as a flag, or you can just make it a parameter to a flag to avoid the
          issue. *)
      val int : int t

      val char : char t
      val float : float t
      val bool : bool t
      val sexp : Sexp.t t
      val sexp_conv : ?complete:Auto_complete.t -> (Sexp.t -> 'a) -> 'a t
    end

    val auto_complete : _ t -> Auto_complete.t
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

    (** [one_or_more_as_pair] flags must be passed one or more times. *)
    val one_or_more_as_pair : 'a Arg_type.t -> ('a * 'a list) t

    (** Like [one_or_more_as_pair], but returns the flag values as a list. *)
    val one_or_more_as_list : 'a Arg_type.t -> 'a list t

    (** [no_arg] flags may be passed at most once.  The boolean returned is true iff the
        flag is passed on the command line. *)
    val no_arg : bool t

    (** [no_arg_register ~key ~value] is like [no_arg], but associates [value] with [key]
        in the autocomplete environment. *)
    val no_arg_register : key:'a Univ_map.With_default.Key.t -> value:'a -> bool t

    (** [no_arg_some value] is like [no_arg], but will return [Some value] if the flag is
        passed on the command line, and return [None] otherwise. *)
    val no_arg_some : 'a -> 'a option t

    (** [no_arg_required value] is like [no_arg], but the argument is required.
        This is useful in combination with [choose_one_non_optional]. *)
    val no_arg_required : 'a -> 'a t

    (** [no_arg_abort ~exit] is like [no_arg], but aborts command-line parsing by calling
        [exit].  This flag type is useful for "help"-style flags that just print something
        and exit. *)
    val no_arg_abort : exit:(unit -> Nothing.t) -> unit t

    (** [escape] flags may be passed at most once.  They cause the command line parser to
        abort and pass through all remaining command line arguments as the value of the
        flag.

        A standard choice of flag name to use with [escape] is ["--"]. *)
    val escape : string list option t

    val escape_with_autocomplete
      :  complete:Auto_complete.For_escape.t
      -> string list option t

    (** [map_flag flag ~f] transforms the parsed result of [flag] by applying [f]. *)
    val map_flag : 'a t -> f:('a -> 'b) -> 'b t
  end

  (** Anonymous command-line argument specification. *)
  module Anons : sig
    (** A specification of some number of anonymous arguments. *)

    type +'a t

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

    val non_empty_sequence_as_list : 'a t -> 'a list t

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

    (** [map_anons anons ~f] transforms the parsed result of [anons] by applying [f]. *)
    val map_anons : 'a t -> f:('a -> 'b) -> 'b t
  end

  (** Command-line parameter specification.

      This module replaces {{!Core.Command.Spec}[Command.Spec]}, and should be used
      in all new code.  Its types and compositional rules are much easier to
      understand. *)
  module Param : sig
    module type S = sig
      type +'a t

      (** [Command.Param] is intended to be used with the [[%map_open]] syntax defined in
          [ppx_let], like so:
          {[
            let command =
              Command.basic ~summary:"..."
                (let%map_open count = anon ("COUNT" %: int)
                 and port = flag "port" (optional int) ~doc:"N listen on this port"
                 and person = person_param
                 in
                 (* ... Command-line validation code, if any, goes here ... *)
                 fun () ->
                   (* The body of the command *)
                   do_stuff count port person)
          ]}

          One can also use [[%map_open]] to define composite command line parameters, like
          [person_param] in the previous snippet:

          {[
            type person = { name : string; age : int }

            let person_param : person Command.Param.t =
              (let%map_open name =
                 flag "name" (required string) ~doc:"X name of the person"
               and age = flag "age" (required int) ~doc:"N how many years old"
               in
               {name; age})
          ]}

          The right-hand sides of [[%map_open]] definitions have [Command.Param] in scope.

          See example/command/main.ml for more examples.
      *)
      include Applicative.S with type 'a t := 'a t

      (** {2 Various internal values} *)

      (** The help text for the command. *)
      val help : string Lazy.t t

      (** The subcommand path of the command. *)
      val path : string list t

      (** The arguments passed to the command. *)
      val args : string list t

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
        :  ?aliases:string list
        -> ?full_flag_required:unit
        -> string
        -> 'a Flag.t
        -> doc:string
        -> 'a t

      (** [flag_optional_with_default_doc name arg_type sexp_of_default ~default ~doc] is
          a shortcut for [flag], where:
          + The [Flag.t] is [optional_with_default default arg_type]
          + The [doc] is passed through with an explanation of what the default value
          appended. *)
      val flag_optional_with_default_doc
        :  ?aliases:string list
        -> ?full_flag_required:unit
        -> string
        -> 'a Arg_type.t
        -> ('a -> Sexp.t)
        -> default:'a
        -> doc:string
        -> 'a t

      (** [anon spec] specifies a command that, among other things, takes the anonymous
          arguments specified by [spec]. *)
      val anon : 'a Anons.t -> 'a t

      (** [escape_anon ~final_anon] parses [anon] and then stops parsing. Remaining
          command line arguments are collected in the [string list], even if they start
          with dashes.

          See [escape] for the flag version of this behavior.

          The final anon is required to indicate when to stop parsing. *)
      val escape_anon : final_anon:'a Anons.t -> ('a * string list) t

      module If_nothing_chosen : sig
        type (_, _) t =
          | Default_to : 'a -> ('a, 'a) t
          | Raise : ('a, 'a) t
          | Return_none : ('a, 'a option) t
      end

      (** [choose_one clauses ~if_nothing_chosen] expresses a sum type.  It raises if more
          than one of [clauses] is [Some _].  When [if_nothing_chosen = Raise], it also
          raises if none of [clauses] is [Some _]. *)
      val choose_one
        :  'a option t list
        -> if_nothing_chosen:('a, 'b) If_nothing_chosen.t
        -> 'b t

      (** [choose_one_non_optional clauses ~if_nothing_chosen] expresses a sum type.
          It raises if more than one of the [clauses] has any flags given on the
          command-line, and returns the value parsed from the clause that's given.

          When [if_nothing_chosen = Raise], it also raises if none of the [clauses] are
          given. *)
      val choose_one_non_optional
        :  'a t list
        -> if_nothing_chosen:('a, 'b) If_nothing_chosen.t
        -> 'b t

      (** [and_arg_names t] returns both the value of [t] and the names of the arguments
          that went into [t]. Useful for errors that reference multiple params. *)
      val and_arg_names : 'a t -> ('a * string list) t

      (** Like [and_arg_names], but asserts that there is exactly one name. *)
      val and_arg_name : 'a t -> ('a * string) t

      val arg_names : 'a t -> string list
    end

    include S (** @inline *)

    (** If [None] is returned, then the param acts as a required flag that was omitted.
        This is intended to be used with [choose_one_non_optional]. *)
    val optional_to_required : 'a option t -> 'a t

    (** Values included for convenience so you can specify all command line parameters
        inside a single local open of [Param]. *)

    module Arg_type : module type of Arg_type with type 'a t = 'a Arg_type.t
    include module type of Arg_type.Export
    include module type of Flag with type 'a t := 'a Flag.t
    include module type of Anons with type 'a t := 'a Anons.t

    (** [parse t cmdline] will attempt to parse [t] out of [cmdline]. Beware there is
        nothing stopping effectful operations in [t] from being performed. *)
    val parse : 'a t -> string list -> 'a Or_error.t
  end

  module Let_syntax : sig
    (** Substituted below. *)
    type 'a t

    val return : 'a -> 'a t

    include Applicative.Applicative_infix with type 'a t := 'a t

    module Let_syntax : sig
      (** Substituted below. *)
      type 'a t

      val return : 'a -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val both : 'a t -> 'b t -> ('a * 'b) t

      module Open_on_rhs = Param
    end
    with type 'a t := 'a Param.t
  end
  with type 'a t := 'a Param.t

  (** The old interface for command-line specifications -- {b Do Not Use}.

      This interface should not be used. See the {{!Core.Command.Param}[Param]}
      module for the new way to do things. *)
  module Spec : sig
    (** {2 Command parameters} *)

    (** Specification of an individual parameter to the command's main function. *)
    type 'a param = 'a Param.t
    [@@deprecated "[since 2019-03] use [Command.Param.t] instead"]

    include Param.S with type 'a t := 'a Param.t

    (** Superceded by [return], preserved for backwards compatibility. *)
    val const : 'a -> 'a Param.t
      [@@deprecated
        "[since 2018-10] use [Command.Param.return] instead of [Command.Spec.const]"]

    (** Superceded by [both], preserved for backwards compatibility. *)
    val pair : 'a Param.t -> 'b Param.t -> ('a * 'b) Param.t
      [@@deprecated
        "[since 2018-10] use [Command.Param.both] instead of [Command.Spec.pair]"]

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
    val ( ++ ) : ('m1, 'm2) t -> ('m2, 'm3) t -> ('m1, 'm3) t

    (** Adds a rightmost parameter onto the type of main. *)
    val ( +> ) : ('m1, 'a -> 'm2) t -> 'a Param.t -> ('m1, 'm2) t

    (** Adds a leftmost parameter onto the type of main.

        This function should only be used as a workaround in situations where the order of
        composition is at odds with the order of anonymous arguments because you're
        factoring out some common spec. *)
    val ( +< ) : ('m1, 'm2) t -> 'a Param.t -> ('a -> 'm1, 'm2) t

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

    (** [flags_of_args_exn args] creates a spec from [Caml.Arg.t]s, for compatibility with
        OCaml's base libraries.  Fails if it encounters an arg that cannot be converted.

        NOTE: There is a difference in side effect ordering between [Caml.Arg] and
        [Command].  In the [Arg] module, flag handling functions embedded in [Caml.Arg.t]
        values will be run in the order that flags are passed on the command line.  In the
        [Command] module, using [flags_of_args_exn flags], they are evaluated in the order
        that the [Caml.Arg.t] values appear in [args].  *)
    val flags_of_args_exn
      :  (Stdlib.Arg.key * Stdlib.Arg.spec * Stdlib.Arg.doc) list
      -> ('a, 'a) t
      [@@deprecated "[since 2018-10] switch to Command.Param"]

    (** A specification of some number of anonymous arguments. *)
    type 'a anons = 'a Anons.t

    include module type of Anons with type 'a t := 'a anons

    (** Conversions to and from new-style [Param] command line specifications. *)

    val to_param : ('a, 'r) t -> 'a -> 'r Param.t
    val of_param : 'r Param.t -> ('r -> 'm, 'm) t
  end

  (** Commands which can be combined into a hierarchy of subcommands. *)
  type t

  type ('main, 'result) basic_spec_command =
    summary:string
    -> ?readme:(unit -> string)
    -> ('main, unit -> 'result) Spec.t
    -> 'main
    -> t

  (** [basic_spec ~summary ?readme spec main] is a basic command that executes a function
      [main] which is passed parameters parsed from the command line according to [spec].
      [summary] is to contain a short one-line description of its behavior.  [readme] is to
      contain any longer description of its behavior that will go on that command's help
      screen. *)
  val basic_spec : ('main, unit) basic_spec_command

  type 'result basic_command =
    summary:string -> ?readme:(unit -> string) -> (unit -> 'result) Param.t -> t

  (** Same general behavior as [basic_spec], but takes a command line specification built up
      using [Params] instead of [Spec]. *)
  val basic : unit basic_command

  (** [basic_or_error] is like [basic], except that the main function it expects may
      return an error, in which case it prints out the error message and shuts down with
      exit code 1. *)
  val basic_or_error : unit Or_error.t basic_command

  (** [group ~summary subcommand_alist] is a compound command with named subcommands, as
      found in [subcommand_alist].  [summary] is to contain a short one-line description of
      the command group.  [readme] is to contain any longer description of its behavior that
      will go on that command's help screen.

      NOTE: subcommand names containing underscores will be rejected; use dashes instead.

      [body] is called when no additional arguments are passed -- in particular, when no
      subcommand is passed.  Its [path] argument is the subcommand path by which the group
      command was reached. *)
  val group
    :  summary:string
    -> ?readme:(unit -> string)
    -> ?preserve_subcommand_order:unit
    -> ?body:(path:string list -> unit)
    -> (string * t) list
    -> t

  (** [lazy_group] is the same as [group], except that the list of subcommands may be
      generated lazily. *)
  val lazy_group
    :  summary:string
    -> ?readme:(unit -> string)
    -> ?preserve_subcommand_order:unit
    -> ?body:(path:string list -> unit)
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
    :  summary:string
    -> ?readme:(unit -> string)
    -> ?child_subcommand:string list
    -> ?env:env
    -> path_to_exe:
         [ `Absolute of string
         | `Relative_to_argv0 of string
         | `Relative_to_me of string
         ]
    -> unit
    -> t

  (** [of_lazy thunk] constructs a lazy command that is forced only when necessary to run it
      or extract its shape. *)
  val of_lazy : t Lazy.t -> t

  (** Extracts the summary string for a command. *)
  val summary : t -> string

  module Shape = Shape

  (** call this instead of [Core.exit] if in command-related code that you want to run in
      tests.  For example, in the body of [Command.Param.no_arg_abort] *)
  val exit : int -> _

  module For_telemetry : sig
    (** Returns the command and the list of subcommands. Arguments to the [Command.t] are
        not included.

        If a subcommand was specified as a unique prefix [normalized_path] will contain
        the full subcommand name.

        If the entry point to the program was not through [Command] returns [None].
    *)
    val normalized_path : unit -> string list option

    (** Returns the full list of arguments that were parsed by [Command].

        If a flag name was specified as a unique prefix [normalized_args] will contain the
        full flag name.

        If the entry point to the program was not through [Command] returns [None].
    *)
    val normalized_args : unit -> string list option
  end

  (** [Deprecated] should be used only by [Deprecated_command].  At some point
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

    val get_flag_names : t -> string list
  end

  (** Deprecated values that moved from [Core.Command] to [Command_unix]. *)

  val run : [ `Use_Command_unix ] [@@deprecated "[since 2021-03] Use [Command_unix]"]

  module Path : sig end [@@deprecated "[since 2021-03] Use [Command_unix]"]

  val shape : [ `Use_Command_unix ] [@@deprecated "[since 2021-03] Use [Command_unix]"]

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    val abs_path : dir:string -> string -> string
    val word_wrap : string -> int -> string list

    module Anons : sig
      val normalize : string -> string
    end

    module Path : sig
      type t

      val empty : t
      val create : path_to_exe:string -> t
      val append : t -> subcommand:string -> t
      val parts : t -> string list
      val replace_first : t -> from:string -> to_:string -> t
      val to_string : t -> string
      val to_string_dots : t -> string
    end

    module Cmdline : sig
      type t [@@deriving compare]

      val of_list : string list -> t
      val extend : t -> extend:(string list -> string list) -> path:Path.t -> t
    end

    module Spec : sig
      val flags_of_args_exn : (string * Stdlib.Arg.spec * string) list -> ('a, 'a) Spec.t
      val to_string_for_choose_one : _ Param.t -> string
    end

    module For_unix (M : For_unix with type env_var := string) : sig
      val shape : t -> Shape.t

      val help_for_shape
        :  Shape.t
        -> Path.t
        -> expand_dots:bool
        -> flags:bool
        -> recursive:bool
        -> string

      val run
        :  ?add_validate_parsing_flag:bool
             (** When [add_validate_parsing_flag] is true a new flag `-validate-parsing` is
            added to all subcommands. When this flag is passed the command will exit
            immediately if parsing is succesfull and return 0. This flag does not take any
            steps to stop side effects from occurring. *)
        -> ?verbose_on_parse_error:bool
        -> ?version:string
        -> ?build_info:string
        -> ?argv:string list
        -> ?extend:(string list -> string list)
        -> ?when_parsing_succeeds:(unit -> unit)
        -> ?complete_subcommands:
             (path:string list -> part:string -> string list list -> string list option)
             (** [complete_subcommands ~path ~part options] allows users to provide a custom
            tab completion handler to an invocation of [run]. By default, completion is
            performed via standard bash completion.

            One may use this to, e.g. use a fuzzy finder for completion.

            Imagine a command whose structure is "subcommand1 subcommand2",

            [path] represents the subcommand invocations interpreted thusfar in being
            matched.

            [part] is the portion of the auto-completion that is being matched upon.

            If a user attempts to complete "exe subcommand1 s<TAB>", they will get
            ["subcommand1"] in [path] and "s" in [part].

            [options] are the valid subcommand invocations to present to the user.

            [complete_subcommands] should return the selection made, if any. *)
        -> t
        -> unit

      val deprecated_run
        :  t
        -> cmd:string
        -> args:string list
        -> is_help:bool
        -> is_help_rec:bool
        -> is_help_rec_flags:bool
        -> is_expand_dots:bool
        -> unit
    end
  end
end
