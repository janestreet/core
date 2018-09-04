open! Import
open Import_time

module Unix = Core_unix

module Command = Core_kernel.Command

include (Command
         : (module type of struct include Command end
             with module Arg_type := Command.Arg_type
             with module Deprecated := Command.Deprecated
             with module Let_syntax := Command.Let_syntax
             with module Param := Command.Param
             with module Spec := Command.Spec))

module For_unix = Private.For_unix (struct
    module Signal = Signal
    module Sys = Core_sys
    module Thread = Core_thread
    module Time = Time
    module Unix = Unix
  end)

let run = For_unix.run
let shape = For_unix.shape

module Arg_type = struct
  module Arg_type = Command.Arg_type
  include (Arg_type
           : (module type of struct include Arg_type end
               with module Export := Arg_type.Export))

  let file ?key of_string =
    create ?key of_string ~complete:(fun _ ~part ->
      let completions =
        (* `compgen -f` handles some fiddly things nicely, e.g. completing "foo" and
           "foo/" appropriately. *)
        let command = sprintf "bash -c 'compgen -f %s'" part in
        let chan_in = Unix.open_process_in command in
        let completions = In_channel.input_lines chan_in in
        ignore (Unix.close_process_in chan_in);
        List.map (List.sort ~compare:String.compare completions) ~f:(fun comp ->
          if Caml.Sys.is_directory comp
          then comp ^ "/"
          else comp)
      in
      match completions with
      | [dir] when String.is_suffix dir ~suffix:"/" ->
        (* If the only match is a directory, we fake out bash here by creating a bogus
           entry, which the user will never see - it forces bash to push the completion
           out to the slash. Then when the user hits tab again, they will be at the end
           of the line, at the directory with a slash and completion will continue into
           the subdirectory.
        *)
        [dir; dir ^ "x"]
      | _ -> completions
    )

  module Export = struct
    include Arg_type.Export

    let time               = create Time.of_string_abs
    let time_ns            = create Core_time_ns.of_string_abs
    let time_ofday         = create Time.Ofday.Zoned.of_string
    let time_ns_ofday      = create Core_time_ns.Ofday.Zoned.of_string
    let time_ofday_unzoned = create Time.Ofday.of_string
    let time_ns_ofday_unzoned = create Time_ns.Ofday.of_string
    let time_zone          = create Time.Zone.of_string
    let time_span          = create Time.Span.of_string
    let time_ns_span       = create Time_ns.Span.of_string

    let ip_address         = create Unix.Inet_addr.of_string

    let file = file Fn.id
  end
end

module Deprecated = struct
  include Command.Deprecated
  let run = For_unix.deprecated_run
end

module Param = struct
  module Param = Command.Param
  include (Param : (module type of struct include Param end
                     with module Arg_type := Param.Arg_type))
  module Arg_type = Arg_type
  include Arg_type.Export
end

module Spec = struct
  module Spec = Command.Spec
  include (Spec : (module type of struct include Spec end
                    with module Arg_type := Spec.Arg_type))
  module Arg_type = Arg_type
  include Arg_type.Export
end

module Let_syntax = struct
  module Z = Command.Let_syntax
  include (Z : (module type of struct include Z end
                 with module Let_syntax := Z.Let_syntax))

  module Let_syntax = struct
    include (Z.Let_syntax : (module type of struct include Z.Let_syntax end
                              with module Open_on_rhs := Z.Let_syntax.Open_on_rhs))
    module Open_on_rhs = Param
  end
end
