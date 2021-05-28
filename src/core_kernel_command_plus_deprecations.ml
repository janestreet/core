include Core_kernel.Command

let run = `Use_Command_unix
let shape = `Use_Command_unix

module Path = struct end

module Shape = struct
  include Shape

  let help_text = `Use_Command_unix
end
