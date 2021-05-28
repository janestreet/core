
let cwd = Core_unix.getcwd ()

let%bench_fun "mkdir_p" =
  fun () -> Core_unix.mkdir_p cwd
