include module type of struct
  include Core_kernel.Filename
end

val arg_type : [ `Use_Filename_unix ] [@@deprecated "[since 2021-04] Use [Filename_unix]"]

val create_arg_type : [ `Use_Filename_unix ]
[@@deprecated "[since 2021-04] Use [Filename_unix]"]

val open_temp_file : [ `Use_Filename_unix ]
[@@deprecated "[since 2021-04] Use [Filename_unix]"]

val open_temp_file_fd : [ `Use_Filename_unix ]
[@@deprecated "[since 2021-04] Use [Filename_unix]"]

val realpath : [ `Use_Filename_unix ] [@@deprecated "[since 2021-04] Use [Filename_unix]"]
val temp_dir : [ `Use_Filename_unix ] [@@deprecated "[since 2021-04] Use [Filename_unix]"]

val temp_file : [ `Use_Filename_unix ]
[@@deprecated "[since 2021-04] Use [Filename_unix]"]
