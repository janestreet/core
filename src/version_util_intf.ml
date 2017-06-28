open! Import
open Import_time

module Version_util = Core_kernel.Version_util

module Application_specific_fields = Version_util.Application_specific_fields

module type Version_util = sig

  include (module type of struct include Version_util end
            with module Application_specific_fields := Application_specific_fields)

  module Application_specific_fields : sig
    include module type of struct include Application_specific_fields end

    (** [putenv t] stores [t] in the process environment so that build_info.sh will see
        it.  One calls [putenv t] in a program before calling OMake to set the appropriate
        environment variable so that the application-specific fields in the program being
        compiled will have value [t].  That is, one calls [putenv] in the program building
        the application and [Version_util.application_specific_fields] in the application
        itself. *)
    val putenv : t -> unit
  end

  val build_time : Time.t option
end
