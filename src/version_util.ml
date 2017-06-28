open! Import

module Upstream = Import.Version_util
module Unix = Core_unix

include (Upstream : module type of Upstream
         with module Application_specific_fields := Upstream.Application_specific_fields)

module Application_specific_fields = struct
  include Upstream.Application_specific_fields

  (* BUILD_INFO_APP_FIELDS is used by build_info.sh to determine the
     [application_specific_fields] field of the sexp that ultimately becomes [build_info].
  *)
  let putenv t =
    Unix.putenv ~key:"BUILD_INFO_APP_FIELDS" ~data:(Sexp.to_string (sexp_of_t t))

end
