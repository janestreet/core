let package_name = "core"

let sections =
  [ ("lib",
    [ ("built_lib_core", None)
    ; ("built_lib_core_top", None)
    ; ("built_lib_version_util_fallback", None)
    ],
    [ ("META", None)
    ; ("src/config.h", None)
    ; ("src/config.mlh", None)
    ])
  ; ("bin",
    [],
    [ ("coretop", None)
    ; ("corebuild", None)
    ])
  ]
