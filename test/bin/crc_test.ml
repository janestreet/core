open OUnit;;
open Core

let test =
  "crc" >:::
  begin
    [
      "crc32" >::
      (fun () ->
         "consistent" @? (Crc.crc32 "foo bar baz" = Crc.crc32 "foo bar baz");
      );
      "crc32hex" >::
      (fun () ->
         "consistent" @? (Crc.crc32hex "baz quux" = Crc.crc32hex "baz quux");
      );
    ]
  end
