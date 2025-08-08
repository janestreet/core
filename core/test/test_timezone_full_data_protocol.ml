open! Core
open! Import

let%expect_test "Time.Stable.Zone.Full_data.V1" =
  let sample =
    Time_float.Zone.input_tz_file
      ~zonename:"Sample Time Zone"
      ~filename:"sample_time_zone_file"
  in
  Dynamic.with_temporarily sexp_style Sexp_style.simple_pretty ~f:(fun () ->
    print_and_check_stable_type
      (module Time_float.Stable.Zone.Full_data.V1)
      [ Time_float.Zone.utc
      ; Time_float.Zone.of_utc_offset ~hours:(-24)
      ; Time_float.Zone.of_utc_offset_explicit_name ~name:"my zone" ~hours:1
      ; Time_float.Zone.of_utc_offset_in_seconds_round_down
          (Time_float.Span.of_string "-3h2m1s")
      ; sample
      ; Time_float.Zone.add_offset_in_seconds_round_down
          sample
          ~name:"offset sample"
          ~span:Time_float.Span.hour
      ]);
  [%expect
    {|
    (bin_shape_digest 819b3454610960b488fbed8a9e63887c)
    ((sexp
      ((name UTC)
       (original_filename ())
       (digest ())
       (transitions ())
       (last_regime_index 0)
       (default_local_time_type
        ((utc_offset_in_seconds 0) (is_dst false) (abbrv UTC)))
       (leap_seconds ())))
     (bin_io "\003UTC\000\000\000\000\000\000\003UTC\000"))
    ((sexp
      ((name UTC-24)
       (original_filename ())
       (digest ())
       (transitions ())
       (last_regime_index 0)
       (default_local_time_type
        ((utc_offset_in_seconds -86_400) (is_dst false) (abbrv UTC-24)))
       (leap_seconds ())))
     (bin_io "\006UTC-24\000\000\000\000\253\128\174\254\255\000\006UTC-24\000"))
    ((sexp
      ((name "my zone")
       (original_filename ())
       (digest ())
       (transitions ())
       (last_regime_index 0)
       (default_local_time_type
        ((utc_offset_in_seconds 3_600) (is_dst false) (abbrv "my zone")))
       (leap_seconds ())))
     (bin_io "\007my zone\000\000\000\000\254\016\014\000\007my zone\000"))
    ((sexp
      ((name UTC-3:02:01)
       (original_filename ())
       (digest ())
       (transitions ())
       (last_regime_index 0)
       (default_local_time_type
        ((utc_offset_in_seconds -10_921) (is_dst false) (abbrv UTC-3:02:01)))
       (leap_seconds ())))
     (bin_io "\011UTC-3:02:01\000\000\000\000\254W\213\000\011UTC-3:02:01\000"))
    ((sexp
      ((name "Sample Time Zone")
       (original_filename (sample_time_zone_file))
       (digest ("\228\2028\0165\163Kz\133!\132\204\r\216\155\170"))
       (transitions
        (((start_time_in_seconds_since_epoch -2_717_650_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_633_280_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_615_140_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_601_830_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_583_690_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_570_381_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_551_636_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_536_512_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_523_210_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_504_458_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_491_760_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_473_008_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_459_706_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_441_558_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_428_256_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_410_109_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_396_807_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_378_659_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_365_357_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_347_210_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_333_908_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_315_155_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_301_853_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_283_706_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_270_404_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_252_256_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_238_954_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_220_806_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_207_504_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_189_357_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_176_055_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_157_302_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_144_605_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_125_853_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_112_551_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_094_403_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_081_101_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_062_954_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_049_652_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_031_504_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_018_202_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_000_054_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -986_752_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -968_000_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -955_303_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -936_550_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -923_248_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -905_101_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -891_799_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -880_218_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EWT))))
         ((start_time_in_seconds_since_epoch -769_395_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EPT))))
         ((start_time_in_seconds_since_epoch -765_396_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -747_248_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -733_946_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -715_798_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -702_496_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -684_349_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -671_047_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -652_899_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -639_597_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -620_845_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -608_148_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -589_395_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -576_093_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -557_946_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -544_644_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -526_496_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -513_194_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -495_046_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -481_744_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -463_597_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -447_271_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -431_542_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -415_821_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -400_093_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -384_372_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -368_643_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -352_922_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -337_194_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -321_472_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -305_744_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -289_418_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -273_690_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -257_968_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -242_240_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -226_519_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -210_790_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -195_069_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -179_341_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -163_620_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -147_891_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -131_565_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -116_442_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -100_116_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -84_387_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -68_666_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -52_938_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -37_216_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -21_488_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -5_767_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 9_961_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 25_682_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 41_410_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 57_736_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 73_465_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 89_186_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 104_914_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 120_636_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 126_687_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 152_085_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 162_370_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 183_535_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 199_263_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 215_589_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 230_713_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 247_039_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 262_767_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 278_488_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 294_217_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 309_938_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 325_666_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 341_388_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 357_116_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 372_837_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 388_566_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 404_892_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 420_015_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 436_341_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 452_070_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 467_791_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 483_519_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 499_240_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 514_969_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 530_690_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 544_604_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 562_140_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 576_054_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 594_194_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 607_503_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 625_644_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 638_953_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 657_093_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 671_007_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 688_543_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 702_457_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 719_992_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 733_906_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 752_047_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 765_356_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 783_496_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 796_806_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 814_946_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 828_860_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 846_396_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 860_310_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 877_845_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 891_759_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 909_295_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 923_209_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 941_349_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 954_658_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 972_799_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 986_108_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_004_248_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_018_162_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_035_698_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_049_612_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_067_148_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_081_062_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_099_202_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_112_511_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_130_652_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_143_961_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_162_101_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_173_596_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_194_156_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_205_046_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_225_605_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_236_495_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_257_055_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_268_550_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_289_109_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_299_999_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_320_559_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_331_449_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_352_008_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_362_898_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_383_458_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_394_348_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_414_908_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_425_798_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_446_357_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_457_852_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_478_412_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_489_302_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_509_861_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_520_751_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_541_311_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_552_201_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_572_760_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_583_650_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_604_210_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_615_705_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_636_264_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_647_154_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_667_714_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_678_604_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_699_164_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_710_054_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_730_613_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_741_503_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_762_063_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_772_953_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_793_512_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_805_007_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_825_567_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_836_457_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_857_016_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_867_906_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_888_466_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_899_356_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_919_916_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_930_806_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_951_365_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_962_860_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_983_420_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_994_310_000)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_014_869_600)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 2_025_759_600)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_046_319_200)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 2_057_209_200)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_077_768_800)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 2_088_658_800)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_109_218_400)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 2_120_108_400)
          (new_regime ((utc_offset_in_seconds -14_400) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_140_668_000)
          (new_regime
           ((utc_offset_in_seconds -18_000) (is_dst false) (abbrv EST))))))
       (last_regime_index 0)
       (default_local_time_type
        ((utc_offset_in_seconds -17_762) (is_dst false) (abbrv LMT)))
       (leap_seconds ())))
     (bin_io
      "\016Sample Time Zone\001\021sample_time_zone_file\001\016\228\2028\0165\163Kz\133!\132\204\r\216\155\170\254\236\000\252\144\240\003^\255\255\255\255\254\176\185\000\003EST\253p\030\166\158\254\192\199\001\003EDT\253`\235\186\159\254\176\185\000\003EST\253p\000\134\160\254\192\199\001\003EDT\253`\205\154\161\254\176\185\000\003EST\253p\226e\162\254\192\199\001\003EDT\253\224\233\131\163\254\176\185\000\003EST\253p\174j\164\254\192\199\001\003EDT\253`\1675\165\254\176\185\000\003EST\253\240\202S\166\254\192\199\001\003EDT\253`\137\021\167\254\176\185\000\003EST\253\240\1723\168\254\192\199\001\003EDT\253\224\165\254\168\254\176\185\000\003EST\253\240\142\019\170\254\192\199\001\003EDT\253\224\135\222\170\254\176\185\000\003EST\253\240p\243\171\254\192\199\001\003EDT\253\224i\190\172\254\176\185\000\003EST\253\240R\211\173\254\192\199\001\003EDT\253\224K\158\174\254\176\185\000\003EST\253\2404\179\175\254\192\199\001\003EDT\253\224-~\176\254\176\185\000\003EST\253pQ\156\177\254\192\199\001\003EDT\253`Jg\178\254\176\185\000\003EST\253p3|\179\254\192\199\001\003EDT\253`,G\180\254\176\185\000\003EST\253p\021\\\181\254\192\199\001\003EDT\253`\014'\182\254\176\185\000\003EST\253p\247;\183\254\192\199\001\003EDT\253`\240\006\184\254\176\185\000\003EST\253p\217\027\185\254\192\199\001\003EDT\253`\210\230\185\254\176\185\000\003EST\253\240\245\004\187\254\192\199\001\003EDT\253`\180\198\187\254\176\185\000\003EST\253\240\215\228\188\254\192\199\001\003EDT\253\224\208\175\189\254\176\185\000\003EST\253\240\185\196\190\254\192\199\001\003EDT\253\224\178\143\191\254\176\185\000\003EST\253\240\155\164\192\254\192\199\001\003EDT\253\224\148o\193\254\176\185\000\003EST\253\240}\132\194\254\192\199\001\003EDT\253\224vO\195\254\176\185\000\003EST\253\240_d\196\254\192\199\001\003EDT\253\224X/\197\254\176\185\000\003EST\253p|M\198\254\192\199\001\003EDT\253\224:\015\199\254\176\185\000\003EST\253p^-\200\254\192\199\001\003EDT\253`W\248\200\254\176\185\000\003EST\253p@\r\202\254\192\199\001\003EDT\253`9\216\202\254\176\185\000\003EST\253p\240\136\203\254\192\199\001\003EWT\253p\244#\210\254\192\199\001\003EPT\253\224\251`\210\254\176\185\000\003EST\253\240\228u\211\254\192\199\001\003EDT\253\224\221@\212\254\176\185\000\003EST\253\240\198U\213\254\192\199\001\003EDT\253\224\191 \214\254\176\185\000\003EST\253\240\1685\215\254\192\199\001\003EDT\253\224\161\000\216\254\176\185\000\003EST\253\240\138\021\217\254\192\199\001\003EDT\253\224\131\224\217\254\176\185\000\003EST\253p\167\254\218\254\192\199\001\003EDT\253\224e\192\219\254\176\185\000\003EST\253p\137\222\220\254\192\199\001\003EDT\253`\130\169\221\254\176\185\000\003EST\253pk\190\222\254\192\199\001\003EDT\253`d\137\223\254\176\185\000\003EST\253pM\158\224\254\192\199\001\003EDT\253`Fi\225\254\176\185\000\003EST\253p/~\226\254\192\199\001\003EDT\253`(I\227\254\176\185\000\003EST\253p\017^\228\254\192\199\001\003EDT\253\224.W\229\254\176\185\000\003EST\253\240-G\230\254\192\199\001\003EDT\253\224\0167\231\254\176\185\000\003EST\253\240\015'\232\254\192\199\001\003EDT\253\224\242\022\233\254\176\185\000\003EST\253\240\241\006\234\254\192\199\001\003EDT\253\224\212\246\234\254\176\185\000\003EST\253\240\211\230\235\254\192\199\001\003EDT\253\224\182\214\236\254\176\185\000\003EST\253\240\181\198\237\254\192\199\001\003EDT\253`\211\191\238\254\176\185\000\003EST\253p\210\175\239\254\192\199\001\003EDT\253`\181\159\240\254\176\185\000\003EST\253p\180\143\241\254\192\199\001\003EDT\253`\151\127\242\254\176\185\000\003EST\253p\150o\243\254\192\199\001\003EDT\253`y_\244\254\176\185\000\003EST\253pxO\245\254\192\199\001\003EDT\253`[?\246\254\176\185\000\003EST\253pZ/\247\254\192\199\001\003EDT\253\224w(\248\254\176\185\000\003EST\253p<\015\249\254\192\199\001\003EDT\253\224Y\b\250\254\176\185\000\003EST\253\240X\248\250\254\192\199\001\003EDT\253\224;\232\251\254\176\185\000\003EST\253\240:\216\252\254\192\199\001\003EDT\253\224\029\200\253\254\176\185\000\003EST\253\240\028\184\254\254\192\199\001\003EDT\253\224\255\167\255\254\176\185\000\003EST\253\240\254\151\000\254\192\199\001\003EDT\253\224\225\135\001\254\176\185\000\003EST\253\240\224w\002\254\192\199\001\003EDT\253`\254p\003\254\176\185\000\003EST\253p\253`\004\254\192\199\001\003EDT\253`\224P\005\254\176\185\000\003EST\253p\223@\006\254\192\199\001\003EDT\253`\1940\007\254\176\185\000\003EST\253p\025\141\007\254\192\199\001\003EDT\253`\164\016\t\254\176\185\000\003EST\253\240\148\173\t\254\192\199\001\003EDT\253`\134\240\n\254\176\185\000\003EST\253p\133\224\011\254\192\199\001\003EDT\253\224\162\217\012\254\176\185\000\003EST\253pg\192\r\254\192\199\001\003EDT\253\224\132\185\014\254\176\185\000\003EST\253\240\131\169\015\254\192\199\001\003EDT\253\224f\153\016\254\176\185\000\003EST\253\240e\137\017\254\192\199\001\003EDT\253\224Hy\018\254\176\185\000\003EST\253\240Gi\019\254\192\199\001\003EDT\253\224*Y\020\254\176\185\000\003EST\253\240)I\021\254\192\199\001\003EDT\253\224\0129\022\254\176\185\000\003EST\253\240\011)\023\254\192\199\001\003EDT\253`)\"\024\254\176\185\000\003EST\253\240\237\b\025\254\192\199\001\003EDT\253`\011\002\026\254\176\185\000\003EST\253p\n\242\026\254\192\199\001\003EDT\253`\237\225\027\254\176\185\000\003EST\253p\236\209\028\254\192\199\001\003EDT\253`\207\193\029\254\176\185\000\003EST\253p\206\177\030\254\192\199\001\003EDT\253`\177\161\031\254\176\185\000\003EST\253\240\000v \254\192\199\001\003EDT\253`\147\129!\254\176\185\000\003EST\253\240\226U\"\254\192\199\001\003EDT\253\224\175j#\254\176\185\000\003EST\253\240\1965$\254\192\199\001\003EDT\253\224\145J%\254\176\185\000\003EST\253\240\166\021&\254\192\199\001\003EDT\253\224s*'\254\176\185\000\003EST\253p\195\254'\254\192\199\001\003EDT\253\224U\n)\254\176\185\000\003EST\253p\165\222)\254\192\199\001\003EDT\253\2247\234*\254\176\185\000\003EST\253p\135\190+\254\192\199\001\003EDT\253`T\211,\254\176\185\000\003EST\253pi\158-\254\192\199\001\003EDT\253`6\179.\254\176\185\000\003EST\253pK~/\254\192\199\001\003EDT\253`\024\1470\254\176\185\000\003EST\253\240gg1\254\192\199\001\003EDT\253`\250r2\254\176\185\000\003EST\253\240IG3\254\192\199\001\003EDT\253`\220R4\254\176\185\000\003EST\253\240+'5\254\192\199\001\003EDT\253`\19026\254\176\185\000\003EST\253\240\r\0077\254\192\199\001\003EDT\253\224\218\0278\254\176\185\000\003EST\253\240\239\2308\254\192\199\001\003EDT\253\224\188\2519\254\176\185\000\003EST\253\240\209\198:\254\192\199\001\003EDT\253\224\158\219;\254\176\185\000\003EST\253p\238\175<\254\192\199\001\003EDT\253\224\128\187=\254\176\185\000\003EST\253p\208\143>\254\192\199\001\003EDT\253\224b\155?\254\176\185\000\003EST\253p\178o@\254\192\199\001\003EDT\253`\127\132A\254\176\185\000\003EST\253p\148OB\254\192\199\001\003EDT\253`adC\254\176\185\000\003EST\253pv/D\254\192\199\001\003EDT\253`CDE\254\176\185\000\003EST\253\240\168\243E\254\192\199\001\003EDT\253\224_-G\254\176\185\000\003EST\253\240\138\211G\254\192\199\001\003EDT\253\224A\rI\254\176\185\000\003EST\253\240l\179I\254\192\199\001\003EDT\253\224#\237J\254\176\185\000\003EST\253p\137\156K\254\192\199\001\003EDT\253`@\214L\254\176\185\000\003EST\253pk|M\254\192\199\001\003EDT\253`\"\182N\254\176\185\000\003EST\253pM\\O\254\192\199\001\003EDT\253`\004\150P\254\176\185\000\003EST\253p/<Q\254\192\199\001\003EDT\253`\230uR\254\176\185\000\003EST\253p\017\028S\254\192\199\001\003EDT\253`\200UT\254\176\185\000\003EST\253p\243\251T\254\192\199\001\003EDT\253`\1705V\254\176\185\000\003EST\253\240\015\229V\254\192\199\001\003EDT\253\224\198\030X\254\176\185\000\003EST\253\240\241\196X\254\192\199\001\003EDT\253\224\168\254Y\254\176\185\000\003EST\253\240\211\164Z\254\192\199\001\003EDT\253\224\138\222[\254\176\185\000\003EST\253\240\181\132\\\254\192\199\001\003EDT\253\224l\190]\254\176\185\000\003EST\253\240\151d^\254\192\199\001\003EDT\253\224N\158_\254\176\185\000\003EST\253p\180M`\254\192\199\001\003EDT\253`k\135a\254\176\185\000\003EST\253p\150-b\254\192\199\001\003EDT\253`Mgc\254\176\185\000\003EST\253px\rd\254\192\199\001\003EDT\253`/Ge\254\176\185\000\003EST\253pZ\237e\254\192\199\001\003EDT\253`\017'g\254\176\185\000\003EST\253p<\205g\254\192\199\001\003EDT\253`\243\006i\254\176\185\000\003EST\253p\030\173i\254\192\199\001\003EDT\253`\213\230j\254\176\185\000\003EST\253\240:\150k\254\192\199\001\003EDT\253\224\241\207l\254\176\185\000\003EST\253\240\028vm\254\192\199\001\003EDT\253\224\211\175n\254\176\185\000\003EST\253\240\254Uo\254\192\199\001\003EDT\253\224\181\143p\254\176\185\000\003EST\253\240\2245q\254\192\199\001\003EDT\253\224\151or\254\176\185\000\003EST\253\240\194\021s\254\192\199\001\003EDT\253\224yOt\254\176\185\000\003EST\253p\223\254t\254\192\199\001\003EDT\253`\1508v\254\176\185\000\003EST\253p\193\222v\254\192\199\001\003EDT\253`x\024x\254\176\185\000\003EST\253p\163\190x\254\192\199\001\003EDT\253`Z\248y\254\176\185\000\003EST\253p\133\158z\254\192\199\001\003EDT\253`<\216{\254\176\185\000\003EST\253pg~|\254\192\199\001\003EDT\253`\030\184}\254\176\185\000\003EST\253pI^~\254\192\199\001\003EDT\253`\000\152\127\254\176\185\000\003EST\000\254\158\186\000\003LMT\000"))
    ((sexp
      ((name "offset sample")
       (original_filename ())
       (digest ())
       (transitions
        (((start_time_in_seconds_since_epoch -2_717_650_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_633_280_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_615_140_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_601_830_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_583_690_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_570_381_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_551_636_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_536_512_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_523_210_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_504_458_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_491_760_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_473_008_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_459_706_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_441_558_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_428_256_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_410_109_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_396_807_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_378_659_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_365_357_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_347_210_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_333_908_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_315_155_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_301_853_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_283_706_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_270_404_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_252_256_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_238_954_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_220_806_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_207_504_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_189_357_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_176_055_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_157_302_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_144_605_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_125_853_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_112_551_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_094_403_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_081_101_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_062_954_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_049_652_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_031_504_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -1_018_202_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -1_000_054_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -986_752_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -968_000_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -955_303_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -936_550_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -923_248_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -905_101_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -891_799_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -880_218_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EWT))))
         ((start_time_in_seconds_since_epoch -769_395_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EPT))))
         ((start_time_in_seconds_since_epoch -765_396_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -747_248_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -733_946_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -715_798_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -702_496_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -684_349_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -671_047_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -652_899_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -639_597_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -620_845_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -608_148_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -589_395_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -576_093_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -557_946_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -544_644_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -526_496_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -513_194_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -495_046_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -481_744_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -463_597_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -447_271_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -431_542_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -415_821_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -400_093_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -384_372_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -368_643_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -352_922_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -337_194_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -321_472_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -305_744_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -289_418_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -273_690_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -257_968_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -242_240_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -226_519_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -210_790_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -195_069_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -179_341_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -163_620_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -147_891_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -131_565_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -116_442_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -100_116_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -84_387_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -68_666_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -52_938_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -37_216_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch -21_488_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch -5_767_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 9_961_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 25_682_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 41_410_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 57_736_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 73_465_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 89_186_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 104_914_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 120_636_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 126_687_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 152_085_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 162_370_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 183_535_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 199_263_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 215_589_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 230_713_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 247_039_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 262_767_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 278_488_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 294_217_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 309_938_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 325_666_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 341_388_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 357_116_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 372_837_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 388_566_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 404_892_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 420_015_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 436_341_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 452_070_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 467_791_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 483_519_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 499_240_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 514_969_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 530_690_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 544_604_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 562_140_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 576_054_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 594_194_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 607_503_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 625_644_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 638_953_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 657_093_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 671_007_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 688_543_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 702_457_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 719_992_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 733_906_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 752_047_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 765_356_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 783_496_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 796_806_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 814_946_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 828_860_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 846_396_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 860_310_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 877_845_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 891_759_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 909_295_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 923_209_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 941_349_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 954_658_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 972_799_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 986_108_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_004_248_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_018_162_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_035_698_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_049_612_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_067_148_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_081_062_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_099_202_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_112_511_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_130_652_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_143_961_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_162_101_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_173_596_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_194_156_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_205_046_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_225_605_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_236_495_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_257_055_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_268_550_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_289_109_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_299_999_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_320_559_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_331_449_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_352_008_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_362_898_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_383_458_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_394_348_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_414_908_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_425_798_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_446_357_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_457_852_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_478_412_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_489_302_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_509_861_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_520_751_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_541_311_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_552_201_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_572_760_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_583_650_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_604_210_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_615_705_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_636_264_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_647_154_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_667_714_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_678_604_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_699_164_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_710_054_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_730_613_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_741_503_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_762_063_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_772_953_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_793_512_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_805_007_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_825_567_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_836_457_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_857_016_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_867_906_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_888_466_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_899_356_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_919_916_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_930_806_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_951_365_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_962_860_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 1_983_420_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 1_994_310_000)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_014_869_600)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 2_025_759_600)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_046_319_200)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 2_057_209_200)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_077_768_800)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 2_088_658_800)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_109_218_400)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))
         ((start_time_in_seconds_since_epoch 2_120_108_400)
          (new_regime ((utc_offset_in_seconds -10_800) (is_dst true) (abbrv EDT))))
         ((start_time_in_seconds_since_epoch 2_140_668_000)
          (new_regime
           ((utc_offset_in_seconds -14_400) (is_dst false) (abbrv EST))))))
       (last_regime_index 0)
       (default_local_time_type
        ((utc_offset_in_seconds -14_162) (is_dst false) (abbrv LMT)))
       (leap_seconds ())))
     (bin_io
      "\roffset sample\000\000\254\236\000\252\144\240\003^\255\255\255\255\254\192\199\000\003EST\253p\030\166\158\254\208\213\001\003EDT\253`\235\186\159\254\192\199\000\003EST\253p\000\134\160\254\208\213\001\003EDT\253`\205\154\161\254\192\199\000\003EST\253p\226e\162\254\208\213\001\003EDT\253\224\233\131\163\254\192\199\000\003EST\253p\174j\164\254\208\213\001\003EDT\253`\1675\165\254\192\199\000\003EST\253\240\202S\166\254\208\213\001\003EDT\253`\137\021\167\254\192\199\000\003EST\253\240\1723\168\254\208\213\001\003EDT\253\224\165\254\168\254\192\199\000\003EST\253\240\142\019\170\254\208\213\001\003EDT\253\224\135\222\170\254\192\199\000\003EST\253\240p\243\171\254\208\213\001\003EDT\253\224i\190\172\254\192\199\000\003EST\253\240R\211\173\254\208\213\001\003EDT\253\224K\158\174\254\192\199\000\003EST\253\2404\179\175\254\208\213\001\003EDT\253\224-~\176\254\192\199\000\003EST\253pQ\156\177\254\208\213\001\003EDT\253`Jg\178\254\192\199\000\003EST\253p3|\179\254\208\213\001\003EDT\253`,G\180\254\192\199\000\003EST\253p\021\\\181\254\208\213\001\003EDT\253`\014'\182\254\192\199\000\003EST\253p\247;\183\254\208\213\001\003EDT\253`\240\006\184\254\192\199\000\003EST\253p\217\027\185\254\208\213\001\003EDT\253`\210\230\185\254\192\199\000\003EST\253\240\245\004\187\254\208\213\001\003EDT\253`\180\198\187\254\192\199\000\003EST\253\240\215\228\188\254\208\213\001\003EDT\253\224\208\175\189\254\192\199\000\003EST\253\240\185\196\190\254\208\213\001\003EDT\253\224\178\143\191\254\192\199\000\003EST\253\240\155\164\192\254\208\213\001\003EDT\253\224\148o\193\254\192\199\000\003EST\253\240}\132\194\254\208\213\001\003EDT\253\224vO\195\254\192\199\000\003EST\253\240_d\196\254\208\213\001\003EDT\253\224X/\197\254\192\199\000\003EST\253p|M\198\254\208\213\001\003EDT\253\224:\015\199\254\192\199\000\003EST\253p^-\200\254\208\213\001\003EDT\253`W\248\200\254\192\199\000\003EST\253p@\r\202\254\208\213\001\003EDT\253`9\216\202\254\192\199\000\003EST\253p\240\136\203\254\208\213\001\003EWT\253p\244#\210\254\208\213\001\003EPT\253\224\251`\210\254\192\199\000\003EST\253\240\228u\211\254\208\213\001\003EDT\253\224\221@\212\254\192\199\000\003EST\253\240\198U\213\254\208\213\001\003EDT\253\224\191 \214\254\192\199\000\003EST\253\240\1685\215\254\208\213\001\003EDT\253\224\161\000\216\254\192\199\000\003EST\253\240\138\021\217\254\208\213\001\003EDT\253\224\131\224\217\254\192\199\000\003EST\253p\167\254\218\254\208\213\001\003EDT\253\224e\192\219\254\192\199\000\003EST\253p\137\222\220\254\208\213\001\003EDT\253`\130\169\221\254\192\199\000\003EST\253pk\190\222\254\208\213\001\003EDT\253`d\137\223\254\192\199\000\003EST\253pM\158\224\254\208\213\001\003EDT\253`Fi\225\254\192\199\000\003EST\253p/~\226\254\208\213\001\003EDT\253`(I\227\254\192\199\000\003EST\253p\017^\228\254\208\213\001\003EDT\253\224.W\229\254\192\199\000\003EST\253\240-G\230\254\208\213\001\003EDT\253\224\0167\231\254\192\199\000\003EST\253\240\015'\232\254\208\213\001\003EDT\253\224\242\022\233\254\192\199\000\003EST\253\240\241\006\234\254\208\213\001\003EDT\253\224\212\246\234\254\192\199\000\003EST\253\240\211\230\235\254\208\213\001\003EDT\253\224\182\214\236\254\192\199\000\003EST\253\240\181\198\237\254\208\213\001\003EDT\253`\211\191\238\254\192\199\000\003EST\253p\210\175\239\254\208\213\001\003EDT\253`\181\159\240\254\192\199\000\003EST\253p\180\143\241\254\208\213\001\003EDT\253`\151\127\242\254\192\199\000\003EST\253p\150o\243\254\208\213\001\003EDT\253`y_\244\254\192\199\000\003EST\253pxO\245\254\208\213\001\003EDT\253`[?\246\254\192\199\000\003EST\253pZ/\247\254\208\213\001\003EDT\253\224w(\248\254\192\199\000\003EST\253p<\015\249\254\208\213\001\003EDT\253\224Y\b\250\254\192\199\000\003EST\253\240X\248\250\254\208\213\001\003EDT\253\224;\232\251\254\192\199\000\003EST\253\240:\216\252\254\208\213\001\003EDT\253\224\029\200\253\254\192\199\000\003EST\253\240\028\184\254\254\208\213\001\003EDT\253\224\255\167\255\254\192\199\000\003EST\253\240\254\151\000\254\208\213\001\003EDT\253\224\225\135\001\254\192\199\000\003EST\253\240\224w\002\254\208\213\001\003EDT\253`\254p\003\254\192\199\000\003EST\253p\253`\004\254\208\213\001\003EDT\253`\224P\005\254\192\199\000\003EST\253p\223@\006\254\208\213\001\003EDT\253`\1940\007\254\192\199\000\003EST\253p\025\141\007\254\208\213\001\003EDT\253`\164\016\t\254\192\199\000\003EST\253\240\148\173\t\254\208\213\001\003EDT\253`\134\240\n\254\192\199\000\003EST\253p\133\224\011\254\208\213\001\003EDT\253\224\162\217\012\254\192\199\000\003EST\253pg\192\r\254\208\213\001\003EDT\253\224\132\185\014\254\192\199\000\003EST\253\240\131\169\015\254\208\213\001\003EDT\253\224f\153\016\254\192\199\000\003EST\253\240e\137\017\254\208\213\001\003EDT\253\224Hy\018\254\192\199\000\003EST\253\240Gi\019\254\208\213\001\003EDT\253\224*Y\020\254\192\199\000\003EST\253\240)I\021\254\208\213\001\003EDT\253\224\0129\022\254\192\199\000\003EST\253\240\011)\023\254\208\213\001\003EDT\253`)\"\024\254\192\199\000\003EST\253\240\237\b\025\254\208\213\001\003EDT\253`\011\002\026\254\192\199\000\003EST\253p\n\242\026\254\208\213\001\003EDT\253`\237\225\027\254\192\199\000\003EST\253p\236\209\028\254\208\213\001\003EDT\253`\207\193\029\254\192\199\000\003EST\253p\206\177\030\254\208\213\001\003EDT\253`\177\161\031\254\192\199\000\003EST\253\240\000v \254\208\213\001\003EDT\253`\147\129!\254\192\199\000\003EST\253\240\226U\"\254\208\213\001\003EDT\253\224\175j#\254\192\199\000\003EST\253\240\1965$\254\208\213\001\003EDT\253\224\145J%\254\192\199\000\003EST\253\240\166\021&\254\208\213\001\003EDT\253\224s*'\254\192\199\000\003EST\253p\195\254'\254\208\213\001\003EDT\253\224U\n)\254\192\199\000\003EST\253p\165\222)\254\208\213\001\003EDT\253\2247\234*\254\192\199\000\003EST\253p\135\190+\254\208\213\001\003EDT\253`T\211,\254\192\199\000\003EST\253pi\158-\254\208\213\001\003EDT\253`6\179.\254\192\199\000\003EST\253pK~/\254\208\213\001\003EDT\253`\024\1470\254\192\199\000\003EST\253\240gg1\254\208\213\001\003EDT\253`\250r2\254\192\199\000\003EST\253\240IG3\254\208\213\001\003EDT\253`\220R4\254\192\199\000\003EST\253\240+'5\254\208\213\001\003EDT\253`\19026\254\192\199\000\003EST\253\240\r\0077\254\208\213\001\003EDT\253\224\218\0278\254\192\199\000\003EST\253\240\239\2308\254\208\213\001\003EDT\253\224\188\2519\254\192\199\000\003EST\253\240\209\198:\254\208\213\001\003EDT\253\224\158\219;\254\192\199\000\003EST\253p\238\175<\254\208\213\001\003EDT\253\224\128\187=\254\192\199\000\003EST\253p\208\143>\254\208\213\001\003EDT\253\224b\155?\254\192\199\000\003EST\253p\178o@\254\208\213\001\003EDT\253`\127\132A\254\192\199\000\003EST\253p\148OB\254\208\213\001\003EDT\253`adC\254\192\199\000\003EST\253pv/D\254\208\213\001\003EDT\253`CDE\254\192\199\000\003EST\253\240\168\243E\254\208\213\001\003EDT\253\224_-G\254\192\199\000\003EST\253\240\138\211G\254\208\213\001\003EDT\253\224A\rI\254\192\199\000\003EST\253\240l\179I\254\208\213\001\003EDT\253\224#\237J\254\192\199\000\003EST\253p\137\156K\254\208\213\001\003EDT\253`@\214L\254\192\199\000\003EST\253pk|M\254\208\213\001\003EDT\253`\"\182N\254\192\199\000\003EST\253pM\\O\254\208\213\001\003EDT\253`\004\150P\254\192\199\000\003EST\253p/<Q\254\208\213\001\003EDT\253`\230uR\254\192\199\000\003EST\253p\017\028S\254\208\213\001\003EDT\253`\200UT\254\192\199\000\003EST\253p\243\251T\254\208\213\001\003EDT\253`\1705V\254\192\199\000\003EST\253\240\015\229V\254\208\213\001\003EDT\253\224\198\030X\254\192\199\000\003EST\253\240\241\196X\254\208\213\001\003EDT\253\224\168\254Y\254\192\199\000\003EST\253\240\211\164Z\254\208\213\001\003EDT\253\224\138\222[\254\192\199\000\003EST\253\240\181\132\\\254\208\213\001\003EDT\253\224l\190]\254\192\199\000\003EST\253\240\151d^\254\208\213\001\003EDT\253\224N\158_\254\192\199\000\003EST\253p\180M`\254\208\213\001\003EDT\253`k\135a\254\192\199\000\003EST\253p\150-b\254\208\213\001\003EDT\253`Mgc\254\192\199\000\003EST\253px\rd\254\208\213\001\003EDT\253`/Ge\254\192\199\000\003EST\253pZ\237e\254\208\213\001\003EDT\253`\017'g\254\192\199\000\003EST\253p<\205g\254\208\213\001\003EDT\253`\243\006i\254\192\199\000\003EST\253p\030\173i\254\208\213\001\003EDT\253`\213\230j\254\192\199\000\003EST\253\240:\150k\254\208\213\001\003EDT\253\224\241\207l\254\192\199\000\003EST\253\240\028vm\254\208\213\001\003EDT\253\224\211\175n\254\192\199\000\003EST\253\240\254Uo\254\208\213\001\003EDT\253\224\181\143p\254\192\199\000\003EST\253\240\2245q\254\208\213\001\003EDT\253\224\151or\254\192\199\000\003EST\253\240\194\021s\254\208\213\001\003EDT\253\224yOt\254\192\199\000\003EST\253p\223\254t\254\208\213\001\003EDT\253`\1508v\254\192\199\000\003EST\253p\193\222v\254\208\213\001\003EDT\253`x\024x\254\192\199\000\003EST\253p\163\190x\254\208\213\001\003EDT\253`Z\248y\254\192\199\000\003EST\253p\133\158z\254\208\213\001\003EDT\253`<\216{\254\192\199\000\003EST\253pg~|\254\208\213\001\003EDT\253`\030\184}\254\192\199\000\003EST\253pI^~\254\208\213\001\003EDT\253`\000\152\127\254\192\199\000\003EST\000\254\174\200\000\003LMT\000"))
    |}]
;;
