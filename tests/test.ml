(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* module Tm_struct : Alcotest.TESTABLE with type t = Unix.tm = struct
 *   type t = Unix.tm
 * 
 *   let pp fmt tm =
 *     let open Unix in
 *     let s = Printf.sprintf "%d-%02d-%02dT%02d:%02d:%02d"
 *         (1900+tm.tm_year) (tm.tm_mon+1) tm.tm_mday
 *         tm.tm_hour tm.tm_min tm.tm_sec
 *     in
 *     Format.pp_print_string fmt s
 * 
 *   let equal a b =
 *     Unix.(a.tm_sec = b.tm_sec
 *           && a.tm_min = b.tm_min
 *           && a.tm_hour = b.tm_hour
 *           && a.tm_mday = b.tm_mday
 *           && a.tm_mon = b.tm_mon
 *           && a.tm_year = b.tm_year)
 * end
 * 
 * let tm_struct = (module Tm_struct : Alcotest.TESTABLE with type t = Unix.tm) *)

module Dt_testable : Alcotest.TESTABLE with type t = Timere.Date_time.t = struct
  type t = Timere.Date_time.t

  let pp =
    Timere.Date_time.pp ()

  let equal = Timere.Date_time.equal
end

let dt_testable = (module Dt_testable : Alcotest.TESTABLE with type t = Timere.Date_time.t)

type hemi = Neg | Pos
type tz = Local | Z | Tz of hemi * int * int

let est = Tz (Neg, 5, 0)
let ist = Tz (Pos, 5, 30)
let vet = Tz (Neg, 4, 30)

let time_tests f = [
  "before_1900",      `Quick, f 1861  9  1 8 0 0 Local;
  "before_epoch",     `Quick, f 1969  1  1 0 0 1 Local;
  "nowish",           `Quick, f 2015 12 27 0 0 1 Local;
  "before_1900_z",    `Quick, f 1861  9  1 8 0 0 Z;
  "before_epoch_z",   `Quick, f 1969  1  1 0 0 1 Z;
  "nowish_z",         `Quick, f 2015 12 27 0 0 1 Z;
  "before_1900_est",  `Quick, f 1861  9  1 8 0 0 est;
  "before_epoch_est", `Quick, f 1969  1  1 0 0 1 est;
  "nowish_est",       `Quick, f 2015 12 27 0 0 1 est;
  "before_1900_ist",  `Quick, f 1861  9  1 8 0 0 ist;
  "before_epoch_ist", `Quick, f 1969  1  1 0 0 1 ist;
  "nowish_ist",       `Quick, f 2015 12 27 0 0 1 ist;
  "before_1900_vet",  `Quick, f 1861  9  1 8 0 0 vet;
  "before_epoch_vet", `Quick, f 1969  1  1 0 0 1 vet;
  "nowish_vet",       `Quick, f 2015 12 27 0 0 1 vet;
]

let fixed_time_tests f = [
  "fixed_unix_time_nowish_utc", `Quick,
  f 1451407335. 0.        "2015-12-29T16:42:15Z";
  "fixed_unix_time_nowish_est", `Quick,
  f 1451407335. (-18000.) "2015-12-29T11:42:15-05:00";
  "fixed_unix_time_nowish_ist", `Quick,
  f 1451407335. 19800.    "2015-12-29T22:12:15+05:30";
  "fixed_unix_time_nowish_vet", `Quick,
  f 1451407335. (-16200.) "2015-12-29T12:12:15-04:30";
]

let timere_tz_of_tz tz =
  match tz with
  | Local -> (match Timere.Time_zone.local () with
      | None -> failwith "Failed to obtain local time zone"
      | Some tz -> tz)
  | Z -> Timere.Time_zone.utc
  | Tz (hemi, hours, minutes) ->
    let offset_magnitude =
      Timere.Duration.(make ~hours ~minutes () |> to_seconds)
      |> Int64.to_int
    in
    let offset =
      match hemi with
      | Neg -> - offset_magnitude
      | Pos -> offset_magnitude
    in
    Timere.Time_zone.make_offset_only offset

let str_dt year month day hour minute second tz =
  let str = Printf.sprintf "%d-%02d-%02dT%02d:%02d:%02d%s"
      year month day hour minute second
      (match tz with
       | Local -> ""
       | Z -> "Z"
       | Tz (Neg, hr, mn) -> Printf.sprintf "-%02d:%02d" hr mn
       | Tz (Pos, hr, mn) -> Printf.sprintf "+%02d:%02d" hr mn
      )
  in
    let tz = timere_tz_of_tz tz in
    let month =
      match Timere.Utils.month_of_human_int month with
      | None -> failwith "Invalid month (FIXME)"
      | Some month -> month
    in
    (str,
     Timere.Date_time.make_exn ~tz ~year
       ~month
       ~day
       ~hour
       ~minute
       ~second
       ()
    )

let erange = Unix.Unix_error (Unix.ERANGE, "mktime", "")

let parse_test year month day hour minute second tz () =
  (* if year < 1900
   * then Alcotest.check_raises "< 1900 is ERANGE" erange (fun () ->
   *   ignore (ISO8601.Permissive.datetime "1861-01-01T00:00:00Z")
   * )
   * else *)
    let str, dt = str_dt year month day hour minute second tz in
    let parsed = ISO8601.Permissive.datetime str in
    Printf.printf "parsed: %f\n" parsed;
    let output =
      match
        Timere.Date_time.of_timestamp_float
          ~tz_of_date_time:(timere_tz_of_tz tz)
          parsed
      with
      | None -> failwith "Failed to convert"
      | Some dt -> dt
    in
    Alcotest.(check dt_testable ("parse "^str) dt output)

let parse_fixed_unix_time unix_time _tz s () =
  let parsed = int_of_float (ISO8601.Permissive.datetime s) in
  Alcotest.(check int ("parse "^s) (int_of_float unix_time) parsed)

let parse_tests =
  time_tests parse_test @ fixed_time_tests parse_fixed_unix_time

(* let string_of_datetime unix_time = function
 *   | Local -> ISO8601.Permissive.string_of_datetime unix_time
 *   | Z -> ISO8601.Permissive.string_of_datetimezone (unix_time,0.)
 *   | Tz (Neg,hr,mn) ->
 *     let tz = float_of_int (- (hr * 3600 + mn * 60)) in
 *     ISO8601.Permissive.string_of_datetimezone (unix_time, tz)
 *   | Tz (Pos,hr,mn) ->
 *     let tz = float_of_int (hr * 3600 + mn * 60) in
 *     ISO8601.Permissive.string_of_datetimezone (unix_time, tz) *)

let string_of_datetime unix_time tz =
  match
    Timere.Date_time.of_timestamp_float
      ~tz_of_date_time:(timere_tz_of_tz tz) unix_time
  with
  | None -> failwith "Failed to convert timestamp"
  | Some dt ->
    let format =
      match tz with
      | Local ->
        "{year}-{mon:0X}-{mday:0X}T{hour:0X}:{min:0X}:{sec:0X}"
      | Z ->
        "{year}-{mon:0X}-{mday:0X}T{hour:0X}:{min:0X}:{sec:0X}Z"
      | Tz _ ->
        "{year}-{mon:0X}-{mday:0X}T{hour:0X}:{min:0X}:{sec:0X}{tzoff-sign}{tzoff-hour:0X}:{tzoff-min:0X}"
    in
    Timere.Date_time.to_string ~format dt

let print_test year month day hour minute second tz () =
  (* We use Unix.mktime to find the epoch time but with year < 1900 it
     will error with ERANGE. *)
  (* if year < 1900
   * then ()
   * else *)
    let str, dt = str_dt year month day hour minute second tz in
    let unix_time = Timere.Date_time.to_timestamp_float_single dt in
    let output = string_of_datetime unix_time tz in
    Alcotest.(check string ("print "^str) str output)

let print_fixed_unix_time unix_time tz s () =
  let output = ISO8601.Permissive.string_of_datetimezone (unix_time, tz) in
  Alcotest.(check string ("print "^s) s output)

let print_tests =
  time_tests print_test @ fixed_time_tests print_fixed_unix_time

let rt_test year month day hour minute second tz () =
  (* if year < 1900
   * then Alcotest.check_raises "< 1900 is ERANGE" erange (fun () ->
   *   ignore (ISO8601.Permissive.datetime "1861-01-01T00:00:00")
   * )
   * else *)
    let str, _ = str_dt year month day hour minute second tz in
    let output = string_of_datetime (ISO8601.Permissive.datetime str) tz in
    Alcotest.(check string ("roundtrip "^str) str output)

let rt_fixed_unix_time unix_time tz s () =
  let output = ISO8601.Permissive.(string_of_datetimezone (datetime s, tz)) in
  Alcotest.(check string ("roundtrip "^s) s output);
  let output = int_of_float ISO8601.Permissive.(
    datetime (string_of_datetimezone (unix_time, tz))
  ) in
  let unix_time = int_of_float unix_time in
  Alcotest.(check int ("roundtrip "^string_of_int unix_time) unix_time output)

let rt_tests =
  time_tests rt_test @ fixed_time_tests rt_fixed_unix_time

let suites = [
  "parse", parse_tests;
  "print", print_tests;
  "rt",    rt_tests;
]

;;
Alcotest.run "ISO8601" suites
