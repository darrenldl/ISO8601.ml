{
  let int = int_of_string

  (* Date helpers *)
  let mkdate year month day =
    let open Timere in
    let month =
      match Timere.Utils.month_of_human_int month with
      | None -> failwith "Invalid month (FIXME)"
      | Some month -> month
    in
    Date_time.make_exn ~tz:Time_zone.utc ~year ~month ~day ~hour:0 ~minute:0 ~second:0 ()

  let ymd y m d = mkdate (int y) (int m) (int d)
  let ym y m = mkdate (int y) (int m) 1
  let y x = mkdate (int x) 1 1
  let yd y d = mkdate (int y) 1 (int d)

  (* Time helpers *)
  let mktime hour minute second =
    let open Timere in
    make_hms_exn ~hour ~minute ~second
    |> Utils.second_of_day_of_hms
    |> float_of_int

  let hms h m s = mktime (int h) (int m) (int s)
  let hm h m =  mktime (int h) (int m) 0
  let h x =  mktime (int x) 0 0
  let z = 0.
  let sign s = if s = '-' then fun x -> "-" ^ x else fun x -> x
  let frac = function
    | "" -> 0.
    | f -> float_of_string ("." ^ (String.sub f 1 (String.length f - 1)))

}

(* FIXME: Some 0 values should not be allowed. *)

let num = ['0'-'9']
let year = num num num num
let year_ext = ['+''-'] num year
let month = ('0'num) | ('1'['0'-'2'])
let day = (['0'-'2']num) | ('3'['0''1'])
let week = ('0'num) | (['1'-'4']num) | ('5'['0'-'3'])
let week_day = ['1'-'7']
let hour = (['0'-'1']num) | ('2'['0'-'4'])
let minute = (['0'-'5']num)
let second = ((['0'-'5']num) | '6''0')
let year_day = (['0'-'2'] num num) | ('3' (['0'-'5'] num | '6' ['0'-'6']))
let frac = [',''.']num+

rule date = parse

(* YYYY / ±YYYYY *)
| (year | year_ext) as x
  { y x }

(* YYYY-MM *)
| (year as y) '-' (month as m)
  { ym y m }

(* YYYYMMDD / YYYY-MM-DD *)
| ((year as y) (month as m) (day as d))
| ((year as y) '-' (month as m) '-' (day as d))
  { ymd y m d}

(* YYYYWww / YYYY-Www *)
| (year as _y) 'W' (week as _w)
| (year as _y) "-W" (week as _w)
  { assert false }

(* YYYYWwwD / YYYY-Www-D *)
| (year as _y) 'W' (week as _w) (week_day as _d)
| (year as _y) '-' 'W' (week as _w) '-' (week_day as _d)
  { assert false }

(* YYYYDDD / YYYY-DDD *)
| (year as y) (year_day as d)
| (year as y) '-' (year_day as d)
  { yd y d }

and time = parse

(* hhmmss / hh:mm:ss *)
| (hour as h) (minute as m) (second as s) (frac? as f)
| (hour as h) ':' (minute as m) ':' (second as s) (frac? as f)
  { hms h m s +. frac f}

(* hhmm / hh:mm *)
| (hour as h) ':'? (minute as m) (frac? as f)
  { hm h m +. (frac f *. 60.)}

(* hh *)
| hour as x (frac? as f)
  { h x +. (frac f *. 3600.) }

and timezone = parse

(* Z *)
| 'Z' | 'z'
  { Some z }

(* ±hhmm / ±hh:mm *)
| (['+''-'] as s) (hour as h) ':'? (minute as m)
  { let s = sign s in Some ( hm (s h) (s m) ) }

(* ±hh *)
| (['+''-'] as s) (hour as x)
  { Some ( h ((sign s) x) ) }

| "" { None }

and delim = parse 'T' | 't' | ' ' as d { Some d } | "" { None }
