module Lexer = ISO8601_lexer

module Permissive = struct

    let date_lex' lexbuf = Lexer.date lexbuf

    let date_lex lexbuf =
      date_lex' lexbuf
      |> Timere.Date_time.to_timestamp_single
      |> Int64.to_float

    let time_tz_lex' lexbuf =
      let t = Lexer.time lexbuf in
      let tz_offset = Lexer.timezone lexbuf in
      let t' = int_of_float t in
      let hms =
        Timere.Utils.hms_of_second_of_day t'
      in
      let frac = t -. (float_of_int t') in
      let tz_offset = match tz_offset with
        | None -> None
        | Some tz -> Some (int_of_float tz)
      in
      (hms, frac, tz_offset)

    let time_tz_lex lexbuf =
      let (hms, frac, tz_offset) = time_tz_lex' lexbuf in
      let tz_offset =
        match tz_offset with
        | None -> None
        | Some x -> Some (float_of_int x)
      in
      (float_of_int (Timere.Utils.second_of_day_of_hms hms) +. frac, tz_offset)

    let datetime_tz_lex' ~reqtime lexbuf =
      let open Timere in
      let d = date_lex' lexbuf in
      match Lexer.delim lexbuf with
      | None ->
        (* TODO: this should be a real exception *)
        if reqtime then assert false else (d, 0., true)
      | Some _ ->
        let (hms, frac, tz_offset_s) = time_tz_lex' lexbuf in
        match tz_offset_s with
        | None ->
          let tz = match Time_zone.local () with
            | None -> failwith "Failed to obtain local time zone"
            | Some tz -> tz
          in
          (
            Date_time.make_exn ~tz
              ~year:d.year
              ~month:d.month
              ~day:d.day
              ~hour:hms.hour
              ~minute:hms.minute
              ~second:hms.second (),
            frac,
            false
          )
        | Some tz_offset_s ->
          (
            Date_time.make_precise_exn
              ~year:d.year
              ~month:d.month
              ~day:d.day
              ~hour:hms.hour
              ~minute:hms.minute
              ~second:hms.second
              ~tz_offset_s (),
            frac,
            false
          )

    let datetime_tz_lex ?(reqtime=true) lexbuf =
      let (date_time, frac, _only_date) = datetime_tz_lex' ~reqtime lexbuf in
      (
        Int64.to_float (Timere.Date_time.to_timestamp_single date_time) +. frac,
        let open Timere.Date_time in
        match date_time.tz_info with
        | `Tz_only _ -> None
        | `Tz_offset_s_only offset
        | `Tz_and_tz_offset_s (_, offset) ->
          Some (float_of_int offset)
      )

    let time_lex lexbuf =
      fst (time_tz_lex lexbuf)

    let datetime_lex ?(reqtime=true) lexbuf =
      fst (datetime_tz_lex ~reqtime:reqtime lexbuf)

    let date s = date_lex (Lexing.from_string s)

    let time s = time_lex (Lexing.from_string s)

    let time_tz s = time_tz_lex (Lexing.from_string s)

    let datetime_tz ?(reqtime=true) s =
      datetime_tz_lex ~reqtime:reqtime (Lexing.from_string s)

    let datetime ?(reqtime=true) s =
      datetime_lex ~reqtime:reqtime (Lexing.from_string s)

    let pp_format fmt format x tz =

      let open Unix in
      let open Format in

      (* Be careful, do not forget to print timezone if there is one,
       * or information printed will be wrong. *)
      let x = match tz with
        | None    -> localtime x
        | Some tz -> gmtime (x +. tz)
      in

      let print_tz_hours fmt tz =
        fprintf fmt "%0+3d" (Stdlib.truncate (tz /. 3600.))
      in

      let print_tz_minutes fmt tz =
        fprintf fmt "%02.0f" (mod_float (abs_float (tz /. 60.)) 60.0)
      in

      let conversion =
        let pad2 = fprintf fmt "%02d" in
        let pad4 = fprintf fmt "%04d" in
        function

        (* Date *)
        | 'Y' -> pad4 (x.tm_year + 1900)
        | 'M' -> pad2 (x.tm_mon + 1)
        | 'D' -> pad2 x.tm_mday

        (* Time *)
        | 'h' -> pad2 x.tm_hour
        | 'm' -> pad2 x.tm_min
        | 's' -> pad2 x.tm_sec

        (* Timezone *)
        | 'Z' -> begin match tz with (* with colon *)
          | None    -> ()
          | Some 0. -> fprintf fmt "Z"
          | Some tz ->
            print_tz_hours fmt tz;
            fprintf fmt ":";
            print_tz_minutes fmt tz
        end
        | 'z' -> begin match tz with (* without colon *)
          | None    -> ()
          | Some 0. -> fprintf fmt "Z"
          | Some tz ->
            print_tz_hours fmt tz;
            print_tz_minutes fmt tz
        end

        | '%' -> pp_print_char fmt '%'
        |  c  -> failwith ("Bad format: %" ^ String.make 1 c)

      in

      let len = String.length format in
      let rec parse_format i =
        if i = len then ()
        else match String.get format i with
             | '%' -> conversion (String.get format (i + 1)) ;
                      parse_format (i + 2)
             |  c  -> pp_print_char fmt c ;
                      parse_format (i + 1) in

      parse_format 0

    let pp_date_utc fmt x = pp_format fmt "%Y-%M-%D" x (Some 0.)
    let pp_date     fmt x = pp_format fmt "%Y-%M-%D" x None

    let pp_time_utc fmt x = pp_format fmt "%h:%m:%s" x (Some 0.)
    let pp_time     fmt x = pp_format fmt "%h:%m:%s" x None

    let pp_datetime_utc fmt x = pp_format fmt "%Y-%M-%DT%h:%m:%s" x (Some 0.)
    let pp_datetime     fmt x = pp_format fmt "%Y-%M-%DT%h:%m:%s" x None

    let pp_datetimezone fmt (x, tz) =
      pp_format fmt "%Y-%M-%DT%h:%m:%s%Z" x (Some tz)

    let pp_date_basic_utc fmt x = pp_format fmt "%Y%M%D" x (Some 0.)
    let pp_date_basic     fmt x = pp_format fmt "%Y%M%D" x None

    let pp_time_basic_utc fmt x = pp_format fmt "%h%m%s" x (Some 0.)
    let pp_time_basic     fmt x = pp_format fmt "%h%m%s" x None

    let pp_datetime_basic_utc fmt x = pp_format fmt "%Y%M%DT%h%m%s" x (Some 0.)
    let pp_datetime_basic     fmt x = pp_format fmt "%Y%M%DT%h%m%s" x None

    let pp_datetimezone_basic fmt (x, tz) =
      pp_format fmt "%Y%M%DT%h%m%s%z" x (Some tz)

    let string_of_aux printer x =
      ignore (Format.flush_str_formatter ()) ;
      printer Format.str_formatter x ;
      Format.flush_str_formatter ()

    let string_of_date_utc = string_of_aux pp_date_utc
    let string_of_date     = string_of_aux pp_date

    let string_of_time_utc = string_of_aux pp_time_utc
    let string_of_time     = string_of_aux pp_time

    let string_of_datetime_utc = string_of_aux pp_datetime_utc
    let string_of_datetime     = string_of_aux pp_datetime

    let string_of_datetimezone = string_of_aux pp_datetimezone

    let string_of_date_basic_utc = string_of_aux pp_date_basic_utc
    let string_of_date_basic     = string_of_aux pp_date_basic

    let string_of_time_basic_utc = string_of_aux pp_time_basic_utc
    let string_of_time_basic     = string_of_aux pp_time_basic

    let string_of_datetime_basic_utc = string_of_aux pp_datetime_basic_utc
    let string_of_datetime_basic     = string_of_aux pp_datetime_basic

    let string_of_datetimezone_basic = string_of_aux pp_datetimezone_basic

end
