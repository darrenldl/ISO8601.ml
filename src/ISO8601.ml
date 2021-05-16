module Lexer = ISO8601_lexer

module Permissive = struct

    let date_lex' lexbuf = Lexer.date lexbuf

    let date_lex lexbuf =
      date_lex' lexbuf
      |> Timedesc.to_timestamp_float_s_single

    let time_tz_lex' lexbuf =
      let t = Lexer.time lexbuf in
      let tz_offset = Lexer.timezone lexbuf in
      let time_of_day =
        match Timedesc.Time.of_span (Timedesc.Span.of_float_s t) with
        | None -> failwith "Unexpected case"
        | Some x -> x
      in
      let tz_offset = match tz_offset with
        | None -> None
        | Some tz -> Some (int_of_float tz)
      in
      (time_of_day, tz_offset)

    let time_tz_lex lexbuf =
      let (time_of_day, tz_offset) = time_tz_lex' lexbuf in
      let tz_offset =
        match tz_offset with
        | None -> None
        | Some x -> Some (float_of_int x)
      in
      (Timedesc.Span.to_float_s @@ Timedesc.Time.to_span time_of_day, tz_offset)

    let datetime_tz_lex' ~reqtime lexbuf =
      let d = date_lex' lexbuf in
      match Lexer.delim lexbuf with
      | None ->
        (* TODO: this should be a real exception *)
        if reqtime then assert false else (d, true)
      | Some _ ->
        let (time_of_day, tz_offset_s) = time_tz_lex' lexbuf in
        match tz_offset_s with
        | None ->
          let tz = match Timedesc.Time_zone.local () with
            | None -> failwith "Failed to obtain local time zone"
            | Some tz -> tz
          in
          (
            Timedesc.make_exn ~tz
              ~year:(Timedesc.year d)
              ~month:(Timedesc.month d)
              ~day:(Timedesc.day d)
              ~hour:time_of_day.hour
              ~minute:time_of_day.minute
              ~second:time_of_day.second
              ~ns:time_of_day.ns
              (),
            false
          )
        | Some tz_offset_s ->
          (
            let offset_from_utc = Timedesc.Span.make_small ~s:tz_offset_s () in
            Timedesc.make_unambiguous_exn
              ~year:(Timedesc.year d)
              ~month:(Timedesc.month d)
              ~day:(Timedesc.day d)
              ~hour:time_of_day.hour
              ~minute:time_of_day.minute
              ~second:time_of_day.second
              ~ns:time_of_day.ns
              ~offset_from_utc
              (),
            false
          )

    let datetime_tz_lex ?(reqtime=true) lexbuf =
      let (date_time, _only_date) = datetime_tz_lex' ~reqtime lexbuf in
      (
        Timedesc.to_timestamp_float_s_single date_time,
        match Timedesc.offset_from_utc date_time with
        | `Single x -> Some (Timedesc.Span.to_float_s x)
        | `Ambiguous _ -> None
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
