let mkdatetime year month day hour minute second =
  let open Timere in
  let month = match Timere.Utils.month_of_human_int month with
    | None -> failwith "Invalid month"
    | Some month -> month
  in
  Date_time.make_exn ~tz:Time_zone.utc ~year ~month ~day ~hour ~minute ~second ()
  |> Date_time.to_timestamp_single
  |> Int64.to_float

let mkdate y m d = mkdatetime y m d 0 0 0

let mktime h m s = h *. 3600. +. m *. 60. +. s
