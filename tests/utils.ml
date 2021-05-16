let mkdatetime year month day hour minute second =
  Timedesc.make_exn ~tz:Timedesc.Time_zone.utc ~year ~month ~day ~hour ~minute ~second ()
  |> Timedesc.to_timestamp_float_s_single

let mkdate y m d = mkdatetime y m d 0 0 0

let mktime h m s = h *. 3600. +. m *. 60. +. s
