blowb_by_hour = blowb |> 
  dplyr::filter(lubridate::year(blow_by_time) == my_opts$year) |> 
  dplyr::mutate(the_hour = lubridate::hour(blow_by_time)) |> 
  dplyr::mutate(is_overnight = the_hour <= 5 | the_hour > 20) |> 
  dplyr::count(is_overnight,the_hour, name = 'Number of Blowbys')

blowb_by_hour |> 
  dplyr::group_by(is_overnight) |> 
  dplyr::reframe(total_blowbys = sum(`Number of Blowbys`))