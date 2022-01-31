# date vs iso ====
#' ISO year (character) from Date object
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return ISO year in character
#' @export
#'
#' @examples
#' date_to_isoyear_c('2021-08-11')
#' date_to_isoyear_c(lubridate::today())
date_to_isoyear_c <- function(x = lubridate::today()) {
  yr <- format.Date(x, "%G")
  return(yr)
}



#' ISO week (character) from Date object
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return ISO week in character
#' @export
#'
#' @examples
#' date_to_isoyear_c('2021-08-11')
#' date_to_isoyear_c(lubridate::today())
date_to_isoweek_c <- function(x = lubridate::today()) {
  # wk <- data.table::isoweek(date)
  # wk <- formatC(wk, flag = "0", width = 2)
  wk <- format.Date(x, "%V")
  return(wk)
}





#' ISO year and week (character) from Date object
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return ISO year and week in character
#' @export
#'
#' @examples
#' date_to_isoyearweek_c('2021-08-11')
#' date_to_isoyearweek_c(lubridate::today())
date_to_isoyearweek_c <- function(x = lubridate::today()){
  return(paste0(date_to_isoyear_c(x),"-",date_to_isoweek_c(x)))
}




#' ISO year (numeric) from Date object
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return ISO year in numeric
#' @export
#'
#' @examples
#' date_to_isoyear_n('2021-08-11')
#' date_to_isoyear_n(lubridate::today())
date_to_isoyear_n <- function(x = lubridate::today()) {
  yr <- as.numeric(date_to_isoyear_c(x))
  return(yr)
}

#' ISO week (numeric) from Date object
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return ISO week in numeric
#' @export
#'
#' @examples
#' date_to_isoweek_n('2021-08-11')
#' date_to_isoweek_n(lubridate::today())
date_to_isoweek_n <- function(x = lubridate::today()) {
  wk <- as.numeric(date_to_isoweek_c(x))
  return(wk)
}








# isoyearweek vs isoyear, isoweek ====

#' ISO yearweek to year (numeric)
#' 
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_isoyear_n <- function(yrwk){
  year_n <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[1]}) %>% as.numeric()
  return(year_n)
}


#' ISO yearweek to year (character)
#' 
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_isoyear_c <- function(yrwk){
  year_c <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[1]})
  return(year_c)
}

#' ISO yearweek to week (numeric)
#' 
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_isoweek_n <- function(yrwk){
  week_n <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[2]}) %>% as.numeric()
  return(week_n)
}


#' ISO yearweek to week (character)
#' 
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_isoweek_c <- function(yrwk){
  week_c <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[2]})
  return(week_c)
}
