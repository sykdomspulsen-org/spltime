# 
# date -> calyear ====
# 

#' Date -> calyear (character)
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return ISO year in character
#' @export
#'
#' @examples
#' date_to_calyear_c('2021-08-11')
#' date_to_calyear_c(lubridate::today())
date_to_calyear_c <- function(x = lubridate::today()) {
  yr <- format.Date(x, "%Y")
  return(yr)
}

#' Date -> calmonth (character)
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return calmonth ("XX")
#' @export
#'
#' @examples
#' date_to_calmonth_c('2021-08-11')
#' date_to_calmonth_c(lubridate::today())
date_to_calmonth_c <- function(x = lubridate::today()) {
  # wk <- data.table::isoweek(date)
  # wk <- formatC(wk, flag = "0", width = 2)
  wk <- format.Date(x, "%m")
  return(wk)
}

#' Date -> calyearmonth (character)
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return calyearmonth ("YYYYMXX")
#' @export
#'
#' @examples
#' date_to_calyearmonth_c('2021-08-11')
#' date_to_calyearmonth_c(lubridate::today())
date_to_calmonth_c <- function(x = lubridate::today()) {
  format.Date(x, "%YM%m")
}


