# isoxx_c ====
#' ISO year (character) from Date object
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return ISO year in character
#' @export
#'
#' @examples
#' isoyear_c('2021-08-11')
#' isoyear_c(lubridate::today())
isoyear_c <- function(x = lubridate::today()) {
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
#' isoyear_c('2021-08-11')
#' isoyear_c(lubridate::today())
isoweek_c <- function(x = lubridate::today()) {
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
#' isoyear_c('2021-08-11')
#' isoyear_c(lubridate::today())
isoyearweek_c <- function(x = lubridate::today()){
  return(paste0(isoyear_c(x),"-",isoweek_c(x)))
}



# isoxx_n ====

#' ISO year (numeric) from Date object
#'
#' @param x a Date object or string, in the form of 'yyyy-mm-dd'
#'
#' @return ISO year in numeric
#' @export
#'
#' @examples
#' isoyear_n('2021-08-11')
#' isoyear_n(lubridate::today())
isoyear_n <- function(x = lubridate::today()) {
  yr <- as.numeric(isoyear_c(x))
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
#' isoweek_n('2021-08-11')
#' isoweek_n(lubridate::today())
isoweek_n <- function(x = lubridate::today()) {
  wk <- as.numeric(isoweek_c(x))
  return(wk)
}










