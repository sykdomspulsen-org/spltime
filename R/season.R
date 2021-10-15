#' week_to_seasonweek_n
#' Natural week to season week. Season week 1 is natural week 30.
#' @param week Natural week in a year
#' @export
week_to_seasonweek_n <- function(week){
  # take both char/n in input
  
  # real week 30 is the start of season, week 1
  # original: fhi::x(20)
  if(week>53 | week <1){stop("natural week needs to be between 1 to 53")}
  
  retval <- week
  retval[week >= 30] <- week[week >= 30] - 29
  retval[week < 30] <- week[week < 30] + 23
  retval[week == 53] <- 23.5
  
  return(retval)
}

#' seasonweek_to_week_c
#' Season week to natural week. Season week 1 is natural week 30.
#' @param seasonweek Season week in a year
#' @export
seasonweek_to_week_c <- function(seasonweek){
  # influenza week 1 (x) is real week 30
  if(seasonweek >52 | seasonweek<1){stop("seasonweek needs to be between 1 to 52, or 23.5")}
  
  retval <- seasonweek
  retval[seasonweek <= 23] <- seasonweek[seasonweek <= 23] + 29
  retval[seasonweek > 23] <- seasonweek[seasonweek >23] - 23
  retval[seasonweek == 23.5] <- 53
  # return double digit: 01, 09, 10, 11
  retval <- formatC(retval, width=2, flag="0")
  
  return(retval)
}

#' seasonweek_to_week_n
#' Season week to natural week. Season week 1 is natural week 30.
#' @param seasonweek Season week in a year
#' @export
seasonweek_to_week_n <- function(seasonweek){
  # influenza week 1 (x) is real week 30
  if(seasonweek >52 | seasonweek<1){stop("seasonweek needs to be between 1 to 52, or 23.5")}
  
  retval <- seasonweek
  retval[seasonweek <= 23] <- seasonweek[seasonweek <= 23] + 29
  retval[seasonweek > 23] <- seasonweek[seasonweek >23] - 23
  retval[seasonweek == 23.5] <- 53
  return(retval)
}



