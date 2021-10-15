#' isoyearweek_to_year_n
#' isoyearweek to year (numeric)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_year_n <- function(yrwk){
  year_n <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[1]}) %>% as.numeric()
  return(year_n)
}


#' isoyearweek_to_year_c
#' isoyearweek to year (character)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_year_c <- function(yrwk){
  year_c <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[1]})
  return(year_c)
}

#' isoyearweek_to_week_n
#' isoyearweek to week (numeric)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_week_n <- function(yrwk){
  week_n <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[2]}) %>% as.numeric()
  return(week_n)
}


#' isoyearweek_to_week_c
#' isoyearweek to week (character)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_week_c <- function(yrwk){
  week_c <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[2]})
  return(week_c)
}
