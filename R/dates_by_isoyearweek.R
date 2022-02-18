#' Dates of different days within isoyearweeks
#'
#' @format
#' \describe{
#' \item{isoyear}{Isoyear.}
#' \item{isoyearweek}{Isoweek-isoyear.}
#' \item{mon}{Date of Monday.}
#' \item{tue}{Date of Tuesday.}
#' \item{wed}{Date of Wednesday.}
#' \item{thu}{Date of Thursday.}
#' \item{fri}{Date of Friday.}
#' \item{sat}{Date of Saturday.}
#' \item{sun}{Date of Sunday.}
#' \item{weekdays}{List of dates from Mon-Fri}
#' \item{weekend}{List of dates from Sat-Sun}
#' \item{days}{List of dates from Mon-Sun}
#' }
#' @examples
#' # Constructing a vector of dates without removing the Date class
#' do.call("c", dates_by_isoyearweek[isoyearweek %in% c("2021-01", "2021-02")]$weekdays)
"dates_by_isoyearweek"

# Creates the norway_locations data.table
gen_dates_by_isoyearweek <- function() {
  . <- NULL
  isoyear <- NULL
  isoyearweek <- NULL
  day <- NULL
  mon <- NULL
  tue <- NULL
  wed <- NULL
  thu <- NULL
  fri <- NULL
  sat <- NULL
  sun <- NULL
  # declare isoyear and isoyearweek to NULL 
  # 36,37
  
  days <- data.table::data.table(day = seq.Date(as.Date("1950-01-02"), as.Date("2100-01-01"), by = "days"))
  days[, isoyear := as.integer(format.Date(day, format = "%G"))]
  days[, isoyearweek := format.Date(day, format = "%G-%V")]
  days <- days[, .(mon = as.Date(min(day))), by = .(isoyear, isoyearweek)]
  days[, tue := mon + 1]
  days[, wed := mon + 2]
  days[, thu := mon + 3]
  days[, fri := mon + 4]
  days[, sat := mon + 5]
  days[, sun := mon + 6]
  days[, weekdays := list(list(c(mon[1], tue[1], wed[1], thu[1], fri[1]))), by = isoyearweek]
  days[, weekend := list(list(c(sat[1], sun[1]))), by = isoyearweek]
  days[, days := list(list(c(mon[1], tue[1], wed[1], thu[1], fri[1], sat[1], sun[1]))), by = isoyearweek]
  
  setkey(days, isoyear, isoyearweek, mon, tue, wed, thu, fri, sat, sun)
  
  return(days)
}
# x <- gen_dates_by_isoyearweek()