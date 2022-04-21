
gen_data <- function() {
  # gen_data(file.path(getwd(),"data"))
  base_loc <- file.path(getwd(), "data")

  # gen_dates_by_isoyearweek ----
  dates_by_isoyearweek <- gen_dates_by_isoyearweek()
  save(dates_by_isoyearweek, file = file.path(base_loc, paste0("dates_by_isoyearweek", ".rda")), compress = "xz")
  
  # gen_norway_workdays_by_date ----
  norway_workdays_by_date <- gen_norway_workdays_by_date()
  save(norway_workdays_by_date, file = file.path(base_loc, paste0("norway_workdays_by_date", ".rda")), compress = "xz")
  
  # gen_norway_workdays_by_isoyearweek ----
  norway_workdays_by_isoyearweek <- gen_norway_workdays_by_isoyearweek()
  save(norway_workdays_by_isoyearweek, file = file.path(base_loc, paste0("norway_workdays_by_isoyearweek", ".rda")), compress = "xz")
}
