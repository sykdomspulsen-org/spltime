
gen_data <- function(){
  # gen_data(file.path(getwd(),"data"))
  base_loc <- file.path(getwd(),"data")
  
  # gen_dates_by_isoyearweek ----
  dates_by_isoyearweek <- gen_dates_by_isoyearweek()
  save(dates_by_isoyearweek, file = file.path(base_loc, paste0("dates_by_isoyearweek",".rda")), compress = "xz")
  
  
}

