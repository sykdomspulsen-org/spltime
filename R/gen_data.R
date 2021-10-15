
gen_data <- function(){
  # gen_data(file.path(getwd(),"data"))
  base_loc <- file.path(getwd(),"data")
  
  # gen_world_dates_isoyearweek ----
  world_dates_isoyearweek <- gen_world_dates_isoyearweek()
  save(world_dates_isoyearweek, file = file.path(base_loc, paste0("world_dates_isoyearweek",".rda")), compress = "xz")
  
  
}

