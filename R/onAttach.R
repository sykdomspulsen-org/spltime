#' @import data.table
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("spltime", fields = "Version"),
    warning = function(w){
      1
    }
  )
  
  packageStartupMessage(paste0(
    "spltime ",
    version,
    "\n",
    "https://docs.sykdomspulsen.no/spltime"
  ))
}
