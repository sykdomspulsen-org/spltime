#' @import data.table
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "spltime ",
    utils::packageDescription("spltime")$Version,
    "\n",
    "https://docs.sykdomspulsen.no/spltime"
  ))
}
