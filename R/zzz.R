#' @export
.onAttach <- function(libname, pkgname) {
  # to show a startup message
  packageStartupMessage("The PurIST interface is loading")
}
