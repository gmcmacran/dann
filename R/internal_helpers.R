#' @keywords internal
.onUnload <- function(libpath) {
  library.dynam.unload("dann", libpath)
}
