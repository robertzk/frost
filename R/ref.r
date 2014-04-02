# Implicit references for embedding frost objects.
# This is just another word for substitute.
#' name ref
#' @docType class
#' @export
ref <- function(x) {
  if (is.character(x)) ref_obj <- x
  else ref_obj <- deparse(substitute(x))
  class(ref_obj) <- c("frostRef")
  ref_obj
}

#' @export
is.ref <- function(x) inherits(x, "frostRef")

