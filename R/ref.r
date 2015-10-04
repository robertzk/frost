# Implicit references for embedding frost objects.
# This is just another word for substitute.
#' name ref
#' @param x object. This will be converted to a reference. That is,
#'   if character, it will be wrapped in a \code{frostRef} class,
#'   and if not, it will be deparsed and substituted and wrapped
#'   in a \code{frostRef} class.
#' @docType class
#' @export
ref <- function(x) {
  if (tryCatch(is.character(x), error = function(.) FALSE)) ref_obj <- x
  else ref_obj <- deparse(substitute(x))
  class(ref_obj) <- c("frostRef")
  ref_obj
}

#' Whether or not an object is a frost reference.
#'
#' @param x ANY. Any R object.
#' @return whether or not it is a frost reference of S3 class
#'    \code{"frostRef"}.
#' @export
is.ref <- function(x) inherits(x, "frostRef")

