#' Frost
#'
#' Frost allows one to take homogeneously structured nested lists
#' and introduce cross-references. For example, if we have
#'
#' \code{one <- list(a = 1, b = list(c = ref(two), d = 2))}
#' \code{two <- list(a = ref(one), b = list(c = 3, d = 4))}
#'
#' Frost could compile these two lists into
#'
#' one <- list(a = 1, b = list(c = 3, d = 2))
#' two <- list(a = 1, b = list(c = 3, d = 4))
#'
#' So that neither contains a full specification of its values
#' but in unison they do.
#'
#' In particular, Frost is useful for the problem of satisfying DRY
#' (Don't Repeat Yourself) when giving Syberia model definitions.
#'
#' @name frost
#' @docType package
NULL

