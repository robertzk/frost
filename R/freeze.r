#' Compute the cross-reference of N lists
#' 
#' @param ... a list of lists. These will be interpolated
#'    using each other's references. (see examples)
#' @param out integer vector. A list of indices from \code{...}
#'    whose computed static lists should be returned. The default value
#'    is \code{1}.
#' @return a list of lists corresponding to the \code{out} parameter
#'    with values cross-referentially computed. If \code{length(out) == 1},
#'    that list will be returned (and not wrapped in a 1-element list).
#' @export
#' @examples
#' one <- list(a = 1, b = list(c = ref(two), d = 2))
#' two <- list(a = ref(one), b = list(c = 3, d = 4))
#' process_one <- freeze(one, two)  # process_one$b$c is now 3
freeze <- function(..., out = c(1)) {
  # TODO: Re-write this function to use a graph visitation algorithm
  lists <- list(...) 
  if (is.null(names(lists)))
    names(lists) <- as.list(match.call())[-c(1, which('out' == names(match.call())))]

  nested_replace <- function(sublist, keychain = list()) {
    sublist_names <-
      if (is.null(names(sublist))) seq_along(sublist)
      else ifelse(names(sublist) == "", as.list(seq_along(sublist)), as.list(names(sublist)))
    structure(lapply(seq_along(sublist), function(index) {
      if (is.ref(cur <- sublist[[index]])) {
        used_refs <- as.character(cur)
        # Follow the rabbit hole! We are going after references to references
        while (is.ref(cur)) {
          # Find the reference in another nested list with the same keychain
          cur <- Reduce(`[[`, c(as.character(cur), keychain, sublist_names[[index]]), lists)

          if (is.ref(cur) && as.character(cur) %in% used_refs)
            stop("Frost reference '", as.character(cur),

                 "' circularly points to another reference")
          else if (is.ref(cur)) used_refs[length(used_refs) + 1] <- as.character(cur)
        }
        cur
      } else if (is.list(sublist[[index]]))
        nested_replace(sublist[[index]], append(keychain, sublist_names[[index]]))
      else sublist[[index]]
    }), .Names = names(sublist))
  }

  res <- lapply(lists, nested_replace)[out]
  if (length(out) == 1) res[[1]]
  else res
}

