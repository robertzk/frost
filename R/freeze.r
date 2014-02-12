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
#' @examples
#' one <- list(a = 1, b = list(c = ref(two), d = 2))
#' two <- list(a = ref(one), b = list(c = 3, d = 4))
#' process_one <- freeze(one, two)  # process_one$b$c is now 3
freeze <- function(..., out = c(1)) {
  lists <- list(...) 
  names(lists) <- match.call()[-1]
  nested_replace <- function(sublist, keychain = list()) {
    lapply(seq_len(sublist), function(index) {
      if (is.ref(sublist[[index]]))
        Reduce(`[[`, append(append(list(as.character(sublist[[index]])), keychain), index), lists)
      else if (is.list(sublist[[index]]))
        nested_replace(sublist[[index]], append(keychain, names(sublist)[[index]] %||% index))
      else sublist[[index]]
    })
  }


}

