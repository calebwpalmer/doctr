#' Print dx functions
#' @export
#' @param dx_fn dx function.

print.dx_fn <- function(dx_fn) {
  writeLines(dx_fn$dx)
  if(!is.null(dx_fn$output)) print(dx_fn$output)
  if(!is.null(dx_fn$visualization)) print(dx_fn$visualization)
}
