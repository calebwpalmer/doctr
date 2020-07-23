#' Diagnose model
#' @export
#' @param .model A model.
#' @param ... One or more unquoted dx functions (see details). User-defined
#' dx functions can receive arguments native to the function and will supersede
#' dx arguments. Leaving "..." blank will cause dx to automatically select 
#' dx functions based on model class and family. See details for documentation
#' on selected dx_functions.
#' @param .out Print output to console. User-defined dx functions will overwrite
#' this argument.
#' @param .viz Print plots to plotting window. User-defined dx functions will
#' overwrite this argument.
#'
#' @details A blank "..." will cause dx to automatically select dx functions
#' based on class and family (if applicable).
#' 
#' At this time only lm() class models are supported.
#' 
#' lm() diagnostics:
#' \itemize{
#'   \item \code{\link{dx_cooks}}
#'   \item \code{\link{dx_res_stand}}
#'   \item \code{\link{dx_res_stud}}
#'   \item \code{\link{dx_dif_fits}}
#' }
#' 
#' @examples 
#' dx(model)
#' dx(model, dx_cooks(threshold = 2, out = TRUE), dx_res_stand(),out = FALSE)

dx <- function(.model, ..., .out = TRUE, .viz = TRUE) {
  if(missing(.model)) {
    stop("dx functions require a model object")
    }
  if(missing(.out)) {
    .out <- TRUE
    }
  if(missing(.viz)) {
    .viz <- TRUE
    }

  envcur <- current_env()
  
  out <- enquo(.out)
  viz <- enquo(.viz)
  
  dx_calls <- exprs(...)

  if(length(dx_calls) == 0) dx_calls <- doctr:::dx_select(model = .model)
  
  dx_results <- map(.x = dx_calls, .f = ~ {
    set_model <- call_modify(.x, .model = enexpr(.model))
    stand_fn <- call_standardise(set_model)
    set_out <- if(is.null(stand_fn$out)) {
      call_modify(stand_fn, .out = .out)} else {stand_fn}
    set_viz <- if(is.null(set_out$viz)) {
      call_modify(set_out, .viz = .viz)} else {set_out}
  }, .model, .out = out, .viz = viz) %>% 
  map(., .f = ~ exec(as.function(list(.))), envcur)
  return(dx_results)
}
