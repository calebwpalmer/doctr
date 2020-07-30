#' Select Model
#'
#' @param model A model.
#'
#' @description Internal function for dx, selects dx functions using model
#' class and family.
#'

dx_select <- function(model) {
  mclass <- class(model)
  
dx_calls <<- if(mclass [[1]] == "lm") {
    exprs(
      dx_res_stand(),
              dx_res_stud(),
              dx_cooks(),
              dx_dfits())
  }
  
  else if(model.type == "glm") {
    exprs(dx_cooks())
  }
  
  dx_calls
}

