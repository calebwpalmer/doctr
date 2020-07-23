#' Select Model
#'
#' @param model A model.
#'
#' @description Internal function for dx, selects dx functions using model
#' class and family.
#'

dx_select <- function(model) {
  mclass <- class(model)
  
  if(mclass [[1]] == "lm") {
    dx_calls <- exprs(dx_res_stand(),
              dx_res_stud(),
              dx_cooks(),
              dx_dif_fits())
  }
  
  else if(model.type == "glm") {
    funs <- c(dx_cooks())
  }
  
  return(dx_calls)
}

