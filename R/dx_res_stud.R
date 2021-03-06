#' Student Residuals
#' @export
#' @param .model A model
#' @param .threshold Cutoff point for significant values, used for output and visualization.
#'  No argument will use a threshold of 2.
#' @param .out Print output to console.
#' @param .viz Print visualization to plotting window.
#'

dx_res_stud <- function(.model, .threshold = 2, .out = TRUE, .viz = TRUE) {  
  
  if(missing(.model))
    stop("dx functions require a model object")
  if(missing(.threshold))
    .threshold <- 2
  if(missing(.out))
    .out <- TRUE
  if(missing(.viz))
    .viz <- TRUE
  
  model_name <- deparse(substitute(.model))
  
  dx_return <- list()
  
  dx_return$dx <- c("Student Residuals:")
  
  diag <- ls.diag(.model)
  diag.df <- diag$stud.res %>%
    as.data.frame() %>%
    mutate("Observation Number" = as.numeric(rownames(.model$model)),
           "Student Residuals" = as.numeric(.)) %>%
    select("Observation Number", "Student Residuals")
  if(.out == TRUE) {
    obs <- diag.df %>%
    filter(abs(`Student Residuals`) > .threshold)
    dx_return$output <- obs
  }
  
  if(.viz == TRUE) {
    viz <- ggplot(data = diag.df, aes(x = `Observation Number`, y = `Student Residuals`)) +
      geom_col() +
      geom_abline(intercept = .threshold, slope = 0, size = 1, color = "red") +
      geom_abline(intercept = (-1 * .threshold), slope = 0, size = 1, color = "red") +
      coord_cartesian(ylim = c(.threshold * -2,.threshold * 2)) +
      ggtitle(paste0(model_name, ": Student Residuals"))  +
      labs(x = "Observation Number", y = "Residual Distance")
    dx_return$visualization <- viz
  }
  
  class(dx_return) <- "dx_fn"
  dx_return
  
}
