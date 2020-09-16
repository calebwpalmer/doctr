#' Cook's Distance
#' @export
#' @param .model A model
#' @param .threshold Cutoff point for significant values, used for output and visualisation.
#'  No argument will use a threshold of 1.
#' @param .out Print output to console.
#' @param .viz Print visualization to plotting window.
#'

dx_cooks <-function(.model, .threshold = 1, .out = TRUE, .viz = TRUE) {
  
  if(missing(.model))
    stop("dx functions require a model object")
  if(missing(.threshold))
    .threshold <- 1
  if(!.threshold > 0 | !is.numeric(.threshold))
    stop(".threshold must be a number greater than 0")
  if(missing(.out))
    .out <- TRUE
  if(missing(.viz))
    .viz <- TRUE
  
  model_name <- deparse(substitute(.model))
  
  dx_return <- list()
  
  dx_return$dx <- c("Cook's Distance:")
  
  diag <- ls.diag(.model)
  diag.df <- diag$cooks %>%
    as.data.frame() %>%
    dplyr::mutate("Observation Number" = as.numeric(rownames(.model$model)),
           "Cook's Distance" = as.numeric(.)) %>%
    dplyr::select("Observation Number", "Cook's Distance")
  if(.out == TRUE) {
    obs <- diag.df %>%
    dplyr::filter(abs(`Cook's Distance`) > .threshold)
    dx_return$output <- obs
  }
  
  if(.viz == TRUE) {
    viz <- ggplot2::ggplot(data = diag.df, aes(x = `Observation Number`, y = `Cook's Distance`)) +
      ggplot2::geom_col() +
      ggplot2::geom_abline(intercept = .threshold, slope = 0, size = 1, color = "red") +
      ggplot2::geom_abline(intercept = (-1 * .threshold), slope = 0, size = 1, color = "red") +
      ggplot2::coord_cartesian(ylim = c(0, .threshold*2)) +
      ggplot2::ggtitle(paste0(model_name, ": Cook's Distance"))  +
      ggplot2::labs(x = "Observation Number", y = "Cook's Distance")
    dx_return$visualization <- viz
  }
  
  class(dx_return) <- "dx_fn"
  dx_return
  
}
