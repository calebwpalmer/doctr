#' Standardized Residuals
#' @export
#' @param .model A model
#' @param .threshold Cutoff point for significant values, used for output and
#'  visualization. A missing argument will use a threshold of 3.
#' @param .out Print output to console.
#' @param .visualisation Print plot to plotting window.
#'

dx_res_stand <- function(.model, .threshold = 3, .out = TRUE, .viz = TRUE) {
  
  if(missing(.model))
    stop("dx functions require a model object")
  if(missing(.threshold))
    .threshold <- 3
  if(missing(.out))
    .out <- TRUE
  if(missing(.viz))
    .viz <- TRUE
  
  model_name <- deparse(substitute(.model))
  
  diag <- ls.diag(.model)
  diag.df <- diag$std.res %>%
    as.data.frame() %>%
    mutate("Observation Number" = as.numeric(rownames(.model$model)),
           "Standardized Residuals" = as.numeric(.)) %>%
    select("Observation Number", "Standardized Residuals")
  if(.out == TRUE) {
    obs <- diag.df %>%
    filter(abs(`Standardized Residuals`) > .threshold) %>%
    print(.)
    out <- list("Standardized Residuals", obs)
  }
  
  if(.viz == TRUE) {
    viz <- ggplot(data = diag.df, aes(x = `Observation Number`, y = `Standardized Residuals`)) +
      geom_col() +
      geom_abline(intercept = .threshold, slope = 0, size = 1, color = "red") +
      geom_abline(intercept = (-1 * .threshold), slope = 0, size = 1, color = "red") +
      coord_cartesian(ylim = c(.threshold * -2,.threshold * 2)) +
      ggtitle(paste0(model_name, ": Standardized Residuals"))  +
      labs(x = "Observation Number", y = "Residual Distance")
    print(viz)
  }
}
