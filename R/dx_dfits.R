#' DFITS
#' @export
#' @param .model A model
#' @param .threshold Cutoff point for significant values, used for output and visualization.
#'  No argument will use a threshold of 1.
#' @param .out Print output to console.
#' @param .viz Print visualization to plotting window.
#'

dx_dfits <- function(.model, .threshold = 1, .out = TRUE, .viz = TRUE) {  
  
  if(missing(.model))
    stop("dx functions require a model object")
  if(missing(.threshold))
    .threshold <- 1
  if(missing(.out))
    .out <- TRUE
  if(missing(.viz))
    .viz <- TRUE
  
  model_name <- deparse(substitute(.model))
  
  dx_return <- list()
  
  dx_return$dx <- c("DFITS:")
  
  diag <- ls.diag(.model)
  diag.df <- diag$dfits %>%
    as.data.frame() %>%
    mutate("Observation Number" = as.numeric(rownames(.model$model)),
           "DFITS" = as.numeric(.)) %>%
    select("Observation Number", "DFITS")
  if(.out == TRUE) {
    obs <- diag.df %>%
      filter(abs(`DFITS`) > .threshold)
    dx_return$output <- obs
  }
  
  if(.viz == TRUE) {
    viz <- ggplot(data = diag.df, aes(x = `Observation Number`, y = `DFITS`)) +
    geom_col() +
    geom_abline(intercept = .threshold, slope = 0, size = 1, color = "red") +
    geom_abline(intercept = (-1 * .threshold), slope = 0, size = 1, color = "red") +
    coord_cartesian(ylim = c(.threshold * -2,.threshold * 2)) +
    ggtitle(paste0(model_name,": DFITS"))  +
    labs(x = "Observation Number", y = "DFITS Statistic")
    dx_return$visualization <- viz
  }
  
  class(dx_return) <- "dx_fn"
  dx_return
  
}
