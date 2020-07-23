dx_dif_beta <- function(.model, .threshold = 1, .out = TRUE, .viz = TRUE) {
  
  if(missing(.model))
    stop("dx functions require a model object")
  if(missing(.threshold))
    .threshold <- 1
  if(missing(.out))
    .out <- TRUE
  if(missing(.viz))
    .viz <- TRUE
  
  model_name <- deparse(substitute(.model))
  if(.viz == TRUE)
    ggplot(data = .model, aes(seq_along(.cooksd), .cooksd)) +
      geom_col() +
      geom_abline(intercept = .threshold, slope = 0, size = 1, color = "red") +
      ylim(0,(.threshold * 4)) +
      ggtitle(paste0(deparse(substitute(.model)), ": Cook's Distance"))  +
      labs(x = "Observation", y = "Cook's Distance")
}
