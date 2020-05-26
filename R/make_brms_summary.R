#' @title Summary of brms models
#' @rdname make_brms_summary
#' @export
#'
make_brms_summary <- function(model_obj, unit, effect_type, response, model_title, row, logarithm=FALSE){

  # Treatment effect estimate
  CI_randomization <- c(extract_CI_brms(model_obj,
                                        pars = "randomization_factorHighFlownasaloxygen",
                                        unit = unit,
                                        effect_type = effect_type,
                                        logarithm = logarithm))

  estimate <- c(CI_randomization$full)

  result <- data.frame(model=response, estimate)

  # Explicitly define variable types
  result$model <- model_title
  result$row <- row
  result$effect_type <- effect_type
  result$estimate <- result$estimate %>% as.character()

  return(result)
}

