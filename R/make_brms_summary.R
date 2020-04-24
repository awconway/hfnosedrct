#' @title Summary of brms models
#' @rdname make_brms_summary
#' @export
#'
make_brms_summary <- function(model_obj, unit, effect_type, response, response_title){

  # Treatment effect estimate
  CI_randomization <- c(extract_CI_brms(model_obj,
                                        pars = "randomization_factorHighFlownasaloxygen",
                                        unit = unit))

  estimate <- c(CI_randomization$full)

  result <- data.frame(model=response, estimate)

  # Explicitly define variable types
  result$model <- response_title
  result$effect_type <- effect_type
  result$estimate <- result$estimate %>% as.character()

  return(result)
}

