#' @title Summaries from posterior probabilities
#' @name posterior_prob_summary
#' @export
#' @importFrom brms posterior_samples

posterior_prob_summary <- function(model_obj,unit=NULL,
                 pars="randomization_factorHighFlownasaloxygen",
                 threshold){
result <- extract_CI_brms(model_obj=model_obj, unit=unit, pars=pars)
# Compute probability statements
post_samp <- posterior_samples(model_obj, pars=pars)
post_samp <- exp(post_samp)
result[["p_above"]] <- (sum(post_samp > threshold)/nrow(post_samp)) %>%
  round(2) %>%
  format(nsmall=2)
result[["p_below"]] <- (sum(post_samp < threshold)/nrow(post_samp)) %>%
  round(2) %>%
  format(nsmall=2)
return(result)
}
