#' @title CI for subgroup
#' @rdname extract_CI_subgroup
#' @export
#'
#' @importFrom brms posterior_samples
extract_CI_subgroup <- function(model_obj, unit = NULL, pars){

  result <- extract_CI_brms(model_obj=model_obj, unit=unit, pars=pars)

  # Compute probability statements
  post_samp <- posterior_samples(model_obj, pars=pars)
  result[["p_above"]] <- (sum(post_samp > 4)/nrow(post_samp)) %>%
    round(2) %>%
    format(nsmall=2)
  result[["p_below"]] <- (sum(post_samp < -4)/nrow(post_samp)) %>%
    round(2) %>%
    format(nsmall=2)

  return(result)
}
