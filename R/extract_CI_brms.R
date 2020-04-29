#' @title Credible intervals for treatment effect
#' @rdname extract_CI_brms
#' @export
#' @importFrom brms fixef
#'
extract_CI_brms <- function(model_obj, unit=NULL, pars="randomization_factorHighFlownasaloxygen", effect_type){

  result <- list()

  mu <- fixef(model_obj, pars=pars)[1]

  lower <- fixef(model_obj, pars=pars)[3]

  upper <- fixef(model_obj, pars=pars)[4]

  if(effect_type=="Odds ratio"){
    mu <- mu %>%
      exp()

    lower <- lower %>%
      exp()

    upper <- upper %>%
      exp()
  }

  # Formatting
  mu <- mu %>%
    round(1) %>%
    format(nsmall = 1)

  lower <- lower %>%
    round(1) %>%
    format(nsmall = 1)

  upper <- upper %>%
    round(1) %>%
    format(nsmall = 1)

  result[["mean"]] <- paste0(mu, " " ,unit)

  result[["interval"]] <- paste0("(", lower, ", ",upper ,")")

  result[["full"]] <- paste0(result[["mean"]], " ", result[["interval"]])

  return(result)
}
