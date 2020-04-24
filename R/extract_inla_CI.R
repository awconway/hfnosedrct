#' @title Credible intervals for FANOVA
#' @rdname extract_inla_CI
#' @export
#' @importFrom brms fixef
extract_inla_CI <- function(model_obj, unit=NULL, pars="randomization_factorHighFlownasaloxygen"){

  result <- list()

  mu <- fixef(model_obj, pars=pars)[1] %>%
    round(1) %>%
    format(nsmall = 1)

  lower <- fixef(model_obj, pars=pars)[3] %>%
    round(1) %>%
    format(nsmall = 1)

  upper <- fixef(model_obj, pars=pars)[4] %>%
    round(1) %>%
    format(nsmall = 1)

  result[["mean"]] <- paste0(mu, " " ,unit)

  result[["interval"]] <- paste0("(", lower, ", ",upper ,")")

  result[["full"]] <- paste0(result[["mean"]], " ", result[["interval"]])

  return(result)
}
