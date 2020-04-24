#' @title Credible intervals for treatment effect
#' @rdname extract_CI
#' @export
#'
#' @importFrom brms fixef
extract_CI <- function(model, unit=NULL, pars="randomization_factorHighFlownasaloxygen"){

  result <- list()

  mu <- fixef(model, pars=pars)[1] %>%
    round(1) %>%
    format(nsmall = 1)

  lower <- fixef(model, pars=pars)[3] %>%
    round(1) %>%
    format(nsmall = 1)

  upper <- fixef(model, pars=pars)[4] %>%
    round(1) %>%
    format(nsmall = 1)

  result[["mean"]] <- paste0(mu, unit)

  result[["interval"]] <- paste0("(", lower, ", ",upper ,")")

  result[["full"]] <- paste0(result[["mean"]], " ", result[["interval"]])

  return(result)
}


