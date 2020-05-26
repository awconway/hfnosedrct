#' @title Credible intervals for treatment effect
#' @rdname extract_CI_brms
#' @export
#' @importFrom brms fixef posterior_samples
#' @importFrom glue glue
#'
extract_CI_brms <- function(model_obj,
                            unit=NULL,
                            pars="randomization_factorHighFlownasaloxygen",
                            effect_type,
                            logarithm=FALSE){

  result <- list()

  mu <- fixef(model_obj, pars=pars)[1]

  lower <- fixef(model_obj, pars=pars)[3]

  upper <- fixef(model_obj, pars=pars)[4]

  if (effect_type=="Odds ratio"){
    mu <- mu %>%
      exp()

    lower <- lower %>%
      exp()

    upper <- upper %>%
      exp()
  }
  else if(logarithm==TRUE){

    # For log-scale continuous outcomes, must compute effects manually
    post <- posterior_samples(model_obj, pars = c("Intercept", "randomization_factor"))

    eff_fmo <- post[,1] %>% exp()
    eff_hfno <- apply(post,1,sum) %>% exp()
    eff_diff <- eff_hfno - eff_fmo

    mu <- eff_diff %>%
      mean()

    lower <- eff_diff %>%
      quantile(probs = 0.025)

    upper <- eff_diff %>%
      quantile(probs = 0.975)
  }

  # Formatting
  mu <- mu %>%
    round(1) %>%
    format(nsmall = 1)

  lower <- lower %>%
    round(2) %>%
    format(nsmall = 1)

  upper <- upper %>%
    round(2) %>%
    format(nsmall = 1)

  result[["mean"]] <- paste0(mu, " " ,unit)

  result[["interval"]] <- paste0("(", lower, ", ",upper ,")")



  result[["fullCIbrackets"]] <-  paste0("(", mu, unit, "; 95% CI = ", lower, " to ", upper, ")")

  result[["fullCI"]] <- paste0(mu, unit, " (95% CI = ", lower, " to ", upper, ")")

  result[["full"]] <- paste0(result[["mean"]], " ", result[["interval"]])

  result[["effect"]] <- as.numeric(mu)
  result[["lower"]] <- as.numeric(lower)
  result[["upper"]] <- as.numeric(upper)

  return(result)
}
