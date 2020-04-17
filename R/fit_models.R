#' @title Fit model for tcco2 outcomes
#' @rdname fit_primary
#' @param data_primary dataframe
#' @param response peak or mean
#' @export

#' @importFrom brms prior prior_string brm student
fit_primary <- function(data_primary, response){
  
  # Filter only required data
  data_primary <- data_primary[,c(response, "osa_factor", "crt_factor", "randomization_factor", "co2_baseline")]
  
  # Explicitly remove missing data
  data_primary <- data_primary[complete.cases(data_primary),]
  
  # Define formula for 'response'
  form <- formula(paste0(response," ~ s(co2_baseline, k=20, bs='tp') + randomization_factor + crt_factor + osa_factor"))
  
  # Explicitly define priors

  # Mean response is used for intercept prior location
  mean_response <- data_primary[,response] %>% unlist() %>% mean()
  
  prior_int <- paste0("normal(", mean_response,",", 25,")")
  
  priors <- c(prior(normal(0,25), class="b"),
              prior_string(prior_int, class="Intercept"),
              prior(gamma(2,0.1), class="nu"),
              prior(student_t(3, 0, 10) , class="sigma"),
              prior(student_t(3, 0, 10) , class="sds"))
  
  result <- brm(formula = form,
                       data = data_primary,
                       family = student("identity"),
                       control=list(adapt_delta=0.99),
                prior = priors
                )

  # Save model fit
  # fpath <- paste0("./analysis/models/", response , ".RDS")
  # saveRDS(result, file=fpath)
  
  return(result)
}

# **********************
#' @title Functional ANOVA model
#' @rdname fit_fanova
#' @param data_fanova dataframe
#' @export
# **********************

#' @importFrom INLA f inla
fit_fanova <- function(data_fanova){
  
  # Explicit prior specification (See INLA documentation for parameterization details)
  prior_rw2 <- list(prec = list(prior = "loggamma", param = c(1, 5e-05)))
  
  prior_iid <- list(prec = list(prior = "loggamma", param = c(1, 5e-05)))
  
  prior_ar1 <- list(theta1 = list(prior = "loggamma", param = c(1, 5e-05)),
                    theta2 = list(prior = "normal", param = c(0, 10^(-2))))
  
  # Define formula
  form <- y ~ -1 + crt_num + osa_num + 
    f(mu, model = "rw2", constr = FALSE, scale.model = TRUE, hyper = prior_rw2) + 
    f(alpha, model = "rw2", constr = FALSE, scale.model = TRUE, hyper = prior_rw2) + 
    f(id, model="iid", hyper = prior_iid) +
    f(eps, model = "ar1", values=values_arg,replicate = id, hyper = prior_ar1)
  

  values_arg <- data_fanova$time_int %>% unique()
  
  result <- inla(formula = form, data = data_fanova,
                     control.predictor=list(compute=TRUE),
                     control.compute = list(config=TRUE),
                     control.family = list(hyper = list(prec = list(initial= 15, fixed = TRUE))),
                     control.fixed = list(mean=0, prec=3^(-2)))
  
  return(result)
}

#' @title Fit model for ISAS
#' @rdname fit_isas
#' @param data_isas dataframe
#' @description Iowa Satisfaction with Anesthesia Scale
#' @export

#' @importFrom brms prior brm student
fit_isas <- function(data_isas){
  
  # Filter only required data
  data_isas <- data_isas[,c("isas_mean", "osa_factor", "crt_factor", "randomization_factor")]
  
  # Explicitly remove missing data
  data_propodds <- data_isas[complete.cases(data_isas),]
  
  # Explicitly define priors
  priors <- c(prior(normal(0,10), class="b"),
              prior(normal(0,10), class="Intercept"),
              prior(gamma(2,0.1), class="nu"),
              prior(student_t(3, 0, 10) , class="sigma"))
  
  result <- brm(isas_mean ~ randomization_factor + osa_factor + crt_factor,
                        family = student("identity"),
                        control = list(adapt_delta = 0.8),
                        prior=priors,
                        data=data_isas)
  
  # Save model fit
  # saveRDS(result, file="./analysis/models/isas.RDS")
  # 
  # return(result)
}

#' @title Fit proportional odds models
#' @rdname fit_propodds
#' @param data_propodds dataframe
#' @param response outcome variable
#' @export

#' @importFrom brms cumulative
fit_propodds <- function(data_propodds, response){
  
  # Filter only required data
  data_propodds <- data_propodds[,c(response, "osa_factor", "crt_factor", "randomization_factor")]
  
  # Explicitly remove missing data
  data_propodds <- data_propodds[complete.cases(data_propodds),]
  
  # Explicitly define priors
  priors <- c(prior(normal(0,10), class="b"),
              prior(normal(0,10), class="Intercept"))
  
  # Define formula for 'response'
  form <- formula(paste0(response, " ~ randomization_factor + osa_factor + crt_factor")) 
  
  result <- brm(formula = form,
                       family = cumulative("logit"),
                       control = list(adapt_delta = 0.9),
                       prior = priors,
                       data = data_propodds)
  
  # Save model fit
  # fpath <- paste0("./analysis/models/", response , ".RDS")
  # saveRDS(result, file=fpath)
  
  return(result)
}

#' @title Fit model for TROOPS
#' @rdname fit_troops
#' @param data_troops dataframe
#' @description Tracking and reporting outcomes of procedural sedation
#' @export

#' @importFrom brms prior brm bernoulli
fit_troops <- function(data_troops){
  
  # Filter only required data
  data_troops <- data_troops[,c("troopsminairway_num", "osa_factor", "crt_factor", "randomization_factor")]
  
  # Explicitly remove missing data
  data_troops <- data_troops[complete.cases(data_troops),]
  
  # Explicitly define priors
  priors <- c(prior(normal(0,10), class="b"))
  
  # Define formula
  form <- formula("troopsminairway_num ~ randomization_factor + osa_factor + crt_factor") 
  
  result <- brm(formula = form,
                family = bernoulli("logit"),
                control = list(adapt_delta = 0.9),
                prior = priors,
                data = data_troops)
  
  # Save model fit
  # fpath <- "./analysis/models/troops.RDS"
  # saveRDS(result, file=fpath)
  
  return(result)
}

#' @title Fit model for spo2
#' @rdname fit_spo2
#' @param data_spo2 dataframe
#' @description Outcome for oxygenation
#' @export

#' @importFrom brms prior prior_string brm student
#' @importFrom dplyr filter
fit_spo2 <- function(data_spo2){
  
  # Remove id P051, whose SPO2 was near 0 with 98% missing data
  data_spo2 <- data_spo2 %>%
    filter(id != "P051") 
  
  # Remove id P058 for missing stratification variables
  data_spo2 <- data_spo2 %>%
    filter(id != "P058") 
  
  # Define formula for 'response'
  form <- formula("spo2_mean ~  randomization_factor + crt_factor + osa_factor")
  
  # Explicitly define priors
  # Mean response is used for intercept prior location
  mean_response <- data_spo2[,"spo2_mean"] %>% unlist() %>% mean()
  
  prior_int <- paste0("normal(", mean_response,",", 25,")")
  
  priors <- c(prior(normal(0,25), class="b"),
              prior_string(prior_int, class="Intercept"),
              prior(gamma(2,0.1), class="nu"),
              prior(student_t(3, 0, 10) , class="sigma"))
  
  result <- brm(formula = form,
                data = data_spo2,
                family = student("identity"),
                control=list(adapt_delta=0.9),
                prior = priors)
  
  # Save model fit
  # fpath <- "./analysis/models/spo2.RDS"
  # saveRDS(result, file=fpath)
  
  return(result)
}
