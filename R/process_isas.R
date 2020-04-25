
# *********************************
#' @title Process data for ISAS analysis
#' @rdname process_isas
#' @param trial_data dataframe of other variables
#' @description Iowa Satisfaction with Anesthesia Scale
#' @export
# *********************************
#' @importFrom dplyr recode select rowwise summarise
process_isas <- function(trial_data){

  isas_names <- grep("^isas.", names(trial_data), value = TRUE)

  result <- trial_data %>%
    convert_stratification_factors()

  isas <- trial_data %>%
    select(starts_with("isas"), -ends_with("factor"))  %>%
    rowwise() %>%
    summarise(mean = mean(c(isasvomit,
                            isassameanesthetic,
                            isasitch,
                            isasrelaxed,
                            isaspain,
                            isassafe,
                            isastoocoldhot,
                            isassurgerypain,
                            isassatisfiedcare,
                            isasfeltgood,
                            isashurt), na.rm = TRUE))

  result$isas_mean <- isas$mean

  # Select only relevant variables
  result <- result %>%
    select(id,
           all_of(isas_names),
           isas_mean,
           osa_factor,
           crt_factor,
           randomization_factor)

  return(result)

}
