
# ***************************
#' @title Convert stratification factors
#' @description Convert stratification factors to be consistent in their levels and names,
#' and add indicator variables
#' @rdname convert_stratification_factors
#' @param trial_data dataframe of other variables
#' @export
# ***************************
#' @importFrom dplyr mutate select
convert_stratification_factors <- function(trial_data){

  result <- trial_data  %>%
    mutate(osa_factor = osa.factor,
           crt_factor = relevel(crtstratify.factor, ref="No"),
           randomization_factor = relevel(randomization.factor, ref = "Face mask oxygen")) %>%
    mutate(osa_num = ifelse(osa_factor == "Yes", 1, 0),
           crt_num = ifelse(crt_factor == "Yes", 1, 0),
           randomization_num = ifelse(randomization_factor == "High Flow nasal oxygen", 1, 0)) %>%
    select(-c(osa.factor, crtstratify.factor, randomization.factor))

  return(result)
}
