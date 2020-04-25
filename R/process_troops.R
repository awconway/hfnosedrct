
# *********************************
#' @title Process data for TROOPS analysis
#' @rdname process_troops
#' @param trial_data dataframe of other variables
#' @description Tracking and reporting outcomes of procedural sedation
#' @export
# *********************************
#' @importFrom dplyr mutate select
process_troops <- function(trial_data){

  result <- trial_data %>%
    convert_stratification_factors() %>%
    mutate(troopsminairway_factor = troopsminairway.factor) %>%
    mutate(troopsminairway_num = ifelse(is.na(troopsminairway.factor), 0, 1)) %>%
    select(id,
           troopsminairway_factor,
           troopsminairway_num,
           osa_factor,
           crt_factor,
           randomization_factor)

  return(result)
}
