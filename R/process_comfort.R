
# *********************************
#' @title Process data for patient comfort analysis
#' @rdname process_comfort
#' @param trial_data dataframe of other variables
#' @description Comfort with oxygen device
#' @export
# *********************************
#' @importFrom dplyr mutate select
process_comfort <- function(trial_data){

  result <- trial_data %>%
    convert_stratification_factors() %>%
    mutate(oxygencomfort_factor = ordered(oxygencomfort.factor,
                                          levels = c("Maximal discomfort",
                                                     "Very uncomfortable",
                                                     "Uncomfortable",
                                                     "Comfortable",
                                                     "Very comfortable",
                                                     "Maximal comfort"))) %>%
    mutate(oxygencomfort_num = as.numeric(oxygencomfort_factor)) %>%
    select(id,
           oxygencomfort_factor,
           oxygencomfort_num,
           osa_factor,
           crt_factor,
           randomization_factor)

  return(result)
}
