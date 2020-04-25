
# *********************************
#' @title Process data for anesthesia assistant ratings analysis
#' @rdname process_assist
#' @param trial_data dataframe of other variables
#' @description Ratings for ease of use of oxygen device
#' @export
# *********************************
#' @importFrom dplyr mutate select
process_assist <- function(trial_data){

  fct_levels <- c("Extremely difficult",
                  "Very difficult",
                  "Difficult",
                  "Easy",
                  "Very easy",
                  "Extremely easy")

  result <- trial_data %>%
    convert_stratification_factors() %>%
    mutate(diffoxygen_factor = ordered(diffoxygen.factor, levels = fct_levels),
           diffuseoxygen_factor = ordered(diffuseoxygen.factor, levels = fct_levels)) %>%
    mutate(diffoxygen_num = as.numeric(diffoxygen_factor),
           diffuseoxygen_num = as.numeric(diffuseoxygen_factor)) %>%
    select(id,
           diffoxygen_factor,
           diffoxygen_num,
           diffuseoxygen_factor,
           diffuseoxygen_num,
           osa_factor,
           crt_factor,
           randomization_factor)

  return(result)
}
