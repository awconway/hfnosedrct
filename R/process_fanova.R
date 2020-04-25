# *********************************
#' @title Process data for FANOVA analysis
#' @rdname process_fanova
#' @param co2_data dataframe of data from tcco2 monitor
#' @param trial_data dataframe of other variables
#' @param reso Number of seconds resolution for functional data
#' @export
# *********************************
#' @importFrom dplyr group_by filter select mutate
process_fanova <- function(co2_long, reso=30){

  # Reduce resolution of data
  result <- co2_long %>%
    group_by(id) %>%
    filter(time_int %% reso == 0) %>%
    select(id,
           id_str,
           time_int,
           co2,
           osa_factor,
           osa_num,
           crt_factor,
           crt_num,
           randomization_factor,
           randomization_num)

  # Create variables for INLA model
  result <- result %>%
    mutate(y = co2,
           mu = time_int,
           alpha = ifelse(randomization_num == 1, time_int, NA),
           eps = time_int)

  return(result)
}
