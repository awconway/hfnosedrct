# *********************************
#' @title Process data for Primary analysis
#' @rdname process_primary
#' @description  Baseline is taken as first non-missing co2 value. Peak is taken as largest co2 value
#' @param co2_data dataframe of data from tcco2 monitor
#' @param trial_data dataframe of other variables
#' @export
# *********************************
#' @importFrom dplyr filter group_by summarize first ungroup
process_primary <- function(co2_long){

  result <- co2_long %>%
    filter(!is.na(co2)) %>%
    group_by(id, id_str) %>%
    summarize(co2_baseline = first(na.omit(co2), default=NA),
              co2_mean = mean(co2, na.rm=TRUE),
              co2_peak = max(co2, na.rm=TRUE),
              randomization_factor = first(randomization_factor),
              randomization_num = first(randomization_num),
              osa_factor = first(osa_factor),
              osa_num = first(osa_num),
              crt_factor = first(crt_factor),
              crt_num = first(crt_num)) %>%
    ungroup()

  return(result)
}
