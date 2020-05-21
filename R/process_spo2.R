
# *********************************
#' @title Process data for SPO2 analysis
#' @rdname process_spo2
#' @param spo2_trial long dataframe of data from anesthesia monitor
#' and trial data
#' @description Outcome to assess oxygenation
#' @export
# *********************************
#' @importFrom dplyr mutate group_by summarize
#' @importFrom tidyr replace_na
#'
process_spo2 <- function(spo2_trial){

  summary <- spo2_trial %>%
    group_by(id, id_str) %>%
    summarize(
      desat_event = any(spo2 < 90, na.rm=TRUE),
      spo2_mean = mean(spo2, na.rm = TRUE),
      pct_na = sum(is.na(spo2))/length(spo2),
      spo2_min = min(spo2, na.rm = TRUE),
      spo2_mins_below_90 = sum(spo2 < 90, na.rm = TRUE),
      randomization_factor = first(randomization_factor),
      randomization_num = first(randomization_num),
      osa_factor = first(osa_factor),
      osa_num = first(osa_num),
      crt_factor = first(crt_factor),
      crt_num = first(crt_num)) %>%
    ungroup()

  summary_auc <- co2_long %>%
    filter(spo2 < 90) %>%
    group_by(id, id_str) %>%
    summarize(
      spo2_auc = sum(90-spo2, na.rm = TRUE)) %>%
    ungroup()

  result <- summary %>% left_join(summary_auc) %>%
    mutate(spo2_auc = replace_na(spo2_auc, 0))

  return(result)
}
