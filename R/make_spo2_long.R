# *********************************
#' @title Combine spo2 and trial data into a long-format dataframe
#' @rdname make_spo2_long
#' @param spo2_data dataframe of data from anesthesia monitor
#' @param trial_data dataframe of other variables
#' @export
# *********************************
#' @importFrom dplyr mutate filter if_else pull group_by n left_join group_indices ungroup select
#' @importFrom lubridate ymd_hms hours
make_spo2_long <- function(spo2_data, trial_data){


  # *********************
  # Pre-process trial_data
  # *********************

  trial_data <- trial_data %>%
    convert_stratification_factors() %>%
    select(id,
           randomization_factor,
           randomization_num,
           osa_factor,
           osa_num,
           crt_factor,
           crt_num
    )

  # Correct typo in id P058:
  trial_data <- trial_data %>%
    mutate(id = ifelse(id=="P58","P058", id))

  # Remove entries from trial_data not observed in data
  spo2_data <- spo2_data %>%
    filter(id %in% unique(trial_data$id))



  # *********************
  # Combine spo2_data and trial_data
  # *********************

  result <- spo2_data %>%
    left_join(trial_data, by="id")

  # Convert patient IDs to integers
  result <- result %>%
    group_by(id) %>%
    mutate(id_int = group_indices()) %>%
    ungroup() %>%
    mutate(id_str = id, id=id_int) %>%
    select(-id_int)

  result <- result %>%
  mutate(time_int = index)

  return(result)
}
