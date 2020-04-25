# *********************************
#' @title Combine co2 and trial data into a long-format dataframe
#' @rdname make_co2_long
#' @param co2_data dataframe of data from tcco2 monitor
#' @param trial_data dataframe of other variables
#' @export
# *********************************
#' @importFrom dplyr mutate filter if_else pull group_by n left_join group_indices ungroup select
#' @importFrom lubridate ymd_hms hours
make_co2_long <- function(co2_data,trial_data){


  # *********************
  # Pre-process trial_data
  # *********************

  trial_data <- trial_data %>%
    convert_stratification_factors() %>%
    select(id,
           randomization_factor,
           randomization_num,
           procedurestart,
           procedureend,
           osa_factor,
           osa_num,
           crt_factor,
           crt_num
           )

  # Correct typo in id P058:
  trial_data <- trial_data %>%
    mutate(id = ifelse(id=="P58","P058", id))

  # Remove entries from trial_data not observed in data
  # trial_data <- trial_data %>%
  #   filter(id %in% unique(co2_data$id))

  # 'procedurestart' and 'procedureend' of P001 - P004 must be advanced 15 hours due to mismatch in trial and co2 data
  trial_data$procedurestart <- ymd_hms(trial_data$procedurestart)
  trial_data$procedureend <- ymd_hms(trial_data$procedureend)

  trial_data <- trial_data %>%
    mutate(procedurestart = if_else(
      (id %in% c("P001", "P002", "P003", "P004")),
      procedurestart + hours(15),
      procedurestart),
      procedureend = if_else(
        (id %in% c("P001", "P002", "P003", "P004")),
        procedureend + hours(15),
        procedureend))

  # *********************
  # Pre-process co2_data
  # *********************

  # Remove IDs with all missing data
  # co2_data <- co2_data %>%
  #   filter(!(id %in% c("P011", "P014")))

  # Re-format Time to datetime
  co2_data$Time <- time_to_date(co2_data)

  for(i in trial_data$id){
    co2_data <-co2_data %>%
      filter(if_else(id == i,
                     Time >= (trial_data %>% filter(id==i) %>% pull(procedurestart)),
                     TRUE)) %>%
      filter(if_else(id == i,
                     Time <= (trial_data %>% filter(id==i) %>% pull(procedureend)),
                     TRUE))
  }

  # Add integer time
  co2_data <- co2_data %>%
    group_by(id) %>%
    mutate(time_int = 1:n()) %>%
    mutate(time_int = time_int - 1)

  # Observations with co2 == 0 or sp02==0 | pr ==0 are considered missing
  co2_data <- co2_data %>%
    mutate(co2 = ifelse(co2==0, NA,co2)) %>%
  mutate(spo2 = ifelse((spo2<10 | pr==0 | is.na(pr)) , NA, spo2))


  # *********************
  # Combine co2_data and trial_data
  # *********************

  result <- co2_data %>%
    left_join(trial_data, by="id")

  # Convert patient IDs to integers
  result <- result %>%
    group_by(id) %>%
    mutate(id_int = group_indices()) %>%
    ungroup() %>%
    mutate(id_str = id, id=id_int) %>%
    select(-id_int)

  return(result)
}



