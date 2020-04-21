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
    convert_stratification_factors()

  # Correct typo in id P058:
  trial_data <- trial_data %>%
    mutate(id = ifelse(id=="P58","P058", id))

  # Remove entries from trial_data not observed in data
  trial_data <- trial_data %>%
    filter(id %in% unique(co2_data$id))

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
  co2_data <- co2_data %>%
    filter(!(id %in% c("P011", "P014")))

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

  # Observations with co2 == 0 are considered missing
  co2_data <- co2_data %>%
    mutate(co2 = ifelse(co2==0, NA,co2))

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

# *********************************
#' @title Process data for Primary analysis
#' @rdname process_primary
#' @description  Baseline is taken as first non-missing co2 value. Peak is taken as largest co2 value
#' @param co2_data dataframe of data from tcco2 monitor
#' @param trial_data dataframe of other variables
#' @export
# *********************************
#' @importFrom dplyr filter group_by summarize first ungroup
process_primary <- function(co2_data, trial_data){

  co2_long <- make_co2_long(co2_data, trial_data)

  result <- co2_long %>%
    filter(!is.na(co2)) %>%
    group_by(id, id_str) %>%
    summarize(co2_baseline = first(co2),
              co2_mean = mean(co2),
              co2_peak = max(co2),
              randomization_factor = first(randomization_factor),
              randomization_num = first(randomization_num),
              osa_factor = first(osa_factor),
              osa_num = first(osa_num),
              crt_factor = first(crt_factor),
              crt_num = first(crt_num)) %>%
    ungroup()

  return(result)
}

# *********************************
#' @title Process data for FANOVA analysis
#' @rdname process_fanova
#' @param co2_data dataframe of data from tcco2 monitor
#' @param trial_data dataframe of other variables
#' @param reso Number of seconds resolution for functional data
#' @export
# *********************************
#' @importFrom dplyr group_by filter select mutate
process_fanova <- function(co2_data, trial_data, reso=30){

  co2_long <- make_co2_long(co2_data, trial_data)

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
           randomization_num) %>%
    filter(!is.na(co2))

  # Create variables for INLA model
  result <- result %>%
    mutate(y = co2,
           mu = time_int,
           alpha = ifelse(randomization_num == 1, time_int, NA),
           eps = time_int)

  return(result)
}

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

  ### Code all ISAS items according to their ISAS scores
  # for(isas in isas_names){
  #
  #   result[,isas] <- result[,isas] %>%
  #     unlist() %>%
  #     recode("Disagree very much" = -3,
  #            "Disagree moderately" = -2,
  #            "Disagree slightly" = -1,
  #            "Agree slightly" = 1,
  #            "Agree moderately" = 2,
  #            "Agree very much" = 3)
  # }
  #
  # result$isas_mean <- apply(result[,isas_names],1,mean)
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
           diffoxygen.factor,
           diffoxygen_num,
           diffuseoxygen.factor,
           diffuseoxygen_num,
           osa_factor,
           crt_factor,
           randomization_factor)

  return(result)
}

# *********************************
#' @title Process data for SPO2 analysis
#' @rdname process_spo2
#' @param trial_data dataframe of other variables
#' @param co2_data dataframe of data from tcco2 monitor
#' @description Outcome to assess oxygenation
#' @export
# *********************************
#' @importFrom dplyr mutate group_by summarize
process_spo2 <- function(co2_data, trial_data){

  trial_data <- trial_data %>%
    convert_stratification_factors()

  trial_data <- trial_data %>%
    mutate(id = ifelse(id=="P58","P058", id))

  co2_data <- co2_data %>%
    mutate(spo2 = ifelse((spo2<10 | pr==0 | is.na(pr)) , NA, spo2)) %>%
    group_by(id) %>%
    summarize(spo2_mean = mean(spo2, na.rm=TRUE), pct_na = sum(is.na(spo2))/length(spo2),
              spo2_min = min(spo2, na.rm=TRUE))

  result <- co2_data %>%
    left_join(trial_data, by="id") %>%
    select(id,
           spo2_mean,
           osa_factor,
           crt_factor,
           randomization_factor,
           pct_na,
           spo2_min)

  return(result)
}

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

# ***************************
#' @title Convert Time to a datetime object
#' @rdname time_to_date
#' @description Date format is mixed, so we have to treat each format separately
#' @param data Used for co2_data
#' @export
# ***************************
#' @importFrom lubridate dmy_hms ymd_hms
time_to_date <- function(data){

  time_dmy <- dmy_hms(data$Time, tz = "UTC", quiet = TRUE)
  time_ymd <- ymd_hms(data$Time, tz = "UTC", quiet = TRUE)


  time_result <- time_dmy
  time_result[is.na(time_result)] <- time_ymd[!is.na(time_ymd)]

  return(time_result)

}
