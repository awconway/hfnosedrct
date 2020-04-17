#' @title Load data from tcco2 monitor
#' @rdname create_tcco2_data
#' @param data Filepath to directory of csv files

#' @export
#' @importFrom dplyr mutate select everything
#' @importFrom fs dir_ls
#' @importFrom glue glue
#' @importFrom stringr str_remove_all str_remove
#' @importFrom vroom vroom
create_tcco2_data <- function(TCCO2) {
  
  # Dataframe of all co2, spo2, and pr values at every specific date-time 
  # point for each participant
  files <- dir_ls(glue("data/", TCCO2), glob = "*csv")
  files
  vroom(files, id = "path") %>%
    mutate(id = str_remove_all(path, glue("data/", TCCO2, "/"))) %>%
    mutate(id = str_remove(id, ".csv")) %>%
    select(id, everything(), -path)
  
}


#' @title Plot to explore amount of missing co2 data per participant
#' @rdname create_co2_plot
#' @param tcco2_data dataframe of co2, spo2, pulse rate
#' @param trial_mod dataframe of all other variables collected

#' @export
#' @importFrom dplyr select mutate filter group_by n ungroup if_else
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_brewer theme_minimal theme element_blank labs
create_co2_plot <- function(tcco2_data, trial_mod) {
  
  # Dataframe of all co2 values at every specific date-time point for each 
  # participant (properly formatted)
  co2 <- tcco2_data %>%
    select(id, Time, co2) %>%
    mutate(Time = format(strptime(Time, "%d/%m/%Y %H:%M:%S")))
  
  # Add procedure start date-time column for each participant from trial_mod 
  # dataframe to co2 dataframe
  for (i in seq_along(unique(co2$id))) {
    co2[co2$id == trial_mod[-20, ]$id[i], "procedurestart"] <-
      if_else(trial_mod$id[i] %in% c("P001", "P002", "P003", "P004"),
              format(trial_mod$procedurestart[i] + 15*60*60),
              format(trial_mod[-20, ]$procedurestart[i]))
  }
  
  co2[co2$id == "P058", "procedurestart"] <-
    format(trial_mod[trial_mod$id == "P58", ]$procedurestart)
  
  # Dataframe of all co2 values, and their respective value type (actual 
  # value, value of 0, or missing/NA value) at every time point, where co2 
  # date-time is less than procedure start date-time
  co2_mod <- co2 %>%
    filter(Time >= procedurestart) %>%
    group_by(id) %>%
    mutate(minute = 1:n() / 60) %>%
    ungroup() %>%
    mutate(value_type = if_else(is.na(co2),
                                "Missing (no value recorded)",
                                if_else(co2 == 0,
                                        "Zero (0) value",
                                        "Measurable value (above 0)")))
  
  # Plot of all co2 value types observed in sequence during procedure for all 
  # participants
  co2_values <- co2_mod %>%
    ggplot(aes(x = minute, y = id, fill = value_type)) +
    geom_tile() +
    scale_fill_brewer(palette = "RdYlBu") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Procedure duration (minutes)")
  
}

#' @title Plot to explore amount of missing spo2 data per participant
#' @rdname create_spo2_plot
#' @param tcco2_data dataframe of co2, spo2, pulse rate
#' @param trial_mod dataframe of all other variables collected
#' 
#' @export
#' @importFrom dplyr select mutate if_else filter group_by n
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_brewer theme_minimal theme element_blank labs
create_spo2_plot <- function(tcco2_data, trial_mod) {
  
  # Dataframe of all spo2 values at every specific date-time point for each 
  # participant (properly formatted)
  spo2 <- tcco2_data %>%
    select(id, Time, spo2) %>%
    mutate(Time = format(strptime(Time, "%d/%m/%Y %H:%M:%S")))
  
  # Add procedure start date-time column for each participant from trial_mod 
  # dataframe to spo2 dataframe
  for (i in seq_along(unique(spo2$id))) {
    spo2[spo2$id == trial_mod[-20, ]$id[i], "procedurestart"] <-
      if_else(trial_mod$id[i] %in% c("P001", "P002", "P003", "P004"),
              format(trial_mod$procedurestart[i] + 15*60*60),
              format(trial_mod[-20, ]$procedurestart[i]))
  }
  
  spo2[spo2$id == "P058", "procedurestart"] <-
    format(trial_mod[trial_mod$id == "P58", ]$procedurestart)
  
  # Dataframe of all spo2 values, and their respective value type (actual 
  # value, value of 0, or missing/NA value) at every time point, where spo2 
  # date-time is less than procedure start date-time
  spo2_mod <- spo2 %>%
    filter(Time >= procedurestart) %>%
    group_by(id) %>%
    mutate(minute = 1:n() / 60) %>%
    mutate(value_type = if_else(is.na(spo2),
                                "Missing (no value recorded)",
                                if_else(spo2 == 0,
                                        "Zero (0) value",
                                        "Measurable value (above 0)")))
  
  # Plot of all spo2 value types observed in sequence during procedure for all 
  # participants
  spo2_values <- spo2_mod %>%
    ggplot(aes(x = minute, y = id, fill = value_type)) +
    geom_tile() +
    scale_fill_brewer(palette = "RdYlBu") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Procedure duration (minutes)")
  
}
