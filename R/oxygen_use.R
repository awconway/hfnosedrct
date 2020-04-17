#' @title Create a table containing variables related to measuring oxygen device settings
#' 
#' @description Dataframe of all columns needed from trial dataframe (participant id, all 
#' oxygen flow rates used, all oxygen fio2 values used, and assigned 
#' randomization of each participant), with all time intervals between 
#' changes in oxygen flow rate/oxygen fio2 value
#' @rdname create_oxygen
#' @export
#' @importFrom dplyr if_else mutate
create_oxygen <- function(trial_mod) {
  

  oxygen <- trial_mod %>%
    mutate(oxygentimeinterval1 = if_else(
      is.na(oxygenchangetime1),
      difftime(
        strptime(format(trial_mod$procedureend, "%H:%M:%S"), "%H:%M:%S"),
        strptime(trial_mod$oxygenbaselinetime, "%H:%M:%S")),
      (oxygenchangetime1 - oxygenbaselinetime) / 60)) %>%
    mutate(oxygentimeinterval2 = if_else(
      is.na(oxygenchangetime2),
      difftime(
        strptime(format(trial_mod$procedureend, "%H:%M:%S"), "%H:%M:%S"),
        strptime(trial_mod$oxygenchangetime1, "%H:%M:%S")),
      (oxygenchangetime2 - oxygenchangetime1) / 60)) %>%
    mutate(oxygentimeinterval3 = if_else(
      is.na(oxygenchangetime3),
      difftime(
        strptime(format(trial_mod$procedureend, "%H:%M:%S"), "%H:%M:%S"),
        strptime(trial_mod$oxygenchangetime2, "%H:%M:%S")),
      (oxygenchangetime3 - oxygenchangetime2) / 60)) %>%
    mutate(oxygentimeinterval4 = if_else(
      is.na(oxygenchangetime4),
      difftime(
        strptime(format(trial_mod$procedureend, "%H:%M:%S"), "%H:%M:%S"),
        strptime(trial_mod$oxygenchangetime3, "%H:%M:%S")),
      (oxygenchangetime4 - oxygenchangetime3) / 60)) %>%
    mutate(oxygentimeinterval5 = difftime(
      strptime(format(trial_mod$procedureend, "%H:%M:%S"), "%H:%M:%S"),
      strptime(trial_mod$oxygenchangetime4, "%H:%M:%S"))) %>%
    mutate(totaltimeinterval = difftime(
      strptime(format(trial_mod$procedureend, "%H:%M:%S"), "%H:%M:%S"),
      strptime(trial_mod$oxygenbaselinetime, "%H:%M:%S"))) %>%
    select(id, oxygenbaselineflow, oxygenchangeflow1, oxygenchangeflow2,
           oxygenchangeflow3, oxygenchangeflow4, oxygenbaselinefio2,
           oxygenchange1fio2, oxygenchange2fio2, oxygenchange3fio2,
           oxygenchange4fio2, oxygentimeinterval1, oxygentimeinterval2,
           oxygentimeinterval3, oxygentimeinterval4, oxygentimeinterval5,
           totaltimeinterval, randomization.factor)
  
}

#' @title Oxygen flow rate table in wide format
#' @rdname create_oxygen_table
#' @description Dataframe of all oxygen flow rate values at every minute, with assigned 
#' randomization of each participant
#' @export
create_oxygen_table <- function(oxygen) {
  
  # Replace time interval values of NA with a value of 1 (rep() function 
  # incompatible with 0, NA, and NULL values)
  oxygen_mod <- oxygen %>%
    replace_na(list(oxygentimeinterval1 = 1,
                    oxygentimeinterval2 = 1,
                    oxygentimeinterval3 = 1,
                    oxygentimeinterval4 = 1,
                    oxygentimeinterval5 = 1))
  
  # Empty dataframe of all oxygen flow rate values at every minute, with 
  # participant id and assigned randomization of each participant
  oxygen_table <- oxygen_mod %>%
    select(id, randomization.factor)
  
  # Fills dataframe, where for each oxygen flow rate value used during 
  # procedure, repeat value in consecutive time point columns for amount of 
  # time value was used for
  for (i in seq_along(oxygen_mod$oxygentimeinterval1)) {
    oxygen_table[i, 3:(2 + oxygen_mod$oxygentimeinterval1[i])] <-
      as.numeric(oxygen_mod$oxygenbaselineflow)[i]
    oxygen_table[i,
                 (3 + oxygen_mod$oxygentimeinterval1[i]):
                   (2 + oxygen_mod$oxygentimeinterval2[i] +
                      oxygen_mod$oxygentimeinterval1[i])] <-
      as.numeric(oxygen_mod$oxygenchangeflow1)[i]
    oxygen_table[i,
                 (3 + oxygen_mod$oxygentimeinterval2[i] +
                    oxygen_mod$oxygentimeinterval1[i]):
                   (2 + oxygen_mod$oxygentimeinterval3[i] +
                      oxygen_mod$oxygentimeinterval2[i] +
                      oxygen_mod$oxygentimeinterval1[i])] <-
      as.numeric(oxygen_mod$oxygenchangeflow2)[i]
    oxygen_table[i,
                 (3 + oxygen_mod$oxygentimeinterval3[i] +
                    oxygen_mod$oxygentimeinterval2[i] +
                    oxygen_mod$oxygentimeinterval1[i]):
                   (2 + oxygen_mod$oxygentimeinterval4[i] +
                      oxygen_mod$oxygentimeinterval3[i] +
                      oxygen_mod$oxygentimeinterval2[i] +
                      oxygen_mod$oxygentimeinterval1[i])] <-
      as.numeric(oxygen_mod$oxygenchangeflow3)[i]
    oxygen_table[i,
                 (3 + oxygen_mod$oxygentimeinterval4[i] +
                    oxygen_mod$oxygentimeinterval3[i] +
                    oxygen_mod$oxygentimeinterval2[i] +
                    oxygen_mod$oxygentimeinterval1[i]):
                   (2 + oxygen_mod$oxygentimeinterval5[i] +
                      oxygen_mod$oxygentimeinterval4[i] +
                      oxygen_mod$oxygentimeinterval3[i] +
                      oxygen_mod$oxygentimeinterval2[i] +
                      oxygen_mod$oxygentimeinterval1[i])] <-
      as.numeric(oxygen_mod$oxygenchangeflow4)[i]
  }
  
  oxygen_table
  
}


#' @title Oxygen flow rate table in long format
#' @rdname create_oxygen_table_long
#' @description Merge all oxygen flow rate values into one column named "flowrate"

#' @export
#' @importFrom dplyr group_by mutate n starts_with
#' @importFrom tidyr pivot_longer drop_na
create_oxygen_table_long <- function(oxygen, oxygen_table) {
  
  # In each randomization, assign a separate number to each participant id
  oxygen_table <- oxygen_table %>%
    group_by(randomization.factor) %>%
    mutate(randid = 1:n())
  
  # Merge all oxygen flow rate values into one column named "flowrate"
  oxygen_table %>%
    pivot_longer(names_to = "minute",
                 values_to = "flowrate",
                 cols = starts_with("...")) %>%
    drop_na()
  
}


#' @title Oxygen flow rate sequence plot
#' @rdname create_oxygen_flow_plot
#' @description Plot of all oxygen flow rate values used in sequence during procedures

#' @export
#' @importFrom dplyr group_by n ungroup
#' @importFrom ggplot2 ggplot aes geom_tile facet_wrap theme_minimal theme element_blank scale_fill_brewer scale_colour_brewer labs
create_oxygen_flow_plot <- function(oxygen_table_long) {
  
  # Plot of all oxygen flow rate values used in sequence during procedure for 
  # all participants for each randomization
  oxygen_table_long %>%
    group_by(id) %>%
    mutate(minute = 1:n()) %>%
    ungroup() %>%
    group_by(randomization.factor) %>%
    ggplot(aes(x = minute,
               y = randid,
               fill = factor(flowrate),
               colour = factor(flowrate))) +
    geom_tile() +
    facet_wrap(~randomization.factor, scales = "free_y", ncol = 2) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot") +
    scale_fill_brewer(palette = "RdYlBu") +
    scale_colour_brewer(palette = "RdYlBu") +
    labs(x = "Procedure duration (minutes)")
  
}


#' @title Oxygen flow rate stacked plot
#' @rdname create_oxygen_proportion_plot
#' @description Plot of proportion of time spent during procedure using different oxygen 
#' flow rates
#' 
#' @export
#' @importFrom ggplot2 ggplot aes geom_bar scale_y_continuous scale_fill_brewer theme element_line element_blank coord_flip
create_oxygen_proportion_plot <- function(oxygen_table_long) {
  
  # Plot of proportion of time spent during procedure using different oxygen 
  # flow rates for each randomization
  ggplot(oxygen_table_long,
         aes(x = randomization.factor, fill = factor(flowrate))) +
    geom_bar(position = "fill") +
    scale_y_continuous("Procedure time length (%)",
                       labels = c(0, 25, 50, 75, 100)) +
    scale_fill_brewer(palette = "RdYlBu") +
    theme(axis.line = element_line(colour = "black"),
          axis.title.y = element_blank(),
          legend.position = "bottom",
          panel.background = element_blank(),
          plot.title.position = "plot",
          legend.title = element_blank()) +
    coord_flip()
  
}

#' @title Dataframe of fio2 settings
#' @rdname create_fio2 
#' @description Dataframe of all columns needed from oxygen dataframe (participant id, all 
#' oxygen fio2 values used, all time intervals between changes in oxygen fio2 
#' value, and assigned randomization of each high flow nasal oxygen 
#' participant)

#' @export
#' @importFrom dplyr filter mutate select
create_fio2 <- function(oxygen) {
  
  # Dataframe of all columns needed from oxygen dataframe (participant id, all 
  # oxygen fio2 values used, all time intervals between changes in oxygen fio2 
  # value, and assigned randomization of each high flow nasal oxygen 
  # participant)
  fio2 <- oxygen %>%
    filter(randomization.factor == "High Flow nasal oxygen") %>%
    mutate(oxygenbaselinefio2 = ifelse(
      oxygenbaselineflow < 15, 1, oxygenbaselinefio2)) %>%
    mutate(oxygenchange1fio2 = ifelse(
      oxygenchangeflow1 < 15, 1, oxygenchange1fio2)) %>%
    mutate(oxygenchange2fio2 = ifelse(
      oxygenchangeflow2 < 15, 1, oxygenchange2fio2)) %>%
    mutate(oxygenchange3fio2 = ifelse(
      oxygenchangeflow3 < 15, 1, oxygenchange3fio2)) %>%
    mutate(oxygenchange4fio2 = ifelse(
      oxygenchangeflow4 < 15, 1, oxygenchange4fio2)) %>%
    select(-2:-6)
  
}

#' @title fio2 table in wide format
#' @rdname create_fio2_table
#' @description Wide dataframe of all oxygen fio2 values at every minute for each high flow 
# nasal oxygen participant

#' @export
#' @importFrom dplyr select
#' @importFrom tidyr replace_na
create_fio2_table <- function(fio2) {
  
  # Replace NA values with a value of 1 (rep() function incompatible with 0, 
  # NA, and NULL values)
  fio2_mod <- fio2 %>%
    replace_na(list(oxygentimeinterval1 = 1,
                    oxygentimeinterval2 = 1,
                    oxygentimeinterval3 = 1,
                    oxygentimeinterval4 = 1,
                    oxygentimeinterval5 = 1))
  
  # Empty dataframe of all oxygen fio2 values at every minute, with 
  # participant id for each high flow nasal oxygen participant
  fio2_table <- fio2_mod %>%
    select(id)
  
  # For each oxygen fio2 value used during procedure, repeat value in 
  # consecutive time point columns for amount of time value was used for
  for (i in seq_along(fio2_mod$oxygentimeinterval1)) {
    fio2_table[i, 2:(1 + fio2_mod$oxygentimeinterval1[i])] <-
      as.numeric(fio2_mod$oxygenbaselinefio2)[i]
    fio2_table[i,
               (2 + fio2_mod$oxygentimeinterval1[i]):
                 (1 + fio2_mod$oxygentimeinterval2[i] +
                    fio2_mod$oxygentimeinterval1[i])] <-
      as.numeric(fio2_mod$oxygenchange1fio2)[i]
    fio2_table[i,
               (2 + fio2_mod$oxygentimeinterval2[i] +
                  fio2_mod$oxygentimeinterval1[i]):
                 (1 + fio2_mod$oxygentimeinterval3[i] +
                    fio2_mod$oxygentimeinterval2[i] +
                    fio2_mod$oxygentimeinterval1[i])] <-
      as.numeric(fio2_mod$oxygenchange2fio2)[i]
    fio2_table[i,
               (2 + fio2_mod$oxygentimeinterval3[i] +
                  fio2_mod$oxygentimeinterval2[i] +
                  fio2_mod$oxygentimeinterval1[i]):
                 (1 + fio2_mod$oxygentimeinterval4[i] +
                    fio2_mod$oxygentimeinterval3[i] +
                    fio2_mod$oxygentimeinterval2[i] +
                    fio2_mod$oxygentimeinterval1[i])] <-
      as.numeric(fio2_mod$oxygenchange3fio2)[i]
    fio2_table[i,
               (2 + fio2_mod$oxygentimeinterval4[i] +
                  fio2_mod$oxygentimeinterval3[i] +
                  fio2_mod$oxygentimeinterval2[i] +
                  fio2_mod$oxygentimeinterval1[i]):
                 (1 + fio2_mod$oxygentimeinterval5[i] +
                    fio2_mod$oxygentimeinterval4[i] +
                    fio2_mod$oxygentimeinterval3[i] +
                    fio2_mod$oxygentimeinterval2[i] +
                    fio2_mod$oxygentimeinterval1[i])] <-
      as.numeric(fio2_mod$oxygenchange4fio2)[i]
  }
  
  fio2_table
  
}

#' @title fio2 table in long format
#' @rdname create_fio2_table_long
#' @description Merge all oxygen fio2 values into one column named "fio2"

#' @export
#' @importFrom dplyr mutate n starts_with
#' @importFrom tidyr pivot_longer drop_na
create_fio2_table_long <- function(fio2, fio2_table) {
  
  # Assign a separate number to each participant id
  fio2_table <- fio2_table %>%
    mutate(randid = 1:n())
  
  # Merge all oxygen fio2 values into one column named "fio2"
  fio2_table %>%
    pivot_longer(names_to = "minute",
                 values_to = "fio2",
                 cols = starts_with("...")) %>%
    drop_na()
  
}

#' @title fio2 table in long format
#' @rdname create_oxygen_fio2_plot
#' @description fio2 values used in sequence during procedure for participants randomized to HFNO

#' @export
#' @importFrom dplyr group_by mutate n ungroup
#' @importFrom ggplot2 ggplot aes geom_tile theme_minimal theme element_blank scale_fill_brewer scale_colour_brewer labs
create_oxygen_fio2_plot <- function(fio2_table_long) {
  
  # Plot of all oxygen fio2 values used in sequence during procedure for all 
  # high flow nasal oxygen participants
  fio2_table_long %>%
    group_by(id) %>%
    mutate(minute = 1:n()) %>%
    ungroup() %>%
    ggplot(aes(x = minute,
               y = id,
               fill = factor(fio2),
               colour = factor(fio2))) +
    geom_tile() +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot") +
    scale_fill_brewer(palette = "RdYlBu") +
    scale_colour_brewer(palette = "RdYlBu") +
    labs(x = "Procedure duration (minutes)")
  
}
