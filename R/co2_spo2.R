
#' @title Plot to explore amount of missing co2 data per participant
#' @rdname create_co2_plot
#' @param tcco2_data dataframe of co2, spo2, pulse rate
#' @param trial_mod dataframe of all other variables collected

#' @export
#' @importFrom dplyr select mutate filter group_by n ungroup if_else
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual theme_minimal theme element_blank labs
create_co2_plot <- function(co2_long) {

  co2_long %>%
    select(id_str, co2, randomization_factor, time_int, id) %>%
    mutate(value_type = if_else(is.na(co2),
                                "Missing (no value recorded)",
                                "Measurable value (above 0)")) %>%
    ggplot(aes(x = time_int, y = id, fill = value_type)) +
    geom_tile() +
    scale_fill_manual(values = c("#4475b4", "#fc8d59")) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Procedure duration (seconds)")

}

#' @title Plot to explore amount of missing spo2 data per participant
#' @rdname create_spo2_plot
#' @param tcco2_data dataframe of co2, spo2, pulse rate
#' @param trial_mod dataframe of all other variables collected
#'
#' @export
#' @importFrom dplyr select mutate if_else filter group_by n
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual theme_minimal theme element_blank labs
create_spo2_plot <- function(co2_long) {

  co2_long %>%
    select(id_str, spo2, randomization_factor, time_int, id, pr) %>%
    mutate(value_type = if_else(is.na(spo2),
                                "Missing (no value recorded)",
                                "Measurable value (above 0)")) %>%
    ggplot(aes(x = time_int, y = id, fill = value_type)) +
    geom_tile() +
    scale_fill_manual(values = c("#4475b4", "#fc8d59")) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Procedure duration (seconds)")

}


#' @title spo2 facet plot
#' @rdname create_spo2_facet_plot
#' @export
#' @importFrom dplyr group_by mutate n
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap theme_minimal theme geom_hline labs scale_color_manual
#'
create_spo2_facet_plot <- function(co2_long){
  co2_long %>%
  ggplot()+
  geom_line(aes(y=spo2, x=time_int, group=id, colour = randomization_factor), alpha=0.2)+
  facet_wrap(~ randomization_factor)+
  theme_minimal()+
  scale_color_manual(values = c("#4475b4", "#fc8d59")) +
  theme(
    legend.position = "none"
  ) +
  geom_hline(yintercept = 90, color="red", linetype="dashed")+
  labs(x = "Procedure duration (secs)")
}
