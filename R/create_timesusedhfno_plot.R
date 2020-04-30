#' @title Plot of number of times Anesthesia Assistants used HFNO
#' @rdname create_timesusedhfno_plot
#' @param trial_mod dataframe of all variables collected

#' @export
#' @importFrom dplyr select mutate filter group_by n ungroup if_else
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual theme_minimal theme element_blank labs

create_timesusedhfno_plot <- function(trial_mod){
readd(trial_mod) %>%
  select(id, randomization.factor, contains("used")) %>%
  filter(randomization.factor=="High Flow nasal oxygen") %>%
  drop_na(timesusedhfno.factor) %>%
  ggplot() +
  geom_bar(aes(y=timesusedhfno.factor, fill=timesusedhfno.factor))+
  scale_fill_brewer(palette = "RdYlBu") +
  theme(axis.line = element_line(colour = "black"),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        plot.title.position = "plot") +
  labs(x="Count")
}
