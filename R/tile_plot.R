#' @title tile plot for categorical outcomes

#' @name tile_plot
#' @export

#' @importFrom dplyr select mutate arrange desc
#' @importFrom forcats fct_explicit_na
#' @importFrom ggplot2 ggplot aes geom_tile facet_wrap scale_fill_brewer theme element_blank
#'
tile_plot <- function(data, outcome){

data %>%
  select(id, randomization.factor, {{outcome}}) %>%
  mutate(outcome = fct_explicit_na({{outcome}},
                                            na_level = "Not reported")) %>%
    arrange(desc(outcome)) %>%
  ggplot(aes(y=outcome, x=reorder(id, as.numeric(outcome))))+
  geom_tile(width = 0.5, height = .8, aes(fill = outcome))+
    facet_wrap(~randomization.factor, nrow = 2)+
  scale_fill_brewer(palette = "RdYlBu")+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot",
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank())

}

tile_plot(trial_mod, diffoxygen.factor)
