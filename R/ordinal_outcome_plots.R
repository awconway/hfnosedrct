#' @title Difference between groups for ordinal outcomes
#' @param trial_mod dataframe
#' @param factor ordinal outcome to plot
#' @rdname create_ordinal_outcome_plot
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_continuous scale_fill_brewer theme element_line element_blank
#' @importFrom tidyr drop_na
#' @export
create_ordinal_outcome_plot <- function(trial_mod, factor) {

  trial_mod %>%
    select(randomization.factor, {{factor}}) %>%
    drop_na() %>%
  ggplot(aes(y = randomization.factor, fill = {{factor}})
         ) +
    geom_bar(position = "fill") +
    scale_x_continuous("Proportion",
                        labels = c(0, 25, 50, 75, 100)) +
    scale_fill_brewer(palette = "RdYlBu") +
    theme(axis.line = element_line(colour = "black"),
          axis.title.y = element_blank(),
          legend.position = "bottom",
          panel.background = element_blank(),
          plot.title.position = "plot",
          legend.title = element_blank())

}

