#' @title Plot of ISAS ratings
#' @rdname create_isas_plot
#' @export
#' @importFrom dplyr group_by mutate n
#' @importFrom ggplot2 ggplot aes theme_minimal theme labs scale_color_manual
#' @importFrom ggbeeswarm geom_beeswarm
#'

create_isas_plot <- function(data_isas){
  data_isas %>%
  ggplot()+
  ggbeeswarm::geom_beeswarm(aes(y=isas_mean, x=randomization_factor,
                                colour = randomization_factor))+
  theme_minimal()+
  scale_color_manual(values = c("#4475b4", "#fc8d59")) +
  theme(axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  labs(y= "ISAS score")
}
