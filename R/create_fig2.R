#' @title Figure 2 for manuscript
#' @name create_fig2
#' @description Plot of tcco2 trajectories
#' @export
#' @importFrom patchwork plot_layout
#' @importFrom ggplot2 ggsave


create_fig2 <- function(plot_fanova_data,
                        plot_fanova_effect){

p1 <- plot_fanova_data
p2 <- plot_fanova_effect

layout <- "
AABB
#CC#
"
combined <- p1 + p2 +
  plot_layout(design = layout)

ggsave(combined, device = "png", filename = "plots/fig-2.png",
       width = 174, units = "mm")

}
