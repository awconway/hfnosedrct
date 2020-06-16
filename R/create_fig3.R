#' @title Figure 3 for manuscript
#' @name create_fig3
#' @description Plot of oxygen use
#' @export
#' @importFrom ggplot2 ggsave


create_fig3 <- function(oxygen_flow_plot){

  plot <- oxygen_flow_plot

  ggsave(plot, device = "png", filename = "plots/fig-3.png",
         width = 174, units = "mm")

}

#' @title Figure 3 for manuscript
#' @name create_fig3_tiff
#' @description Plot of oxygen use
#' @export
#' @importFrom ggplot2 ggsave


create_fig3_tiff <- function(oxygen_flow_plot){

  plot <- oxygen_flow_plot

  ggsave(plot, device = "tiff", filename = "plots/fig-3.tiff",
         width = 174, units = "mm")

}

