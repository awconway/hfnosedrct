#' @title FANOVA effect plot
#' @rdname make_plot_component
#' @export
#' @importFrom dplyr mutate n group_by summarize pull filter
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_x_continuous scale_y_continuous
#' ggtitle theme_bw theme element_blank geom_vline annotate geom_hline coord_cartesian
# ***************************
make_plot_component <- function(data_fanova, model, reso, component_str, title, ylim=c(-40,40)){

  component_data <- cbind(component_str,model$summary.random[[component_str]][,4:6])
  names(component_data) <- c("component","lower", "med", "upper")

  data_plot <- component_data %>%
    mutate(time=1:n() - 1) %>%
    mutate(time=time*reso)

  # Create axis labels
  x_labels <- seq(0, 360, by=60)
  x_breaks <- x_labels*60

  # Find time where 50% and 75% and 90% of participants completed
  pct_complete <- data_fanova %>%
    group_by(id) %>%
    summarize(len = max(time_int)) %>%
    pull(len) %>%
    quantile(probs=c(.50,.75,.90))

  # Completion % label postition
  label_pos <- ifelse(component_str == "mu", 10, -30)

  result <- data_plot %>%
    filter(component==component_str) %>%
    ggplot(aes(x=time, y=med)) +
    geom_line() +
    geom_ribbon(aes(x=time, ymin=lower, ymax=upper), alpha=0.25) +
    geom_hline(yintercept = 0, lty=2) +
    coord_cartesian(ylim=ylim) +
    scale_x_continuous(name = "Time (min)", breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(name = "TcCO2 (mmHg)") +
    ggtitle(title) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_vline(aes(xintercept = pct_complete[1]),
               lty=3,
               col="gray75") +
    geom_vline(aes(xintercept = pct_complete[2]),
               lty=3,
               col="gray75") +
    geom_vline(aes(xintercept = pct_complete[3]),
               lty=3,
               col="gray75") +
    annotate("text",
             x=pct_complete,
             label=c("50%\n","75%\n", "90%\n"),
             y=label_pos,
             angle=90,
             col="gray75")+
    geom_hline(yintercept = c(-4,4), colour="red", lty="dotted", size=0.75)

  return(result)
}
