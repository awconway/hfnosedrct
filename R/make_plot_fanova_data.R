# ***************************
# Plot fanova data
# ***************************
#' @title Plot fanova data
#' @rdname make_plot_fanova_data
#' @param data_fanova dataframe
#' @importFrom dplyr ungroup group_by summarize first arrange desc slice pull mutate filter
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_continuous
#' scale_alpha_continuous scale_size_continuous theme_bw theme element_blank ggtitle geom_vline annotate
#' @importFrom patchwork plot_layout
#' @export


make_plot_fanova_data <- function(data_fanova){

  data_plot <- data_fanova

  # Create axis labels
  x_labels <- seq(0, 360, by=60)
  x_breaks <- x_labels*60
  xmax <- data_plot %>%
    ungroup() %>%
    pull(time_int) %>%
    max()
  x_limits <- c(0,xmax)

  # Highlight the longest trace
  id_highlight <- data_plot %>%
    group_by(id) %>%
    summarize(longest = max(time_int),
              randomization_factor = first(randomization_factor)) %>%
    arrange(desc(longest)) %>%
    group_by(randomization_factor) %>%
    slice(1) %>%
    pull(id)

  data_plot <- data_plot %>%
    mutate(highlight = ifelse(id %in% id_highlight,1,0)) %>%
    mutate(highlight_size = ifelse(highlight, 0.5, 0.25),
           highlight_alpha = ifelse(highlight, 1.0, 0.5))

  # Find time where 50% and 75% of participants completed
  pct_complete <- data_fanova %>%
    group_by(id) %>%
    summarize(len = max(time_int)) %>%
    pull(len) %>%
    quantile(probs=c(.50,.75, .9))

  label_pos <- 10

  plot_fmo <- data_plot %>%
    filter(randomization_num == 0) %>%
    ggplot(aes(x=time_int, y=co2, group=id,alpha=highlight, size=highlight)) +
    geom_line(colour="#4475b4") +
    scale_y_continuous(name = "TcCO2 (mmHg)",
                       limits=c(0,100)) +
    scale_x_continuous(name = "Time (min)",
                       breaks = x_breaks,
                       labels = x_labels,
                       limits = x_limits) +
    scale_alpha_continuous(range=c(0.20,1)) +
    scale_size_continuous(range=c(.25,.75)) +
    theme_minimal()  +
    theme(legend.position = "none",
          panel.grid = element_blank()) +
    ggtitle("Face mask oxygen") +
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
             col="gray75")

  plot_hfno <- data_plot %>%
    filter(randomization_num == 1) %>%
    ggplot(aes(x=time_int, y=co2, group=id,alpha=highlight, size=highlight)) +
    geom_line(colour = "#fc8d59") +
    scale_y_continuous(name = "TcCO2 (mmHg)",
                       limits=c(0,100)) +
    scale_x_continuous(name = "Time (min)",
                       breaks = x_breaks,
                       labels = x_labels,
                       limits = x_limits) +
    scale_alpha_continuous(range=c(0.20,1)) +
    scale_size_continuous(range=c(.25,.75)) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle("High flow nasal oxygen") +
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
             col="gray75")

  plot_fmo + plot_hfno
}
