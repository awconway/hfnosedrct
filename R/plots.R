# ***************************
# ***************************
# Primary analysis plots
# ***************************
# ***************************
#' @title Plot peak co2
#' @rdname make_plot_co2_peak
#' @param data_primary dataframe
#' @param model model object
#' @description Plots peak co2 against baseline
#' @export

#' @importFrom brms conditional_effects
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual geom_line geom_ribbon scale_y_continuous scale_x_continuous ggtitle theme_bw theme element_rect labs
make_plot_co2_peak <- function(data_primary, model){

  # Plot trial data
  result <- data_primary %>%
    ggplot(aes(x=co2_baseline, y=co2_peak)) +
    geom_point(aes(shape=randomization_factor)) +
    scale_shape_manual(values = c(1,16)) +
    geom_line(aes(x=co2_baseline, y=estimate__), data = conditional_effects(model)[[4]]) +
    geom_ribbon(aes(x=co2_baseline, ymin=lower__, ymax=upper__), data = conditional_effects(model)[[4]],alpha=0.25) +
    scale_y_continuous(name = "Peak TcCO2 (mmHg)", limits = c(25,95)) +
    scale_x_continuous(name = "Baseline TcCO2 (mmHg)") +
    ggtitle("Baseline TcCO2 effect") +
    theme_bw() +
    theme(
          # legend.justification = c(-0.05, 1.05),
          # legend.position = c(0, 1),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
    labs(shape="Treatment group")

  return(result)
}

#' @title Plot mean co2
#' @rdname make_plot_co2_mean
#' @param data_primary dataframe
#' @param model model object
#' @description Plots mean co2 against baseline
#' @export

#' @importFrom brms conditional_effects
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual geom_line geom_ribbon scale_y_continuous scale_x_continuous ggtitle theme_bw theme element_rect labs
make_plot_co2_mean <- function(data_primary, model){

  # Plot trial data
  result <- data_primary %>%
    ggplot(aes(x=co2_baseline, y=co2_mean)) +
    geom_point(aes(shape=randomization_factor)) +
    scale_shape_manual(values = c(1,16)) +
    geom_line(aes(x=co2_baseline, y=estimate__), data = conditional_effects(model)[[4]]) +
    geom_ribbon(aes(x=co2_baseline, ymin=lower__, ymax=upper__), data = conditional_effects(model)[[4]],alpha=0.25) +
    scale_y_continuous(name = "Mean TcCO2 (mmHg)", limits = c(25,95)) +
    scale_x_continuous(name = "Baseline TcCO2 (mmHg)") +
    ggtitle("Baseline TcCO2 effect") +
    theme_bw() +
    theme(
      # legend.justification = c(-0.05, 1.05),
      # legend.position = c(0, 1),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
    labs(shape="Treatment group")

  return(result)
}


# ***************************
# ***************************
# FANOVA Plots
# ***************************
# ***************************

# ***************************


# ***************************
# Plot fanova data
# ***************************
#' @title Plot fanova data
#' @rdname make_plot_fanova_data
#' @param data_fanova dataframe
#' @param reso Number of seconds resolution for functional data
#' @importFrom dplyr ungroup group_by summarize first arrange desc slice pull mutate filter
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_continuous
#' scale_alpha_continuous scale_size_continuous theme_bw theme element_blank ggtitle geom_vline annotate
#' @importFrom gridExtra grid.arrange

#' @export


make_plot_fanova_data <- function(data_fanova, reso){

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
    geom_line() +
    scale_y_continuous(name = "TcCO2 (mmHg)",
                       limits=c(0,100)) +
    scale_x_continuous(name = "Time (min)",
                       breaks = x_breaks,
                       labels = x_labels,
                       limits = x_limits) +
    scale_alpha_continuous(range=c(0.20,1)) +
    scale_size_continuous(range=c(.25,.75)) +
    theme_bw()  +
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
    geom_line() +
    scale_y_continuous(name = "TcCO2 (mmHg)",
                       limits=c(0,100)) +
    scale_x_continuous(name = "Time (min)",
                       breaks = x_breaks,
                       labels = x_labels,
                       limits = x_limits) +
    scale_alpha_continuous(range=c(0.20,1)) +
    scale_size_continuous(range=c(.25,.75)) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid = element_blank()) +
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

  result <- grid.arrange(plot_fmo, plot_hfno, ncol=2)
  # saveRDS(result, file = "./plots/plot_fanova_data.RDS")

  return(result)
}

# ***************************
# Stacked FANOVA plots
# ***************************
#not in drake plan
make_plot_fanova_stack <- function(data_plot,plot_effect){

  result <- grid.arrange(data_plot, plot_effect)
  saveRDS(result, file="./plots/fanova_stack_plot.RDS")

  return(result)
}

# ***************************
# Posterior predictive plots
# ***************************

# ***************************
# Make a posterior predictive plot for both groups
# ***************************
#not in drake plan
make_ppred_plot <- function(model, n_sim, reso, seed=42){

  set.seed(seed)
  sim_data <- fanova_simulate(model, n_sim)

  plot_CI_fmo <- component_CI_plot(sim_data = sim_data,
                                   reso = reso,
                                   component_str = "fmo",
                                   title = "Face mask oxygen",
                                   ylim = c(0,100))
  plot_CI_hfno <- component_CI_plot(sim_data = sim_data,
                                    reso = reso,
                                    component_str = "hfno",
                                    title = "High flow nasal oxygen",
                                    ylim = c(0,100))



  plot_CI <- grid.arrange(plot_CI_fmo, plot_CI_hfno, ncol=2,
                          left="TcCO2 (mmHg)",
                          bottom="Time (min)")
  saveRDS(plot_CI, file="./plots/fanova_ppred_plot.RDS")

  return(plot_CI_fmo)
}

# ***************************
# Simulate from the fanova model
# ***************************
#not in drake plan
fanova_simulate <- function(model, n_sim){

  posterior_sample <- inla.posterior.sample(n=n_sim, result = model)

  # Extract smooth component
  post_mu <- extract_effect(posterior_sample, component = "mu")
  post_alpha <- extract_effect(posterior_sample, component = "alpha")
  post_hyper <- extract_hyperpar(posterior_sample)

  n_obs <- nrow(post_mu)

  # Simulate random effects
  iid_sd <- (post_hyper["Precision for id"]^-.5) %>%
    unlist() %>%
    unname()

  ar1_rho <- (post_hyper["Rho for eps"]) %>%
    unlist() %>%
    unname()

  ar1_sd_marginal <- (post_hyper["Precision for eps"]^-.5) %>%
    unlist() %>%
    unname()

  ar1_sd <- ar1_sd_marginal*sqrt(1-ar1_rho^2)


  post_iid <- rnorm(n=n_sim, mean=0, sd=iid_sd)
  post_ar1 <- simulate_ar1(n_obs=n_obs, n_sim=n_sim, sd=ar1_sd, rho=ar1_rho)

  result_fmo <- post_mu + post_ar1
  result_hfno <- post_mu + post_alpha + post_ar1

  result_fmo <- apply(result_fmo, 1, function(x) x + post_iid) %>% t()
  result_hfno <- apply(result_hfno, 1, function(x) x + post_iid) %>% t()

  result <- list("fmo" = result_fmo, "hfno" = result_hfno)

  return(result)
}

# ***************************
# Simulate the AR(1) process
# ***************************
#not in drake plan
simulate_ar1 <- function(n_obs, n_sim, sd, rho){

  sd_stationary <- sd/sqrt(1-rho^2)

  result <- matrix(NA, nrow=n_obs, ncol=n_sim)
  result[1,] <- rnorm(n = n_sim, mean=0, sd= sd_stationary)

  ar1_diff <- sapply(sd, function(x) rnorm(n= n_obs-1, mean=0, sd=x))

  for(i in 2:n_obs){
    result[i,] <- rho*result[i-1,] + ar1_diff[i-1,]
  }

  return(result)
}

# ***************************
# Extract the group effects from the posterior samples
# ***************************
#not in drake plan
extract_effect <- function(posterior_sample, component){
  # Get indices of smooth components
  pattern <-  paste0("^",component,".")
  component_idx <- grep(pattern,rownames(posterior_sample[[1]]$latent))

  # Convert to data.frame
  result <- lapply(posterior_sample, function(x) x$latent) %>%
    bind_cols()

  result <- result[component_idx,]

  return(result)
}

# ***************************
# Extract the hyperparameters from the posterior samples
# ***************************
#not in drake plan
extract_hyperpar <- function(posterior_sample){
  posterior_hyper <- lapply(posterior_sample, function(x) x$hyperpar) %>%
    unlist() %>%
    matrix(ncol=5, byrow=TRUE) %>%
    data.frame()

  names(posterior_hyper) <- names(posterior_sample[[1]]$hyperpar)

  return(posterior_hyper)
}

# ***************************
# Helper function for creating each posterior predictive plot
# ***************************
#not in drake plan
component_CI_plot <- function(sim_data, reso, component_str, title, ylim=c(-40,40)){

  # Construct approximate credible intervals
  CI_data <- apply(sim_data[[component_str]], 1,
                   function(x) quantile(x, probs=c(0.025,0.975))) %>%
    t() %>%
    data.frame()

  time_int <- reso*(0:(nrow(CI_data)-1))

  CI_data <- cbind(time_int, CI_data)

  # Make names consistent
  names(CI_data) <- c("time", "lower", "upper")

  # Get highlight
  if(component_str == "fmo"){
    highlight <- sim_data[[component_str]][,1]

  }
  else{
    highlight <- sim_data[[component_str]][,2]
  }
  highlight_data <- data.frame(time=time_int,
                               co2 = highlight)

  # Create axis labels
  x_labels <- seq(0, 360, by=60)
  x_breaks <- x_labels*60

  plot_CI <- ggplot(aes(x=time, ymin=lower, ymax=upper), data=CI_data) +
    geom_ribbon(alpha=0.25) +
    geom_hline(yintercept = 0, lty=2) +
    coord_cartesian(ylim=ylim) +
    scale_x_continuous(name = "Time (min)", breaks = x_breaks, labels = x_labels) +
    ggtitle(title) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    geom_line(aes(x=highlight_data$time, y = highlight_data$co2))

  return(plot_CI)
}



