#' @title Subgroup table
#' @rdname make_table_subgroup
#' @export
#'
#'
#' @importFrom dplyr mutate case_when group_by summarize n ungroup select left_join
#' @importFrom gt gt tab_header md tab_spanner vars cols_label fmt_missing tab_style cell_text cells_body
#' @importFrom tidyr pivot_wider
make_table_subgroup <- function(model_obj){

  # Descriptive statistics
  data_obj <- model_obj$data %>%
    mutate(subgroup = case_when(osa_factor == "No" & crt_factor == "No" ~ "Baseline",
                                osa_factor == "Yes" & crt_factor == "No" ~ "OSA",
                                osa_factor == "No" & crt_factor == "Yes" ~ "CRT",
                                osa_factor == "Yes" & crt_factor == "Yes" ~ "OSA + CRT"))

  # Row containing counts
  row1 <- data_obj %>%
    mutate(response = "N") %>%
    group_by(subgroup, response, randomization_factor) %>%
    summarize(summary = n()) %>%
    ungroup() %>%
    mutate(summary = as.character(summary)) %>%
    select(subgroup, response, randomization_factor, summary) %>%
    pivot_wider(names_from = "randomization_factor", values_from = "summary")

  # Row containing means
  row2 <- data_obj %>%
    mutate(response = "mean (sd)") %>%
    group_by(subgroup, response, randomization_factor) %>%
    summarize(mean = mean(co2_peak), sd = sd(co2_peak)) %>%
    mutate(
      mean = mean %>% round(1) %>% format(nsmall = 1),
      sd = sd %>% round(1) %>% format(nsmall = 1)
    ) %>%
    mutate(summary = paste0(mean, " mmHg (", sd,")")) %>%
    ungroup() %>%
    select(subgroup, response, randomization_factor, summary) %>%
    pivot_wider(names_from = "randomization_factor", values_from = "summary")

  table_descr <- rbind(row1,row2) %>%
    arrange(subgroup) %>%
    filter(subgroup !="OSA + CRT") %>%
    group_by(subgroup) %>%
    mutate(row=1:n()) %>%
    ungroup()

  # Marginal posterior summaries
  CI <- make_subgroup(model_obj, unit="mmHg")

  table_post <- data.frame(subgroup = c("Baseline", "CRT", "OSA"),
                           estimate = lapply(CI, function(x) x$full) %>% unlist(),
                           p_above =lapply(CI, function(x) x$p_above) %>% unlist(),
                           p_below =lapply(CI, function(x) x$p_below) %>% unlist())

  # Combined table
  table_post$subgroup <- as.character(table_post$subgroup)
  table_post$row <- 2

  result <- table_descr %>% left_join(table_post, by = c("subgroup", "row"))

  # Change 'subgroup' column to only have entry in 1st row
  result <- result %>%
    mutate(subgroup = ifelse(row == 1, subgroup, NA))

  # Format table
  result <- result %>%
    select(-row) %>%
    gt() %>%
    tab_header(
      title = md("Subgroup analysis"),
      subtitle = md("Marginal posterior summaries of treatment effect when treatment is allowed to vary with stratification variables")
    ) %>%
    tab_spanner(
      label = "Peak TcCO2",
      columns = vars(`Face mask oxygen`,
                     `High Flow nasal oxygen`)
    ) %>%
    tab_spanner(
      label = "Posterior probability",
      columns = vars(p_above,
                     p_below)
    ) %>%
    cols_label(subgroup = "Subgroup",
               response = "Summary value",
               `Face mask oxygen` = "Face mask oxygen",
               `High Flow nasal oxygen` = "High flow nasal oxygen",
               estimate = "Treatment effect (95% CI)",
               p_above = "> 4 mmHg",
               p_below = "< -4 mmHg") %>%
    fmt_missing(columns = vars(subgroup, estimate, p_above, p_below),
                missing_text = " ") %>%
    tab_style(style = list(
      cell_text(weight = "bold")),
      locations = cells_body(columns = "subgroup")
    )

  return(result)
}
