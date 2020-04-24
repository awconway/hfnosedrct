#' @title Format outcomes for table
#' @rdname make_table_outcomes_formatted
#' @export

#' @importFrom dplyr mutate select
#' @importFrom gt gt fmt_missing cols_label tab_spanner vars tab_style cell_text cells_body
#'
make_table_outcomes_formatted <- function(model_list, data_list){
  table_data <- make_table_outcomes_data(model_list, data_list)

  # Change 'model' column to only have entry in 1st row
  table_data <- table_data %>%
    mutate(
      model = ifelse(row==1, model, NA)
    )

  # Change 'estimate' and 'effect_type' column to only have entry in 2nd row
  table_data <- table_data %>%
    mutate(
      estimate = ifelse(row==2, estimate, NA),
      effect_type = ifelse(row==2, effect_type, NA)
    )

  # Arrange columns

  result <- table_data %>%
    select(
      model,
      response,
      `Face mask oxygen`,
      `High Flow nasal oxygen`,
      effect_type,
      estimate
    ) %>%
    gt() %>%
    fmt_missing(columns = c("model", "estimate","effect_type"),
                missing_text = " "
    ) %>%
    fmt_missing(
      columns = c("Face mask oxygen","High Flow nasal oxygen"),
      missing_text = "0",
    ) %>%
    cols_label(
      model = "Outcome",
      response = "Summary value",
      `Face mask oxygen` = "Face mask oxygen",
      `High Flow nasal oxygen` = "High flow nasal oxygen",
      estimate = "Estimated treatment effect",
      effect_type = "Effect type"
    ) %>%
    tab_spanner(
      label = "Randomization",
      columns = vars(`Face mask oxygen`,
                     `High Flow nasal oxygen`)
    ) %>%
    tab_style(style = list(
      cell_text(weight = "bold")),
      locations = cells_body(columns = "model")
    )

}
