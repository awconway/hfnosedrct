#' @title Format outcomes for table
#' @rdname make_flextable
#' @export

#' @importFrom dplyr mutate select
#' @importFrom gt gt fmt_missing cols_label tab_spanner vars tab_style cell_text cells_body
#' fmt_markdown tab_source_note
#' @importFrom flextable flextable hline_bottom hline hline_top add_header_row border_remove
#' merge_at align footnote as_paragraph as_sub compose set_header_labels autofit width align_text_col
#' @importFrom officer fp_border
#'
make_flextable_outcomes_formatted <- function(model_list, data_list){
  table_data <- make_table_outcomes_data(model_list, data_list)

  # Change 'model' column to only have entry in 1st row
  table_data <- table_data %>%
    mutate(
      model = ifelse(row==1, model, NA)
    )

  # # Change 'estimate' and 'effect_type' column to only have entry in 2nd row
  # table_data <- table_data %>%
  #   mutate(
  #     estimate = ifelse(row==2, estimate, NA),
  #     effect_type = ifelse(row==2, effect_type, NA)
  #   )

  # Arrange columns

  result <- table_data %>%
    select(
      model,
      response,
      `Face mask oxygen`,
      `High Flow nasal oxygen`,
      effect_type,
      estimate
    )

  result %>%
    flextable() %>%
    autofit() %>%
    set_header_labels(model = "Outcome",
      response = "Summary value",
      `Face mask oxygen` = "Face mask oxygen",
      `High Flow nasal oxygen` = "High flow nasal oxygen",
      estimate = "Estimated treatment effect (95% CI)",
      effect_type = "Effect type") %>%
     compose(i = 1, j =~model, part = "body",
    value = as_paragraph(
      "Peak TcCO",
      as_sub("2")
    ) ) %>%
    compose(i = 3, j =~model, part = "body",
    value = as_paragraph(
      "Mean TcCO",
      as_sub("2")
    ) ) %>%
    compose(i = 5, j =~model, part = "body",
    value = as_paragraph(
      "SpO",
      as_sub("2")
    ) ) %>%
    compose(i = 6, j =~response, part = "body",
    value = as_paragraph(
      "SpO",
      as_sub("2"),
      " <90% event"
    ) ) %>%
    compose(i = 7, j =~response, part = "body",
    value = as_paragraph(
      "Median (IQR) Area under SpO",
      as_sub("2"),
      " desaturation curve"
    ) ) %>%
      footnote(i =2, j = 1,
               value = as_paragraph(
                "Odds ratios are interpreted as the odds of the event occuring in the HFNO group compared with the odds of the event occuring in the facemask group"
               ),
               ref_symbols = "",
               part = "body") %>%
          footnote(i =2, j = 1,
               value = as_paragraph(
                "TcCO",
                as_sub("2"),
                " = Transcutaneous carbon dioxide concentration"
               ),
               ref_symbols = "",
               part = "body") %>%
          footnote(i =2, j = 1,
               value = as_paragraph(
                "SpO",
                as_sub("2"),
                " = Percentage of hemoglobin saturate with oxygen"
               ),
               ref_symbols = "",
               part = "body") %>%
          footnote(i =2, j = 1,
               value = as_paragraph(
                "ISAS = Iowa Satisfaction with Anesthesia Scale"
               ),
               ref_symbols = "",
               part = "body") %>%
         footnote(i =2, j = 1,
               value = as_paragraph(
                "95% CI = 95% credible intervals"
               ),
               ref_symbols = "",
               part = "body") %>%
      add_header_row(values =
                       c("","Randomization", ""),  colwidths = c(2, 2 , 2)) %>%
    merge_at(i=1, j=3:4, part = "header") %>%
      align(align = "center", part = "header")  %>%
    border_remove() %>%
    hline(i =1, j=3, part = "header",
            border = fp_border(color="black", width = 0.5)) %>%
    hline_top(part = "header",
            border = fp_border(color="black", width = 2)) %>%
    hline_bottom(part = "header",
            border = fp_border(color="black", width = 2)) %>%
   hline_bottom(part = "body",
            border = fp_border(color="black", width = 2)) %>%
  width(j=1,width = 1.31) %>%
      width(j=2,width = 1.94) %>%
      width(j=3,width = 1.69) %>%
          width(j=4,width = 1.69) %>%
          width(j=5,width = 1.47) %>%
          width(j=6,width = 1.66) %>%
    align_text_col(align = "left") %>%
          align(align = "center", part = "header")


}
