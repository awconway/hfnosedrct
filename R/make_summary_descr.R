#' @title Summary description
#' @rdname make_summary_descr
#' @export
#'
#' @importFrom brms fixef
#' @importFrom dplyr mutate group_by summarize select n arrange desc ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom glue glue
make_summary_descr <- function(data_obj, unit, response, response_title, factor_levels=NULL){

  table_data <- data_obj
  table_data$value<- data_obj[[response]]

  # Summaries for continuous outcomes
  if(response %in% c("co2_peak",
                     "co2_mean",
                     "isas_mean")){

    # Row containing counts
    row1 <- make_row_count(data_obj=table_data, response, response_title)

    # Row containing means
    row2 <- table_data %>%
      mutate(model = response_title,
             response = "Mean (sd)") %>%
      group_by(model, response, randomization_factor) %>%
      summarize(mean = mean(value), sd = sd(value)) %>%
      mutate(
        mean = mean %>% round(1) %>% format(nsmall = 1),
        sd = sd %>% round(1) %>% format(nsmall = 1)
      ) %>%
      mutate(summary = paste0(mean, " ", unit, " (", sd,")")) %>%
      ungroup() %>%
      select(model, response, randomization_factor, summary) %>%
      pivot_wider(names_from = "randomization_factor", values_from = "summary")

    result <- rbind(row1,row2)
  }
  # Summaries for ordinal outcomes
  else if(response %in% c("diffoxygen_num",
                          "diffuseoxygen_num",
                          "troopsminairway_num",
                          "oxygencomfort_num"))
  {

    if(response == "troopsminairway_num"){
      table_data$value_factor<- ifelse(table_data[[response]] == 1, "Yes","No")
    }
    else{
      table_data$value_factor<- factor_levels[table_data[[response]]]
    }

    # Row containing counts
    row1 <- make_row_count(data_obj=table_data, response, response_title)

    # Rows containing values
    row2 <- table_data %>%
      mutate(model = response_title,
             response = value_factor) %>%
      group_by(model, response, value, randomization_factor) %>%
      summarize(summary = n()) %>%
      arrange(desc(value)) %>%
      ungroup() %>%
      mutate(response = as.character(response),
             summary = as.character(summary))  %>%
      select(model, response, randomization_factor, summary) %>%
      pivot_wider(names_from = "randomization_factor", values_from = "summary")

    result <- rbind(row1, row2)
  }
  else if(response=="spo2_mean"){

    # Row containing counts
    row1 <- make_row_count(data_obj=table_data, response, response_title)

    # Row containing desaturation event summaries
    row2 <- table_data %>%
      mutate(model = response_title,
             response = "SpO<sub>2</sub> <90% event") %>%
      group_by(model, response, randomization_factor) %>%
      summarize(n = n(),
                count = sum(desat_event),
                prop = count/n * 100) %>%
      mutate(
        prop = prop %>% round(0)
      ) %>%
      mutate(summary = paste0(count, " (", prop,"%)")) %>%
      ungroup() %>%
      select(model, response, randomization_factor, summary) %>%
      pivot_wider(names_from = "randomization_factor", values_from = "summary")

    # Row containing AUC summaries
    row3 <- table_data %>%
      mutate(model = response_title,
             response = "Median (IQR)") %>%
      group_by(model, response, randomization_factor) %>%
      summarize(median = median(ifelse(spo2_auc != 0, spo2_auc, NA), na.rm=TRUE),
                iqr_low = quantile(ifelse(spo2_auc != 0, spo2_auc, NA), 0.25, na.rm=TRUE),
                iqr_high = quantile(ifelse(spo2_auc != 0, spo2_auc, NA), 0.75, na.rm=TRUE)) %>%
      mutate(summary = glue("{median} ({iqr_low}, {iqr_high})")) %>%
      ungroup() %>%
      select(model, response, randomization_factor, summary) %>%
      pivot_wider(names_from = "randomization_factor", values_from = "summary")

    result <- rbind(row1,row2, row3)
  }
  else{
    warning(paste("response",response," not in function definition"))
  }

  return(result)
}
