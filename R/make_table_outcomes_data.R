#' @title Make table dataframe
#' @rdname make_table_outcomes_data
#' @export
#' @importFrom dplyr bind_rows group_by mutate n ungroup left_join

make_table_outcomes_data <- function(model_list, data_list){

  # Build the full descriptive statistics table
  table_descr <- list()
  table_descr[["co2_peak"]] <- make_summary_descr(model_list[["model_co2_peak"]]$data,
                                                  unit="mmHg",
                                                  response="co2_peak",
                                                  response_title = "Peak TcCO<sub>2</sub>")
  table_descr[["co2_mean"]] <- make_summary_descr(model_list[["model_co2_mean"]]$data,
                                                  unit="mmHg",
                                                  response="co2_mean",
                                                  response_title = "Mean TcCO<sub>2</sub>")
  table_descr[["spo2"]] <- make_summary_descr(model_list[["model_spo2"]],
                                              unit="",
                                              response="spo2_mean",
                                              response_title = "SpO<sub>2</sub>"
  )
  table_descr[["isas"]] <- make_summary_descr(model_list[["model_isas"]]$data,
                                              unit="",
                                              response="isas_mean",
                                              response_title = "ISAS score")
  table_descr[["comfort"]] <- make_summary_descr(model_list[["model_comfort"]]$data,
                                                 unit="",
                                                 response="oxygencomfort_num",
                                                 response_title = "Patient comfort",
                                                 factor_levels = levels(data_list[["data_comfort"]]$oxygencomfort_factor))
  table_descr[["diffoxygen"]] <- make_summary_descr(model_list[["model_diffoxygen"]]$data,
                                                    unit="",
                                                    response="diffoxygen_num",
                                                    response_title = "Difficulty maintaining oxygenation status",
                                                    factor_levels = levels(data_list[["data_assist"]]$diffoxygen_factor))
  table_descr[["diffuseoxygen"]] <- make_summary_descr(model_list[["model_diffuseoxygen"]]$data,
                                                       unit="",
                                                       response="diffuseoxygen_num",
                                                       response_title = "Difficulty using oxygen delivery device",
                                                       factor_levels = levels(data_list[["data_assist"]]$diffuseoxygen_factor))
  table_descr[["troops"]] <- make_summary_descr(model_list[["model_troops"]]$data,
                                                unit="",
                                                response="troopsminairway_num",
                                                response_title = "Minor airway or breathing event")

  table_descr <- table_descr %>%
    bind_rows() %>%
    group_by(model) %>%
    mutate(row=1:n()) %>%
    ungroup()

  table_model <- list()

  # Build full model summary table
  table_model[["co2_peak"]] <- make_brms_summary(model_list[["model_co2_peak"]],
                                                 unit="mmHg",
                                                 effect_type = "Absolute difference",
                                                 response="co2_peak",
                                                 response_title = "Peak TcCO<sub>2</sub>")
  table_model[["co2_mean"]] <- make_brms_summary(model_list[["model_co2_mean"]],
                                                 unit="mmHg",
                                                 effect_type = "Absolute difference",
                                                 response="co2_mean",
                                                 response_title = "Mean TcCO<sub>2</sub>")
  # table_model[["spo2"]] <- make_brms_summary(model_list[["model_spo2"]],
  #                                            unit="",
  #                                            effect_type = "absolute difference",
  #                                            response="spo2_mean",
  #                                            response_title = "Mean SpO<sub>2</sub>")
  table_model[["isas"]] <- make_brms_summary(model_list[["model_isas"]],
                                             unit="",
                                             response="isas_mean",
                                             effect_type = "Absolute difference",
                                             response_title = "ISAS score")
  table_model[["comfort"]] <- make_brms_summary(model_list[["model_comfort"]],
                                                unit="",
                                                effect_type = "Odds ratio",
                                                response="oxygencomfort_num",
                                                response_title = "Patient comfort")
  table_model[["diffoxygen"]] <- make_brms_summary(model_list[["model_diffoxygen"]],
                                                   unit="",
                                                   effect_type = "Odds ratio",
                                                   response="diffoxygen_num",
                                                   response_title = "Difficulty maintaining oxygenation status")
  table_model[["diffuseoxygen"]] <- make_brms_summary(model_list[["model_diffuseoxygen"]],
                                                      unit="",
                                                      effect_type = "Odds ratio",
                                                      response="diffuseoxygen_num",
                                                      response_title = "Difficulty using oxygen delivery device")
  table_model[["troops"]] <- make_brms_summary(model_list[["model_troops"]],
                                               unit="",
                                               effect_type = "Odds ratio",
                                               response="troopsminairway_num",
                                               response_title = "Minor airway or breathing event")

  table_model <- table_model %>%
    bind_rows()

  result <- left_join(table_descr, table_model, by=c("model"))

  return(result)
}
