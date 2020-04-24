#' @title Table co2 mean
#' @rdname make_table_co2_mean
#' @export

#' @importFrom gt gt
make_table_co2_mean <- function(model_obj, unit, title, format = "html"){

  summary_model <- make_brms_summary(model_obj, unit)
  result <- summary_model %>% gt()
  return(result)
}
