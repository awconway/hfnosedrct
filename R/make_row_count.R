#' @title Row counts for table
#' @rdname make_row_count
#' @export
#' @importFrom dplyr mutate group_by summarize n ungroup select
#' @importFrom tidyr pivot_wider
make_row_count <- function(data_obj, response, response_title){
  result <- data_obj %>%
    mutate(model = response_title,
           response = "N") %>%
    group_by(model, response, randomization_factor) %>%
    summarize(summary = n()) %>%
    ungroup() %>%
    mutate(summary = as.character(summary)) %>%
    select(model, response, randomization_factor, summary) %>%
    pivot_wider(names_from = "randomization_factor", values_from = "summary")

  return(result)
}
