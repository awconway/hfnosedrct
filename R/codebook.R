#' @title Codebook of variables
#' @param trial_datatable Dataframe of all variables collected

#' @rdname create_codebook
#' @importFrom labelled var_label
#' @importFrom tibble tibble
#' @importFrom dplyr mutate right_join arrange
#' @importFrom naniar miss_var_summary
#' @export
create_codebook <- function(trial_datatable){

na <- miss_var_summary(trial_datatable)

var_label_list <- var_label(trial_datatable)

class_list <- lapply(trial_datatable, class)

var_levels_list <- lapply(trial_datatable, levels)

tibble(variable = names(trial_datatable)) %>%
      dplyr::mutate(Label=var_label_list) %>%
  dplyr::mutate(Type = class_list) %>%
    dplyr::mutate(Levels=var_levels_list)   %>%
  dplyr::right_join(na, by="variable") %>%
  dplyr::arrange(n_miss)

}
