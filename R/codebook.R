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

class_list <- var_class(trial_datatable)

var_levels_list <- var_levels(trial_datatable)

tibble(variable = names(trial_datatable)) %>%
      dplyr::mutate(Label=var_label_list) %>%
  dplyr::mutate(Type = class_list) %>%
    dplyr::mutate(Levels=var_levels_list)   %>%
  dplyr::right_join(na, by="variable") %>%
  dplyr::arrange(n_miss)

}

#' @export
var_class <- function(x, unlist = FALSE) {
  UseMethod("var_class")
}
#' @export
var_class.default <- function(x, unlist = FALSE) {
  class(x)
}
#' @export
var_class.data.frame <- function(x, unlist = FALSE) {
  r <- lapply(x, var_class)
  if (unlist) {
    r <- lapply(r, function(x){if (is.null(x)) "" else x})
    base::unlist(r, use.names = TRUE)
  } else
    r
}
#' @export
var_levels <- function(x, unlist = FALSE) {
  UseMethod("var_levels")
}
#' @export
var_levels.default <- function(x, unlist = FALSE) {
  levels(x)
}
#' @export
var_levels.data.frame <- function(x, unlist = FALSE) {
  r <- lapply(x, var_levels)
  if (unlist) {
    r <- lapply(r, function(x){if (is.null(x)) "" else x})
    base::unlist(r, use.names = TRUE)
  } else
    r
}

