#' @title Subgroup for table
#' @rdname make_subgroup
#' @export
#'
make_subgroup <- function(model_obj, unit=NULL){

  result <- list(base = extract_CI_subgroup(model_obj, unit,
                                            pars="randomization_factorHighFlownasaloxygen"),
                 crt = extract_CI_subgroup(model_obj, unit,
                                           pars="crt_treat_factorTRUE"),
                 osa = extract_CI_subgroup(model_obj, unit,
                                           pars="osa_treat_factorTRUE"))

  return(result)
}
