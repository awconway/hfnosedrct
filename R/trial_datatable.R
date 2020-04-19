#' @title Selection of all variables used in the analysis
#' @param trial_mod Dataframe of all variables collected
#' @param data_labelled Dataframe with labels of variables

#' @rdname create_trial_datatable
#'
#' @export
#' @importFrom dplyr select ends_with rename_at vars
#' @importFrom labelled copy_labels_from
#' @importFrom janitor clean_names


create_trial_datatable <- function(trial_mod, data_labelled) {

  # Dataframe of all columns of interest from trial dataframe
  trial_datatable <- trial_mod %>%
    select(id, ends_with(".factor"), -ends_with("_complete.factor"),
           -starts_with("redcap"),
           age, height, weight, lastfood, lastfluids,
           procedurestart, procedureother, ccitotal,
           procedureend, oxygenbaselinetime, oxygenbaselineflow,
           oxygenbaselinefio2, oxygenbaselinetemp, oxygenchangetime1,
           oxygenchangeflow1, oxygenchange1fio2, oxygenchangetemp1,
           oxygenchangetime2, oxygenchangeflow2, oxygenchange2fio2,
           oxygenchangetemp2, oxygenchangetime3, oxygenchangeflow3,
           oxygenchange3fio2, oxygenchangetemp3, oxygenchangetime4,
           oxygenchangeflow4, oxygenchange4fio2, oxygenchangetemp4, propofol,
           midazolam, fentanyl, remifentanil, otheropioidname, otheropioiddose) %>%
           copy_labels_from(data_labelled) %>%
           clean_names() %>%
    rename_at(.vars = vars(ends_with("_factor")),
              .funs = list(~sub("[_]factor$", "", .)))

trial_datatable

}
