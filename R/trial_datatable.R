#' @title Selection of all variables used in the analysis
#' @param trial_mod Dataframe of all variables collected
#' @rdname create_trial_datatable
#' 
#' @export
#' @importFrom dplyr select ends_with
create_trial_datatable <- function(trial_mod) {
  
  # Dataframe of all columns of interest from trial dataframe
  trial_table <- trial_mod %>%
    select(id, ends_with(".factor"), -ends_with("_complete.factor"),
           date_baseline, age, height, weight, lastfood, lastfluids,
           randomizationdate, adverseeffectsoxygen, aacode, procedurestart,
           procedureend, oxygenbaselinetime, oxygenbaselineflow,
           oxygenbaselinefio2, oxygenbaselinetemp, oxygenchangetime1,
           oxygenchangeflow1, oxygenchange1fio2, oxygenchangetemp1,
           oxygenchangetime2, oxygenchangeflow2, oxygenchange2fio2,
           oxygenchangetemp2, oxygenchangetime3, oxygenchangeflow3,
           oxygenchange3fio2, oxygenchangetemp3, oxygenchangetime4,
           oxygenchangeflow4, oxygenchange4fio2, oxygenchangetemp4, propofol,
           midazolam, fentanyl, remifentanil, otheropioidname, otheropioiddose,
           tcco2peak, tcco2mean)
  
}
