#' @title Medication summary

#' @param trial_mod dataframe of all variables collected
#'
#' @export

#' @importFrom dplyr select
#' @importFrom gtsummary tbl_summary all_continuous italicize_labels
#' as_flextable add_p
create_med_summary <- function(trial_mod) {

  # Dataframe of all participant characteristics
  trial_mod %>%
    select(randomization.factor,
           midazolam,
           propofol,
           fentanyl
    ) %>%
    tbl_summary(by = randomization.factor,
                # change statistics printed in table
                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                type = list(midazolam ~ "continuous",
                            fentanyl ~ "continuous"),
                missing = "no"
    ) %>%
    add_p(list(all_continuous() ~ "t.test")) %>%
    as_flextable()

}
