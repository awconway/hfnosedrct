#' @title Table of participant characteristics

#' @param trial_mod dataframe of all variables collected
#' 
#' @export

#' @importFrom dplyr select
#' @importFrom gtsummary tbl_summary all_continuous italicize_labels
create_characteristics_table <- function(trial_mod) {
  
  # Dataframe of all participant characteristics
  trial_mod %>% 
    select(randomization.factor,
           age, sex.factor,
           smoke.factor,
           osa.factor,
           cpap.factor,
           admward.factor,
           asaclass.factor,
           procedure.factor,
           ccitotal,
           midazolam,
           propofol,
           fentanyl
    ) %>% 
    tbl_summary(by = randomization.factor,
                label = list(sex.factor ~ "Gender",
                             smoke.factor ~ "Smoking history",
                             osa.factor ~ "Obstructive sleep apnea",
                             cpap.factor ~ "Uses CPAP",
                             admward.factor ~ "Admission source",
                             asaclass.factor ~ "ASA classification status",
                             procedure.factor ~ "Procedure",
                             ccitotal ~ "Charlson Comorbidity Index"
                ),
                # change statistics printed in table
                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                type = list(midazolam ~ "continuous",
                            fentanyl ~ "continuous"),
                missing = "no"
    ) %>%
    italicize_labels()
  
}