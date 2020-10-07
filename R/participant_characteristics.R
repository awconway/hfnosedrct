#' @title Table of participant characteristics

#' @param trial_mod dataframe of all variables collected
#'
#' @export

#' @importFrom dplyr select
#' @importFrom forcats fct_drop
#' @importFrom gtsummary tbl_summary all_continuous italicize_labels
#' as_flextable
create_characteristics_table <- function(trial_mod) {

  # Dataframe of all participant characteristics
  trial_mod %>%
    mutate(bmi = weight/(height/100)^2) %>%
    select(randomization.factor,
           age, sex.factor,
           smoke.factor,
           osa.factor,
           cpap.factor,
           admward.factor,
           asaclass.factor,
           bmi,
           procedure.factor,
           ccitotal
    ) %>%
    mutate(procedure.factor = fct_drop(procedure.factor),
           admward.factor = fct_drop(admward.factor),
           sex.factor = fct_drop(sex.factor)) %>%
    mutate(procedure.factor = case_when(
      procedure.factor=="PPM" ~ "Permanent pacemaker insertion",
      procedure.factor=="PPM generator change" ~ "Permanent pacemaker generator change",
      procedure.factor=="PPM lead revision" ~ "Permanent pacemaker lead revision",
      procedure.factor=="ICD" ~ "Implantable cardioverter defibrillator",
      procedure.factor=="ICD lead revision" ~ "Implantable cardioverter defibrillator lead revision",
      procedure.factor=="ICD generator change" ~ "Implantable cardioverter defibrillator generator change",
      procedure.factor=="CRT-D" ~ "Cardiac resynchronisation therapy with defibrillator",
      procedure.factor=="CRT-P" ~ "Cardiac resynchronisation therapy with pacing",
      TRUE ~ "Other"
    )) %>%
    mutate(admward.factor = case_when(
      admward.factor=="CVICU"~ "Cardiovascular Intensive Care Unit",
      TRUE ~ as.character(admward.factor)
    )) %>%
    tbl_summary(by = randomization.factor,
                label = list(sex.factor ~ "Gender",
                             smoke.factor ~ "Smoking history",
                             osa.factor ~ "Obstructive sleep apnea",
                             cpap.factor ~ "Uses Continuous Positive Airway Pressure therapy for sleep apnea",
                             admward.factor ~ "Admission source",
                             asaclass.factor ~ "American Society of Anesthesiology classification status",
                             procedure.factor ~ "Procedure",
                             ccitotal ~ "Charlson Comorbidity Index",
                             bmi ~ "Body mass index"
                ),
                # change statistics printed in table
                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                missing = "no",
                sort = list(procedure.factor ~ "frequency",
                            admward.factor ~ "frequency")
    ) %>%
    italicize_labels() %>%
    as_flextable()

}
