#' @title Table of participant characteristics

#' @param trial_mod dataframe of all variables collected
#'
#' @export

#' @importFrom dplyr select
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
           ccitotal,
           midazolam,
           propofol,
           fentanyl
    ) %>%
    mutate(procedure.factor = case_when(
      procedure.factor=="PPM" ~ "Permanent pacemaker insertion",
      procedure.factor=="PPM generator change" ~ "Permanent pacemaker generator change"
    )) %>%
    tbl_summary(by = randomization.factor,
                label = list(sex.factor ~ "Gender",
                             smoke.factor ~ "Smoking history",
                             osa.factor ~ "Obstructive sleep apnea",
                             cpap.factor ~ "Uses CPAP",
                             admward.factor ~ "Admission source",
                             asaclass.factor ~ "ASA classification status",
                             procedure.factor ~ "Procedure",
                             ccitotal ~ "Charlson Comorbidity Index",
                             bmi ~ "Body mass index"
                ),
                # change statistics printed in table
                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                type = list(midazolam ~ "continuous",
                            fentanyl ~ "continuous"),
                missing = "no"
    ) %>%
    italicize_labels() %>%
    as_flextable() %>%
    footnote(i =2, j = 1,
             value = as_paragraph(
               "ASA = American Society of Anesthesiology"
             ),
             ref_symbols = "",
             part = "body") %>%
    footnote(i =2, j = 1,
             value = as_paragraph(
               "CPAP = Continuous Positive Airway Pressure therapy for sleep apnea"
             ),
             ref_symbols = "",
             part = "body") %>%
    footnote(i =2, j = 1,
             value = as_paragraph(
               "CVICU = Cardiovascular Intensive Care Unit"
             ),
             ref_symbols = "",
             part = "body") %>%
    footnote(i =2, j = 1,
             value = as_paragraph(
               "PPM = American Society of Anesthesiology"
             ),
             ref_symbols = "",
             part = "body")

}
