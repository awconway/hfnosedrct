#' @title Label original data
#' @param data dataframe from csv downloaded from redcap


#' @export
#'
label_data <- function(data){

  #Setting Factors(will create new variable for factors)
  data$redcap_event_name.factor =
    factor(data$redcap_event_name,levels=c("baseline_arm_1","screening_arm_2"))
  data$screenprocedure.factor = factor(data$screenprocedure,levels=c("1","2"))
  data$screenage.factor = factor(data$screenage,levels=c("1","2"))
  data$screenchronicoxygen.factor =
    factor(data$screenchronicoxygen,levels=c("1","2"))
  data$screenhypercapnia.factor =
    factor(data$screenhypercapnia,levels=c("1","2"))
  data$screenpneumothorax.factor =
    factor(data$screenpneumothorax,levels=c("1","2"))
  data$screen_tee.factor = factor(data$screen_tee,levels=c("1","2"))
  data$screenepistaxis.factor = factor(data$screenepistaxis,levels=c("1","2"))
  data$screennasalobstruction.factor =
    factor(data$screennasalobstruction,levels=c("1","2"))
  data$screenairwaysurgery.factor =
    factor(data$screenairwaysurgery,levels=c("1","2"))
  data$screenprior.factor = factor(data$screenprior,levels=c("1","2"))
  data$screenpriorrefusal.factor =
    factor(data$screenpriorrefusal,levels=c("1","2"))
  data$screen_eligible.factor = factor(data$screen_eligible,levels=c("1","2"))
  data$screeninvited.factor = factor(data$screeninvited,levels=c("1","2"))
  data$screenconsented.factor = factor(data$screenconsented,levels=c("1","2"))
  data$screening_complete.factor =
    factor(data$screening_complete,levels=c("0","1","2"))
  data$sex.factor = factor(data$sex,levels=c("0","1","3","4"))
  data$smoke.factor = factor(data$smoke,levels=c("1","2","3"))
  data$osa.factor = factor(data$osa,levels=c("1","2"))
  data$cpap.factor = factor(data$cpap,levels=c("1","2"))
  data$admward.factor = factor(data$admward,levels=c("1","2","3","4"))
  data$admsource.factor = factor(data$admsource,levels=c("1","2"))
  data$asaclass.factor = factor(data$asaclass,levels=c("1","2","3","4"))
  data$procedure.factor =
    factor(data$procedure,levels=c("1","2","3","4","5","6","7","8","9","10"))
  data$baseline_information_v1_complete.factor =
    factor(data$baseline_information_v1_complete,levels=c("0","1","2"))
  data$cci___1.factor = factor(data$cci___1,levels=c("0","1"))
  data$cci___2.factor = factor(data$cci___2,levels=c("0","1"))
  data$cci___3.factor = factor(data$cci___3,levels=c("0","1"))
  data$cci___4.factor = factor(data$cci___4,levels=c("0","1"))
  data$cci___5.factor = factor(data$cci___5,levels=c("0","1"))
  data$cci___6.factor = factor(data$cci___6,levels=c("0","1"))
  data$cci___7.factor = factor(data$cci___7,levels=c("0","1"))
  data$cci___8.factor = factor(data$cci___8,levels=c("0","1"))
  data$cci___9.factor = factor(data$cci___9,levels=c("0","1"))
  data$cci___10.factor = factor(data$cci___10,levels=c("0","1"))
  data$cci___11.factor = factor(data$cci___11,levels=c("0","1"))
  data$cci___12.factor = factor(data$cci___12,levels=c("0","1"))
  data$cci___13.factor = factor(data$cci___13,levels=c("0","1"))
  data$cci___14.factor = factor(data$cci___14,levels=c("0","1"))
  data$cci___15.factor = factor(data$cci___15,levels=c("0","1"))
  data$cci___16.factor = factor(data$cci___16,levels=c("0","1"))
  data$cci___17.factor = factor(data$cci___17,levels=c("0","1"))
  data$cci___18.factor = factor(data$cci___18,levels=c("0","1"))
  data$cci___19.factor = factor(data$cci___19,levels=c("0","1"))
  data$cciage.factor = factor(data$cciage,levels=c("1","2","3","4","5"))
  data$charlson_comorbidity_index_v1_complete.factor =
    factor(data$charlson_comorbidity_index_v1_complete,levels=c("0","1","2"))
  data$eligibility_confirmed.factor =
    factor(data$eligibility_confirmed,levels=c("1","0"))
  data$sleepapneastratify.factor =
    factor(data$sleepapneastratify,levels=c("1","2"))
  data$crtstratify.factor = factor(data$crtstratify,levels=c("1","2"))
  data$randomization.factor = factor(data$randomization,levels=c("1","2"))
  data$randomization_complete.factor =
    factor(data$randomization_complete,levels=c("0","1","2"))
  data$adverse_effects_v1_complete.factor =
    factor(data$adverse_effects_v1_complete,levels=c("0","1","2"))
  data$isasvomit.factor =
    factor(data$isasvomit,levels=c("3","2","1","-1","-2","-3"))
  data$isassameanesthetic.factor =
    factor(data$isassameanesthetic,levels=c("-3","-2","-1","1","2","3"))
  data$isasitch.factor =
    factor(data$isasitch,levels=c("3","2","1","-1","-2","-3"))
  data$isasrelaxed.factor =
    factor(data$isasrelaxed,levels=c("-3","-2","-1","1","2","3"))
  data$isaspain.factor =
    factor(data$isaspain,levels=c("3","2","1","-1","-2","-3"))
  data$isassafe.factor =
    factor(data$isassafe,levels=c("-3","-2","-1","1","2","3"))
  data$isastoocoldhot.factor =
    factor(data$isastoocoldhot,levels=c("3","2","1","-1","-2","-3"))
  data$isassatisfiedcare.factor =
    factor(data$isassatisfiedcare,levels=c("-3","-2","-1","1","2","3"))
  data$isassurgerypain.factor =
    factor(data$isassurgerypain,levels=c("3","2","1","-1","-2","-3"))
  data$isasfeltgood.factor =
    factor(data$isasfeltgood,levels=c("-3","-2","-1","1","2","3"))
  data$isashurt.factor =
    factor(data$isashurt,levels=c("3","2","1","-1","-2","-3"))
  data$oxygencomfort.factor =
    factor(data$oxygencomfort,levels=c("1","2","3","4","5","6"))
  data$participant_satisfaction_v1_complete.factor =
    factor(data$participant_satisfaction_v1_complete,levels=c("0","1","2"))
  data$troopsnoevents.factor = factor(data$troopsnoevents,levels=c("1"))
  data$troopssenairway.factor =
    factor(data$troopssenairway,levels=c("1","2","3","4"))
  data$troopsintairway.factor =
    factor(data$troopsintairway,levels=c("1","2","3","4"))
  data$troopsetairway___1.factor =
    factor(data$troopsetairway___1,levels=c("0","1"))
  data$troopsetairway___2.factor =
    factor(data$troopsetairway___2,levels=c("0","1"))
  data$troopsetairway___3.factor =
    factor(data$troopsetairway___3,levels=c("0","1"))
  data$troopsetairway___4.factor =
    factor(data$troopsetairway___4,levels=c("0","1"))
  data$troopsetairway___5.factor =
    factor(data$troopsetairway___5,levels=c("0","1"))
  data$troopsetairway___6.factor =
    factor(data$troopsetairway___6,levels=c("0","1"))
  data$troopsminairway.factor =
    factor(data$troopsminairway,levels=c("1","2","3","4","5","6","7"))
  data$troopssencirculation.factor =
    factor(data$troopssencirculation,levels=c("1","2","3","4"))
  data$troopsintcirculation.factor =
    factor(data$troopsintcirculation,levels=c("1","2"))
  data$troopsetcirculation___1.factor =
    factor(data$troopsetcirculation___1,levels=c("0","1"))
  data$troopsetcirculation___2.factor =
    factor(data$troopsetcirculation___2,levels=c("0","1"))
  data$troopsetcirculation___3.factor =
    factor(data$troopsetcirculation___3,levels=c("0","1"))
  data$troopsetcirculation___4.factor =
    factor(data$troopsetcirculation___4,levels=c("0","1"))
  data$troopsetcirculation___5.factor =
    factor(data$troopsetcirculation___5,levels=c("0","1"))
  data$troopsmingi.factor =
    factor(data$troopsmingi,levels=c("1","2","3"))
  data$troopsetgi___1.factor =
    factor(data$troopsetgi___1,levels=c("0","1"))
  data$troopsetgi___2.factor =
    factor(data$troopsetgi___2,levels=c("0","1"))
  data$troopssenneuro.factor =
    factor(data$troopssenneuro,levels=c("1","2"))
  data$troopsintneuro.factor =
    factor(data$troopsintneuro,levels=c("1","2"))
  data$troopsminneuro.factor =
    factor(data$troopsminneuro,levels=c("1","2"))
  data$troopsetneuro___1.factor =
    factor(data$troopsetneuro___1,levels=c("0","1"))
  data$troopsetneuro___2.factor =
    factor(data$troopsetneuro___2,levels=c("0","1"))
  data$troopsintallergy.factor =
    factor(data$troopsintallergy,levels=c("1","2","3"))
  data$troopsminallergy.factor =
    factor(data$troopsminallergy,levels=c("1","2"))
  data$troopsetallergy___1.factor =
    factor(data$troopsetallergy___1,levels=c("0","1"))
  data$troopsetallergy___2.factor =
    factor(data$troopsetallergy___2,levels=c("0","1"))
  data$troopsintquality.factor =
    factor(data$troopsintquality,levels=c("1","2","3","4","5"))
  data$troopsetquality___1.factor =
    factor(data$troopsetquality___1,levels=c("0","1"))
  data$troopsetquality___2.factor =
    factor(data$troopsetquality___2,levels=c("0","1"))
  data$troopsetquality___3.factor =
    factor(data$troopsetquality___3,levels=c("0","1"))
  data$troopsetquality___4.factor =
    factor(data$troopsetquality___4,levels=c("0","1"))
  data$troopsetquality___5.factor =
    factor(data$troopsetquality___5,levels=c("0","1"))
  data$deepsedation.factor = factor(data$deepsedation,levels=c("0","1"))
  data$ga.factor = factor(data$ga,levels=c("0","1"))
  data$otheropioidunits.factor =
    factor(data$otheropioidunits,levels=c("1","2"))
  data$diffuseoxygen.factor =
    factor(data$diffuseoxygen,levels=c("1","2","3","4","5","6"))
  data$diffoxygen.factor =
    factor(data$diffoxygen,levels=c("1","2","3","4","5","6"))
  data$timesusedhfno.factor =
    factor(data$timesusedhfno,levels=c("1","2","3","4"))
  data$anesthesia_assistant_ratings_complete.factor =
    factor(data$anesthesia_assistant_ratings_complete,levels=c("0","1","2"))
  data$tcco2_and_spo2_outcomes_complete.factor =
    factor(data$tcco2_and_spo2_outcomes_complete,levels=c("0","1","2"))

  levels(data$redcap_event_name.factor)=
    c("Baseline (Arm 1: Participation)","Screening (Arm 2: Screening)")
  levels(data$screenprocedure.factor)=c("Yes","No")
  levels(data$screenage.factor)=c("Yes","No")
  levels(data$screenchronicoxygen.factor)=c("Yes","No")
  levels(data$screenhypercapnia.factor)=c("Yes","No")
  levels(data$screenpneumothorax.factor)=c("Yes","No")
  levels(data$screen_tee.factor)=c("Yes","No")
  levels(data$screenepistaxis.factor)=c("Yes","No")
  levels(data$screennasalobstruction.factor)=c("Yes","No")
  levels(data$screenairwaysurgery.factor)=c("Yes","No")
  levels(data$screenprior.factor)=c("Yes","No")
  levels(data$screenpriorrefusal.factor)=c("Yes","No")
  levels(data$screen_eligible.factor)=c("Yes","No")
  levels(data$screeninvited.factor)=c("Yes","No")
  levels(data$screenconsented.factor)=c("Yes","No")
  levels(data$screening_complete.factor)=
    c("Incomplete","Unverified","Complete")
  levels(data$sex.factor)=c("Female","Male","Prefer not to say","Other")
  levels(data$smoke.factor)=c("Never","Current","Past")
  levels(data$osa.factor)=c("No","Yes")
  levels(data$cpap.factor)=c("No","Yes")
  levels(data$admward.factor)=c("Ward","Day surgery","CVICU","CICU")
  levels(data$admsource.factor)=c("Emergency","Elective")
  levels(data$asaclass.factor)=c("I","II","III","IV")
  levels(data$procedure.factor)=
    c("PPM","PPM generator change","PPM lead revision","ICD",
      "ICD generator change","ICD lead revision","CRT-D","CRT-P",
      "Wound revision","Other")
  levels(data$baseline_information_v1_complete.factor)=
    c("Incomplete","Unverified","Complete")
  levels(data$cci___1.factor)=c("Unchecked","Checked")
  levels(data$cci___2.factor)=c("Unchecked","Checked")
  levels(data$cci___3.factor)=c("Unchecked","Checked")
  levels(data$cci___4.factor)=c("Unchecked","Checked")
  levels(data$cci___5.factor)=c("Unchecked","Checked")
  levels(data$cci___6.factor)=c("Unchecked","Checked")
  levels(data$cci___7.factor)=c("Unchecked","Checked")
  levels(data$cci___8.factor)=c("Unchecked","Checked")
  levels(data$cci___9.factor)=c("Unchecked","Checked")
  levels(data$cci___10.factor)=c("Unchecked","Checked")
  levels(data$cci___11.factor)=c("Unchecked","Checked")
  levels(data$cci___12.factor)=c("Unchecked","Checked")
  levels(data$cci___13.factor)=c("Unchecked","Checked")
  levels(data$cci___14.factor)=c("Unchecked","Checked")
  levels(data$cci___15.factor)=c("Unchecked","Checked")
  levels(data$cci___16.factor)=c("Unchecked","Checked")
  levels(data$cci___17.factor)=c("Unchecked","Checked")
  levels(data$cci___18.factor)=c("Unchecked","Checked")
  levels(data$cci___19.factor)=c("Unchecked","Checked")
  levels(data$cciage.factor)=
    c("50 - 59 (+1)","60 - 69 (+2)","70 - 79 (+3)","80 - 89 (+4)",
      "90 - 99 (+5)")
  levels(data$charlson_comorbidity_index_v1_complete.factor)=
    c("Incomplete","Unverified","Complete")
  levels(data$eligibility_confirmed.factor)=c("Yes","No")
  levels(data$sleepapneastratify.factor)=c("Yes","No")
  levels(data$crtstratify.factor)=c("Yes","No")
  levels(data$randomization.factor)=
    c("High Flow nasal oxygen","Face mask oxygen")
  levels(data$randomization_complete.factor)=
    c("Incomplete","Unverified","Complete")
  levels(data$adverse_effects_v1_complete.factor)=
    c("Incomplete","Unverified","Complete")
  levels(data$isasvomit.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isassameanesthetic.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isasitch.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isasrelaxed.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isaspain.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isassafe.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isastoocoldhot.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isassatisfiedcare.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isassurgerypain.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isasfeltgood.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$isashurt.factor)=
    c("Disagree very much","Disagree moderately","Disagree slightly",
      "Agree slightly","Agree moderately","Agree very much")
  levels(data$oxygencomfort.factor)=
    c("Maximal discomfort","Very uncomfortable","Uncomfortable","Comfortable",
      "Very comfortable","Maximal comfort")
  levels(data$participant_satisfaction_v1_complete.factor)=
    c("Incomplete","Unverified","Complete")
  levels(data$troopsnoevents.factor)=c("No events occured")
  levels(data$troopssenairway.factor)=
    c("Tracheal intubation","Neuromuscular blockade","Pulmonary aspiration",
      "None of the above")
  levels(data$troopsintairway.factor)=
    c("Positive pressure ventilation","Naloxone or flumazenil","Oral airway",
      "None of the above")
  levels(data$troopsetairway___1.factor)=c("Unchecked","Checked")
  levels(data$troopsetairway___2.factor)=c("Unchecked","Checked")
  levels(data$troopsetairway___3.factor)=c("Unchecked","Checked")
  levels(data$troopsetairway___4.factor)=c("Unchecked","Checked")
  levels(data$troopsetairway___5.factor)=c("Unchecked","Checked")
  levels(data$troopsetairway___6.factor)=c("Unchecked","Checked")
  levels(data$troopsminairway.factor)=
    c("Increased or added supplemental oxygen","Airway repositioning",
      "Tactile stimulation","Suctioning for hypersalivation",
      "Anticholinergic for hypersalivation","Nasal airway","None of the above")
  levels(data$troopssencirculation.factor)=
    c("Vasoactive drug administration","Chest compressions","Death",
      "None of the above")
  levels(data$troopsintcirculation.factor)=
    c("Bolus IV fluids","None of the above")
  levels(data$troopsetcirculation___1.factor)=c("Unchecked","Checked")
  levels(data$troopsetcirculation___2.factor)=c("Unchecked","Checked")
  levels(data$troopsetcirculation___3.factor)=c("Unchecked","Checked")
  levels(data$troopsetcirculation___4.factor)=c("Unchecked","Checked")
  levels(data$troopsetcirculation___5.factor)=c("Unchecked","Checked")
  levels(data$troopsmingi.factor)=
    c("Anti-emetic for nausea/vomiting","Suctioning for emesis",
      "None of the above")
  levels(data$troopsetgi___1.factor)=c("Unchecked","Checked")
  levels(data$troopsetgi___2.factor)=c("Unchecked","Checked")
  levels(data$troopssenneuro.factor)=
    c("Neurological deficit","None of the above")
  levels(data$troopsintneuro.factor)=
    c("Anticonvulsant administration","None of the above")
  levels(data$troopsminneuro.factor)=
    c("Additional sedative for myoclonus/ rigidity","None of the above")
  levels(data$troopsetneuro___1.factor)=c("Unchecked","Checked")
  levels(data$troopsetneuro___2.factor)=c("Unchecked","Checked")
  levels(data$troopsintallergy.factor)=
    c("Administration of inhaled β-agonist",
      "Administration of epinephrine (adrenaline) for anaphylaxis",
      "None of the above")
  levels(data$troopsminallergy.factor)=
    c("Administration of antihistamine","None of the above")
  levels(data$troopsetallergy___1.factor)=c("Unchecked","Checked")
  levels(data$troopsetallergy___2.factor)=c("Unchecked","Checked")
  levels(data$troopsintquality.factor)=
    c("Sedation insufficient","Escalation of care or hospitalization",
      "Provider dissatisfied","Patient/family dissatisfied",
      "None of the above")
  levels(data$troopsetquality___1.factor)=c("Unchecked","Checked")
  levels(data$troopsetquality___2.factor)=c("Unchecked","Checked")
  levels(data$troopsetquality___3.factor)=c("Unchecked","Checked")
  levels(data$troopsetquality___4.factor)=c("Unchecked","Checked")
  levels(data$troopsetquality___5.factor)=c("Unchecked","Checked")
  levels(data$deepsedation.factor)=c("No","Yes")
  levels(data$ga.factor)=c("no","yes")
  levels(data$otheropioidunits.factor)=c("mg","mcg")
  levels(data$diffuseoxygen.factor)=
    c("Extremely difficult","Very difficult","Difficult","Easy","Very easy",
      "Extremely easy")
  levels(data$diffoxygen.factor)=
    c("Extremely difficult","Very difficult","Difficult","Easy","Very easy",
      "Extremely easy")
  levels(data$timesusedhfno.factor)=
    c("Once","Between 2 and five times","Between 5 and 10 times",
      "More than 10 times")
  levels(data$anesthesia_assistant_ratings_complete.factor)=
    c("Incomplete","Unverified","Complete")
  levels(data$tcco2_and_spo2_outcomes_complete.factor)=
    c("Incomplete","Unverified","Complete")


  attr(data$id, "label")="Record ID"
  attr(data$redcap_event_name.factor, "label")="Event Name"
  attr(data$screendate, "label")="Date of screening"
  attr(data$screenprocedure, "label")=
    "CIED procedure with sedation by Anesthesia Assistant?"
  attr(data$screenage, "label")="Under 16 years of age"
  attr(data$screenchronicoxygen, "label")=
    "Underlying condition requiring chronic oxygen supplementation?"
  attr(data$screenhypercapnia, "label")=
    "Diagnosed respiratory condition with confirmed current hypercapnia
    (hypercapnic COPD or obesity hypoventilation syndrome with PaCO2 during
  current admission over 45mmHg)."
  attr(data$screenpneumothorax, "label")="Pre-existing untreated pneumothorax"
  attr(data$screen_tee, "label")=
    "Transesophageal echocardiography planned for the procedure."
  attr(data$screenepistaxis, "label")="Active nasal bleeding"
  attr(data$screennasalobstruction, "label")="Complete nasal obstruction"
  attr(data$screenairwaysurgery, "label")=
    "Recent upper airway surgery or base of skull fracture"
  attr(data$screenprior, "label")="Prior participation in the study"
  attr(data$screenpriorrefusal, "label")="Prior refusal to participate"
  attr(data$screen_eligible, "label")="Eligible?"
  attr(data$screeninvited, "label")="Invitation to participate provided?"
  attr(data$screenconsented, "label")="Provided consent?"
  attr(data$studyid, "label")="Study ID"
  attr(data$screening_complete, "label")="Complete?"
  attr(data$date_baseline, "label")="Date"
  attr(data$age, "label")="Age (years)"
  attr(data$sex.factor, "label")="Gender"
  attr(data$height, "label")="Height (cm)"
  attr(data$weight, "label")="Weight (kilograms)"
  attr(data$smoke.factor, "label")="Smoking Status"
  attr(data$osa.factor, "label")="Do you have sleep apnoea"
  attr(data$cpap.factor, "label")="Do you use CPAP?"
  attr(data$admward.factor, "label")="Admitted from"
  attr(data$admsource.factor, "label")="Admission status"
  attr(data$asaclass.factor, "label")="ASA class"
  attr(data$lastfood, "label")="Time last food/non-clear fluids"
  attr(data$lastfluids, "label")="Time last clear fluids"
  attr(data$procedure.factor, "label")="Scheduled procedure"
  attr(data$procedureother, "label")="Describe procedure if other selected"
  attr(data$comments_v2, "label")="Comments"
  attr(data$baseline_information_v1_complete, "label")="Complete?"
  attr(data$cci___1.factor, "label")=
    "Myocardial infarct (+1)"
  attr(data$cci___2.factor, "label")=
    "choice=Congestive heart failure (+1)"
  attr(data$cci___3.factor, "label")=
    "Peripheral vascular disease (+1)"
  attr(data$cci___4.factor, "label")=
    "Cerebrovascular disease (except hemiplegia) (+1)"
  attr(data$cci___5.factor, "label")=
    "Dementia (+1)"
  attr(data$cci___6.factor, "label")=
    "Chronic pulmonary disease (+1)"
  attr(data$cci___7.factor, "label")=
    "Connective tissue disease (+1)"
  attr(data$cci___8.factor, "label")=
    "Ulcer disease (+1)"
  attr(data$cci___9.factor, "label")=
    "Mild liver disease (+1)"
  attr(data$cci___10.factor, "label")=
    "Diabetes (without complications) (+1)"
  attr(data$cci___11.factor, "label")=
    "Diabetes with end organ damage (+2)"
  attr(data$cci___12.factor, "label")=
    "Hemiplegia (+2)"
  attr(data$cci___13.factor, "label")=
    "Moderate or severe renal disease (+2)"
  attr(data$cci___14.factor, "label")=
    "Solid tumor (non metastatic) (+2)"
  attr(data$cci___15.factor, "label")=
    "Leukemia (+2)"
  attr(data$cci___16.factor, "label")=
    "Lymphoma, Multiple myeloma (+2)"
  attr(data$cci___17.factor, "label")=
    "Moderate or severe liver disease (+3)"
  attr(data$cci___18.factor, "label")=
    "Metastatic solid tumor (+6)"
  attr(data$cci___19.factor, "label")=
    "AIDS (+6)"
  attr(data$cciage, "label")="Age score for CCI"
  attr(data$ccitotal, "label")="Charlson Comorbidity Index Score"
  attr(data$charlson_comorbidity_index_v1_complete, "label")="Complete?"
  attr(data$randomizationdate, "label")="Date of randomization"
  attr(data$eligibility_confirmed.factor, "label")=
    "Eligibility confirmed and consent form signed"
  attr(data$sleepapneastratify.factor, "label")="Diagnosis of sleep apnea"
  attr(data$crtstratify.factor, "label")=
    "Is a bi-ventricular device implant (CRT-P or CRT-D) to be performed?"
  attr(data$randomization.factor, "label")="Randomization"
  attr(data$randomization_complete.factor, "label")="Complete?"
  attr(data$adverseeffectsoxygen, "label")=
    "RA to describe any adverse effects associated with oxygen delivery (e.g.,
    nose bleeding, damage to mucosal surface, or pressure injury to skin from
  device)."
  attr(data$adverse_effects_v1_complete.factor, "label")="Complete?"
  attr(data$isasvomit.factor, "label")="I threw up or felt like throwing up"
  attr(data$isassameanesthetic.factor, "label")=
    "I would want to have the same anesthetic again"
  attr(data$isasitch.factor, "label")="I itched"
  attr(data$isasrelaxed.factor, "label")="I felt relaxed"
  attr(data$isaspain.factor, "label")="I felt pain"
  attr(data$isassafe.factor, "label")="I felt safe"
  attr(data$isastoocoldhot.factor, "label")="I was too cold or hot"
  attr(data$isassatisfiedcare.factor, "label")=
    "I was satisfied with my anesthetic care"
  attr(data$isassurgerypain.factor, "label")="I felt pain during surgery"
  attr(data$isasfeltgood.factor, "label")="I felt good"
  attr(data$isashurt.factor, "label")="I hurt"
  attr(data$oxygencomfort.factor, "label")=
    "How comfortable did you feel with the oxygen mask or nasal prongs?"
  attr(data$participant_satisfaction_v1_complete.factor, "label")="Complete?"
  attr(data$aacode, "label")="AA unique code"
  attr(data$procedurestart, "label")="Procedure start time"
  attr(data$procedureend, "label")="Procedure end time"
  attr(data$oxygenbaselinetime, "label")="Time oxygen started"
  attr(data$oxygenbaselineflow, "label")=
    "Baseline oxygen flow (litres per minute)"
  attr(data$oxygenbaselinefio2, "label")="Baseline Fi02"
  attr(data$oxygenbaselinetemp, "label")=
    "Baseline temperature setting of HFNO device"
  attr(data$oxygenchangetime1, "label")="Change in oxygen setting Time 1"
  attr(data$oxygenchangeflow1, "label")="Change 1 oxygen flow (L/min)"
  attr(data$oxygenchange1fio2, "label")="Change 1 FiO2"
  attr(data$oxygenchangetemp1, "label")=
    "Change 1 temperature setting of HFNO device"
  attr(data$oxygenchangetime2, "label")="Change in oxygen setting Time 2"
  attr(data$oxygenchangeflow2, "label")="Change 2 oxygen flow (L/min)"
  attr(data$oxygenchange2fio2, "label")="Change 2 FiO2"
  attr(data$oxygenchangetemp2, "label")=
    "Change 2 temperature setting of HFNO device"
  attr(data$oxygenchangetime3, "label")="Change in oxygen setting Time 3"
  attr(data$oxygenchangeflow3, "label")="Change 3 oxygen flow (L/min)"
  attr(data$oxygenchange3fio2, "label")="Change 3 FiO2"
  attr(data$oxygenchangetemp3, "label")=
    "Change 3 temperature setting of HFNO device "
  attr(data$oxygenchangetime4, "label")="Change in oxygen setting Time 4"
  attr(data$oxygenchangeflow4, "label")="Change 4 oxygen flow (L/min)"
  attr(data$oxygenchange4fio2, "label")="Change 4 FiO2"
  attr(data$oxygenchangetemp4, "label")=
    "Change 4 temperature setting of HFNO device"
  attr(data$troopsnoevents.factor, "label")="No events occured"
  attr(data$troopssenairway.factor, "label")=
    "Sentinel adverse events related to airway and breathing"
  attr(data$troopsintairway.factor, "label")=
    "Intermediate adverse events related to airway and breathing"
  attr(data$troopsetairway___1.factor, "label")=
    "Suspected etiology for adverse events related to airway and breathing
  (select as many that apply) (choice=Apnea)"
  attr(data$troopsetairway___2.factor, "label")=
    "Suspected etiology for adverse events related to airway and breathing
  (select as many that apply) (choice=Respiratory distress)"
  attr(data$troopsetairway___3.factor, "label")=
    "Suspected etiology for adverse events related to airway and breathing
  (select as many that apply) (choice=Upper airway obstruction)"
  attr(data$troopsetairway___4.factor, "label")=
    "Suspected etiology for adverse events related to airway and breathing
  (select as many that apply) (choice=Laryngospasm)"
  attr(data$troopsetairway___5.factor, "label")=
    "Suspected etiology for adverse events related to airway and breathing
  (select as many that apply) (choice=Oxygen desaturation)"
  attr(data$troopsetairway___6.factor, "label")=
    "Suspected etiology for adverse events related to airway and breathing
  (select as many that apply) (choice=Abnormal capnography)"
  attr(data$troopsminairway.factor, "label")=
    "Minor adverse events related to airway and breathing "
  attr(data$troopssencirculation.factor, "label")=
    "Sentinel adverse events related to circulation "
  attr(data$troopsintcirculation.factor, "label")=
    "Intermediate adverse events related to circulation"
  attr(data$troopsetcirculation___1.factor, "label")=
    "Suspected etiology for adverse events related to circulation(select as
  many that apply) (choice=Hypotension)"
  attr(data$troopsetcirculation___2.factor, "label")=
    "Suspected etiology for adverse events related to circulation(select as
  many that apply) (choice=Hypertension)"
  attr(data$troopsetcirculation___3.factor, "label")=
    "Suspected etiology for adverse events related to circulation(select as
  many that apply) (choice=Bradycardia)"
  attr(data$troopsetcirculation___4.factor, "label")=
    "Suspected etiology for adverse events related to circulation(select as
  many that apply) (choice=Tachycardia)"
  attr(data$troopsetcirculation___5.factor, "label")=
    "Suspected etiology for adverse events related to circulation(select as
  many that apply) (choice=Cardiac arrest)"
  attr(data$troopsmingi.factor, "label")="Minor adverse events related to GI"
  attr(data$troopsetgi___1.factor, "label")=
    "Suspected etiology for adverse events related to GI (select as many that
  apply) (choice=Nausea)"
  attr(data$troopsetgi___2.factor, "label")=
    "Suspected etiology for adverse events related to GI (select as many that
  apply) (choice=Vomiting)"
  attr(data$troopssenneuro.factor, "label")="Sentinel adverse events related to neuro"
  attr(data$troopsintneuro.factor, "label")=
    "Intermediate adverse events related to neuro"
  attr(data$troopsminneuro.factor, "label")="Minor adverse events related to neuro"
  attr(data$troopsetneuro___1.factor, "label")=
    "Suspected etiology for adverse events related to neuro (select as many
  that apply) (choice=Seizure or seizure-like movements)"
  attr(data$troopsetneuro___2.factor, "label")=
    "Suspected etiology for adverse events related to neuro (select as many
  that apply) (choice=Myoclonus/ muscle rigidity)"
  attr(data$troopsintallergy.factor, "label")=
    "Intermediate adverse events related to allergy"
  attr(data$troopsminallergy.factor, "label")="Minor adverse events related to allergy"
  attr(data$troopsetallergy___1.factor, "label")=
    "Suspected etiology of adverse events related to allergy (select as many
  that apply) (choice=Allergic reaction)"
  attr(data$troopsetallergy___2.factor, "label")=
    "Suspected etiology of adverse events related to allergy (select as many
  that apply) (choice=Anaphylaxis)"
  attr(data$troopsintquality, "label")=
    "Intermediate adverse events related to sedation quality and patient
  experience"
  attr(data$troopsetquality___1.factor, "label")=
    "Suspected etiology for adverse events related to sedation quality and
    patient experience (select as many that apply) (choice=Patient active
  resistance or need for restraint)"
  attr(data$troopsetquality___2.factor, "label")=
    "Suspected etiology for adverse events related to sedation quality and
  patient experience (select as many that apply) (choice=Sedation)"
  attr(data$troopsetquality___3.factor, "label")=
    "Suspected etiology for adverse events related to sedation quality and
  patient experience (select as many that apply) (choice=Paradoxical response)"
  attr(data$troopsetquality___4.factor, "label")=
    "Suspected etiology for adverse events related to sedation quality and
    patient experience (select as many that apply) (choice=Unpleasant recovery
  reaction/agitation)"
  attr(data$troopsetquality___5.factor, "label")=
    "Suspected etiology for adverse events related to sedation quality and
  patient experience (select as many that apply) (choice=Unpleasant recall)"
  attr(data$deepsedation.factor, "label")=
    "Did the anesthesioloist attend the procedure to administer deep sedation?"
  attr(data$ga.factor, "label")=
    "Did an anesthesiologist attend the procedure for airway management/GA?"
  attr(data$propofol, "label")="Total dose of propofol (mg)"
  attr(data$midazolam, "label")="Total dose of midazolam (mg)"
  attr(data$fentanyl, "label")="Total dose of fentanyl (mcg)"
  attr(data$remifentanil, "label")="Remifentanil (mcg)"
  attr(data$otheropioidname, "label")="Other opioid name"
  attr(data$otheropioiddose, "label")="Other opioid total dose"
  attr(data$otheropioidunits.factor, "label")="Other opioid unit of measurement"
  attr(data$diffuseoxygen.factor, "label")=
    "How difficult was it to use the supplemental oxygen delivery device?"
  attr(data$diffoxygen.factor, "label")=
    "How difficult was it to maintain the patients oxygenation status
  throughout the procedure?"
  attr(data$timesusedhfno.factor, "label")=
    "How many times have you used high flow nasal oxygen?"
  attr(data$anesthesia_assistant_ratings_complete.factor, "label")="Complete?"
  attr(data$tcco2peak, "label")="PeakTcCO2 concentration"
  attr(data$tcco2mean, "label")="Mean TcCO2 concentration"
  attr(data$aucdesat, "label")="AUCdesat"
  attr(data$tcco2_and_spo2_outcomes_complete.factor, "label")="Complete?"

  data
}
