#' @param data dataframe from csv downloaded from redcap

#' @export
label_data <- function(data){
Hmisc::label(data$id)="Record ID"
Hmisc::label(data$redcap_event_name)="Event Name"
Hmisc::label(data$screendate)="Date of screening"
Hmisc::label(data$screenprocedure)="CIED procedure with sedation by Anesthesia Assistant?"
Hmisc::label(data$screenage)="Under 16 years of age"
Hmisc::label(data$screenchronicoxygen)="Underlying condition requiring chronic oxygen supplementation?"
Hmisc::label(data$screenhypercapnia)="Diagnosed respiratory condition with confirmed current hypercapnia (hypercapnic COPD or obesity hypoventilation syndrome with PaCO2 during current admission over 45mmHg)."
Hmisc::label(data$screenpneumothorax)="Pre-existing untreated pneumothorax"
Hmisc::label(data$screen_tee)="Transesophageal echocardiography planned for the procedure."
Hmisc::label(data$screenepistaxis)="Active nasal bleeding"
Hmisc::label(data$screennasalobstruction)="Complete nasal obstruction"
Hmisc::label(data$screenairwaysurgery)="Recent upper airway surgery or base of skull fracture"
Hmisc::label(data$screenprior)="Prior participation in the study"
Hmisc::label(data$screenpriorrefusal)="Prior refusal to participate"
Hmisc::label(data$screen_eligible)="Eligible?"
Hmisc::label(data$screeninvited)="Invitation to participate provided?"
Hmisc::label(data$screenconsented)="Provided consent?"
Hmisc::label(data$studyid)="Study ID"
Hmisc::label(data$screening_complete)="Complete?"
Hmisc::label(data$date_baseline)="Date"
Hmisc::label(data$age)="Age (years)"
Hmisc::label(data$sex)="Gender"
Hmisc::label(data$height)="Height (cm)"
Hmisc::label(data$weight)="Weight (kilograms)"
Hmisc::label(data$smoke)="Smoking Status"
Hmisc::label(data$osa)="Do you have sleep apnoea"
Hmisc::label(data$cpap)="Do you use CPAP?"
Hmisc::label(data$admward)="Admitted from"
Hmisc::label(data$admsource)="Admission status"
Hmisc::label(data$asaclass)="ASA class"
Hmisc::label(data$lastfood)="Time last food/non-clear fluids"
Hmisc::label(data$lastfluids)="Time last clear fluids"
Hmisc::label(data$procedure)="Scheduled procedure"
Hmisc::label(data$procedureother)="Describe procedure if other selected"
Hmisc::label(data$comments_v2)="Comments"
Hmisc::label(data$baseline_information_v1_complete)="Complete?"
Hmisc::label(data$cci___1)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Myocardial infarct (+1))"
Hmisc::label(data$cci___2)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Congestive heart failure (+1))"
Hmisc::label(data$cci___3)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Peripheral vascular disease (+1))"
Hmisc::label(data$cci___4)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Cerebrovascular disease (except hemiplegia) (+1))"
Hmisc::label(data$cci___5)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Dementia (+1))"
Hmisc::label(data$cci___6)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Chronic pulmonary disease (+1))"
Hmisc::label(data$cci___7)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Connective tissue disease (+1))"
Hmisc::label(data$cci___8)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Ulcer disease (+1))"
Hmisc::label(data$cci___9)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Mild liver disease (+1))"
Hmisc::label(data$cci___10)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Diabetes (without complications) (+1))"
Hmisc::label(data$cci___11)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Diabetes with end organ damage (+2))"
Hmisc::label(data$cci___12)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Hemiplegia (+2))"
Hmisc::label(data$cci___13)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Moderate or severe renal disease (+2))"
Hmisc::label(data$cci___14)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Solid tumor (non metastatic) (+2))"
Hmisc::label(data$cci___15)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Leukemia (+2))"
Hmisc::label(data$cci___16)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Lymphoma, Multiple myeloma (+2))"
Hmisc::label(data$cci___17)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Moderate or severe liver disease (+3))"
Hmisc::label(data$cci___18)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=Metastatic solid tumor (+6))"
Hmisc::label(data$cci___19)="Comorbidity (Choose all that are present)  Assigned weights for each condition the patient has ( ) (choice=AIDS (+6))"
Hmisc::label(data$cciage)="Age"
Hmisc::label(data$ccitotal)="Total points:"
Hmisc::label(data$charlson_comorbidity_index_v1_complete)="Complete?"
Hmisc::label(data$randomizationdate)="Date of randomization"
Hmisc::label(data$eligibility_confirmed)="Eligibility confirmed and consent form signed"
Hmisc::label(data$sleepapneastratify)="Diagnosis of sleep apnea"
Hmisc::label(data$crtstratify)="Is a bi-ventricular device implant (CRT-P or CRT-D) to be performed?"
Hmisc::label(data$randomization)="Randomization"
Hmisc::label(data$randomization_complete)="Complete?"
Hmisc::label(data$adverseeffectsoxygen)="RA to describe any adverse effects associated with oxygen delivery (e.g., nose bleeding, damage to mucosal surface, or pressure injury to skin from device)."
Hmisc::label(data$adverse_effects_v1_complete)="Complete?"
Hmisc::label(data$isasvomit)="I threw up or felt like throwing up"
Hmisc::label(data$isassameanesthetic)="I would want to have the same anesthetic again"
Hmisc::label(data$isasitch)="I itched"
Hmisc::label(data$isasrelaxed)="I felt relaxed"
Hmisc::label(data$isaspain)="I felt pain"
Hmisc::label(data$isassafe)="I felt safe"
Hmisc::label(data$isastoocoldhot)="I was too cold or hot"
Hmisc::label(data$isassatisfiedcare)="I was satisfied with my anesthetic care"
Hmisc::label(data$isassurgerypain)="I felt pain during surgery"
Hmisc::label(data$isasfeltgood)="I felt good"
Hmisc::label(data$isashurt)="I hurt"
Hmisc::label(data$oxygencomfort)="How comfortable did you feel with the oxygen mask or nasal prongs?"
Hmisc::label(data$participant_satisfaction_v1_complete)="Complete?"
Hmisc::label(data$aacode)="AA unique code"
Hmisc::label(data$procedurestart)="Procedure start time"
Hmisc::label(data$procedureend)="Procedure end time"
Hmisc::label(data$oxygenbaselinetime)="Time oxygen started"
Hmisc::label(data$oxygenbaselineflow)="Baseline oxygen flow (litres per minute)"
Hmisc::label(data$oxygenbaselinefio2)="Baseline Fi02"
Hmisc::label(data$oxygenbaselinetemp)="Baseline temperature setting of HFNO device"
Hmisc::label(data$oxygenchangetime1)="Change in oxygen setting Time 1"
Hmisc::label(data$oxygenchangeflow1)="Change 1 oxygen flow (L/min)"
Hmisc::label(data$oxygenchange1fio2)="Change 1 FiO2"
Hmisc::label(data$oxygenchangetemp1)="Change 1 temperature setting of HFNO device"
Hmisc::label(data$oxygenchangetime2)="Change in oxygen setting Time 2"
Hmisc::label(data$oxygenchangeflow2)="Change 2 oxygen flow (L/min)"
Hmisc::label(data$oxygenchange2fio2)="Change 2 FiO2"
Hmisc::label(data$oxygenchangetemp2)="Change 2 temperature setting of HFNO device"
Hmisc::label(data$oxygenchangetime3)="Change in oxygen setting Time 3"
Hmisc::label(data$oxygenchangeflow3)="Change 3 oxygen flow (L/min)"
Hmisc::label(data$oxygenchange3fio2)="Change 3 FiO2"
Hmisc::label(data$oxygenchangetemp3)="Change 3 temperature setting of HFNO device "
Hmisc::label(data$oxygenchangetime4)="Change in oxygen setting Time 4"
Hmisc::label(data$oxygenchangeflow4)="Change 4 oxygen flow (L/min)"
Hmisc::label(data$oxygenchange4fio2)="Change 4 FiO2"
Hmisc::label(data$oxygenchangetemp4)="Change 4 temperature setting of HFNO device"
Hmisc::label(data$troopsnoevents)="No events occured"
Hmisc::label(data$troopssenairway)="Sentinel adverse events related to airway and breathing"
Hmisc::label(data$troopsintairway)="Intermediate adverse events related to airway and breathing"
Hmisc::label(data$troopsetairway___1)="Suspected etiology for adverse events related to airway and breathing (select as many that apply) (choice=Apnea)"
Hmisc::label(data$troopsetairway___2)="Suspected etiology for adverse events related to airway and breathing (select as many that apply) (choice=Respiratory distress)"
Hmisc::label(data$troopsetairway___3)="Suspected etiology for adverse events related to airway and breathing (select as many that apply) (choice=Upper airway obstruction)"
Hmisc::label(data$troopsetairway___4)="Suspected etiology for adverse events related to airway and breathing (select as many that apply) (choice=Laryngospasm)"
Hmisc::label(data$troopsetairway___5)="Suspected etiology for adverse events related to airway and breathing (select as many that apply) (choice=Oxygen desaturation)"
Hmisc::label(data$troopsetairway___6)="Suspected etiology for adverse events related to airway and breathing (select as many that apply) (choice=Abnormal capnography)"
Hmisc::label(data$troopsminairway)="Minor adverse events related to airway and breathing "
Hmisc::label(data$troopssencirculation)="Sentinel adverse events related to circulation "
Hmisc::label(data$troopsintcirculation)="Intermediate adverse events related to circulation"
Hmisc::label(data$troopsetcirculation___1)="Suspected etiology for adverse events related to circulation(select as many that apply) (choice=Hypotension)"
Hmisc::label(data$troopsetcirculation___2)="Suspected etiology for adverse events related to circulation(select as many that apply) (choice=Hypertension)"
Hmisc::label(data$troopsetcirculation___3)="Suspected etiology for adverse events related to circulation(select as many that apply) (choice=Bradycardia)"
Hmisc::label(data$troopsetcirculation___4)="Suspected etiology for adverse events related to circulation(select as many that apply) (choice=Tachycardia)"
Hmisc::label(data$troopsetcirculation___5)="Suspected etiology for adverse events related to circulation(select as many that apply) (choice=Cardiac arrest)"
Hmisc::label(data$troopsmingi)="Minor adverse events related to GI"
Hmisc::label(data$troopsetgi___1)="Suspected etiology for adverse events related to GI (select as many that apply) (choice=Nausea)"
Hmisc::label(data$troopsetgi___2)="Suspected etiology for adverse events related to GI (select as many that apply) (choice=Vomiting)"
Hmisc::label(data$troopssenneuro)="Sentinel adverse events related to neuro"
Hmisc::label(data$troopsintneuro)="Intermediate adverse events related to neuro"
Hmisc::label(data$troopsminneuro)="Minor adverse events related to neuro"
Hmisc::label(data$troopsetneuro___1)="Suspected etiology for adverse events related to neuro (select as many that apply) (choice=Seizure or seizure-like movements)"
Hmisc::label(data$troopsetneuro___2)="Suspected etiology for adverse events related to neuro (select as many that apply) (choice=Myoclonus/ muscle rigidity)"
Hmisc::label(data$troopsintallergy)="Intermediate adverse events related to allergy"
Hmisc::label(data$troopsminallergy)="Minor adverse events related to allergy"
Hmisc::label(data$troopsetallergy___1)="Suspected etiology of adverse events related to allergy (select as many that apply) (choice=Allergic reaction)"
Hmisc::label(data$troopsetallergy___2)="Suspected etiology of adverse events related to allergy (select as many that apply) (choice=Anaphylaxis)"
Hmisc::label(data$troopsintquality)="Intermediate adverse events related to sedation quality and patient experience"
Hmisc::label(data$troopsetquality___1)="Suspected etiology for adverse events related to sedation quality and patient experience (select as many that apply) (choice=Patient active resistance or need for restraint)"
Hmisc::label(data$troopsetquality___2)="Suspected etiology for adverse events related to sedation quality and patient experience (select as many that apply) (choice=Sedation)"
Hmisc::label(data$troopsetquality___3)="Suspected etiology for adverse events related to sedation quality and patient experience (select as many that apply) (choice=Paradoxical response)"
Hmisc::label(data$troopsetquality___4)="Suspected etiology for adverse events related to sedation quality and patient experience (select as many that apply) (choice=Unpleasant recovery reaction/agitation)"
Hmisc::label(data$troopsetquality___5)="Suspected etiology for adverse events related to sedation quality and patient experience (select as many that apply) (choice=Unpleasant recall)"
Hmisc::label(data$deepsedation)="Did the anesthesioloist attend the procedure to administer deep sedation?"
Hmisc::label(data$ga)="Did an anesthesiologist attend the procedure for airway management/GA?"
Hmisc::label(data$propofol)="Total dose of propofol (mg)"
Hmisc::label(data$midazolam)="Total dose of midazolam (mg)"
Hmisc::label(data$fentanyl)="Total dose of fentanyl (mcg)"
Hmisc::label(data$remifentanil)="Remifentanil (mcg)"
Hmisc::label(data$otheropioidname)="Other opioid name"
Hmisc::label(data$otheropioiddose)="Other opioid total dose"
Hmisc::label(data$otheropioidunits)="Other opioid unit of measurement"
Hmisc::label(data$diffuseoxygen)="How difficult was it to use the supplemental oxygen delivery device?"
Hmisc::label(data$diffoxygen)="How difficult was it to maintain the patients oxygenation status throughout the procedure?"
Hmisc::label(data$timesusedhfno)="How many times have you used high flow nasal oxygen?"
Hmisc::label(data$anesthesia_assistant_ratings_complete)="Complete?"
Hmisc::label(data$tcco2peak)="PeakTcCO2 concentration"
Hmisc::label(data$tcco2mean)="Mean TcCO2 concentration"
Hmisc::label(data$aucdesat)="AUCdesat"
Hmisc::label(data$tcco2_and_spo2_outcomes_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("baseline_arm_1","screening_arm_2"))
data$screenprocedure.factor = factor(data$screenprocedure,levels=c("1","2"))
data$screenage.factor = factor(data$screenage,levels=c("1","2"))
data$screenchronicoxygen.factor = factor(data$screenchronicoxygen,levels=c("1","2"))
data$screenhypercapnia.factor = factor(data$screenhypercapnia,levels=c("1","2"))
data$screenpneumothorax.factor = factor(data$screenpneumothorax,levels=c("1","2"))
data$screen_tee.factor = factor(data$screen_tee,levels=c("1","2"))
data$screenepistaxis.factor = factor(data$screenepistaxis,levels=c("1","2"))
data$screennasalobstruction.factor = factor(data$screennasalobstruction,levels=c("1","2"))
data$screenairwaysurgery.factor = factor(data$screenairwaysurgery,levels=c("1","2"))
data$screenprior.factor = factor(data$screenprior,levels=c("1","2"))
data$screenpriorrefusal.factor = factor(data$screenpriorrefusal,levels=c("1","2"))
data$screen_eligible.factor = factor(data$screen_eligible,levels=c("1","2"))
data$screeninvited.factor = factor(data$screeninvited,levels=c("1","2"))
data$screenconsented.factor = factor(data$screenconsented,levels=c("1","2"))
data$screening_complete.factor = factor(data$screening_complete,levels=c("0","1","2"))
data$sex.factor = factor(data$sex,levels=c("0","1","3","4"))
data$smoke.factor = factor(data$smoke,levels=c("1","2","3"))
data$osa.factor = factor(data$osa,levels=c("1","2"))
data$cpap.factor = factor(data$cpap,levels=c("1","2"))
data$admward.factor = factor(data$admward,levels=c("1","2","3","4"))
data$admsource.factor = factor(data$admsource,levels=c("1","2"))
data$asaclass.factor = factor(data$asaclass,levels=c("1","2","3","4"))
data$procedure.factor = factor(data$procedure,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$baseline_information_v1_complete.factor = factor(data$baseline_information_v1_complete,levels=c("0","1","2"))
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
data$charlson_comorbidity_index_v1_complete.factor = factor(data$charlson_comorbidity_index_v1_complete,levels=c("0","1","2"))
data$eligibility_confirmed.factor = factor(data$eligibility_confirmed,levels=c("1","0"))
data$sleepapneastratify.factor = factor(data$sleepapneastratify,levels=c("1","2"))
data$crtstratify.factor = factor(data$crtstratify,levels=c("1","2"))
data$randomization.factor = factor(data$randomization,levels=c("1","2"))
data$randomization_complete.factor = factor(data$randomization_complete,levels=c("0","1","2"))
data$adverse_effects_v1_complete.factor = factor(data$adverse_effects_v1_complete,levels=c("0","1","2"))
data$isasvomit.factor = factor(data$isasvomit,levels=c("3","2","1","-1","-2","-3"))
data$isassameanesthetic.factor = factor(data$isassameanesthetic,levels=c("-3","-2","-1","1","2","3"))
data$isasitch.factor = factor(data$isasitch,levels=c("3","2","1","-1","-2","-3"))
data$isasrelaxed.factor = factor(data$isasrelaxed,levels=c("-3","-2","-1","1","2","3"))
data$isaspain.factor = factor(data$isaspain,levels=c("3","2","1","-1","-2","-3"))
data$isassafe.factor = factor(data$isassafe,levels=c("-3","-2","-1","1","2","3"))
data$isastoocoldhot.factor = factor(data$isastoocoldhot,levels=c("3","2","1","-1","-2","-3"))
data$isassatisfiedcare.factor = factor(data$isassatisfiedcare,levels=c("-3","-2","-1","1","2","3"))
data$isassurgerypain.factor = factor(data$isassurgerypain,levels=c("3","2","1","-1","-2","-3"))
data$isasfeltgood.factor = factor(data$isasfeltgood,levels=c("-3","-2","-1","1","2","3"))
data$isashurt.factor = factor(data$isashurt,levels=c("3","2","1","-1","-2","-3"))
data$oxygencomfort.factor = factor(data$oxygencomfort,levels=c("1","2","3","4","5","6"))
data$participant_satisfaction_v1_complete.factor = factor(data$participant_satisfaction_v1_complete,levels=c("0","1","2"))
data$troopsnoevents.factor = factor(data$troopsnoevents,levels=c("1"))
data$troopssenairway.factor = factor(data$troopssenairway,levels=c("1","2","3","4"))
data$troopsintairway.factor = factor(data$troopsintairway,levels=c("1","2","3","4"))
data$troopsetairway___1.factor = factor(data$troopsetairway___1,levels=c("0","1"))
data$troopsetairway___2.factor = factor(data$troopsetairway___2,levels=c("0","1"))
data$troopsetairway___3.factor = factor(data$troopsetairway___3,levels=c("0","1"))
data$troopsetairway___4.factor = factor(data$troopsetairway___4,levels=c("0","1"))
data$troopsetairway___5.factor = factor(data$troopsetairway___5,levels=c("0","1"))
data$troopsetairway___6.factor = factor(data$troopsetairway___6,levels=c("0","1"))
data$troopsminairway.factor = factor(data$troopsminairway,levels=c("1","2","3","4","5","6","7"))
data$troopssencirculation.factor = factor(data$troopssencirculation,levels=c("1","2","3","4"))
data$troopsintcirculation.factor = factor(data$troopsintcirculation,levels=c("1","2"))
data$troopsetcirculation___1.factor = factor(data$troopsetcirculation___1,levels=c("0","1"))
data$troopsetcirculation___2.factor = factor(data$troopsetcirculation___2,levels=c("0","1"))
data$troopsetcirculation___3.factor = factor(data$troopsetcirculation___3,levels=c("0","1"))
data$troopsetcirculation___4.factor = factor(data$troopsetcirculation___4,levels=c("0","1"))
data$troopsetcirculation___5.factor = factor(data$troopsetcirculation___5,levels=c("0","1"))
data$troopsmingi.factor = factor(data$troopsmingi,levels=c("1","2","3"))
data$troopsetgi___1.factor = factor(data$troopsetgi___1,levels=c("0","1"))
data$troopsetgi___2.factor = factor(data$troopsetgi___2,levels=c("0","1"))
data$troopssenneuro.factor = factor(data$troopssenneuro,levels=c("1","2"))
data$troopsintneuro.factor = factor(data$troopsintneuro,levels=c("1","2"))
data$troopsminneuro.factor = factor(data$troopsminneuro,levels=c("1","2"))
data$troopsetneuro___1.factor = factor(data$troopsetneuro___1,levels=c("0","1"))
data$troopsetneuro___2.factor = factor(data$troopsetneuro___2,levels=c("0","1"))
data$troopsintallergy.factor = factor(data$troopsintallergy,levels=c("1","2","3"))
data$troopsminallergy.factor = factor(data$troopsminallergy,levels=c("1","2"))
data$troopsetallergy___1.factor = factor(data$troopsetallergy___1,levels=c("0","1"))
data$troopsetallergy___2.factor = factor(data$troopsetallergy___2,levels=c("0","1"))
data$troopsintquality.factor = factor(data$troopsintquality,levels=c("1","2","3","4","5"))
data$troopsetquality___1.factor = factor(data$troopsetquality___1,levels=c("0","1"))
data$troopsetquality___2.factor = factor(data$troopsetquality___2,levels=c("0","1"))
data$troopsetquality___3.factor = factor(data$troopsetquality___3,levels=c("0","1"))
data$troopsetquality___4.factor = factor(data$troopsetquality___4,levels=c("0","1"))
data$troopsetquality___5.factor = factor(data$troopsetquality___5,levels=c("0","1"))
data$deepsedation.factor = factor(data$deepsedation,levels=c("0","1"))
data$ga.factor = factor(data$ga,levels=c("0","1"))
data$otheropioidunits.factor = factor(data$otheropioidunits,levels=c("1","2"))
data$diffuseoxygen.factor = factor(data$diffuseoxygen,levels=c("1","2","3","4","5","6"))
data$diffoxygen.factor = factor(data$diffoxygen,levels=c("1","2","3","4","5","6"))
data$timesusedhfno.factor = factor(data$timesusedhfno,levels=c("1","2","3","4"))
data$anesthesia_assistant_ratings_complete.factor = factor(data$anesthesia_assistant_ratings_complete,levels=c("0","1","2"))
data$tcco2_and_spo2_outcomes_complete.factor = factor(data$tcco2_and_spo2_outcomes_complete,levels=c("0","1","2"))

levels(data$redcap_event_name.factor)=c("Baseline (Arm 1: Participation)","Screening (Arm 2: Screening)")
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
levels(data$screening_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$sex.factor)=c("Female","Male","Prefer not to say","Other")
levels(data$smoke.factor)=c("Never","Current","Past")
levels(data$osa.factor)=c("No","Yes")
levels(data$cpap.factor)=c("No","Yes")
levels(data$admward.factor)=c("Ward","Day surgery","CVICU","CICU")
levels(data$admsource.factor)=c("Emergency","Elective")
levels(data$asaclass.factor)=c("I","II","III","IV")
levels(data$procedure.factor)=c("PPM","PPM generator change","PPM lead revision","ICD","ICD generator change","ICD lead revision","CRT-D","CRT-P","Wound revision","Other")
levels(data$baseline_information_v1_complete.factor)=c("Incomplete","Unverified","Complete")
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
levels(data$cciage.factor)=c("50 - 59 (+1)","60 - 69 (+2)","70 - 79 (+3)","80 - 89 (+4)","90 - 99 (+5)")
levels(data$charlson_comorbidity_index_v1_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$eligibility_confirmed.factor)=c("Yes","No")
levels(data$sleepapneastratify.factor)=c("Yes","No")
levels(data$crtstratify.factor)=c("Yes","No")
levels(data$randomization.factor)=c("High Flow nasal oxygen","Face mask oxygen")
levels(data$randomization_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$adverse_effects_v1_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$isasvomit.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isassameanesthetic.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isasitch.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isasrelaxed.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isaspain.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isassafe.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isastoocoldhot.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isassatisfiedcare.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isassurgerypain.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isasfeltgood.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$isashurt.factor)=c("Disagree very much","Disagree moderately","Disagree slightly","Agree slightly","Agree moderately","Agree very much")
levels(data$oxygencomfort.factor)=c("Maximal discomfort","Very uncomfortable","Uncomfortable","Comfortable","Very comfortable","Maximal comfort")
levels(data$participant_satisfaction_v1_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$troopsnoevents.factor)=c("No events occured")
levels(data$troopssenairway.factor)=c("Tracheal intubation","Neuromuscular blockade","Pulmonary aspiration","None of the above")
levels(data$troopsintairway.factor)=c("Positive pressure ventilation","Naloxone or flumazenil","Oral airway","None of the above")
levels(data$troopsetairway___1.factor)=c("Unchecked","Checked")
levels(data$troopsetairway___2.factor)=c("Unchecked","Checked")
levels(data$troopsetairway___3.factor)=c("Unchecked","Checked")
levels(data$troopsetairway___4.factor)=c("Unchecked","Checked")
levels(data$troopsetairway___5.factor)=c("Unchecked","Checked")
levels(data$troopsetairway___6.factor)=c("Unchecked","Checked")
levels(data$troopsminairway.factor)=c("Increased or added supplemental oxygen","Airway repositioning","Tactile stimulation","Suctioning for hypersalivation","Anticholinergic for hypersalivation","Nasal airway","None of the above")
levels(data$troopssencirculation.factor)=c("Vasoactive drug administration","Chest compressions","Death","None of the above")
levels(data$troopsintcirculation.factor)=c("Bolus IV fluids","None of the above")
levels(data$troopsetcirculation___1.factor)=c("Unchecked","Checked")
levels(data$troopsetcirculation___2.factor)=c("Unchecked","Checked")
levels(data$troopsetcirculation___3.factor)=c("Unchecked","Checked")
levels(data$troopsetcirculation___4.factor)=c("Unchecked","Checked")
levels(data$troopsetcirculation___5.factor)=c("Unchecked","Checked")
levels(data$troopsmingi.factor)=c("Anti-emetic for nausea/vomiting","Suctioning for emesis","None of the above")
levels(data$troopsetgi___1.factor)=c("Unchecked","Checked")
levels(data$troopsetgi___2.factor)=c("Unchecked","Checked")
levels(data$troopssenneuro.factor)=c("Neurological deficit","None of the above")
levels(data$troopsintneuro.factor)=c("Anticonvulsant administration","None of the above")
levels(data$troopsminneuro.factor)=c("Additional sedative for myoclonus/ rigidity","None of the above")
levels(data$troopsetneuro___1.factor)=c("Unchecked","Checked")
levels(data$troopsetneuro___2.factor)=c("Unchecked","Checked")
levels(data$troopsintallergy.factor)=c("Administration of inhaled β-agonist","Administration of epinephrine (adrenaline) for anaphylaxis","None of the above")
levels(data$troopsminallergy.factor)=c("Administration of antihistamine","None of the above")
levels(data$troopsetallergy___1.factor)=c("Unchecked","Checked")
levels(data$troopsetallergy___2.factor)=c("Unchecked","Checked")
levels(data$troopsintquality.factor)=c("Sedation insufficient","Escalation of care or hospitalization","Provider dissatisfied","Patient/family dissatisfied","None of the above")
levels(data$troopsetquality___1.factor)=c("Unchecked","Checked")
levels(data$troopsetquality___2.factor)=c("Unchecked","Checked")
levels(data$troopsetquality___3.factor)=c("Unchecked","Checked")
levels(data$troopsetquality___4.factor)=c("Unchecked","Checked")
levels(data$troopsetquality___5.factor)=c("Unchecked","Checked")
levels(data$deepsedation.factor)=c("No","Yes")
levels(data$ga.factor)=c("no","yes")
levels(data$otheropioidunits.factor)=c("mg","mcg")
levels(data$diffuseoxygen.factor)=c("Extremely difficult","Very difficult","Difficult","Easy","Very easy","Extremely easy")
levels(data$diffoxygen.factor)=c("Extremely difficult","Very difficult","Difficult","Easy","Very easy","Extremely easy")
levels(data$timesusedhfno.factor)=c("Once","Between 2 and five times","Between 5 and 10 times","More than 10 times")
levels(data$anesthesia_assistant_ratings_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$tcco2_and_spo2_outcomes_complete.factor)=c("Incomplete","Unverified","Complete")
data
}
