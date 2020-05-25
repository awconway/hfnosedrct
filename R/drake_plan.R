#' @title drake plan
#' @rdname get_analysis_plan
#' @description Targets and functions for analyses
#' @export
#' @importFrom readr col_double cols read_csv
#' @importFrom here here
#' @importFrom dplyr filter select starts_with everything
#' @importFrom stringr str_detect
#' @importFrom drake drake_plan knitr_in
#'
get_analysis_plan <- function(){
 drake_plan(
    original_data = read_csv(
      here("data/HighFlowNasalOxygenT_DATA_2020-05-21_1511.csv")),

    # Label original data
    data_labelled = label_data(original_data),

    # Dataframe of screening data

    screen = data_labelled %>%
      filter(str_detect(id, 'S')) %>%
      select(id, starts_with("screen")),

    # Dataframe of only participants
    trial = data_labelled %>%
      filter(str_detect(id, "P")) %>%
      select(everything(), -starts_with("screen")),

    # P084 removed (procedure was not performed)
    trial_mod = trial %>%
      filter(id != "P084"),

    # Dataframe of all columns needed from trial dataframe (participant id, all
    # oxygen flow rates used, all oxygen fio2 values used, and assigned
    # randomization of each participant), with all time intervals between
    # changes in oxygen flow rate/oxygen fio2 value
    oxygen = create_oxygen(trial_mod),

    # Dataframe of all oxygen flow rate values at every minute, with assigned
    # randomization of each participant
    oxygen_table = create_oxygen_table(oxygen),

    # Merge all oxygen flow rate values into one column named "flowrate"
    oxygen_table_long = create_oxygen_table_long(oxygen, oxygen_table),

    # Plot of all oxygen flow rate values used in sequence during procedure for
    # all participants for each randomization
    oxygen_flow_plot = create_oxygen_flow_plot(oxygen_table_long),

    # Dataframe of all columns needed from oxygen dataframe (participant id, all
    # oxygen fio2 values used, all time intervals between changes in oxygen fio2
    # value, and assigned randomization of each high flow nasal oxygen
    # participant)
    fio2 = create_fio2(oxygen),

    # Dataframe of all oxygen fio2 values at every minute for each high flow
    # nasal oxygen participant
    fio2_table = create_fio2_table(fio2),

    # Merge all oxygen fio2 values into one column named "fio2"
    fio2_table_long = create_fio2_table_long(fio2, fio2_table),

    # Plot of all oxygen fio2 values used in sequence during procedure for all
    # high flow nasal oxygen participants
    oxygen_fio2_plot = create_oxygen_fio2_plot(fio2_table_long),

    # Plot of proportion of time spent during procedure using different oxygen
    # flow rates for each randomization
    oxygen_proportion_plot = create_oxygen_proportion_plot(oxygen_table_long),

    # Plots for ordinal outcomes

    diffoxygen_plot = create_ordinal_outcome_plot(trial_mod, diffoxygen.factor),

    diffuseoxygen_plot = create_ordinal_outcome_plot(trial_mod, diffuseoxygen.factor),

    oxygencomfort_plot = create_ordinal_outcome_plot(trial_mod, oxygencomfort.factor),

    timesusedhfno_plot = create_timesusedhfno_plot(trial_mod),



    # CONSORT diagram of all participants
    consort_png = create_consort_png(screen, trial),

    # Plot of frequencies of all participant exclusion reasons
    exclusions_plot = create_exclusions_plot(screen),

    # Dataframe of all participant characteristics
    characteristics_table = create_characteristics_table(trial_mod),

    # Table of all columns of interest from trial dataframe
    trial_datatable = create_trial_datatable(trial_mod, data_labelled),

    # Dataframe of all co2, spo2, and pr values at every specific date-time point
    tcco2_data = create_tcco2_data("TCCO2"),

    # Plot of all co2 value types observed in sequence during procedure for all
    # participants
    co2_plot = create_co2_plot(co2_long),

    # SpO2 data from DREAM
    spo2_dream = read_csv("data/CATH_SpO2.csv", na = "-",
                                  col_types = cols(spo2 = col_double(),
                                                   index = col_double())),

    # Combine spo2 data with trial data

    spo2_trial = make_spo2_long(spo2_dream, trial_mod),

    # Plot of all spo2 value types observed in sequence during procedure for all
    # participants
    spo2_plot = create_spo2_plot(spo2_trial),

    # Facet plot highlighting patients who had desat event

    spo2_facet_plot = create_spo2_facet_plot(spo2_trial),

    ### Analysis

    # Set seed for BRMS models and choose 30 second resolution for functional data
    seed=42,
    reso=30,

    # Peak and mean co2 analysis

    co2_long = make_co2_long(tcco2_data, trial_mod),
    data_primary = process_primary(co2_long),
    model_co2_peak = fit_primary(data_primary, response="co2_peak"),
    model_co2_mean = fit_primary(data_primary, response="co2_mean"),
    plot_co2_peak = make_plot_co2_peak(data_primary, model_co2_peak),
    plot_co2_mean = make_plot_co2_mean(data_primary, model_co2_mean),

    # Functional ANOVA analysis
    data_fanova = process_fanova(co2_long, reso = reso),
    model_fanova = fit_fanova(data_fanova),
    plot_fanova_data = make_plot_fanova_data(data_fanova),
    plot_fanova_effect = make_plot_component(data_fanova,
                                             model_fanova,
                                             reso,
                                             component_str = "alpha",
                                             title = "Treatment effect",
                                             ylim=c(-40,40)),
    # Omit unless you want to do FANOVA model checking. This can take ~ 10 minutes.
    # fanova_ppred_plot = make_ppred_plot(model=model_fanova, n_sim=1000, reso=reso, seed=seed),

    # SPO2 analysis (we do not model, so fit_model is just filtering valid data)
    data_spo2 = process_spo2(spo2_trial),
    model_spo2 = fit_spo2(data_spo2),

    # ISAS analysis
    data_isas = process_isas(trial_mod),
    model_isas = fit_isas(data_isas),
    isas_plot = create_isas_plot(data_isas),


    # Patient comfort analysis
    data_comfort = process_comfort(trial_mod),
    model_comfort = fit_propodds(data_comfort, response="oxygencomfort_num"),

    # Anesthesia assistant ratings analysis
    data_assist = process_assist(trial_mod),
    model_diffoxygen = fit_propodds(data_assist, response="diffoxygen_num"),
    model_diffuseoxygen = fit_propodds(data_assist, response="diffuseoxygen_num"),

    # TROOPS data analysis
    data_troops = process_troops(trial_mod),
    model_troops = fit_troops(data_troops),


    ## Sensitivity analysis for Anesthesia Assistant data
    model_diffoxygen_best = fit_bestworst(data_assist,
                                          response="diffoxygen_num", method="best"),
    model_diffoxygen_worst = fit_bestworst(data_assist,
                                           response="diffoxygen_num", method="worst"),
    model_diffuseoxygen_best = fit_bestworst(data_assist,
                                             response="diffuseoxygen_num", method="best"),
    model_diffuseoxygen_worst = fit_bestworst(data_assist,
                                              response="diffuseoxygen_num", method="worst"),

    # Subgroup analysis for peak tcco2
    model_co2_peak_interact = fit_effect_modification(data_primary, response="co2_peak"),

    # Make tables and summaries
    data_list = list(
      data_primary = data_primary,
      data_spo2 =  data_spo2,
      data_isas = data_isas,
      data_assist = data_assist,
      data_comfort = data_comfort,
      data_troops = data_troops
    ),
    model_list = list(
      model_co2_peak = model_co2_peak,
      model_co2_mean = model_co2_mean,
      model_spo2 =  model_spo2,
      model_isas = model_isas,
      model_diffoxygen = model_diffoxygen,
      model_diffoxygen_best = model_diffoxygen_best,
      model_diffoxygen_worst = model_diffoxygen_worst,
      model_diffuseoxygen = model_diffuseoxygen,
      model_diffuseoxygen_best = model_diffuseoxygen_best,
      model_diffuseoxygen_worst = model_diffuseoxygen_worst,
      model_comfort = model_comfort,
      model_troops = model_troops
    ),

    table_outcome = make_table_outcomes_formatted(model_list, data_list),
    table_subgroup = make_table_subgroup(model_obj=model_co2_peak_interact),
    flextable_outcome = make_flextable_outcomes_formatted(model_list, data_list),

    # Codebook

    codebook = create_codebook(trial_datatable),

   # Compile rmarkdown manuscript
    manuscript = callr::r(
      function(...) rmarkdown::render(...),
      args= list(
      input = knitr_in(here("manuscript/index.Rmd")),
      output_file = "index.html",
      quiet = TRUE
    )
    ),

    flexdashboard = callr::r(
      function(...) rmarkdown::render(...),
      args = list(
        input = knitr_in(here("flexdashboard/index.Rmd")),
        output_file = "index.html",
      quiet = TRUE)
    )
  )
}
