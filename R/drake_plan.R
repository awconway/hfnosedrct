#' @title drake plan
#' @rdname get_analysis_plan
#' @description Targets and functions for analyses
#' @export
#' 
#' 
get_analysis_plan <- function(){
  drake::drake_plan(
    original_data = readr::read_csv(here::here("data/HighFlowNasalOxygenT_DATA_2020-04-14_1453.csv")),
    
    # Label original data
    data_labelled = label_data(original_data),
    
    # Dataframe of screening data
    
    screen = data_labelled %>%
      dplyr::filter(stringr::str_detect(id, 'S')) %>% 
      dplyr::select(id, dplyr::starts_with("screen")),
    
    # Dataframe of only participants
    trial = data_labelled %>% 
      dplyr::filter(stringr::str_detect(id, "P")) %>% 
      dplyr::select(dplyr::everything(), -dplyr::starts_with("screen")),
    
    # P084 removed (procedure was not performed)
    trial_mod = trial %>%
      dplyr::filter(id != "P084"),
    
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
    
    # CONSORT diagram of all participants
    consort_png = create_consort_png(screen, trial),
    
    # Plot of frequencies of all participant exclusion reasons
    exclusions_plot = create_exclusions_plot(screen),
    
    # Dataframe of all participant characteristics
    characteristics_table = create_characteristics_table(trial_mod),
    
    # Table of all columns of interest from trial dataframe
    trial_datatable = create_trial_datatable(trial_mod),
    
    # Dataframe of all co2, spo2, and pr values at every specific date-time point
    tcco2_data = create_tcco2_data("TCCO2"),
    
    # Plot of all co2 value types observed in sequence during procedure for all 
    # participants
    co2_plot = create_co2_plot(tcco2_data, trial_mod),
    
    # Plot of all spo2 value types observed in sequence during procedure for all 
    # participants
    spo2_plot = create_spo2_plot(tcco2_data, trial_mod),
    
    ### Analysis
    
    # Set seed for BRMS models and choose 30 second resolution for functional data
    seed=42,
    reso=30,
    
    # Peak and mean co2 analysis
    data_primary = process_primary(co2_data = tcco2_data, trial_data = trial_datatable),
    model_co2_peak = fit_primary(data_primary, response="co2_peak"),
    model_co2_mean = fit_primary(data_primary, response="co2_mean"),
    plot_co2_peak = make_plot_co2_peak(data_primary, model_co2_peak),
    plot_co2_mean = make_plot_co2_mean(data_primary, model_co2_mean),
    
    # Functional ANOVA analysis
    data_fanova = process_fanova(tcco2_data, trial_datatable, reso = reso),
    model_fanova = fit_fanova(data_fanova),
    plot_fanova_data = make_plot_fanova_data(data_fanova, reso = reso),
    # fanova_effect_plot = make_fanova_effect_plot(fanova_data, fanova_model, reso),
    # fanova_stack_plot = stack_fanova_plot(fanova_data_plot, fanova_effect_plot),
    
    # Omit unless you want to do FANOVA model checking. This can take ~ 10 minutes.
    # fanova_ppred_plot = make_ppred_plot(model=model_fanova, n_sim=1000, reso=reso, seed=seed),
    
    # SPO2 analysis
    data_spo2 = process_spo2(tcco2_data, trial_datatable),
    model_spo2 = fit_spo2(data_spo2),
    
    # ISAS analysis
    data_isas = process_isas(trial_datatable),
    model_isas = fit_isas(data_isas),
    
    # Patient comfort analysis
    data_comfort = process_comfort(trial_datatable),
    model_comfort = fit_propodds(data_comfort, response="oxygencomfort_num"),
    
    # Anesthesia assistant ratings analysis
    data_assist = process_assist(trial_datatable),
    model_diffoxygen = fit_propodds(data_assist, response="diffoxygen_num"),
    model_diffuseoxygen = fit_propodds(data_assist, response="diffuseoxygen_num"),
    
    # TROOPS data analysis
    data_troops = process_troops(trial_datatable),
    model_troops = fit_troops(data_troops),
    
    # Create rmarkdown report\
    # report = rmarkdown::render(
    #   input = knitr_in("./analysis/report.Rmd"),
    #   output_file = "report.pdf",
    #   output_dir = "./analysis/",
    #   quiet = TRUE
    # ),
    # 
    # report_sweave = knitr::knit2pdf(input = knitr_in("./analysis/report_sweave.Rnw")),
    
    flexdashboard = callr::r(
      function(...) rmarkdown::render(...),
      args = list(
        input = drake::knitr_in(here::here("flexdashboard/index.Rmd")),
        output_file = "index.html")
    )
  )
}
