devtools::load_all()

drake::make(get_analysis_plan(), lock_envir = FALSE)

x drake::vis_drake_graph(get_analysis_plan())

