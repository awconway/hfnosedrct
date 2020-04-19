library(hfnosedrct)

drake::make(get_analysis_plan(), lock_envir = FALSE)

drake::vis_drake_graph(get_analysis_plan())

