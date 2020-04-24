library(diffr)
file1 = "R/plots.R"
file2 = "R/plots_compare.R"
diffr(file1, file2, before = "f1", after = "f2")
