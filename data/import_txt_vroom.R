library(vroom)
library(tidyverse)
# 1. open .xls
# 2. remove header rows and row below column names
# 3. remove 1PCO2 if 7PCO2_DC is present and rename as co2
# 4. remove status columns
# 5. rename to sp02 and pr

create_co2_data <- function(TCCO2) {
  
  files <- fs::dir_ls(glue("data/", TCCO2), glob = "*csv")
  files
  data <- vroom::vroom(files, id = "path") %>%
    mutate(id = stringr::str_remove_all(path, glue("data/", TCCO2, "/"))) %>%
    mutate(id = stringr::str_remove(id, ".csv")) %>%
    select(id, everything(), -path)

}
