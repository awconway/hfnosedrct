#' @title Load data from tcco2 monitor
#' @rdname create_tcco2_data
#' @param data Filepath to directory of csv files

#' @export
#' @importFrom dplyr mutate select everything
#' @importFrom fs dir_ls
#' @importFrom glue glue
#' @importFrom stringr str_remove_all str_remove
#' @importFrom vroom vroom
create_tcco2_data <- function(TCCO2) {

  # Dataframe of all co2, spo2, and pr values at every specific date-time
  # point for each participant
  files <- dir_ls(glue("data/", TCCO2), glob = "*csv")
  files
  vroom(files, id = "path") %>%
    mutate(id = str_remove_all(path, glue("data/", TCCO2, "/"))) %>%
    mutate(id = str_remove(id, ".csv")) %>%
    select(id, everything(), -path)

}
