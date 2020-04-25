
# ***************************
#' @title Convert Time to a datetime object
#' @rdname time_to_date
#' @description Date format is mixed, so we have to treat each format separately
#' @param data Used for co2_data
#' @export
# ***************************
#' @importFrom lubridate dmy_hms ymd_hms
time_to_date <- function(data){

  time_dmy <- dmy_hms(data$Time, tz = "UTC", quiet = TRUE)
  time_ymd <- ymd_hms(data$Time, tz = "UTC", quiet = TRUE)


  time_result <- time_dmy
  time_result[is.na(time_result)] <- time_ymd[!is.na(time_ymd)]

  return(time_result)

}
