#' Prepare data for pre-processing in PupillometrySENDA
#'
#' This should be the first function you run as part of using PupillometrySENDA.
#' This will make sure your data is in the right format for processing.
#' This package is designed to deal with data at it comes out of the eyetracker
#' in a long-form csv style format. Thus data input here would be a long
#' dataframe, wherein each row is a single frame collected by the eyetracker.
#'
#' @param data a raw, long form dataframe organised by subject, trial, and time.
#'    if your data is not long form, look at tidyr for examples of conversion.
#' @param subject column name indicating subject ID
#' @param trial column name indicating trial ID. This should be unique for participants
#' @param time column name indicating time column (should be numeric)
#' @param condition column name indicating experimental condition
#'
#' @examples
#' Sdata <- make_pupillometrys_data(data = pupil_data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#'
#' @export
#'
#' @return A dataframe ready to use in PupillometrySENDA


make_pupillometrys_data <- function(data, subject, trial, time, condition, other){

  class(data) <- c('PupillometrySENDA', class(data))

  attr(data, 'PupillometrySENDA') <- list(Subject = subject,
                                      Trial = trial,
                                      Time = time,
                                      Condition = condition)

  return(data)
}
