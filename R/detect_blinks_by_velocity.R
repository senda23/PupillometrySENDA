#' detect blinks by a change in velocity
#'
#' This allows the user to set a threshold for velocity and remove anything classed as a blink as a result
#'
#' @param data dataset of class PupillometrySENDA
#' @param pupil column name for pupil data
#' @param threshold velocity threshold for blink detection
#' @param extend_forward number of observations to remove forward of blink
#' @param extend_back number of obervations to remove behind blink
#'
#' @examples
#' Sdata <- make_pupillometrysenda_data(data = pupil_data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#'
#' Sdata2 <- detect_blinks_by_velocity(data = Sdata,
#' pupil = "LPupil",
#' threshold = 0.1,
#' extend_forward = 0,
#' extend_back = 0)
#'
#' @importFrom tidyr fill
#' @import dplyr
#'
#' @export
#' @return returns dataframe with blinks removed including forward and back, and data in blink column.

detect_blinks_by_velocity <- function(data, pupil, threshold = 0.1, extend_forward = 0, extend_back = 0){
  
  if('PupillometrySENDA' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometrySENDA. Did you forget to run make_pupillometrysenda_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }
  
  options <- attr(data, 'PupillometrySENDA')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  
  message('Creating column pupil_in_blink')
  
  # まばたきを検知し，タグをつける
  data1 <- data %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    mutate(.diff1 = c(diff(!!sym(pupil)), NA),
           .diff2 = c(diff(!!sym(time)), NA),
           .vel = .diff1/.diff2,
           pupil_in_blink = ifelse(.vel > threshold | .vel < -threshold, 1, 0),
           pupil_in_blink = ifelse(is.na(!!sym(pupil)), 1, pupil_in_blink),
           pupil_in_blink = ifelse(is.na(pupil_in_blink), lag(pupil_in_blink), pupil_in_blink)) %>%
    ungroup()
  
  # まばたきの前後も含めてNAで置換する
  data2 <- data1 %>%
    mutate(!!sym(pupil) := ifelse(pupil_in_blink == 1, NA, !!sym(pupil))) %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    mutate(.mark = ifelse(pupil_in_blink == 1, !!sym(time), NA),
           .foward = .mark + extend_forward,
           .back = .mark - extend_back) %>%
    fill(.foward, .direction = 'down') %>%
    fill(.back, .direction = 'up') %>%
    mutate(.blink = ifelse(!!sym(time) >= .back | !!sym(time) <= .foward, 1, 0),
           .blink = ifelse(is.na(.blink), 0, .blink)) %>%
    mutate(pupil_in_blink := ifelse(.blink == 1, 1, pupil_in_blink),
           !!sym(pupil) := ifelse(pupil_in_blink == 1, NA, !!sym(pupil))) %>%
    select(-.blink, -.foward, -.back, -.mark, -.vel, -.diff1, -.diff2, -pupil_in_blink)
  
  class(data2) <- class(data)
  attr(data2, 'PupillometrySENDA') <- options
  return(data2)
}