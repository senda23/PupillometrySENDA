#' detect blinks by a change in pupil size
#'
#' This allows the user to set a threshold for pupil size and remove anything classed as a blink as a result
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
#' Sdata2 <- detect_blinks_by_size(data = Sdata,
#' pupil = "LPupil",
#' threshold = 2.5,
#' extend_forward = 0,
#' extend_back = 0)
#'
#' @importFrom tidyr fill
#' @import dplyr
#'
#' @export
#' @return returns dataframe with blinks removed including forward and back, and data in blink column.

detect_blinks_by_size <- function(data, pupil, threshold = 2.5, extend_forward = 0, extend_back = 0){

  if('PupillometrySENDA' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometrySENDA. Did you forget to run make_pupillometrysenda_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  options <- attr(data, 'PupillometrySENDA')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition

  # 標準偏差の閾値倍を超える値をまばたきと見なす
  data1 <- data %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    mutate(.min = mean(!!sym(pupil), na.rm = T) - (threshold * sd(!!sym(pupil), na.rm = T)),
           .max = mean(!!sym(pupil), na.rm = T) + (threshold * sd(!!sym(pupil), na.rm = T))) %>%
    ungroup() %>%
    mutate(pupil_in_blink = ifelse(!!sym(pupil) > .max | !!sym(pupil) < .min, 1, 0),
           pupil_in_blink = ifelse(is.na(!!sym(pupil)), 1, pupil_in_blink))

  message('Creating column pupil_in_blink')

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
    select(-.blink, -.foward, -.back, -.mark, -.min, -.max, -pupil_in_blink)

  class(data2) <- class(data)
  attr(data2, 'PupillometrySENDA') <- options
  return(data2)
}
