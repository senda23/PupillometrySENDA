#' Interpolate across the gaps in data
#'
#' Once data is smoothed, it is important to deal with missing observations, such as blinks.
#' This allows simple interpolation over missing values, either linear, or cubic.
#' Depending on the analysis planed, this may not be a necessary option, but it is
#' strongly recommended for the functional analyses planned in this package.
#'
#' @param data a PupillometrySENDA dataframe
#' @param pupil Column name for pupil data to be interpolated
#' @param type string indicating linear or cubic interpolation to be performed.
#'
#' @examples
#' Sdata <- make_pupillometrysenda_data(data = pupil_data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#' 
#' int_data <- interpolate_data(data = Sdata,
#' pupil = "LPupil",
#' type = "linear")
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return interpolated pupillometrySENDA data

interpolate_data <- function(data, pupil, type = c('linear', 'cubic')){
  
  if('PupillometrySENDA' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometrySENDA. Did you forget to run make_pupillometrysenda_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }
  
  options <- attr(data, 'PupillometrySENDA')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  
  intdata <- data
  
  #make functions
  # 線形補間を行う関数を定義
  .spline <- function(x){
    x <- zoo::na.spline(x, na.rm = F)
    return(x)
  }
  # 3次元補間を行う関数を定義
  .approx <- function(x){
    x <- zoo::na.approx(x, na.rm = F, rule = 2)
    return(x)
  }
  #interpolate
  # 引数のtypeに応じて補間を実行
  if(type == 'cubic'){
    message('Performing cubic interpolation \n')
    warning('If start and end values are missing, cubic interpolation can return extreme values. Check data before continuing. If in doubt you should opt for linear interpolation.')
    groupy <- c(subject, trial, condition)
    intdata2 <- intdata %>%
      group_by(!!!syms(groupy)) %>%
      mutate(!!sym(pupil) := .spline(!!sym(pupil))) %>%
      ungroup()
  }else{
    if(type == 'linear'){
      message('Performing linear interpolation \n')
      groupy <- c(subject, trial, condition)
      intdata2 <- intdata %>%
        group_by(!!!syms(groupy)) %>%
        mutate(!!sym(pupil) := .approx(!!sym(pupil))) %>%
        ungroup()
    }
  }
  #cleanup
  class(intdata2) <- c(class(data))
  attr(intdata2, 'PupillometrySENDA') <- options
  return(intdata2)
}