#' Downsample frequency to reduce number of samples and data size
#'
#' This function is useful if you were sampling at a very high frequency (eg 500Hz)
#' causing the data size to be hard to manage, and high autocorrelation.
#' Careful decisions should be made about the time bin size and appropriateness
#' of this function, with respect to the data type.
#'
#' @param data your data of class PupillometrySENDA
#' @param pupil a column name denoting pupil size
#' @param timebin_size the size of the new timebin you wish to use
#' @param option what should be calculated in each timebin - mean or median. Defaults to mean.
#' @examples
#' data(pupil_data)
#' Sdata <- make_pupillometrysenda_data(data = pupil_data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#' new_data <- downsample_time_data(data = Sdata,
#' pupil = "LPupil",
#' timebin_size = 50,
#' option = "mean")
#' @import dplyr
#' @import rlang
#' @importFrom stats,median
#'
#' @export
#' @return A downsampled dataframe of class PupillometrySENDA

downsample_time_data <- function(data, pupil, timebin_size, option = c('mean', 'median')){

  # PupillometrySENDA用のデータフレーム以外を引数に指定した場合，エラーを返す
  if('PupillometrySENDA' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometrySENDA. Did you forget to run make_pupillometrysenda_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  # optionが未定義の場合は'mean'とする
  if(is.null(option)) option = 'mean'

  options <- attr(data, 'PupillometrySENDA')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition

  # timeをtimebin_sizeで割った値の整数部を，新たな列"Timebin"とする
  data[["Timebin"]] <- floor(data[[time]] / timebin_size)

  if(option == 'median'){
    message('Calculating median pupil size in each timebin \n')
    groupy <- c(subject, trial, condition)
    data2 <- data %>%
      # subject，trial，condition，Timebin列でグループ化する
      group_by(!!!syms(groupy),
               Timebin) %>%
      # グループごとに中央値を求め，新たな列pupilとする
      summarise(!!sym(pupil) := median(!!sym(pupil))) %>%
      ungroup() %>%
      # time列をTimebin*timebin_sizeで置換する
      mutate(!!sym(time) := Timebin * timebin_size)
  }else{
    message('Calculating mean pupil size in each timebin \n')
    groupy <- c(subject, trial, condition)
    data2 <- data %>%
      group_by(!!!syms(groupy),
               Timebin) %>%
      summarise(!!sym(pupil) := mean(!!sym(pupil))) %>%
      ungroup() %>%
      mutate(!!sym(time) := Timebin * timebin_size)
  }

  data2[['Timebin']] <- NULL

  class(data2) <- c(class(data))
  attr(data2, 'PupillometrySENDA') <- options

  return(data2)
}
