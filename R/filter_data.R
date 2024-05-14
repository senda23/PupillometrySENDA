#' Run a filter on the data to smooth it out.
#'
#' filter_data allows three different options for filtering, a butterworth lowpass filter, a hanning filter, or
#' a median filter. You can also set the degree of this filter; we recommend a default of 11.
#' This filters on one pupil, it can be re-run on a second pupil if needed. Lowpass makes use of the
#' butterworth filter and filtfilt from package signal, median makes use of runmed.
#'
#' @param data a PupillometrySENDA dataframe
#' @param pupil column name for pupil data
#' @param filter option for filtering the data
#' @param degree filter degree
#'
#' @examples
#' Sdata <- make_pupillometrysenda_data(data = pupil_data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#' filtered_data <- filter_data(data = mean_data,
#' pupil = "pupil",
#' filter = "hanning",
#' degree = 11)
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return filtered pupil data

filter_data <- function(data, pupil, filter = c('median', 'hanning', 'lowpass'), degree = 11){
  
  if('PupillometrySENDA' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometrySENDA. Did you forget to run make_pupillometrysenda_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }
  
  options <- attr(data, 'PupillometrySENDA')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  
  #remove strange values
  # 瞳孔径が0以下，またはピリオドであるデータをNAに置換する
  rdata <- data
  rdata[[pupil]][rdata[[pupil]] == 0] <- NA
  rdata[[pupil]][rdata[[pupil]] < 0] <- NA
  rdata[[pupil]][rdata[[pupil]] == '.'] <- NA
  
  #make filtering functions
  #interpolate
  # NAを補間する関数".int_data"を定義．NA前後のデータの平均により補間する．
  .int_data <- function(x){
    x <- zoo::na.approx(x, na.rm = F, yleft = min(x, na.rm = T) + 1, yright = max(x, na.rm = T) - 1)
    return(x)
  }
  #filtfilt2 (based on danielson's solution from stackoverflow)
  # ゼロ位相フィルタリングを実行するための関数".filtfilt2"を定義．ローパスフィルタでのみ使用する．
  bf <- signal::butter(degree, 0.1)
  .filtfilt2 <- function(filters, x)  {
    filt <- filters$b
    a <- filters$a
    y <- signal::filter(filt, a, c(x, numeric(2 * max(length(a), length(filt)))),
                        init = rep(mean(x, na.rm = T) + 1, degree))
    y <- rev(signal::filter(filt, a, rev(y)))[seq_along(x)]
    return(y)
  }
  
  #lowpass
  # ローパスフィルタを実行する関数".lowpass"を定義．
  .lowpass <- function(x){
    x <- .filtfilt2(bf, x)
    return(x)
  }
  #hanning
  # ハニングフィルタを実行する関数".hanning"を定義．
  hanning_filter <- function(x, n){
    i <- 0:(n - 1)
    w <- 0.5 - 0.5 * cos((2 * pi * i)/(n - 1))
    w <- w/sum(w)
    # class(w) <- 'Ma'
    y <- as.vector(stats::filter(x, w))
    y
  }
  .hanning <- function(x){
    x <- hanning_filter(x, n = degree)
    return(x)
  }
  #median
  # メジアンフィルタを実行する関数".median"を定義．
  .median <- function(x){
    x <- stats::runmed(x, degree, endrule = 'keep')
    return(x)
  }
  
  #reorder
  # subject，trial，timeの順にデータを並べ替える
  vars = c(subject, trial, time)
  rdata <- dplyr::arrange(rdata, UQS(syms(vars)))
  
  # 引数に応じたフィルタリングを実行する
  if(filter == 'lowpass'){
    message('Performing lowpass filter \n')
    warning('Lowpass filter is still under development - it can do some strange things at the start and end of trials. Check your data to see that it works before proceeding.')
    groupy <- c(subject, trial, condition)
    
    rdata2 <- rdata %>%
      group_by(!!!syms(groupy)) %>%
      mutate(!!sym(pupil) := .int_data(!!sym(pupil))) %>%
      mutate(!!sym(pupil) := .lowpass(!!sym(pupil))) %>%
      ungroup()
  }
  
  else{
    if(filter == 'hanning'){
      message('Performing hanning filter \n')
      groupy <- c(subject, trial, condition)
      rdata2 <- rdata %>%
        group_by(!!!syms(groupy)) %>%
        mutate(!!sym(pupil) := .int_data(!!sym(pupil))) %>%
        mutate(!!sym(pupil) := .hanning(!!sym(pupil))) %>%
        ungroup()
    }
    else{
      message('Performing median filter \n')
      groupy <- c(subject, trial, condition)
      rdata2 <- rdata %>%
        group_by(!!!syms(groupy)) %>%
        mutate(!!sym(pupil) := .int_data(!!sym(pupil))) %>%
        mutate(!!sym(pupil) := .median(!!sym(pupil))) %>%
        ungroup()
    }
  }
  
  #remove the badly interpolated NAs
  rdata2[[pupil]][is.na(rdata[[pupil]])] <- NA
  
  #update class
  class(rdata2) <- c(class(data))
  attr(rdata2, 'PupillometrySENDA') <- options
  return(rdata2)
}