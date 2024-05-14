#' Baseline pupil data to the average pupil size within a window
#'
#' This function is for use with the PupillometrySENDA package to baseline each participant's pupil size to the
#' mean pupil size within a window.
#' This may not be necessary if you are doing purely within-subject analyses, but it is convenient for
#' comparison across subjects, and makes results more uniform.
#'
#' @param data a PupillometrySENDA dataframe
#' @param pupil a column name denoting pupil data
#' @param start start time of baseline window
#' @param stop stop time of baseline window
#'
#' @examples
#' Sdata <- make_pupillometrysenda_data(data = pupil_data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#' base_data <- baseline_data(data = Sdata, 
#' pupil = "LPupil", 
#' start = 0, 
#' stop = 100)
#' @import dplyr
#' @import rlang
#'
#' @export
#'
#' @return A PupillometrySENDA dataframe, with baselined pupil


baseline_data <- function(data, pupil, start, stop){
  
  if('PupillometrySENDA' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometrySENDA. Did you forget to run make_pupillometrysenda_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }
  
  options <- attr(data, 'PupillometrySENDA')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  
  data[[time]] <- as.numeric(data[[time]])
  
  basedata <- data
  
  # ベースライン区間の取得
  baseline1 <- dplyr::filter(basedata, !!rlang::sym(time) >= start)
  baseline2 <- dplyr::filter(baseline1, !!rlang::sym(time) <= stop)
  
  message('Baselining for each subject and trial. If this is not the intended behaviour you may wish to do this manually.')
  
  # ベースライン区間の平均を求める
  baseline3 <- baseline2 %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    summarise(Base = mean(!!sym(pupil), na.rm = T)) %>%
    ungroup()
  
  # ベースライン補正を実行
  baselined <- merge(basedata, baseline3, by = c(subject, trial), all.x = T, sort = F)
  baselined[[pupil]] <- baselined[[pupil]] - baselined[['Base']]
  
  # 不要な列"Base"を削除
  baselined[['Base']] <- NULL
  
  # データの並び替え
  vars = c(subject, trial, time)
  baselined <- dplyr::arrange(baselined, !!!syms(vars))
  
  class(baselined) <- c(class(data))
  attr(baselined, 'PupillometrySENDA') <- options
  
  return(baselined)
  
}