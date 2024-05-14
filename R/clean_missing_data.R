#' Clean missing data above an acceptable threshold
#'
#' This function can be used to remove trials and participants
#' who do not meet the threshold for a study. Note that there are two parameters for
#' cleaning, one to remove trials above a threshold,
#' the second to remove participants who drop more than a certain amount of trials.
#'
#' @param data your data of class PupillometrySENDA
#' @param pupil a column name denoting pupil size
#' @param trial_threshold a proportion of missing data over which a trial can be considered lost
#' @param subject_trial_threshold a proportion of missing trials over which a participant can be considered lost.
#' @examples
#' Sdata <- make_pupillometrysenda_data(data = pupil_data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#' 
#' Sdata2 <- clean_missing_data(data = Sdata, 
#' pupil = "LPupil", 
#' trial_threshold = 0.8, 
#' subject_trial_threshold = 0.8)
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return A cleaned PupillometrySENDA dataframe

clean_missing_data <- function(data, pupil, trial_threshold = 1, subject_trial_threshold = 1){
  
  if('PupillometrySENDA' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometrySENDA.
         Did you forget to run make_pupillometrysenda_data?
         Some tidyverse functions associated with dplyr and tidyr
         can also interfere with this functionality.')
  }
  
  options <- attr(data, 'PupillometrySENDA')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  
  # 1被験者あたりの許容欠損割合が1のときは，被験者のリジェクトは行わない
  if(subject_trial_threshold == 1){
    if(trial_threshold > 1){
      stop('Please input trial threshold as a proportion')
    }
    data_trial <- data %>%
      group_by(!!sym(subject), !!sym(trial)) %>%
      mutate(Missing = ifelse(is.na(!!sym(pupil)), 1, 0)) %>%
      summarise(SumMissing = sum(Missing),
                PropMissing = sum(Missing)/length(Missing)) %>%
      ungroup()
    
    data_trial2 <- data_trial[data_trial[['PropMissing']] < trial_threshold,]
    data_bad <- data_trial[data_trial[['PropMissing']] > trial_threshold,]
    
    data_trial <- data_trial %>%
      mutate(Remove = ifelse(PropMissing > trial_threshold, 1, 0))
    
    bad_num <- length(data_bad[['PropMissing']])
    
    message(paste('Removing trials with a proportion missing >', trial_threshold,
                  '\n ...removed', bad_num, 'trials \n'))
    
    # 欠損値の多い試行をリジェクトする
    data_out <- left_join(data_trial2, data, by = c(subject, trial))
    data_out$SubjProp <- 1
    
  }else{ # both put in
    if(subject_trial_threshold > 1){
      stop('Please input subject trial threshold as a proportion')
    }
    if(trial_threshold > 1){
      stop('Please input trial threshold as a proportion')
    }
    data_trial <- data %>%
      group_by(!!sym(subject), !!sym(trial)) %>%
      mutate(Missing = ifelse(is.na(!!sym(pupil)), 1, 0)) %>%
      summarise(SumMissing = sum(Missing),
                PropMissing = sum(Missing)/length(Missing)) %>%
      ungroup()
    
    data_trial2 <- data_trial[data_trial$PropMissing < trial_threshold,]
    data_bad <- data_trial[data_trial$PropMissing > trial_threshold,]
    
    bad_num <- length(data_bad[['PropMissing']])
    
    data_trial <- data_trial %>%
      mutate(Remove = ifelse(PropMissing > trial_threshold, 1, 0))
    
    message(paste('Removing trials with a proportion missing >', trial_threshold,
                  '\n ...removed', bad_num, 'trials \n'))
    
    # by participant
    data_part <- data_trial %>%
      group_by(!!sym(subject)) %>%
      summarise(SubjProp = sum(Remove)/length(Remove)) %>%
      ungroup()
    
    data_part2 <- data_part[data_part[['SubjProp']] < subject_trial_threshold,]
    data_bad2 <- data_part[data_part[['SubjProp']] > subject_trial_threshold,]
    
    part_num <- length(data_bad2[['SubjProp']])
    
    message(paste('Removing subjects with a proportion of missing trials >',
                  subject_trial_threshold,
                  '\n ...removed', part_num, 'subjects \n'))
    
    # 欠損値の多い試行をリジェクトする
    data_out2 <- left_join(data_trial2, data, by = c(subject, trial))
    # リジェクトされた試行の多い被験者をリジェクトする
    data_out <- left_join(data_part2, data_out2, by = subject)
    
  }
  data_out <- data_out %>%
    select(-SubjProp, -PropMissing, -SumMissing)
  
  #update class
  class(data_out) <- c(class(data))
  attr(data_out, 'PupillometrySENDA') <- options
  return(data_out)
}