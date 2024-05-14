#' Perform preprocessing on data frames for PupillometrySENDA
#'
#' Downsampling, filtering, blink removal, rejection by missing values, interpolation of missing data, 
#' and baseline correction are performed on the data frames for PupillometrySENDA.
#' 
#' @param data Dataframe for PupillometrySENDA
#' @param pupil Pupil data column name
#' @param sampling_rate Sampling frequency (unit: ms) 
#' @param trial_threshold Allowable loss ratio per trial
#' @param subject_trial_threshold Allowable missing percentages per subject
#' @param baseline_start Starting point of baseline correction
#' @param baseline_end End point of baseline correction
#'
#' @examples
#' Sdata <- make_pupillometrysenda_data(data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#' result_data <- total_pupil_analysis(data = Sdata,
#' pupil = "LPupil",
#' sampling_rate = 200,
#' trial_threshold = 0.75,
#' subject_trial_threshold = 0.75,
#' baseline_start = 800,
#' baseline_end = 1000)
#'
#' @export
#'
#' @return A dataframe for PupillometrySENDA after preprocessing


total_pupil_analysis = function(data, pupil, sampling_rate, trial_threshold, subject_trial_threshold, baseline_start, baseline_end){

  data = downsample_time_data(data = data,
                              pupil = pupil,
                              timebin_size = 1000 / sampling_rate,
                              option = 'median')
  
  data = filter_data(data = data,
                     pupil = pupil,
                     filter = 'median',
                     degree = 11)
  
  data = detect_blinks_by_velocity(data = data,
                                   pupil = pupil,
                                   threshold = 0.1)
  
  data = detect_blinks_by_size(data = data,
                               pupil = pupil,
                               threshold = 3)
  
  data = clean_missing_data(data = data,
                            pupil = pupil,
                            trial_threshold = trial_threshold,
                            subject_trial_threshold = subject_trial_threshold)
  
  data = interpolate_data(data = data,
                          pupil = pupil,
                          type = 'cubic')
  
  data = baseline_data(data = data,
                       pupil = pupil,
                       start = baseline_start,
                       stop = baseline_end)
  
  return(data)
}
