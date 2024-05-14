#' Pre-prepared plots of PupillometrySENDA data
#'
#' The plot functions are designed to run with just data and pupil selections,
#' with some additional options for fun with plotting. This allows to see
#' raw data as points, grouped by either subject or condition.
#'
#' @param x A PupillometrySENDA dataframe
#' @param pupil Column name of pupil data to be plotted
#' @param group What to group the data by (none, condition, or subject)
#' @param geom Geom to pass to ggplot. Either point, line, or pointrange.
#' @param model Optional argument to plot agains a fitted model
#' @param ... Ignored
#'
#' @examples
#' Sdata <- make_pupillometrysenda_data(data = pupil_data,
#' subject = "ID",
#' trial = "Trial",
#' time = "Time",
#' condition = "Type")
#' Sdata2 <- downsample_time_data(data = Sdata,
#' pupil = "LPupil",
#' timebin_size = 100,
#' option = 'median')
#' p <- plot(Sdata2, pupil = "LPupil", group = "subject")
#' p
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @export
#'
#' @return A ggplot object


plot.PupillometrySENDA <- function(x, pupil, group = c('none', 'condition', 'subject'), geom = c('point', 'line', 'pointrange'), model = NULL, ...){

  data <- x
  if('PupillometrySENDA' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometrySENDA. Did you forget to run make_pupillometrysenda_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }
  options <- attr(data, 'PupillometrySENDA')
  trial <- options$Trial
  condition <- options$Condition
  subject <- options$Subject
  time <- options$Time
  if(!is.null(model)){fit <- model$fitted.values}

  if(is.null(model)){
    if(is.null(group) | length(group) > 1) group = 'none'

    if(group == 'condition'){
      p <- data %>% ggplot2::ggplot(
        ggplot2::aes_string(x = time, y = pupil, colour = condition, shape = condition))
    }
    else{
      if(group == 'subject'){
        p <- data %>% ggplot2::ggplot(
          ggplot2::aes_string(x = time, y = pupil, group = subject))
      }
      else{
        p <- data %>% ggplot2::ggplot(
          ggplot2::aes_string(x = time, y = pupil))
      }
    }

    #Add plot layers
    if(is.null(geom) | length(geom) > 1) geom = 'point'

    if(geom == 'pointrange'){
      q <- p + ggplot2::stat_summary(geom = 'pointrange', fun.data = 'mean_se', size = 0.5, inherit.aes = T, alpha = 0.1) +
        ggplot2::ylab('Pupil Size') +
        ggplot2::xlab('Time') +
        ggplot2::theme(legend.position = c(0.85, 0.85))
    }else{
      if(geom == 'line'){
        q <- p + ggplot2::stat_summary(geom = 'line', fun = 'mean', size = 1, inherit.aes = T) +
          ggplot2::ylab('Pupil Size') +
          ggplot2::xlab('Time') +
          ggplot2::theme(legend.position = c(0.85, 0.85))
      }else{

        q <- p + ggplot2::stat_summary(geom = 'point', fun = 'mean', size = 1, inherit.aes = T) +
          ggplot2::ylab('Pupil Size') +
          ggplot2::xlab('Time') +
          ggplot2::theme(legend.position = c(0.85, 0.85))
      }}


    q
  }else{
    if('gam' %in% class(model)){
      data$fit <- fit
      if(group == 'condition'){
        p <- data %>%
          ggplot2::ggplot(
            ggplot2::aes_string(x = time, y = pupil, colour = condition, shape = condition))
      }
      else{
        if(group == 'subject'){
          p <- data %>%
            ggplot2::ggplot(
              ggplot2::aes_string(x = time, y = pupil, group = subject))
        }
        else{
          p <- data %>%
            ggplot2::ggplot(
              ggplot2::aes_string(x = time, y = pupil))
        }
      }

      #Add plot layers
      q <- p + ggplot2::stat_summary(geom = 'pointrange', fun.data = 'mean_se', size = 1, inherit.aes = T, alpha = 0.1) +
        ggplot2::stat_summary(inherit.aes = T, ggplot2::aes(y = fit), geom = 'line', fun = 'mean', size = 2) +
        ggplot2::ylab('Pupil Size') +
        ggplot2::xlab('Time') +
        ggplot2::theme(legend.position = c(0.85, 0.85))

      q
    }
  }
}