#' play_drm_df
#'
#' @param data A data frame
#' @param drm a variable with drm encoding
#' @param duration note duration
#' @param volume volume duration
#'
#' @return will play notes
#' @export
#'
#' @examples  
play_drm_df <- function(data, drm, duration, volume){
  
  rate <- 44100
  multiplier <- 2 * pi / rate
  bpm <- 80
  default_volume <- 5
  

  drms = data |> dplyr::pull({{drm}}) 
  durations = data |> dplyr::pull({{duration}})
  volumes = data |> dplyr::pull({{volume}})
  
  
  notes <- c()
    
    for (i in 1:nrow(data)){

    notes <- c(notes, 
               note(drm = drms[i],
                    duration = durations[i]))
        
      }  
  
    
  audio::play(notes)
  
}
