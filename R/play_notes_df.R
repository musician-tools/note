#' play_notes_df
#'
#' @param data A data frame
#' @param name character variable with note names
#' @param octave numeric with note octave
#' @param duration numeric with note duration
#' @param volume numeric with note volume
#'
#' @return
#' @export
#'
#' @examples
play_notes_df <- function(data, name, octave, duration,  volume){
  
  rate <- 44100
  multiplier <- 2 * pi / rate
  bpm <- 80
  default_volume <- 5
  
  names = data |> dplyr::pull({{name}})
  durations = data |> dplyr::pull({{duration}})
  octaves = data |> dplyr::pull({{octave}})
  
  
  notes <- c()
    
    for (i in 1:nrow(data)){

    notes <- c(notes, 
               note(name = names[i],
                    octave = octaves[i],
                    duration = durations[i]
                            ))
        
    }
  
    audio::play(notes)

}
