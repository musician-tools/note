#' Title
#'
#' @param df 
#' @param name 
#' @param length 
#' @param octave 
#' @param volume 
#'
#' @return
#' @export
#'
#' @examples
play_notes_df <- function(df, note_name, length, octave, volume){
  
  rate <- 44100
  multiplier <- 2 * pi / rate
  bpm <- 80
  default_volume <- 5
  
  note_names = df |> dplyr::pull({{note_name}})
  lengths = df |> dplyr::pull({{length}})
  octaves = df |> dplyr::pull({{octave}})
  
  
  notes <- c()
    
    for (i in 1:nrow(df)){

    notes <- c(notes, 
               note(note_name = note_names[i],
                    length = lengths[i],
                    octave = octaves[i]
                            ))
        
    }
  
    audio::play(notes)

}
  
  
play_drm_df <- function(df, drm, length, volume){
  
  rate <- 44100
  multiplier <- 2 * pi / rate
  bpm <- 80
  default_volume <- 5
  

  drms = df |> dplyr::pull({{drm}}) 
  lengths = df |> dplyr::pull({{length}})
  volumes = df |> dplyr::pull({{volume}})
  
  
  notes <- c()
    
    for (i in 1:nrow(df)){

    notes <- c(notes, 
               note(note_name = note_names[i],
                    drm = drms[i],
                    length = lengths[i]))
        
      }  
  
    
  audio::play(notes)
  
}
