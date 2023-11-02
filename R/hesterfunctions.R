rate <- 44100
multiplier <- 2 * pi / rate
bpm <- 80
default_volume <- 5

notes <- c("A" = 0, "A#" = 1, "Bb" = 1, "B" = 2, "Cb" = 2, "B#" = 3, "C" = 3,
  "C#" = 4, "Db" = 4, "D" = 5, "D#" = 6, "Eb" = 6, "E" = 7, "Fb" = 7, "E#" = 8,
  "F" = 8, "F#" = 9, "Gb" = 9, "G" = 10, "G#" = 11, "Ab" = 11)

calc_frequency <- function(note, octave) {
  # 440hz is A above middle C
  440 * 2^((unname(notes[note]) + (octave * 12)) / 12)
}

calc_volume <- function(x) {
  # x should be between 1 and 10
  stopifnot(x >= 1, x <= 10)
  
  x / 10
}

calc_length <- function(rate, length, bpm) {
  seq(1, as.integer(rate * length * 60 / bpm))
}

calc_multiplier <- function(rate) {
  2 * pi / rate
}

#' Create music
#'
#' @param note name
#' @param length in beats
#' @param octave from middle C
#' @param volume from 1 to 10
#'
#' @return a note object
#' @export
#'
#' @examples
#' note("A")
#' audio::play(c(
#' note("B", .5, 0),
#' note("E", .75, 0),
#' note("G", .25, 0),
#' note("F#", .5, 0),
#' note("E", 1, 0),
#' note("B", .5, 1),
#' note("A", 1.5, 1),
#' note("F#", 1.5, 0)
#' ))
note <- function(name, length = 1, octave = 0, volume = default_volume) {
  frequency <- calc_frequency(name, octave)
  volume <- calc_volume(volume)
  length <- calc_length(rate, length, bpm)
  multiplier <- calc_multiplier(rate)
  res <- sin(frequency * multiplier * length) * volume
  structure(res, class = "note")
}

print.note <- function(x, ...) {
  audio::play(x, ...)
}

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
play_notes_df <- function(df, name, length, octave, volume){
  
  rate <- 44100
  multiplier <- 2 * pi / rate
  bpm <- 80
  default_volume <- 5
  
  notes <- c()
    
    for (i in 1:nrow(df)){

    notes <- c(notes, note(name = df %>% pull({{name}}) %>% .[i],
       length = (df %>% pull({{length}}) %>% .[i]),
       octave = df %>% pull({{octave}}) %>% .[i]
                            ))
        
      }
    
  audio::play(notes)
  
  }
    

  
  


