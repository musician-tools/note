rate <- 44100
multiplier <- 2 * pi / rate
bpm <- 80
default_volume <- 5

note_names <- c("A" = 0, "A#" = 1, "Bb" = 1, "B" = 2, "Cb" = 2, "B#" = 3, "C" = 3,
  "C#" = 4, "Db" = 4, "D" = 5, "D#" = 6, "Eb" = 6, "E" = 7, "Fb" = 7, "E#" = 8,
  "F" = 8, "F#" = 9, "Gb" = 9, "G" = 10, "G#" = 11, "Ab" = 11)

rel3octaves <- stringr::str_split("DRMFSLTdrmfslt12345678", "")[[1]]
multiplier1octave <- c(1, 9/8, 81/64,4/3, 3/2, 27/16, 243/128)
mult3 <- c(multiplier1octave/2, multiplier1octave, multiplier1octave*2, 4)

drm_names <- mult3 
names(drm_names) <- rel3octaves

drm_to_freq <- function(drm = c("d", "r", "m"), freq_d = 440){
  
  drm_names[drm]*440
  
}

note_name_to_freq <- function(note_name, octave) {
  # 440hz is A above middle C
  440 * 2^((unname(note_names[note_name]) + (octave * 12)) / 12)
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
