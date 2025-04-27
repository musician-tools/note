#' note
#' 
#' @param name note name
#' @param octave octave
#' @param drm drm 
#' @param duration duration
#' @param volume volume
#'
#' @export
#'
#' @examples
#' note("A")
#' audio::play(c(
#' note("B", .5, 0),
#' note("E", .75, 0)))
note <- function(name = NULL, octave = 0, drm = NULL, duration = 1, volume = default_volume) {
  if(!is.null(name)){frequency <- note_name_to_freq(name, octave)}
  if(!is.null(drm)){frequency <- drm_to_freq(drm)}
  volume <- calc_volume(volume)
  duration <- calc_duration(rate, duration, bpm)
  multiplier <- calc_multiplier(rate)
  res <- sin(frequency * multiplier * duration) * volume
  structure(res, class = "note")
}


print.note_rel <- function(x, ...) {
  audio::play(x, ...)
}

print.note <- function(x, ...) {
  audio::play(x, ...)
}
