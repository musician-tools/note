#' @export
#'
#' @examples
#' note("A")
#' audio::play(c(
#' note("B", .5, 0),
#' note("E", .75, 0)))
note <- function(note_name = NULL, drm = NULL, length = 1, octave = 0, volume = default_volume) {
  if(!is.null(note_name)){frequency <- note_name_to_freq(note_name, octave)}
  if(!is.null(drm)){frequency <- drm_to_freq(drm)}
  volume <- calc_volume(volume)
  length <- calc_length(rate, length, bpm)
  multiplier <- calc_multiplier(rate)
  res <- sin(frequency * multiplier * length) * volume
  structure(res, class = "note")
}

# note(drm = "d", volume = 5)


# audio::play(note_rel(doremi = "8"))


print.note_rel <- function(x, ...) {
  audio::play(x, ...)
}

print.note <- function(x, ...) {
  audio::play(x, ...)
}
