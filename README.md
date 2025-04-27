
- [Objective 1: Introducing the {note}
  package!](#objective-1-introducing-the-note-package)
- [Objective 2. Immursive package building experience: readme as a
  checklist, control document 🚧
  ✅](#objective-2-immursive-package-building-experience-readme-as-a-checklist-control-document--)
  - [Step 00: Press play *on the video
    guide*](#step-00-press-play-on-the-video-guide)
- [Part I. Work out functionality ✅](#part-i-work-out-functionality-)
- [helper objects and functions](#helper-objects-and-functions)
- [Exported](#exported)
  - [`note()`](#note)
  - [`potter_phrase_df`](#potter_phrase_df)
  - [`play_notes_df()` and
    `play_drm_df()`](#play_notes_df-and-play_drm_df)
    - [Try this all out…](#try-this-all-out)
  - [Phase 1. Minimal working package](#phase-1-minimal-working-package)
    - [Added roxygen skeleton and manage dependancies?
      ✅](#added-roxygen-skeleton-and-manage-dependancies-)
    - [Moved functions R folder? ✅](#moved-functions-r-folder-)
    - [Check and install](#check-and-install)
  - [Just beyond minimal](#just-beyond-minimal)
    - [Chosen a license? ✅](#chosen-a-license-)
  - [Phase 2: Listen & iterate 🚧](#phase-2-listen--iterate-)
  - [Phase 3: Let things settle](#phase-3-let-things-settle)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Objective 1: Introducing the {note} package!

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of {note} is to play notes using R, based in name (e.g. ‘C’),
duration of tone, and octave.

    devtools::install_github("musician-tools/note")

Try in an interactive session:

``` r
library(note)
audio::play(note("A"))
```

# Objective 2. Immursive package building experience: readme as a checklist, control document 🚧 ✅

The package was initially conceived <a
href="https://resources.rstudio.com/rstudio-conf-2018/you-can-make-a-package-in-20-minutes-jim-hester"
target="_blank">by Jim Hester</a> as an example of how to quickly build
an R package. But this repo is also meant to be an *immursive* companion
guide to Jim Hester’s, ‘You can make an R package in 20 minutes’.

I was extremely lucky to run across his talk when I did. I had promised
to write a package for an RConsortium grant, but I’m not sure that I
would have managed without Jim’s excellent walk-through.

Recently I’ve explored ‘literate package building’ especially from the
README, and have decided to return to Jim’s package to further
experiment with this idea - and because note deserves to be shared!

I’ve written a non-immursive companion guide to Jim’s talk here:
<https://evamaerey.github.io/package_in_20_minutes/package_in_20_minutes>

## Step 00: Press play *on the video guide*

Your video guide is <a
href="https://resources.rstudio.com/rstudio-conf-2018/you-can-make-a-package-in-20-minutes-jim-hester"
target="_blank">here</a>; the package build starts at about minute 8.

# Part I. Work out functionality ✅

Have some functions that you’d like to package up *like the below from
Hester’s ‘note’ example*

# helper objects and functions

``` r
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

calc_duration <- function(rate, duration, bpm) {
  seq(1, as.integer(rate * duration * 60 / bpm))
}

calc_multiplier <- function(rate) {
  2 * pi / rate
}
```

# Exported

## `note()`

``` r
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
```

## `potter_phrase_df`

``` r
potter_phrase_df <- tibble::tribble(~note, ~duration, ~octave, 
         "B", .5, 0,
         "E", .75, 0,
         "G", .25, 0,
         "F#", .5, 0,
         "E", 1, 0,
         "B", .5, 1,
         "A", 1.5, 1,
         "F#", 1.5, 0) 

usethis::use_data(potter_phrase_df, overwrite = T)
#> ✔ Setting active project to '/Users/evangelinereynolds/Google Drive/r_packages/note'
#> ✔ Saving 'potter_phrase_df' to 'data/potter_phrase_df.rda'
#> • Document your data (see 'https://r-pkgs.org/data.html')
```

``` r
#' Harry Potter theme data frame
#'
#' Note, duration and  ...
#'
#' @format ## `potter_phrase_df`
#' A data frame with 8 rows:
#' \describe{
#'   \item{note}{note name}
#'   \item{duration}{duration of note}
#'   \item{octave}{note octave}
#'   ...
#' }
#' @source <https://www.warnerbros.com/movies/harry-potter-complete-8-film-collection>
"potter_phrase_df"
#> [1] "potter_phrase_df"
```

``` r
twinkle_phrase_df <- tibble::tribble(~drm,~duration,
        "d", 1, 
         "d", 1, 
         "s", 1, 
         "s", 1, 
         "l", 1, 
         "l", 1, 
         "s", 2) 

usethis::use_data(twinkle_phrase_df, overwrite = T)
#> ✔ Saving 'twinkle_phrase_df' to 'data/twinkle_phrase_df.rda'
#> • Document your data (see 'https://r-pkgs.org/data.html')
```

``` r
#' Harry Potter theme data frame
#'
#' Note, duration and  ...
#'
#' @format ## `twinkle_phrase_df`
#' A data frame with 8 rows:
#' \describe{
#'   \item{drm}{note name using drm schema}
#'   \item{duration}{duration of note}
#'   ...
#' }
#' @source <https://www.warnerbros.com/movies/harry-potter-complete-8-film-collection>
"twinkle_phrase_df"
#> [1] "twinkle_phrase_df"
```

## `play_notes_df()` and `play_drm_df()`

``` r
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
```

``` r
potter_phrase_df |>
  play_notes_df(name = note, 
                duration = duration, 
                octave = octave)
```

``` r
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
```

### Try this all out…

``` r
twinkle_phrase_df |> 
  dplyr::mutate(volume = 1,
                duration = duration/2) |>
  play_drm_df(drm = drm, 
              duration = duration, 
              volume = volume)
```

## Phase 1. Minimal working package

``` r
# devtools::create(".")
```

### Added roxygen skeleton and manage dependancies? ✅

Use a roxygen skeleton for auto documentation and making sure proposed
functions are *exported* and dependencies documented.

Package dependencies managed, i.e. `depend::function()` in proposed
functions and declared in the DESCRIPTION

``` r
usethis::use_package("audio")
usethis::use_package("dplyr")
```

### Moved functions R folder? ✅

``` r
knitrExtra::chunk_names_get()
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#>  [1] "unnamed-chunk-1"              "unnamed-chunk-2"             
#>  [3] "helperfunctions"              "note"                        
#>  [5] "potter_phrase_df"             "document_potter_phrase_df"   
#>  [7] "harry_potter_phrase_df"       "document_twinkle_phrase_df"  
#>  [9] "play_notes_df"                "unnamed-chunk-3"             
#> [11] "play_drm_df"                  "unnamed-chunk-4"             
#> [13] "unnamed-chunk-5"              "unnamed-chunk-6"             
#> [15] "unnamed-chunk-7"              "unnamed-chunk-8"             
#> [17] "unnamed-chunk-9"              "test_note_name_to_freq_works"
#> [19] "unnamed-chunk-10"
knitrExtra::chunk_to_dir(c( "helperfunctions"   , "note"   , "potter_phrase_df","document_potter_phrase_df" , "harry_potter_phrase_df",       "document_twinkle_phrase_df"  , "play_notes_df", "play_drm_df"    ))
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

### Check and install

``` r
devtools::document()
devtools::check(pkg = ".")
devtools::install(pkg = ".", upgrade = "never")
```

## Just beyond minimal

### Chosen a license? ✅

``` r
usethis::use_mit_license()
#> ✔ Leaving 'LICENSE.md' unchanged
usethis::use_lifecycle_badge("experimental")
```

## Phase 2: Listen & iterate 🚧

Try to get feedback from experts on API, implementation, default
decisions. Is there already work that solves this problem?

## Phase 3: Let things settle

- Settled on examples. Put them in the roxygen skeleton and readme. 🚧
- Written formal tests of functions? 🚧

That would look like this…

``` r
library(testthat)

test_that("note_name_to_freq works", {
  expect_equal(note_name_to_freq("A", 0), 440)
  expect_equal(note_name_to_freq("A", -1), 220)
  
})
```

``` r
knitrExtra:::chunk_to_tests_testthat("test_note_name_to_freq_works")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

- add a description and author information in the DESCRIPTION file? 🚧
- Addressed *all* notes, warnings and errors. 🚧
- Promote to wider audience…
- Package website built? 🚧
- Package website deployed? 🚧
- Submit to CRAN? Or don’t. 🚧
