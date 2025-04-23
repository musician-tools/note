
- [Objective 1: Introducing the {note} package! 🦄 (typical package
  write up; but actually
  aspirational)](#objective-1-introducing-the-note-package--typical-package-write-up-but-actually-aspirational)
  - [Example 🦄 (Aspirational; describes target
    API)](#example--aspirational-describes-target-api)
- [Objective 2. Immursive package building experience: readme as a
  checklist, control document 🚧
  ✅](#objective-2-immursive-package-building-experience-readme-as-a-checklist-control-document--)
  - [Step 00: Press play *on the video
    guide*](#step-00-press-play-on-the-video-guide)
- [Part I. Work out functionality ✅](#part-i-work-out-functionality-)
- [helper objects and functions](#helper-objects-and-functions)
- [Exported](#exported)
  - [`note()`](#note)
  - [`play_notes_df()`](#play_notes_df)
  - [Phase 1. Minimal working package](#phase-1-minimal-working-package)
    - [Added roxygen skeleton and manage dependancies?
      ✅](#added-roxygen-skeleton-and-manage-dependancies-)
    - [Moved functions R folder? ✅](#moved-functions-r-folder-)
    - [Package data set](#package-data-set)
    - [Check and install](#check-and-install)
    - [Traditional readme](#traditional-readme)
  - [Just beyond minimal](#just-beyond-minimal)
    - [Chosen a license? ✅](#chosen-a-license-)
  - [Phase 2: Listen & iterate 🚧](#phase-2-listen--iterate-)
  - [Phase 3: Let things settle](#phase-3-let-things-settle)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Objective 1: Introducing the {note} package! 🦄 (typical package write up; but actually aspirational)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of {note} is to play notes using R, based in name (e.g. ‘C’),
length of tone, and octave.

    devtools::install_github("musician-tools/note")

## Example 🦄 (Aspirational; describes target API)

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

calc_length <- function(rate, length, bpm) {
  seq(1, as.integer(rate * length * 60 / bpm))
}

calc_multiplier <- function(rate) {
  2 * pi / rate
}
```

# Exported

## `note()`

``` r
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
```

## `play_notes_df()`

``` r
harry_potter_phrase_df <- tibble::tribble(~name,~length,~octave, 
        "B", .5, 0,
         "E", .75, 0,
         "G", .25, 0,
         "F#", .5, 0,
         "E", 1, 0,
         "B", .5, 1,
         "A", 1.5, 1,
         "F#", 1.5, 0) 
```

``` r
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
```

``` r
harry_potter_phrase_df |>
  play_notes_df(note_name = name, 
                length = length, 
                octave = octave)

library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4          ✔ readr     2.1.5     
#> ✔ forcats   1.0.0          ✔ stringr   1.5.1     
#> ✔ ggplot2   3.5.1.9000     ✔ tibble    3.2.1     
#> ✔ lubridate 1.9.3          ✔ tidyr     1.3.1     
#> ✔ purrr     1.0.2          
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
ggdoremi:::parse_phrases_drm("ddssllssffmmrrdd", base_freq = 440) |>
  dplyr::mutate(length = 1, volume = 2) |>
  play_drm_df(drm = drm, length = length, volume = volume)
#> Joining with `by = join_by(drm)`

ggdoremi:::parse_phrases_drm("mssssllt
                             t1111112
                             2222233322") |>
  ungroup() |>
  dplyr::mutate(length = rep(c(1,2), 13)/1.4,
         volume = 1) |> 
  play_drm_df(drm = drm, length = length, volume = volume)
#> Joining with `by = join_by(drm)`
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
#> ✔ Setting active project to '/Users/evangelinereynolds/Google
#> Drive/r_packages/note'
```

### Moved functions R folder? ✅

``` r
knitrExtra::chunk_names_get()
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#>  [1] "unnamed-chunk-1"           "unnamed-chunk-2"          
#>  [3] "helperfunctions"           "note"                     
#>  [5] "unnamed-chunk-3"           "play_notes_df"            
#>  [7] "unnamed-chunk-4"           "unnamed-chunk-5"          
#>  [9] "unnamed-chunk-6"           "unnamed-chunk-7"          
#> [11] "harry_potter_phrase_df"    "unnamed-chunk-8"          
#> [13] "unnamed-chunk-9"           "unnamed-chunk-10"         
#> [15] "unnamed-chunk-11"          "test_calc_frequency_works"
#> [17] "unnamed-chunk-12"
knitrExtra::chunk_to_dir(c("helperfunctions", "note", "play_notes_df"))
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

### Package data set

``` r
harry_potter_phrase_df <- tibble::tribble(~name,~length,~octave, 
        "B", .5, 0,
         "E", .75, 0,
         "G", .25, 0,
         "F#", .5, 0,
         "E", 1, 0,
         "B", .5, 1,
         "A", 1.5, 1,
         "F#", 1.5, 0) 

usethis::use_data(harry_potter_phrase_df, overwrite = T)
#> ✔ Saving 'harry_potter_phrase_df' to 'data/harry_potter_phrase_df.rda'
#> • Document your data (see 'https://r-pkgs.org/data.html')
```

``` r
# usethis::use_data_raw()
knitrExtra::chunk_to_dir(c("harry_potter_phrase_df"), 
                         dir = "data-raw")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

### Check and install

``` r
devtools::check(pkg = ".")
devtools::install(pkg = ".", upgrade = "never")
```

### Traditional readme

``` r
# library(note)

audio::play(note("A"))

audio::play(c(
note("B", .5, 0),
note("E", .75, 0),
note("G", .25, 0)
))

harry_potter_phrase_df |>
  play_notes_df(name = name, 
                length = length, 
                octave = octave)
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

test_that("calc frequency works", {
  expect_equal(calc_frequency("A", 0), 440)
  expect_equal(calc_frequency("A", -1), 220)
  
})
```

``` r
knitrExtra:::chunk_to_tests_testthat("test_calc_frequency_works")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

- add a description and author information in the DESCRIPTION file? 🚧
- Addressed *all* notes, warnings and errors. 🚧
- Promote to wider audience…
- Package website built? 🚧
- Package website deployed? 🚧
- Submit to CRAN? Or don’t. 🚧
