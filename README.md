
  - [Objective 1: Introducing the {note} package\! 🦄 (typical package
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
  - [Part II. Packaging and documentation 🚧
    ✅](#part-ii-packaging-and-documentation--)
      - [Phase 1. Minimal working
        package](#phase-1-minimal-working-package)
          - [Created files for package archetecture with
            `devtools::create(".")`
            ✅](#created-files-for-package-archetecture-with-devtoolscreate-)
          - [Moved functions R folder? ✅](#moved-functions-r-folder-)
          - [Added roxygen skeleton? ✅](#added-roxygen-skeleton-)
          - [Managed dependencies ? ✅](#managed-dependencies--)
          - [Chosen a license? ✅](#chosen-a-license-)
          - [Run `devtools::check()` and addressed errors?
            ✅](#run-devtoolscheck-and-addressed-errors-)
          - [Build package 🚧](#build-package-)
          - [Make aspirational part of readme real.
            🚧](#make-aspirational-part-of-readme-real-)
          - [Add lifecycle badge](#add-lifecycle-badge)
      - [Phase 2: Listen & iterate 🚧](#phase-2-listen--iterate-)
      - [Phase 3: Let thinggs settle](#phase-3-let-thinggs-settle)
          - [Settled on examples. Put them in the roxygen skeleton and
            readme.
            🚧](#settled-on-examples-put-them-in-the-roxygen-skeleton-and-readme-)
          - [Written formal tests of functions?
            🚧](#written-formal-tests-of-functions-)
          - [Have you worked added a description and author information
            in the DESCRIPTION file?
            🚧](#have-you-worked-added-a-description-and-author-information-in-the-description-file-)
          - [Addressed *all* notes, warnings and errors.
            🚧](#addressed-all-notes-warnings-and-errors-)
      - [Promote to wider audience…](#promote-to-wider-audience)
          - [Package website built? 🚧](#package-website-built-)
          - [Package website deployed? 🚧](#package-website-deployed-)
      - [Phase 3: Harden/commit](#phase-3-hardencommit)
          - [Submit to CRAN? 🚧](#submit-to-cran-)
  - [Appendix: Reports, Environment](#appendix-reports-environment)
      - [Description file extract](#description-file-extract)
      - [Environment](#environment)
      - [`devtools::check()` report](#devtoolscheck-report)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Objective 1: Introducing the {note} package\! 🦄 (typical package write up; but actually aspirational)

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

The package was initially conceived [by Jim
Hester](https://resources.rstudio.com/rstudio-conf-2018/you-can-make-a-package-in-20-minutes-jim-hester)
as an example of how to quickly build an R package. But this repo is
also meant to be an *immursive* companion guide to Jim Hester’s, ‘You
can make an R package in 20 minutes’.

I was extremely lucky to run across his talk when I did. I had promised
to write a package for an RConsortium grant, but I’m not sure that I
would have managed without Jim’s excellent walk-through.

Recently I’ve explored ‘literate package building’ especially from the
README, and have decided to return to Jim’s package to further
experiment with this idea - and because note deserves to be shared\!

I’ve written a non-immursive companion guide to Jim’s talk here:
<https://evamaerey.github.io/package_in_20_minutes/package_in_20_minutes>

## Step 00: Press play *on the video guide*

Your video guide is
[here](https://resources.rstudio.com/rstudio-conf-2018/you-can-make-a-package-in-20-minutes-jim-hester);
the package build starts at about minute 8.

# Part I. Work out functionality ✅

Have some functions that you’d like to package up *like the below from
Hester’s ‘note’ example*

``` r
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
    

  
  
```

# Part II. Packaging and documentation 🚧 ✅

## Phase 1. Minimal working package

### Created files for package archetecture with `devtools::create(".")` ✅

### Moved functions R folder? ✅

``` r
knitr::knit_code$get() |> names()
#>  [1] "unnamed-chunk-1"           "unnamed-chunk-2"          
#>  [3] "hesterfunctions"           "unnamed-chunk-3"          
#>  [5] "unnamed-chunk-4"           "unnamed-chunk-5"          
#>  [7] "unnamed-chunk-6"           "unnamed-chunk-7"          
#>  [9] "unnamed-chunk-8"           "unnamed-chunk-9"          
#> [11] "test_calc_frequency_works" "unnamed-chunk-10"         
#> [13] "unnamed-chunk-11"          "unnamed-chunk-12"         
#> [15] "unnamed-chunk-13"
```

Use new {readme2pkg} function to do this from readme…

``` r
readme2pkg::chunk_to_r("hesterfunctions")
```

### Added roxygen skeleton? ✅

Use a roxygen skeleton for auto documentation and making sure proposed
functions are *exported*.

### Managed dependencies ? ✅

Package dependencies managed, i.e. `depend::function()` in proposed
functions and declared in the DESCRIPTION

``` r
usethis::use_package("audio")
#> ✔ Setting active project to '/Users/evangelinereynolds/Google Drive/r_packages/note'
#> • Refer to functions with `audio::fun()`
```

### Chosen a license? ✅

``` r
usethis::use_mit_license()
```

### Run `devtools::check()` and addressed errors? ✅

``` r
devtools::check(pkg = ".")
#> ℹ Updating note documentation
#> ℹ Loading note
#> Warning: ── Conflicts ───────────────────────────────────────────────── note conflicts
#> ──
#> ✖ `note` masks `note::note()`.
#> ✖ `play_notes_df` masks `note::play_notes_df()`.
#> ℹ Did you accidentally source a file rather than using `load_all()`?
#>   Run `rm(list = c("note", "play_notes_df"))` to remove the conflicts.
#> Warning: [hesterfunctions.R:73] @return requires a value
#> Warning: [hesterfunctions.R:76] @examples requires a value
#> Error: R CMD check found WARNINGs
```

### Build package 🚧

``` r
devtools::build()
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/Users/evangelinereynolds/Google Drive/r_packages/note/DESCRIPTION’ ... OK
#> * preparing ‘note’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘note_0.0.0.9000.tar.gz’
#> [1] "/Users/evangelinereynolds/Google Drive/r_packages/note_0.0.0.9000.tar.gz"
```

You need to do this before library(mynewpackage) will work.

### Make aspirational part of readme real. 🚧

At this point, you could change eval chunk options to TRUE. You can
remove the 🦄 emoji and perhaps replace it with construction site if you
are still uncertain of the API, and want to highlight that it is subject
to change.

### Add lifecycle badge

``` r
usethis::use_lifecycle_badge("experimental")
```

## Phase 2: Listen & iterate 🚧

Try to get feedback from experts on API, implementation, default
decisions. Is there already work that solves this problem?

## Phase 3: Let thinggs settle

### Settled on examples. Put them in the roxygen skeleton and readme. 🚧

### Written formal tests of functions? 🚧

That would look like this…

``` r
library(testthat)

test_that("calc frequency works", {
  expect_equal(calc_frequency("A", 0), 440)
  expect_equal(calc_frequency("A", -1), 220)
  
})
#> Test passed 😸
```

``` r
readme2pkg::chunk_to_tests_testthat("test_calc_frequency_works")
```

### Have you worked added a description and author information in the DESCRIPTION file? 🚧

### Addressed *all* notes, warnings and errors. 🚧

## Promote to wider audience…

### Package website built? 🚧

### Package website deployed? 🚧

## Phase 3: Harden/commit

### Submit to CRAN? 🚧

# Appendix: Reports, Environment

## Description file extract

## Environment

Here I just want to print the packages and the versions

``` r
all <- sessionInfo() |> print() |> capture.output()
all[11:17]
#> [1] ""                                                                         
#> [2] "attached base packages:"                                                  
#> [3] "[1] stats     graphics  grDevices utils     datasets  methods   base     "
#> [4] ""                                                                         
#> [5] "other attached packages:"                                                 
#> [6] "[1] testthat_3.1.6  note_0.0.0.9000"                                      
#> [7] ""
```

## `devtools::check()` report

``` r
devtools::check(pkg = ".")
#> ℹ Updating note documentation
#> ℹ Loading note
#> Error: R CMD check found WARNINGs
```
