
  - [The {note} package](#the-note-package)
  - [Example](#example)
  - [Immursive package building experience from the
    readme](#immursive-package-building-experience-from-the-readme)
  - [Introduction](#introduction)
      - [Step 00: Press play *on the video
        guide*](#step-00-press-play-on-the-video-guide)
  - [Part I. Work on functionality](#part-i-work-on-functionality)
      - [Step 0: Have some functions that you’d like to package up *like
        the below from Hester’s ‘note’
        example*](#step-0-have-some-functions-that-youd-like-to-package-up-like-the-below-from-hesters-note-example)
  - [How we write the package…](#how-we-write-the-package)
  - [time to test](#time-to-test)
  - [Part II: Package building](#part-ii-package-building)
      - [Step 00: Create github repo](#step-00-create-github-repo)
      - [Step](#step)
      - [Step 1: Create package architecture *using
        usethis::create\_package()*](#step-1-create-package-architecture-using-usethiscreate_package)
      - [Step 2: Describe what the package does *in the DESCRIPTION
        file* in the top level of the package
        directory](#step-2-describe-what-the-package-does-in-the-description-file-in-the-top-level-of-the-package-directory)
      - [Step 3: Add your functions *to an R Script saved in the R
        folder*](#step-3-add-your-functions-to-an-r-script-saved-in-the-r-folder)
      - [Step 4: Make the package “active” *and test your functions
        interactively with
        devtools::load\_all()*](#step-4-make-the-package-active-and-test-your-functions-interactively-with-devtoolsload_all)
      - [Step 5: Run a check on the new package *using
        devtools::check()*](#step-5-run-a-check-on-the-new-package-using-devtoolscheck)
      - [Step 6: Add needed
        dependencies](#step-6-add-needed-dependencies)
          - [Step 6a. Declare dependencies *using
            usethis::use\_package(“package\_name”)*](#step-6a-declare-dependencies-using-usethisuse_packagepackage_name)
          - [Step 6b: Add dependencies and export functions in R
            script](#step-6b-add-dependencies-and-export-functions-in-r-script)
      - [Step 7: Document these additions *using a ‘Roxygen’
        skeleton*](#step-7-document-these-additions-using-a-roxygen-skeleton)
      - [Step 8: Incorporate documentation additions into package *using
        devtools::document()*](#step-8-incorporate-documentation-additions-into-package-using-devtoolsdocument)
      - [Repeat Step 5
        *devtools::check()*](#repeat-step-5-devtoolscheck)
      - [Step 9: Create a readme file *with
        usethis::use\_readme\_md()*](#step-9-create-a-readme-file-with-usethisuse_readme_md)
      - [Step 10: Create some relevent tests *with
        usethis::use\_test()*](#step-10-create-some-relevent-tests-with-usethisuse_test)
  - [Beyond 20 minutes](#beyond-20-minutes)
  - [Step 11: Create a license using
    *usethis::use\_\*\_license()*](#step-11-create-a-license-using-usethisuse__license)
  - [Step 12: Build and install package using
    *devtools::build()*](#step-12-build-and-install-package-using-devtoolsbuild)
  - [More topics you might consider
    investigating:](#more-topics-you-might-consider-investigating)
  - [Additional Resources](#additional-resources)
  - [Additional tools used to create this
    guide](#additional-tools-used-to-create-this-guide)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# The {note} package

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of {note} is to play notes using R, based in name (e.g. ‘C’),
length of tone, and octave.

The package was initially conceived [by Jim
Hester](https://resources.rstudio.com/rstudio-conf-2018/you-can-make-a-package-in-20-minutes-jim-hester)
as an example of how to quickly build an R package.

I’ve written a non-immursive companion guide to Jim’s talk here:
<https://evamaerey.github.io/package_in_20_minutes/package_in_20_minutes>

I was extremely lucky to run across his talk when I did. I had promised
to write a package for an RConsortium grant, but I’m not sure that I
would have managed without Jim’s excellent walk-through.

Recently I’ve explored ‘literate package building’ especially from the
README, and have decided to return to Jim’s package to further
experiment with this idea - and because note deserves to be shared\!

    devtools::install_github("musician-tools/note")

# Example

Try in an interactive session:

``` r
library(note)
audio::play(note("A"))
```

But this repo is also meant to be an *imursive* companion guide to Jim
Hester’s, ‘You can make an R package in 20 minutes’.

# Immursive package building experience from the readme

# Introduction

Recently, several R packages have made creating *new* R packages — with
all the required architecture and implementing best practices — much
easier. They include:

  - [**usethis**](https://CRAN.R-project.org/package=usethis): Automate
    Package and Project Setup. Hadley Wickham and Jennifer Bryan (2019).

  - [**devtools**](https://CRAN.R-project.org/package=devtools): Tools
    to Make Developing R Packages Easier. Hadley Wickham, Jim Hester and
    Winston Chang (2019).

  - [**roxygen2**](https://CRAN.R-project.org/package=roxygen2): In-Line
    Documentation for R. Hadley Wickham, Peter Danenberg and Manuel
    Eugster (2018).

Nevertheless, there is quite a lot to learn to make use of them. There
are a number of excellent guides, and [one is by Jim
Hester](https://resources.rstudio.com/rstudio-conf-2018/you-can-make-a-package-in-20-minutes-jim-hester),
which is a *video* presentation recorded at the RStudio 2018 conference.
I like the video format and the title is enticing: “Make an R package in
20 minutes.” In fact, the build is closer to 12 minutes, as Jim takes
some time to motivate the talk. In the end the pace is really pretty
fast. This “companion guide” collects the steps for reference.

In truth, creating your *first* R package will probably take more than
twenty minutes. You might need to pause and rewind the video several
times. You’ll probably stop and mull over what has been created for you
when using the various helpful functions. You might even scrap a first
attempt and begin again from scratch. But the ability to reuse your work
and share your work in an accessible format is so exciting, isn’t it?
Let’s begin\!

## Step 00: Press play *on the video guide*

Your video guide is
[here](https://resources.rstudio.com/rstudio-conf-2018/you-can-make-a-package-in-20-minutes-jim-hester);
the package build starts at about minute 8.

# Part I. Work on functionality

## Step 0: Have some functions that you’d like to package up *like the below from Hester’s ‘note’ example*

If you are interested in creating a package, you have probably already
written some functions. Maybe you’d like to make them available and want
to make them available to the world. So let’s call this Step 0.

For this tutorial you can use Jim Hester’s code reproduced here:

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

# How we write the package…

``` r
usethis::create_package(".")
```

Try it out interactively… (works but wonky\!)

``` r
library(tidyverse)
potter_phrase = tibble::tribble(~name, ~length, ~octave,
"B", .5, 0,
"E", .75, 0,
"G", .25, 0,
"F#", .5, 0,
"E", 1, 0,
"B", .5, 1,
"A", 1.5, 1,
"F#", 1.5, 0)

play_notes_df(potter_phrase, name = name, length = length, octave = octave)

audio::play(c(
note("B", .5, 0),
note("E", .75, 0),
note("G", .25, 0),
note("F#", .5, 0),
note("E", 1, 0),
note("B", .5, 1),
note("A", 1.5, 1),
note("F#", 1.5, 0)
))
```

# time to test

``` r
library(testthat)

test_that("calc frequency works", {
  expect_equal(calc_frequency("A", 0), 440)
  expect_equal(calc_frequency("A", -1), 220)
  
})
```

``` r

usethis::use_package("audio")
usethis::use_mit_license()
```

``` r
usethis::use_testthat()
#> ✔ Setting active project to '/Users/evangelinereynolds/Google Drive/r_packages/note'
#> • Call `use_test()` to initialize a basic test file and open it for editing.
readme2pkg::chunk_to_tests_testthat("calc_frequency")
```

``` r
devtools::check()
devtools::build()
```

# Part II: Package building

## Step 00: Create github repo

## Step

## Step 1: Create package architecture *using usethis::create\_package()*

``` r
usethis::create_package(".")
```

## Step 2: Describe what the package does *in the DESCRIPTION file* in the top level of the package directory

## Step 3: Add your functions *to an R Script saved in the R folder*

``` r
readme2pkg::chunk_to_r("hesterfunctions")
```

So now you have an R script in the R folder.

## Step 4: Make the package “active” *and test your functions interactively with devtools::load\_all()*

Then make the package active using devtools::load\_all() in

``` r
devtools::load_all()
```

(You can also click “build” tab in rstudio, and then “load all”)

Then you can try out a function from your package.

## Step 5: Run a check on the new package *using devtools::check()*

Let’s see if there are any problems in this package.

``` r
devtools::check()
```

Uh-oh\! You will see warnings\! They relate specifying dependencies
which we’ll address below.

## Step 6: Add needed dependencies

### Step 6a. Declare dependencies *using usethis::use\_package(“package\_name”)*

It is not good practice to use library() or require() to use functions
from other package. Instead we first declare dependencies. Using the
function usethis::use\_package will add dependencies to your DESCRIPTION
file.

``` r
usethis::use_package("audio")
#> • Refer to functions with `audio::fun()`
```

Automatically, you should see that the end of the contents of the
DESCRIPTION file has added “audio”:

### Step 6b: Add dependencies and export functions in R script

## Step 7: Document these additions *using a ‘Roxygen’ skeleton*

Highlight the name of the function (‘note’) you want to document.

Then, in RStudio the menu go to:

**code -\> Insert Roxygen skeleton**

This will give you the *skeleton*:

``` r
#' Title
#'
#' @param note 
#' @param length 
#' @param octave 
#' @param volume 
#'
#' @return
#' @export
#'
#' @examples
note <- function(note, length = 1, octave = 0, volume = default_volume) {
  frequency <- calc_frequency(note, octave)
  volume <- calc_volume(volume)
  length <- calc_length(rate, length, bpm)
  multiplier <- calc_multiplier(rate)
  res <- sin(frequency * multiplier * length) * volume
  structure(res, class = "note")
}
```

Filling in the *skeleton*:

``` r
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
note <- function(note, length = 1, octave = 0, volume = default_volume) {
  frequency <- calc_frequency(note, octave)
  volume <- calc_volume(volume)
  length <- calc_length(rate, length, bpm)
  multiplier <- calc_multiplier(rate)
  res <- sin(frequency * multiplier * length) * volume
  structure(res, class = "note")
}
```

## Step 8: Incorporate documentation additions into package *using devtools::document()*

``` r
devtools::document()  
```

or *build -\> document* to make Roxygen comments part of your package.

You’ll notice the this creates a new subdirectory (man) and documents
related to the exported functions.

``` r
.
├── DESCRIPTION
├── NAMESPACE
├── R
│   └── note.R
├── man
│   ├── note.Rd
│   └── reexports.Rd
└── note.Rproj
```

## Repeat Step 5 *devtools::check()*

Again check you package. You might do this often when you are creating
your own package.

``` r
devtools::check() 
```

## Step 9: Create a readme file *with usethis::use\_readme\_md()*

Help your users understand the package with a readme.

``` r
usethis::use_readme_md()
```

You’ll now see README.md among your files, which you can open and edit.

``` r
.
├── DESCRIPTION
├── NAMESPACE
├── R
│   └── note.R
├── README.md
├── man
│   ├── note.Rd
│   └── reexports.Rd
└── note.Rproj
```

## Step 10: Create some relevent tests *with usethis::use\_test()*

Creates testing infrastructure. Think of some things that should be true
with given inputs in a function. Use the testthat::test\_that() f

``` r
usethis::use_test()
```

You’ll see a new folder dedicated to testing the package functions.

``` r
.
├── DESCRIPTION
├── NAMESPACE
├── R
│   └── note.R
├── README.md
├── man
│   ├── note.Rd
│   └── reexports.Rd
├── note.Rproj
└── tests
    ├── testthat
    └── testthat.R
```

Open testthat.R and do what Jim does\!

-----

# Beyond 20 minutes

Now I’m drawing mostly from [Karl Browman’s primer on building
packages](https://kbroman.org/pkg_primer/).

# Step 11: Create a license using *usethis::use\_\*\_license()*

You might see a warning if you run a check at this point, about having
no license. So consider adding one.

For example:

``` r
usethis::use_mit_license(name = "Your Name, Reader")  # replace "Your Name, Reader" with *your name*
```

# Step 12: Build and install package using *devtools::build()*

``` r
devtools::build()
```

This builds a tar.gz, folder which is your compressed package\!

“\~/Google Drive/note\_0.0.0.9000.tar.gz”

``` r
library(note)
```

-----

# More topics you might consider investigating:

  - What is github actions?
  - What about the coverage report?
  - What about use\_rcpp?

<!-- end list -->

``` r
usethis::use_travis()
usethis::use_coverage()
usethis::use_rcpp()
```

A good starting point for these topics are the resources listed below.

# Additional Resources

Some great additional resources are the following:

  - [Hadley Wickham and Jennifer Bryan’s *R Packages: Chapter II ‘The
    Whole Game’*](https://r-pkgs.org/whole-game.html)
  - [Hillary Parker’s *Writing an R package from
    scratch*](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
  - [Karl Browman’s *R package primer*](https://kbroman.org/pkg_primer/)

# Additional tools used to create this guide

R Core Team (2019). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria. URL
<https://www.R-project.org/>.

Jim Hester and Hadley Wickham (2019). fs: Cross-Platform File System
Operations Based on ‘libuv’. R package version 1.3.1.
<https://CRAN.R-project.org/package=fs>

JJ Allaire and Yihui Xie and Jonathan McPherson and Javier Luraschi and
Kevin Ushey and Aron Atkins and Hadley Wickham and Joe Cheng and Winston
Chang and Richard Iannone (2019). rmarkdown: Dynamic Documents for R. R
package version 1.14. URL <https://rmarkdown.rstudio.com>.

Yihui Xie and J.J. Allaire and Garrett Grolemund (2018). R Markdown: The
Definitive Guide. Chapman and Hall/CRC. ISBN 9781138359338. URL
<https://bookdown.org/yihui/rmarkdown>.
