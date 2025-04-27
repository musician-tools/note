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
