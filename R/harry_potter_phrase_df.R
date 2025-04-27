twinkle_phrase_df <- tibble::tribble(~drm,~duration,
        "d", 1, 
         "d", 1, 
         "s", 1, 
         "s", 1, 
         "l", 1, 
         "l", 1, 
         "s", 2) 

usethis::use_data(twinkle_phrase_df, overwrite = T)
