library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)

read_data <- function(fp, ...) {
  
  print(fp)
  read_csv(fp) %>% select(...)
}


read_all_data <- function(stage=c('raw', 'clean'), min_year=2010, ...) {
  stage = match.arg(stage)
  
  files <- list.files(sprintf('./data/%s', stage), full.names = TRUE)
  
  years <- as.numeric(str_match(files, '(201[0-9]).csv$')[, 2])
  
  dplyr::bind_rows(lapply(files[years>=min_year], function(x) read_data(x, ...)))
}



get_ages_and_genders <- function(titles) {
  pattern <- "[\\{\\[\\(][0-9]{0,2}[ ,/]{0,1}[mfMF][ ,/]{0,1}[0-9]{0,2}[\\)\\]\\}]"
  
  # str_extract_all returns an n x m matrix where n == length(titles) and m == max(number of gender tags)
  # columns where there are fewer than m matches have "" as the value after their last match
  # apply-ing str_remove_all goes row by row getting rid of all of the extraneous punctuation
  ages_and_genders = map(str_extract_all(titles, pattern), 
                           function(x) str_remove_all(x, "[\\[\\]\\(\\)\\{\\} ,/]"))
  index = which(map_int(ages_and_genders, length) == 2)

  list(idx = index, age_tags = ages_and_genders)
}


order_age_and_gender <- function(age_tags) {
  
  # For each pair of age tags gets a list: element `x` with the sorted ages
  # and element `idx` with the indices of the ages sorted by value
  age_and_index <- map(age_tags, 
                function(x) sort.int(as.numeric(str_extract(x, "[0-9]{1,3}")),
                                     index.return = TRUE))
  # Just extracts the genders
  genders <- map(age_tags,
                 function(x) str_extract(x, "[mfMF]{1}"))
  
  
  # Since the ages are sorted just need to pull out the first or second one to get younger/older
  younger_partner_ages = map_dbl(age_and_index, ~.x[['x']][1])
  older_partner_ages = map_dbl(age_and_index, ~.x[['x']][2])
  
  # Slightly more involved. For each title get the genders, then sort them with the indices from above. Then get
  # the younger / older.
  younger_partner_genders = map_chr(seq_along(genders),
                                    ~genders[[.x]][age_and_index[[.x]][['ix']]][1])
  older_partner_genders = map_chr(seq_along(genders),
                               ~genders[[.x]][age_and_index[[.x]][['ix']]][2])
  
  data.frame('younger_partner_age' = younger_partner_ages,
             'older_partner_age' = older_partner_ages,
             'younger_partner_gender' = toupper(younger_partner_genders),
             'older_partner_gender' = toupper(older_partner_genders))
}



produce_age_and_gender_df <- function(df) {
  
  age_tags = get_ages_and_genders(df$title)
  
  df = df[age_tags[['idx']], ]
  
  age_and_gender_df = order_age_and_gender(age_tags[['age_tags']][age_tags[['idx']]])
  cbind(df, age_and_gender_df)
}






