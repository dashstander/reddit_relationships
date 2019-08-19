source('R/lib_clean_data.R')
library(lubridate)

min_year <- 2014
max_year <- 2019
relationship_flair_texts <- c('Relationships', 'Breakups', 'Dating', 'Infidelity')
platonic_indicative <- c('grandmother', 'grandather', 'grandpa', 'grandma', 'father', 'mother', 'mom', 'dad', 'aunt', 'uncle', ' fil ', ' mil ', 'landlord')

# Some amount of 
relationships_posts_raw <- read_all_data('raw', min_year = 2014,
                                         columns = c('id', 'created_utc', 'author', 'score', 'link_flair_text', 'title', 'selftext'))


relationships_parsed_age_and_gender <- relationships_posts_raw %>% 
  filter(link_flair_text %in% relationship_flair_texts) %>%
  filter(!str_detect(tolower(title), paste(platonic_indicative, collapse='|'))) %>%
  produce_age_and_gender_df() %>%
  mutate(
    created_utc = as_datetime(created_utc, origin = '1970-01-01'),
    is_queer = younger_partner_gender == older_partner_gender,
    age_diff = older_partner_age - younger_partner_age)


for (post_year in min_year:max_year) {
  relationships_parsed_age_and_gender %>% 
    filter(year(created_utc) == post_year) %>%
    arrange(created_utc) %>%
    write_csv(sprintf('./data/clean/relationships_cleaned_%s.csv', as.character(post_year)))
}
