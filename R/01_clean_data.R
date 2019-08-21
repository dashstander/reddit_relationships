source('R/lib_process_data.R')
library(lubridate)

min_year <- 2012
max_year <- 2019
# relationship_flair_texts <- c('Relationships', 'Breakups', 'Dating', 'Infidelity')
platonic_words <- c('grandmother', 'grandather', 'grandpa', 'grandma', 'father', 'mother', 
                    'mom', 'dad', 'aunt', 'uncle', ' fil', ' mil', '^mil', '^fil',  'landlord', 'stepmother', 'stepfather',
                    'stepdad', 'stepmom', 'brother', 'sister', 'son', 'daughter', 'boss', 'customer',
                    'gramma', 'mum', 'fmil', 'therapist', 'coworker', 'co-worker', 'manager',
                    'employer', 'nana', 'cousin', 'papa', 'housemate', 'roommate', 'neighbor', 'parent',
                    'tenant', 'student', 'parents', 'child', 'children', 'landlady', 'nanna', 'granny', 'housekeeper', 
                    'maid', 'homeowner', 'niece', 'nephew', 'sponsor', 'neighbour', 'neighbours', 'colleague',
                    'bully', 'roomate', ' dil', 'non-romantic', 'widow', 'supervisor', 'family friend', 'baba')
#non_platonic_words <- c('boyfriend', 'girlfriend', 'husband', 'wife', 'partner', 
#                        'spouse', 'bf', 'gf', 'fwb', 'love interest', 'love-interest', 
#                        'hook up', 'hookup', 'hooking up', 'long distance', 'relationship', 'ex',
#                        'date', 'dating', 'love', 'ending things', 'breaking up', 'break up', 'end things',
#                        'so', 'cheat', 'cheating', 'cheater', 'fiance', 'fiancee', 'marry', 'married',
#                        'engagement', 'engaged', 'propose', 'crush', 'attracted', 'ldr', )
# Some amount of 
relationships_posts_raw <- read_all_data('raw', min_year = min_year,
                                         columns = c('id', 'created_utc', 'author', 'score', 'title', 'selftext'))


relationships_parsed_age_and_gender <- relationships_posts_raw %>% 
  #filter(str_detect(tolower(title), paste(non_platonic_words, collapse='|'))) %>%
  filter(!str_detect(tolower(title), paste(platonic_words, collapse='|'))) %>%
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
