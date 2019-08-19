source('R/lib_clean_data.R')
library(dplyr)
library(ggplot2)
library(purrr)
relationships_clean <- read_all_data(stage='clean')


get_partner_ages <- function(df, analysis_gender = c('M', 'F'), queer = FALSE) {
  analysis_gender <- match.arg(analysis_gender)
  df_orientation <- df %>% dplyr::filter(is_queer == queer)
  
  younger <- df_orientation %>% 
    dplyr::filter(younger_partner_gender == analysis_gender) %>% 
    dplyr::rename(
      flair = link_flair_text,
      gender = younger_partner_gender,
      age = younger_partner_age,
      partner_age = older_partner_age,
      partner_gender = older_partner_gender) %>%
    dplyr::select(flair, score, age, gender, partner_age, partner_gender)
  
  older <- df_orientation %>% 
    dplyr::filter(older_partner_gender == analysis_gender) %>%
    dplyr::rename(
      flair = link_flair_text,
      gender = older_partner_gender,
      age = older_partner_age,
      partner_age = younger_partner_age,
      partner_gender = younger_partner_gender) %>%
    dplyr::select(flair, score, age, gender, partner_age, partner_gender)
  
  rbind(younger, older)
}


make_summary <- function(df) {
  df <- df %>%
    mutate(gender = factor(gender) %>%
    group_by(gender, age) %>% 
    summarize(n = n(),
              med_partner_age = median(partner_age),
              avg_partner_age = mean(partner_age), 
              sd_partner_age = sd(partner_age), 
              bottom_iqr = quantile(partner_age, 0.25), 
              top_iqr = quantile(partner_age, 0.75), 
              avg_score = mean(score)) %>% 
    arrange(age)
    )
   
  levels(df$gender) <- c("Women", "Men")
  
  df
}


straight_ages <- bind_rows(map(c('M', 'F'), ~get_partner_ages(relationships_clean, analysis_gender=.x, queer = FALSE)))
queer_ages <- bind_rows(map(c('M', 'F'), ~get_partner_ages(relationships_clean, analysis_gender = .x, queer=TRUE)))

grouped_ages_queer = make_summary(queer_ages)
grouped_ages_straight = make_summary(straight_ages)

