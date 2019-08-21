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
      gender = younger_partner_gender,
      age = younger_partner_age,
      partner_age = older_partner_age,
      partner_gender = older_partner_gender) %>%
    dplyr::select(score, age, gender, partner_age, partner_gender)
  
  older <- df_orientation %>% 
    dplyr::filter(older_partner_gender == analysis_gender) %>%
    dplyr::rename(
      gender = older_partner_gender,
      age = older_partner_age,
      partner_age = younger_partner_age,
      partner_gender = younger_partner_gender) %>%
    dplyr::select(score, age, gender, partner_age, partner_gender)
  
  rbind(younger, older)
}


make_summary <- function(df) {
  df <- df %>%
    mutate(gender = recode(gender, `F` = "Women", M = 'Men')) %>%
    group_by(gender, age) %>% 
    summarize(n = n(),
              med_partner_age = median(partner_age),
              avg_partner_age = mean(partner_age), 
              sd_partner_age = sd(partner_age), 
              bottom_iqr = quantile(partner_age, 0.25), 
              top_iqr = quantile(partner_age, 0.75), 
              avg_score = mean(score)) %>% 
    arrange(age)

  df
}


straight_ages <- bind_rows(map(c('M', 'F'), ~get_partner_ages(relationships_clean, analysis_gender=.x, queer = FALSE)))
queer_ages <- bind_rows(map(c('M', 'F'), ~get_partner_ages(relationships_clean, analysis_gender = .x, queer=TRUE)))

grouped_ages_queer = make_summary(queer_ages)
grouped_ages_straight = make_summary(straight_ages)


straight_plot <- ggplot(grouped_ages_straight %>% 
                          filter(age >= 15, age <= 60), 
                        aes(x = age, 
                            y = med_partner_age, 
                            ymin=bottom_iqr,
                            ymax=top_iqr)) + 
  geom_point(size=3) + 
  geom_errorbar() + 
  geom_abline(slope=1, intercept=0) + 
  xlim(10, 60) + ylim(10, 60) + 
  facet_wrap(~gender) + theme_minimal() +
  labs(x = "Age (years)", y = "Median age of romantic partner (years)", 
       title = "Age of men and women vs. age of their partners", 
       subtitle = 'Straight couples in /r/relationships from 2012-2019',
       caption = "Source: pushshift.io")


queer_plot <- ggplot(grouped_ages_queer %>% 
                       filter(age >= 18, age <= 60), 
                     aes(x = age,
                         y = med_partner_age, 
                         ymin=bottom_iqr,
                         ymax=top_iqr,
                         color=gender)) + 
  geom_point(size=3) + 
  geom_errorbar() + 
  geom_abline(slope=1, intercept=0) + 
  xlim(10, 60) + ylim(10, 60) + 
  facet_wrap(~gender) + theme_minimal() +
  labs(x = "Age", y = "Median age of romantic partner", 
       title = "There are not very many older queer couples asking for advice on reddit", 
       subtitle = 'Queer couples in /r/relationships from 2012-2019')
queer_plot
