#linear Model 

rm(list = ls())
pacman::p_load(tidyverse)

#categorical

df_fl <- read.csv("data_src/data_fish_length.csv")

m_fl <- lm(length ~ lake, 
        data = df_fl)
summary(m_fl)

#calculate mean and take difference

v_mu <- df_fl %>% 
  group_by(lake) %>%
  summarize(mu = mean(length)) %>% 
  pull(mu)
v_mu[2] - v_mu[1]

df_anova <- read_csv("data_src/data_fish_length_anova.csv")
print(df_anova)


v_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

print(c(v_mu[1], 
        v_mu[2] - v_mu[1], 
        v_mu[3] - v_mu[1]))

m <- lm(length ~ lake,
        data = df_anova)

summary(m)

m_aov <- aov(length ~ lake,
             data = df_anova)

summary(m_aov)

df_anova %>% 
  mutate(lake_factor = factor(lake, levels = c("c", "a", "b")))

#continuous


#mix of continuous and categorical in predictors
