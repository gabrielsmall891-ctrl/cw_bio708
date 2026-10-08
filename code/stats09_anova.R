#more than 2 groups

pacman::p_load(tidyverse)
rm(list = ls())

df_anova <- read_csv("data_src/data_fish_length_anova.csv")
distinct(df_anova, lake)

# geom_violin() - function for violin plots
# geom_jitter() - jittered points

df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency


#anova

aov(length ~ lake,
    data = df_anova)

##overall mean
mu <- mean(df_anova$length)

#group specific means
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length),
                        dev_g = (mu_g - mu)^2,
                        n = n())

df_g %>% 
  mutate(ss_g = dev_g * n) %>% 
  pull(ss_g) %>% 
  sum()

ss_b <- df_g %>% 
  mutate(ss_g = dev_g *n) %>% 
  pull(ss_g) %>% 
  sum()

#withingroup
ss_w <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) %>%  # deviation from group mean for each fish
  pull(dev_i) %>% 
  sum()

#overall variability
ss_o <- sum((df_anova$length - mu)^2)

summary(aov(length ~ lake,
    data = df_anova))


#convert variability to variance

sig_b <- ss_b / 2
sig_w <- ss_w / (nrow(df_anova) - n_distinct(df_anova$lake))

f_value <- sig_b / sig_w
