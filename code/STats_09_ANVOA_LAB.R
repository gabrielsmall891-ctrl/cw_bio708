#ANOVA LAB

pacman::p_load(tidyverse, pwr)
rm(list = ls())

df_plnt <- PlantGrowth

df_plnt %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency


fit <- aov(weight ~ group,
    data = df_plnt)
summary(fit)

#Values to be reported are the degrees of freedom the f statitic and the pvalue

## Power Test

pwr::pwr.anova.test(k = 3,
                    n = NULL,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 6,
                    n = NULL,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 3,
                    n = NULL,
                    f = 0.9,
                    sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 3,
                    n = NULL,
                    f = 0.5,
                    sig.level = 0.001,
                    power = 0.8)

pwr::pwr.anova.test(k = 3,
                    n = NULL,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.6)
