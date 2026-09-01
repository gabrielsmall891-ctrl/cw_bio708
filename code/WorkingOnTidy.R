library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

#Piping

df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)

print(df_vir_sl)

#becomes

df_vir_sl <- iris_sub %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)

print(df_vir_sl)

# 5.3.2 exercise
filter(iris_sub, Species == "setosa") %>% 
  mutate(pw_two_times = 2 * Sepal.Width)

##Working on SWIRL as I'm confused


Gabe

install.packages("tidyverse")

midwest %>%
  filter(state == "IN") %>% 
  select(-PID, -area, -popdensity)

midwest %>%
  filter(state == "IN") %>% 
  select(-PID, -area, -popdensity) %>% 
  arrange(desc(poptotal))
