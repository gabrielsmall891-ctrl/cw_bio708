rm(list = ls())
pacman::p_load(tidyverse)

#for whole dataset

iris_tbl <- iris

m <- lm(Sepal.Width ~ Petal.Width,
        data = iris_tbl)

summary(m)

#setosa

iris_seto <- iris %>% 
  filter(Species == "setosa")

seto_m <- lm(Sepal.Width ~ Petal.Width,
             data = iris_seto)

summary(seto_m)

#virginica

iris_virg <- iris %>% 
  filter(Species == "virginica")

virg_m <- lm(Sepal.Width ~ Petal.Width,
             data = iris_virg)

summary(virg_m)

#versicolor

iris_vers <- iris %>% 
  filter(Species == "versicolor")

vers_m <- lm(Sepal.Width ~ Petal.Width,
             data = iris_vers)

summary(vers_m)

#PART 2

#virginica

virg_m2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
             data = iris_virg)

summary(virg_m2)

#versicolor

vers_m2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
              data = iris_vers)

summary(vers_m2)

#setosa

seto_m2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
              data = iris_seto)

summary(seto_m2)


iris_x <- iris %>% 
  mutate(useless = rnorm(150))


