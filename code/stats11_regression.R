#regression

rm(list = ls())
pacman::p_load(tidyverse)

df_algae <- read_csv("data_src/data_algae.csv")

df_algae %>% 
ggplot(aes(x = conductivity,
           y = biomass)) +
  geom_point()


m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) # draw the line

# extract coefficients
theta <- coef(m)
se <- sqrt(diag(vcov(m)))

# t-value
t_value <- theta / se

df_algae <- df_algae %>% 
  mutate(eps = eps)

# pull vector data
v_x <- df_algae %>% pull(conductivity)
v_y <- df_algae %>% pull(biomass)

error <- v_y - (theta[1] + theta[2] * v_x)

head(cbind(eps, error))


# round values at 5 decimal
eps <- round(eps, 5)
error <- round(error, 5)

all(eps == error)

df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) + 
  geom_segment(aes(x = conductivity, # start-coord x
                   xend = conductivity, # end-coord x
                   y = biomass, # start-coord y
                   yend = biomass - eps), # end-coord y
               linetype = "dashed")