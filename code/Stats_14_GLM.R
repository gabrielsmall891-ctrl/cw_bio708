rm(list = ls())
pacman::p_load(tidyverse)

##read data
df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)


df_count %>% 
  ggplot(aes(x = nitrate,
             y = count))+
  geom_point()

m_lm <- lm(count ~ nitrate, data = df_count)

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count))+
  geom_point() +
  geom_abline(intercept = coef(m_lm)[1], 
              slope = coef(m_lm)[2])


##alternative to ordinary
##"poisson regression for count data"

m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")
summary (m_pois)

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count))+
  geom_point() +
  geom_abline(intercept = coef(m_pois)[1], 
              slope = coef(m_pois)[2])
##link function log(lambda) = alpha * x for poisson

x <- seq(min(df_count$nitrate),
         max(df_count$nitrate),
         length = 100)

a <- coef(m_pois)[1]
b <- coef(m_pois)[2]

log_y_pred <- a + b * x
y_pred <- exp(log_y_pred)

df_pred <- tibble(y_pred = y_pred,
                  nitrate = x)

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count))+
  geom_point()+
  geom_line(data = df_pred,
            aes(y = y_pred))

#compare results between lm() and glm()
summary(m_lm)
summary(m_pois)
