library(tidyverse) # call add-in packages everytime you open new R session
df_fl <- read_csv("data_src/data_fish_length.csv")
print(df_fl)

unique(df_fl$lake)
distinct(df_fl, lake)

##mean and sd
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_1 = mean(length),
            sd_1 = sd(length)
            )
#figure
df_fl %>% 
  ggplot(
    aes(x = lake,
        y = length)
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0,
    alpha = 0.25
  )+
  geom_segment(
    data = df_fl_mu,
    aes(
      x = lake,
      y = mu_1 - sd_1,
      yend = mu_1 + sd_1
    )
  )

#t-test
x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)


t.test(x,y, var.equal = TRUE)

# get t-value
v_mu <- df_fl_mu %>% 
  pull(mu_1)

v_mu[1] - v_mu[2]

df_t <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_1 = mean(length),
            var_1 = var(length),
            n = n()
            )


v_mu <- pull(df_t, mu_1)
v_var <- pull(df_t, var_1)
v_n <- pull(df_t, n)

var_p <- ((v_n[1] - 1) / (sum(v_n) - 2))* v_var[1] +
  ((v_n[2] - 1) / (sum(v_n) - 2))* v_var[2]

t_value <- ((v_mu[1] - v_mu[2]) / sqrt(var_p * ((1/ v_n[1]) + (1 / v_n[2]))))
