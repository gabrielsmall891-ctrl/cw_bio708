#Sampling

# 10 indv samples ---------------------------------------------------------


rm(list = ls())
pacman::p_load(tidyverse, patchwork)

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)
df_h1 <- tibble(plant_id = 1:length(h),
       height = h,
       unit = "cm")

df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)

h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


# 1000 Samples ------------------------------------------------------------



df_h0 <- read_csv("data_src/data_plant_height.csv")

mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)
print(sigma2)

##for_loop
mu_i <- var_i <- NULL

for(i in 1:1000) {
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
  
  mu_i[i] <- mean(df_i$height)
  
  var_i[i] <- sum((df_i$height - mu_i[i])^2 / nrow(df_i))
}

#stuff inside

df_i <- df_h0 %>% 
  sample_n(size = 10)


  mu_i <- mean(df_i$height)

var_i <- sum((df_i$height - mu_i)^2 / nrow(df_i))

#visualization
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i)

g_mu <- df_sample %>% 
  ggplot(aes(x = mu_i)) +
  geom_histogram() +
  geom_vline(xintercept = mu)


g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

# PATCHWORK ---------------------------------------------------------------
#combines multiple figures into one

g_hor <- g_mu + g_var
g_ver <- g_mu / g_var

##for loop

var_ub_i <- NULL

for (i in 1:1000) {
  ##randomly sample 10 individuals but runs only one time
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
  ##variance for subset
  var_ub_i[i] <- var(df_i$height)  
}

df_sample <- df_sample %>% 
  mutate(var_ub_hat = var_ub_i[i])

g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)


gvar_ub_hor <- g_var_ub + g_var
gvar_ub_ver <- g_var_ub / g_var

#Dr.Terui's ver of patchwork
g_var_Terui_vert <- g_mu / g_var / g_var_ub
