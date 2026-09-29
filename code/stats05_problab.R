rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)

#standard distribution

gabe <- rnorm(50, mean = 12, sd= 2.6) #create data

mu <- mean(gabe)
sigma <- sd(gabe)
pd <- dnorm(gabe, mean = mu, sd = sigma)

xmin <- floor(min(gabe)) #minimum values
xmax <- ceiling(max(gabe)) #max values
bin <- seq(xmin, xmax, by = 1) #bin width 1

p <- NULL #empty for prob
for (i in 1:(length(bin) -1)){
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}

gabe_prob <- tibble(p = p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * length(gabe))

df_gabe <- tibble(gabe)

df_gabe %>% 
  ggplot(aes(x = gabe)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # bin's center position
  geom_point(data = gabe_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = gabe_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")



# POISOSN -----------------------------------------------------------------



karla <- rpois(1000, lambda = 10)

lambda <- mean(karla)
bin <- seq(min(karla), max(karla), by = 1)

pm <- dpois(x = bin, lambda = lambda)

df_karla <- tibble(karla = karla)

df_prob <- tibble(pm = pm, bin = bin) %>% 
  mutate(freq = pm * nrow(df_karla))

df_karla %>% 
  ggplot(aes(x = karla))+
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = bin,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = bin,
                 y = freq))