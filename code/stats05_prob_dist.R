rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)
#probability distribution

# load csv data on R
df_h0 <- read_csv("data_src/data_plant_height.csv")

df_h0 %>% 
  ggplot(aes(x = height)) +
  geom_histogram(
    binwidth = 1,
    center = 0.5
  )+ # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

## draw probability distribution

x <- seq(min(df_h0$height),
    max(df_h0$height),
    length = 100)

mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

tibble(y = pd,
       x = x) %>% 
  ggplot(aes(x = x,
             y = y)
         ) + 
  geom_line() +
  labs(y = "Probability Density",
       x = "Plant Height")

#convert prob dense to frequency

p10 <- pnorm(q = 10, mean = mu, sd = sigma)
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
p10_20 <- p20 - p10
x_min <- floor(min(df_h0$height))
x_max <- ceiling(max(df_h0$height))
bin <- seq(x_min, x_max, by = 1)

p <- NULL
for(i in 1:(length(bin) - 1)) {
  p_up <- pnorm(bin[i+1], mean = mu, sd = sigma)
  p_low <- pnorm(bin[i], mean = mu, sd = sigma)
  p[i] <- p_up - p_low
}

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

#combine data and pdf
df_h0 %>% 
  ggplot(aes(x = height))+
  geom_histogram(
    bin = 1,
    center = 0.5
  ) + 
  geom_point(
    data = df_prob,
    aes(x = bin,
        y = freq),
    (color = "salmon")+
      geom_line(data = df_prob,
                aes(y = freq,
                    x = bin),
                color = "salmon")
  )

