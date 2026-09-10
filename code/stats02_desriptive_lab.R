#descriptive stats lab

library(tidyverse)

# central tendencies ------------------------------------------------------

z <- c((exp(rnorm(n = 100, mean = 0, sd = 0.1))))

#arithmean
mean(z)

#geomean
prod(z)^(1/length(z))

#median
median(z)

z_tbl <- tibble(z)

z_tbl %>% 
  ggplot(aes(x = z)
         ) +
geom_histogram() +
  geom_vline(xintercept = 1.002, color = "yellow")+
  geom_vline(xintercept = 0.997, color = "white")+
  geom_vline(xintercept = 0.993, color = "black")

z_rev <- -z + max(z) + 0.5

##### SECOND TIME ######

#arithmean
z_rev_meana <- mean(z_rev)

#geomean
z_rev_meang <- prod(z_rev)^(1/length(z_rev))

#median
z_rev_med <- median(z_rev)

z_revtbl <- tibble(z_rev)

z_revtbl %>% 
  ggplot(aes(x = z_rev)
  ) +
  geom_histogram() +
  geom_vline(xintercept = z_rev_meana, color = "steelblue")+
  geom_vline(xintercept = z_rev_meang, color = "salmon")+
  geom_vline(xintercept = z_rev_med, color = "yellow")

# variation measures ------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

m <- c(w*1000)

#Variance

var_w <- sum((w - mean(w))^2) / length(w)
var_m <- sum((m - mean(m))^2) / length(m)

#stdev

sd_w <- sqrt(var_w)

sd_m <- sqrt(var_m)

#MAD

ad_w <- abs(w - median(w))
mad_w <- median(ad_w)

ad_m <- abs(m - median(m))
mad_m <- median(ad_m)

#CV

cv_w <- sd_w / mean(w) 

cv_m <- sd_m / mean(m)

#MAD/Median

mm_w <- mad_w / median(w)
mm_m <- mad_m / median(m)
