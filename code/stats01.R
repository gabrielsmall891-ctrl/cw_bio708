#Descriptive Statistics
#call every time

library(tidyverse)
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)


# Central Tendencies ------------------------------------------------------

#arithmatic mean
mu_x <- sum(x) / length(x)#or
mean(x)

mu_y <- sum(y) / length(y)#or
mean(y)

#geometric mean
gm_x <- prod(x)^(1/length(x))

exp(sum(log(x))/length(x))

gm_y <- exp(sum(log(y))/length(y))

#median
med_x <- median(x)
med_y <- median(y)


# Variance ------------------------------------------------------

#variance
var_x <- sum((x - mean(x))^2) / length(x)
var_y <- sum((y - mean(y))^2) / length(y)

#stdev

sqrt(var_x)
sqrt(var_y)

#inter quantile range
x_l <- quantile(x, 0.25)
x_u <- quantile(x, 0,75)
iqr_x <- (x_u - x_l)

y_q <- quantile(y, c(.025, 0.75))
iqr_y <- y_q[2] - y_q[1]
