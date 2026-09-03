library(tidyverse) #call every single time

# point figure ------------------------------------------------------------


##point figure error no aes()
# iris %>% 
#   ggplot(
#     x = Sepal.Length,
#     y = Sepal.Width)
#   ) +
#   geom_point()


 iris %>% 
     ggplot(
         mapping = aes(x = Sepal.Length,
                       y = Sepal.Width)
       ) +
     geom_point()

##color by species

iris %>% 
  ggplot(
    aes(x = Sepal.Length,
         y = Sepal.Width,
         color = Species)
  )+
  geom_point()

ggplot(data = iris,
       mapping = aes(x = Sepal.Length,
                     y = Sepal.Width)
       ) +
  geom_point()

##error, outside of aes

iris %>% 
  ggplot(
    aes(x = Sepal.Length,
        y = Sepal.Width),
        color = Species
  )+
  geom_point()


# Line --------------------------------------------------------------------

df0 <- tibble(
  x = rep(1:50, 3),
  y = x * 2
)

df0 %>% 
  ggplot(
    mapping = aes(x = x,
                  y = y)
  ) +
  geom_line()


# Histogram ---------------------------------------------------------------

iris %>% 
  ggplot(mapping = aes(x=Sepal.Length)) +
  geom_histogram()


# boxplot -----------------------------------------------------------------


#color changes outside

iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  color = Species)
  ) +
  geom_boxplot()

#fill changes inside

iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  fill = Species)
  ) +
  geom_boxplot()

       
