
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

#Filter
filter(iris_sub, Species == "virginica")

filter(iris_sub, Species != "virginica")

filter(iris_sub, Species %in% c("virginica", "versicolor"))

filter(iris_sub, Species == "virginica" | Species == "versicolor")

arrange (iris_sub, desc(Sepal.Width))

iris_3 <- filter(iris_sub, Sepal.Width > 3.0)

iris_setosa <- filter(iris_sub, Species == "setosa")

iris_3_setosa <- filter (iris_sub, Sepal.Width > 3.0 & Species == "setosa")


#select
select(iris_sub, c(Sepal.Length, Sepal.Width)) #select several columns
select(iris_sub, -Sepal.Length) #select all but certain columns
select(iris_sub, starts_with("Sepal"))

#mutate
(x_max <- nrow(iris_sub))
x<- 1:x_max
mutate(iris_sub, row_id = x)

#twice sepal length and add as new column

mutate(iris_sub, sl_two_times = 2 * Sepal.Length)

mutate(iris_sub, id_name = paste(Species, Sepal.Length))

iris_pw <- select(iris_sub, Species, Petal.Width)
iris_petal <- select(iris_sub, starts_with("Petal"))
pw_two_times <- mutate(iris_sub, pw_two_times = 2 * Petal.Width)

#pipe
df_vir <- filter(iris_sub, Species == "virginica")

iris_sub %>% 
  filter(Species == "virginica")

#exercise for pipe
select(filter(iris_sub, Species == "virginica"), Sepal.Length) 

iris_sub %>%
  group_by(Species)%>%
  mutate(mu_s1 = mean(Sepal.Length))%>%
  ungroup()
#need to ungroup to prevent errors 

#summarize
iris_sub%>%
  group_by(Species)%>%
  summarize(mu_s1 = mean(Sepal.Length))
  