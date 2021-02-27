library(tidyverse)
library(dplyr)

## Data Collection
lifeExp_data <- read_csv("data/Life Expectancy Data.csv")
continent_data <- read_csv("data/Countries-Continents.csv")


## Data Preparation
whole_data <- left_join(lifeExp_data, continent_data, by = c("Country"="Country"), na = c("NA", "N/A"))

names(whole_data)[names(whole_data) == "GDP"] <- "GDP_per_capita"

whole_data <- whole_data %>% 
  filter(`Life expectancy` != 0 & GDP_per_capita != 0)

whole_data <- whole_data %>% 
  mutate(GDP = GDP_per_capita * Population)

## Visual Analysis
### Histogram for Life Expectancy in All Countries
ggplot(whole_data, aes(x = `Life expectancy`)) +
  geom_histogram() +
  labs(title = "Life Expectancy in All Countries")

### Histogram for GDP per capita in All Countries
whole_data %>% 
  drop_na() %>%
  filter(GDP_per_capita != 0) %>%
  ggplot(aes(x = GDP_per_capita)) +
  geom_histogram() +
  labs(title = "GDP per capita in All Countries")

### Scatter Plot between Life Expectancy and GDP per capita in All Countries
ggplot(whole_data, aes(x = GDP_per_capita, y = `Life expectancy`)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Life Expectancy and GDP per capita in All Countries")

### Boxplot for Life Expectancy in Developing and Developed Countries
ggplot(whole_data, aes(x = Status, y = `Life expectancy`, color = Status)) + 
  geom_boxplot() + 
  labs(title = "Boxplot for Life Expectancy in Developing and Developed Countries")

### Scatter Plot between Life Expectancy and GDP in Developing Countries
developing_data <- filter(whole_data, Status == 'Developing')
ggplot(developing_data, aes(x = GDP_per_capita, y = `Life expectancy`)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "red") + 
  guides(color = FALSE) + 
  labs(title = "GDP per capita and Life Expectancy in Developing Countries")

### Scatter Plot between Life Expectancy and GDP per capita in Developed Countries
developed_data <- filter(whole_data, Status == 'Developed')
ggplot(developed_data, aes(x = GDP_per_capita, y = `Life expectancy`)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "red") + 
  guides(color = FALSE) +
  labs(title = "GDP per capita and Life Expectancy in Developed Countries")





