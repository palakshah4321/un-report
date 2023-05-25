library(tidyverse)

#read in data

gapminder_data <- read.csv("data/gapminder_data.csv")

#what is mean life expectancy
#summarize()

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

#mean population in gapminder dataset
summarize(gapminder_data, averagePopulation = mean(pop))

gapminder_data %>% 
  summarize(meanPop = mean(pop))

#mean pop and lifeexp
gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp),
            meanPop = mean(pop))

#filter and max for mean life expectancy for most recent year
gapminder_data %>% 
  summarize(MaxYear = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(meanLifeExp = mean(lifeExp))

gapminder_data %>% 
  filter (year == max(year)) %>% 
  summarize(meanLifeExp = mean(lifeExp))

#mean gdp per capita for first/earliest year?
gapminder_data %>% 
  filter (year == min(year)) %>% 
  summarize(meangfppercapita = mean(gdpPercap))

#what is mean life expectancy for Each year using group_by()
gapminder_data %>% 
  group_by(year) %>% 
  summarize(meanlifeExp = mean(lifeExp))

#what is mean life exp for each continent?
gapminder_data %>% 
  group_by(continent) %>% 
  summarise(meanlifeExp = mean(lifeExp))

#what is mean life exp and mean gdp per capita for each continent in a single tibble?
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanlifeExp = mean(lifeExp),
           meangdppercap= mean(gdpPercap))

#what is the total gdp (not per capita) using mutate()
gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)

#make a new column for population in millions popInMillions and gdp
gapminder_data %>% 
  mutate(popInMillions = pop/1000000,
         gdp = gdpPercap * pop)

#to overwrite
gapminder_data <- gapminder_data %>% 
  mutate(popInMillions = pop/1000000,
         gdp = gdpPercap * pop)

#if rerun original 1st readcsv line it will remove this added data

#to write in new file
gapminder_data_popmil <- gapminder_data %>% 
  mutate(popInMillions = pop/1000000,
         gdp = gdpPercap * pop)

#is there a relatioship between gdp and co2 emissions using select- select chooses a subset of colums from dataset
gapminder_data %>% 
  select(year, pop)

gapminder_data %>% 
  select(-continent)

#tible with coulntry continent year lifeexp
gapminder_data %>% 
  select(-pop,
         -gdpPercap)

gapminder_data %>% 
  select(country, continent, year, lifeExp)

#select helper function: starts_with(), ends_with(), contains()
gapminder_data %>% 
  select(year, starts_with("c"))

#vectors
#c()

#my_vec <- c("dog", "cat", "horse")
#num_vec <- c(1,2,3,4)
#proof <- gapminder_data %>% 
 # pull(year)

#your_data %>% 
 # filter(id == "id of interest1" & "id of interest2") 
#or filter(id %in% c("id1", "id2", "id3"))

#pivot_wider() and pivot_longer()
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#pivot wider but populate values with gdpPercap
gapminder_data %>% 
  select (country, continent, year, gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = gdpPercap)

#pivot longer
gapminder_data %>% 
  pivot_longer(cols = c(pop, lifeExp, gdpPercap),
               names_to = "measurement_type",
               values_to = "measurement")

#is there a relationship between gdp and co2 emissions?
#new dataset  gapminder_data_2007 with filtering year 2007 and continent Americas, and remove year and continent columns
gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent) 

#read in co2 data
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from=series, 
              values_from = value) %>% 
  filter (year == 2005) %>% 
  select(-year)

View(co2_emissions)

#inner_join
inner_join(gapminder_data_2007, co2_emissions, by = "country")
