#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER
glimpse(ds)

ds$Year <- as.numeric(ds$Year)

typeof(ds$Year)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER

library(janitor)
ds <- ds %>% rename_with(tolower)
ds


### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER

ds <- ds %>% mutate(decade = floor(year/10)*10)
ds


### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER

ds <- ds %>% arrange(rank)
ds

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER
top10 <- ds %>% 
          filter(rank < 11) %>% 
          select(artist, year)
top10

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER
ds_sum <- ds %>% 
            summarize(min_year = min(year, na.rm = T),
                      recent_year = max(year, na.rm = T),
                      avg_year = mean(year, na.rm = T))

ds_sum
glimpse(ds_sum)

ds_sum <- ds_sum %>% mutate(avg_year = floor(avg_year))
glimpse(ds_sum)

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER
ds %>% filter(year == ds_sum$avg_year | year == ds_sum$min_year | year == ds_sum$recent_year) %>% 
              arrange(year) #not using <- so I don't edit the dataset

### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER
ds <- ds %>% mutate(year = ifelse(song == "Brass in Pocket", 1979,year))
ds <- ds %>% mutate(decade = floor(year/10)*10)
ds %>% filter(song == "Brass in Pocket") #check if transformations worked

ds_sum <- ds %>% 
  summarize(min_year = min(year, na.rm = T),
            recent_year = max(year, na.rm = T),
            avg_year = mean(year, na.rm = T))

glimpse(ds_sum)
ds_sum <- ds_sum %>% mutate(avg_year = round(avg_year))
ds_sum

ds %>% filter(year == ds_sum$avg_year | year == ds_sum$min_year | year == ds_sum$recent_year) %>% 
  arrange(year)

### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER

ds %>% arrange(decade)

ds %>% filter(!is.na(decade)) %>%
        group_by(decade) %>% 
        summarise(avg_rank = mean(rank),
                  numofsongs = n())

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER
ds %>% count(decade) %>% 
        slice_max(n, n = 1)
  