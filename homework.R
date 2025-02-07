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
#Use type of to check that your conversion succeeded

#ANSWER
glimpse(ds)

ds <- ds %>%
  mutate(Year = as.numeric(Year))

typeof(ds$Year)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER

library(tidyverse)

ds <- ds %>%
  rename_with(tolower)

colnames(ds)


### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER
library(tidyverse)

ds <- ds %>%
  mutate(decade = floor(year / 10) * 10)


### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER

library(tidyverse)

ds <- ds %>%
  arrange(rank)

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER
library(tidyverse)

top10 <- ds %>%
  filter(rank <= 10) %>%  # Keep only rows where rank is 10 or lower
  select(artist, song)    # Keep only the "artist" and "song" columns


### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER
library(tidyverse)

ds_sum <- ds %>%
  summarize(
    earliest_year = min(year, na.rm = TRUE),  #the oldest song
    most_recent_year = max(year, na.rm = TRUE),  #the newest song
    avg_year = mean(year, na.rm = TRUE)  #the average release year
  )


### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER

library(tidyverse)

earliest <- ds_sum$earliest_year
most_recent <- ds_sum$most_recent_year
avg_year <- round(ds_sum$avg_year)  #rounding average year to nearest number

ds_filtered <- ds %>%
  filter(year %in% c(earliest, most_recent, avg_year)) %>%
  arrange(year) %>%  
  select(year, artist, song)  #just relevant columns



### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER

library(tidyverse)

ds <- ds %>%
  mutate(year = ifelse(song == "Brass in Pocket" & artist == "The Pretenders", 1979, year))

ds <- ds %>%
  mutate(decade = floor(year / 10) * 10)

ds_sum <- ds %>%
  summarize(
    earliest_year = min(year, na.rm = TRUE),  
    most_recent_year = max(year, na.rm = TRUE),  
    avg_year = round(mean(year, na.rm = TRUE)) 
  )

earliest <- ds_sum$earliest_year
most_recent <- ds_sum$most_recent_year
avg_year <- ds_sum$avg_year

ds_filtered <- ds %>%
  filter(year %in% c(earliest, most_recent, avg_year)) %>%
  arrange(year) %>%
  select(year, artist, song)


### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER
library(tidyverse)

ds %>%
  filter(!is.na(decade)) %>%   
  group_by(decade) %>%        
  summarize(
    avg_rank = mean(rank, na.rm = TRUE),  
    num_songs = n() #counting the number
  ) %>%
  arrange(decade)


### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER

library(tidyverse)

ds %>%
  count(decade) %>%      
  slice_max(n, n = 1) 

#realized the "count" function is much more efficient. whit out that I would do this (not efficient!) 
  #ds %>%
  #group_by(decade) %>%
  #summarize(n = n())


  