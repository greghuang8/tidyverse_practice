# R4DS Cookbook 
# Practicing Tidyverse package characteristics

install.packages(c("tidyverse","nycflights13","gapminder","Lahman"))
library(tidyverse)
library(nycflights13)

# tibbles exercise
x <- tibble(`:)` = 1:5, 
            y = 1, 
            z = `:)` ^ 2+ y)

tryFlights <- nycflights13::flights


# dplyr suite + pipe
flightsDLJan <- tryFlights %>%
  filter(carrier == "DL", dep_time <= 1200, month == 1) %>%
  arrange(desc(dep_time)) 

# arrange NA first: arrange first by T/F is.na() values, where F comes 
# before T - i.e., NA values will be T, !is.na() will therefore sort
# NA values first
flightsDLSummer <- tryFlights %>%
  filter(carrier == "DL", month %in% c(6:8)) %>%
  arrange(!is.na(dep_time), desc(dep_time)) 

# Keep only NA departure times
flightsNoDepTime <- tryFlights %>%
  filter(is.na(dep_time))

# Kick all NA values, check indeed they are removed
flightsNoNA <- tryFlights %>%
  drop_na() %>%
  arrange(!is.na(dep_time))

# Select
flightsDLSkinny <- tryFlights %>%
  select(month, day, dep_time, carrier) %>%
  filter(carrier == "DL")

# Select first 3 columns only
flightsDateOnly <- tryFlights %>%
  select(year:day)

# Select with helper functions
flightsTimeOnly <- tryFlights %>%
  select(ends_with("time"))

# Select includes starts_with, ends_with, contains, 
# matches (regex), and num_range

# Create a new, re-arranged df with rename and select(everything())
flightsRearranged <- tryFlights %>%
  rename(tail_num = tailnum) %>% # tail_num is the new colname 
  select(carrier, tail_num, everything())

# Combination of all so far, incl. mutate (new column creation)
flightsWrangled <- tryFlights %>%
  rename(tail_num = tailnum) %>%
  select(carrier, ends_with("time"), everything()) %>%
  filter(carrier == "B6", dep_time <= 500) %>%
  mutate(gain = arr_delay - dep_delay, speed = distance/air_time *60) %>%
  arrange(!is.na(speed), desc(speed))
  
# Transmute (keep only the new columns, as opposed to mutate())
flightsTransmuted <- flightsWrangled %>%
  transmute(speed_doubled = speed *2)

# Grouped summaries
byDay <- group_by(tryFlights, year, month, day)
summarize(byDay, delay = mean(dep_delay, na.rm = TRUE))



install.packages("opendatatoronto")
library(opendatatoronto)


