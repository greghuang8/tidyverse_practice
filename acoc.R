# Advent Calendar of Code
# Day 1

# Probs a brute force kind of deal, but still 
data <- scan("day1.txt")
data <- sort(data, decreasing = TRUE)
a <- 0
b <- 0
for (x in 1:199){
  for (y in 1:200){
    for (z in 1:200){
      if ((data[x] + data[y] + data[z]) == 2020){
        c <- data[z]
        b <- data[y]
        a <- data[x]
        break
        }
      }
    }
  }
result <- a*b*c


# Day 2
install.packages("tidyverse")
library(tidyverse)

day2 <- read_csv("day2.csv") %>%
  separate(times, into = c("times","letter","password"), sep = " ") %>%
  separate(times, into = c("min","max")) %>%
  mutate(min = as.integer(min), max = as.integer(max)) %>%
  mutate(letter = str_sub(letter,1,1)) %>%
  mutate(count = str_count(password, letter)) %>%
# filter(count <= max & count >= min)      #Part 1 solution; 591 obs
  mutate(firstLoc = str_sub(password,min,min), 
         secondLoc = str_sub(password,max,max))%>%
  filter(xor(firstLoc == letter, secondLoc == letter)) #Part 2 solution; 335 obs



