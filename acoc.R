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

# Day 3
day3 <- read_csv("day3.csv", col_names = FALSE) %>%
mutate(X1 = str_dup(X1, 100))

index <- 4
tree3 <- 0
for (x in 2:323){
  if(str_sub(day3$X1[x],index,index) == "#"){
    tree3 <- tree3 + 1
  }
  index <- index + 3
}

# Day 3: Part 2, other parameters
# Right 1, Down 1
index <- 2
tree1 <- 0
for (x in 2:323){
  if(str_sub(day3$X1[x],index,index) == "#"){
    tree1 <- tree1 + 1
  }
  index <- index + 1
}

#Right 5, Down 1
index <- 6
tree5 <- 0
for (x in 2:323){
  if(str_sub(day3$X1[x],index,index) == "#"){
    tree5 <- tree5 + 1
  }
  index <- index + 5
}

#Right 7, Down 1
index <- 8
tree7 <- 0
for (x in 2:323){
  if(str_sub(day3$X1[x],index,index) == "#"){
    tree7 <- tree7 + 1
  }
  index <- index + 7
}

#Right 1, Down 2
index <- 2
tree2 <- 0
for (x in seq(3,323,by=2)){
  if(str_sub(day3$X1[x],index,index) == "#"){
    tree2 <- tree2 + 1
  }
  index <- index + 1
}

resultDay3 <- tree1*tree2*tree3*tree5*tree7

# Hmm. A %% solution should be faster here, but 
# str_dup() by 100 gets it done instantly still...


# Day 4


