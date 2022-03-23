#Problem 1 ----

coin <- c("win", "lose")
budget = 100
bet = 1
attempt = 0

while (budget > 0) {
  if (budget >= bet) {
    choose <- sample(coin, 1, prob = c(0.486, 0.514))
    if (choose == "lose") {
      budget = budget - bet
      bet = bet * 2
      attempt = attempt + 1
      print("lose")
    } else {
      budget = budget + bet
      bet = 1
      attempt = attempt + 1
      print("win")
    }
  } else {
    bet = budget
    attempt = attempt + 1
  }
}
print(attempt)




#Ex. 5.2.4----
library(tidyverse)
library(nycflights13)
nycflights13::flights
View(flights)

#Had an arrival delay of two or more hours
delay <- filter(flights, arr_delay >= 120)
View(delay)
                       
#Flew to Houston (IAH or HOU)
Houst <- filter(flights, dest == "IAH" | dest == "HOU")
View(Houst)

#Were operated by United, American, or Delta
carrier <- filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL")
View(carrier)

#Departed in summer (July, August, and September)
summer <- filter(flights, month %in% 7:9)
summer <- filter(flights, month >= 7, month <= 9)
View(summer)

#Arrived more than two hours late, but didn't leave late
arrive <- filter(flights, arr_delay > 120 & dep_delay <= 0)
View(arrive)

#Were delayed by at least an hour, but made up over 30 minutes in flight
flight <- filter(flights, dep_delay >= 60 & air_time > 30)
View(flight)

#Departed between midnight and 6am (inclusive)
depart <- filter(flights, between(hour,0,6))
depart <- filter(flights, dep_time <= 600 | dep_time == 2400)
View(depart)


#How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
na <- filter(flights, is.na(dep_time))
sum(is.na(flights$dep_time))

View(na)

#Why is NA ^ 0 not missing? - Any number to the power of 0 is equal to 1
#Why is NA | TRUE not missing? - Or, it's either NA or TRUE, so we want one of the statements to be correct
#Why is FALSE & NA not missing? - Anything '.. and FALSE' is always False, if one of the statemnets is false, then the whole statement will be incorrect


#Ex. 5.3.1----

#Sort all missing values to the start

sort <- arrange(flights, desc(is.na(dep_time)), dep_time)
View(sort)

#Sort flights to find the most delayed flights. Find the flights that left earliest.

#most delayed
sort <- arrange(flights, desc(dep_delay))
#earliest
sort <- arrange(flights, dep_delay)
View(sort)

#Sort flights to find the fastest (highest speed) flights.
flights <- mutate(flights, speed = distance/air_time)
flights <- arrange(flights, desc(speed))

View(flights)

#Which flights traveled the farthest? Which traveled the shortest?

#longest
sort <- arrange(flights, desc(distance))
#shortest
sort <- arrange(flights, distance)
View(sort)



#Ex. 5.4.1----

#Different ways to select dep_time, dep_delay, arr_time, and arr_delay from flights

select <- select(flights, dep_time, dep_delay, arr_time, arr_delay)

select <- select(flights, starts_with("dep"), starts_with("arr"))

select <- select(flights, dep_time:arr_delay, -(starts_with("sched")))

select <- select(flights, dep_time:arr_delay, -(sched_dep_time), -(sched_arr_time))

select <- select(flights, dep_time, arr_time, ends_with("delay"))

select <- select(flights, contains("arr_"), contains("_time"))

View(select)

#What happens if you include the name of a variable multiple times in a select() call

#sorts them into an ascending order
a <- select(flights, dep_time, dep_time, dep_time, arr_time)

View(a)


#What does the any_of() function do? Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
x <- select(flights, any_of(vars))
view(x)
#shows all elements of the vector as columns; ignores variables of the vector that are not elements of the data frame or are not present

#result, How do the select helpers deal with case by default? How can you change that default?
select(flights, contains("TIME"))
#shows all columns that contain "time"; it's not case sensitive


#Ex. 5.5.2----

#dep_time and sched_dep_time convert them to a number of minutes since midnight
# %/% only integers; %% - remainder
minutes <- mutate(flights, 
                  Dep_in_min = dep_time %/% 100 *60 + dep_time %% 100, 
                  SchedDep_in_min = sched_dep_time %/% 100*60 + sched_dep_time %% 100)
View(minutes)


#Compare air_time with arr_time - dep_time. What do you need to do to fix it?
# if air_time = arr_time - dep_time = 0
minutes <- mutate(flights, 
                  Dep_in_min = dep_time %/% 100 *60 + dep_time %% 100, 
                  Arr_in_min = arr_time %/% 100*60 + arr_time %% 100, 
                  comapre = air_time - Arr_in_min - Dep_in_min)


#Compare dep_time, sched_dep_time, and dep_delay. How related?

minutes <- mutate(flights, 
                  Dep_in_min = dep_time %/% 100 *60 + dep_time %% 100, 
                  SchedDep_in_min = sched_dep_time %/% 100*60 + sched_dep_time %% 100, 
                  Dep_Delay = dep_delay %/% 100*60 + dep_delay %% 100)


#Find the 10 most delayed flights using a ranking function; min_rank().
#desc(x) to give the largest values the smallest ranks

delay <- mutate(flights, DelayFlight = min_rank(desc(dep_delay)))
delay <- arrange(delay, DelayFlight)
delay <- filter(delay, DelayFlight <= 10)
View(delay)


#What does 1:3 + 1:10 return? Why?
1:3 + 1:10
#not of the same length, it adds the first element of the first vector to the first element of the second vector; when different, R creates a multiple of the longer vector


#What trigonometric functions does R provide?
?trig





