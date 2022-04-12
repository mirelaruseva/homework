#Problem 1----

#1st solution
FactorialFunction <- function(x) {
  result <- x
  for (i in 1:x) {
    a <- (x - i)
    if (x <= 1) {
      answer = "if 0 or 1, answer=1, if negative - not possible"
      break
    } else {
      if (a == 0) {
        break
      } else {
        result = c(result, a)
      }
    }
    answer <- prod(result)
  }
  return(answer)
}

#test
FactorialFunction(6)
factorial(6)


#2nd solution

FactorialFunction <- function(x) {
  if (x<=1){
  result <- "if 0 or 1 = 1, if negative - not possible"
  } else {
    result <- 1
  }
  
  for (i in 1:x) {
    result <- result * i
  }
  return(result)
}


#Problem 2----

SDFunction <- function(x) {
  m = mean(x)
  n = length(x)
  results <- NULL
  for (i in min(x):max(x)) {
    a = (i - m) ^ 2
    results <- c(results, a)
  }
  return(answer = sqrt(sum(results) / n))
}

#test
v <- c(1,2,3,4,5)
SDFunction(v)
sd(v)


# 5.6.7 Exercises----
library(dplyr)
library(nycflights13)
View(flights)

#Ex.1

#50% 15min late/early
not_cancelled <- flights %>%
  filter(!is.na(air_time)) %>%
  mutate(is_early = ifelse(arr_delay <= -15, 1, 0)) %>%
  group_by(tailnum) %>%
  summarise(total_flights = n(),
            sum_early = sum(is_early)) %>%
  mutate(percent_early = (sum_early / total_flights) * 100) %>%
  filter(percent_early == 50)

View(not_cancelled)


#always 10 min late
not_cancelled <- flights %>%
  filter(!is.na(air_time)) %>%
  mutate(is_late = ifelse(arr_delay == 10, 1, 0)) %>%
  group_by(tailnum) %>%
  summarise(late_flights = sum(is_late)) %>%
  filter(late_flights > 0) %>%
  arrange(desc(late_flights))

#50% 30min early
not_cancelled <- flights %>%
  filter(!is.na(air_time)) %>%
  mutate(is_early = ifelse(arr_delay == -30, 1, 0)) %>%
  group_by(tailnum) %>%
  summarise(total_flights = n(),
            sum_early = sum(is_early)) %>%
  mutate(percent_early = (sum_early/total_flights)*100) %>%
  filter(percent_early == 50)

View(not_cancelled)

#on time, 2h delay
not_cancelled <- flights %>%
  filter(!is.na(air_time) & (arr_delay == 0 | arr_delay == 120)) %>%
  mutate(on_time = ifelse(arr_delay == 0, 1, 0)) %>%
  group_by(tailnum) %>%
  summarise(total_flights = n(),
            sum_early = sum(on_time)) %>%
  mutate(percent_early = (sum_early / total_flights) * 100) %>% 
  
#Ex.2
not_cancelled <- flights %>% 
  count(dest)

#without count
not_cancelled <- flights %>%
  group_by(dest) %>%
  summarise(n = length(dest))
           
### 
not_cancelled <- flights %>%
  count(tailnum, wt = distance)

#without count
not_cancelled <- flights %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))

View(not_cancelled)

#Ex.4
not_cancelled <- flights %>%
  mutate(cancelled = (is.na(dep_delay) | is.na(arr_delay))) %>%
  group_by(year, month, day) %>%
  summarise(total_cancelled = sum(cancelled),
            total_flights = n())

not_cancelled <- flights %>%
  mutate(cancelled = (is.na(dep_delay) | is.na(arr_delay))) %>%
  group_by(year, month, day) %>%
  summarise( cancelled1 = mean(cancelled),
             avg_delay_dep = mean(dep_delay, na.rm = T),
             avg_delay_arr = mean(arr_delay, na.rm = T))

#Ex.5
bad_carrier <- flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = T)) %>%
              arrange(desc(arr_delay))
View(bad_carrier)

###
bad_airport <- flights %>%
  group_by(origin, dest, carrier) %>%
  summarise(origin_delay = mean(dep_delay, na.rm=T), 
            dest_delay= mean(arr_delay, na.rm=T),
            flights = n()) %>%
  mutate(sum_origin_delay = sum(origin_delay),
         sum_dest_delay = sum(dest_delay)) %>%
  summarise(total_delay = sum_origin_delay + sum_dest_delay) %>%
  arrange(desc(total_delay))

View(bad_airport)  

# 5.7.1 Exercises----
#Ex.1
bad_plane <- flights %>%
  filter(!is.na(air_time), !is.na(arr_delay), !is.na(dep_delay)) %>%
  group_by(tailnum) %>%
  summarise(total_delay = mean(arr_delay) + mean(dep_delay)) %>%
  arrange(desc(total_delay)) %>%
  slice_head(n=1)

View(bad_plane)
  
#Ex.2
avoid_delay <- flights %>%
  filter(arr_delay <= 0, dep_delay <=0) %>%
  group_by(hour) %>%
  summarise(delay = mean(arr_delay) + mean(dep_delay))

View(avoid_delay)

#Ex.3
dest_del <- flights %>%
  filter(arr_delay > 0) %>%
  mutate(sum_arr_delay = sum(arr_delay),
         prop_delay = arr_delay/sum_arr_delay) %>%
  select(dest, carrier, flight, month, day, prop_delay) %>%
  group_by(dest)
View(dest_del)


#Ex.4 -###-
lagged_delays <- flights %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

View(lagged_delays)



#Ex.5
standardized_flights <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    n = n()) %>%
  ungroup() %>%
  mutate(air_time_standard = (air_time - air_time_mean) / (air_time_sd + 1))

standardized_flights <- standardized_flights %>%
  arrange(air_time_standard) %>%
  select(carrier, flight, origin, dest, month, day, air_time, air_time_mean, air_time_standard) %>%
  head(10) %>%
  print(width = Inf)

##
air_time_delayed <- flights %>%
  group_by(origin, dest) %>%
  mutate(air_time_min = min(air_time, na.rm = TRUE),
    air_time_delay = air_time - air_time_min,
    air_time_delay_pct = air_time_delay / air_time_min * 100)

air_time_delayed <- air_time_delayed %>%
  arrange(desc(air_time_delay)) %>%
  select(air_time_delay_pct, carrier, flight,
    origin, dest, year, month, day, dep_time,
    air_time, air_time_min) %>%
  head() %>%
  print(width = Inf)

#Ex.6
flights1 <- flights %>%
group_by(dest) %>%
  mutate(total_carriers = n_distinct(carrier)) %>%
  filter(total_carriers > 1) %>%
  group_by(carrier) %>%
  summarize(total_dest = n_distinct(dest)) %>%
  arrange(desc(total_dest))

#Ex.7
flights1 <- flights %>%
  select(tailnum, year, month,day, dep_delay) %>%
  filter(!is.na(dep_delay)) %>%
  arrange(tailnum, year, month, day) %>%
  group_by(tailnum) %>%
  mutate(cumulative_hr_delays = cumsum(dep_delay > 60)) %>%
  summarise(total_flights = sum(cumulative_hr_delays < 1)) %>%
  arrange(total_flights)

#Problem 4
#4.1 For each carrier what is the most common destination?----
flights1 <- flights %>% 
  group_by(carrier, dest) %>%
  summarise( total_visits=n()) %>%
  slice_max(total_visits, n=1) %>%
  arrange(desc(total_visits)) 

View(flights1)

#4.2 For each carrier what is the biggest delay?----
delay <- flights %>%
  select(carrier, arr_delay) %>%
  group_by(carrier) %>%
  slice_max(arr_delay, n=1)

View(delay)
#4.3 Which are the three planes which have flown the most/least miles?----
flights1 <- flights %>%
  filter(!is.na(distance),!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  summarise(total_miles1 = sum(distance)) %>%
  arrange(desc(total_miles1)) %>%
    slice_head(n=3)
View(flights1)

#4.4 What are the first/last flights for each day in February 2013?----
flights1 <- flights %>%
  mutate( dep_in_hours_after_midnight = ((dep_time %/% 100 + (dep_time %% 100)/60)%%1440)) %>%
  filter(month == 2, !is.na(dep_in_hours_after_midnight)) %>%
  group_by(day) %>%
  summarise(dep_in_hours_after_midnight) %>%
  arrange(day) %>%
  slice_min(dep_in_hours_after_midnight, n=1)

View(flights1)

#4.5 Which company flew the most miles in March 2013? Which flew the least?----
company_miles <- flights %>%
  filter(!is.na(air_time), month == 3) %>%
  group_by(carrier) %>%
  summarise(total_miles = sum(distance)) %>%
  arrange(desc(total_miles)) %>%
  slice_head(n=1)

View(company_miles)
#4.6 Which month had the most delays over 60 minutes?-----
most_delay_month <- flights %>%
  filter(arr_delay > 60) %>%
  group_by(month) %>%
  summarise(Total_delay_per_month = sum(arr_delay)) %>%
  arrange(desc(Total_delay_per_month))
View(most_delay_month)

#4.7 What is the average time between two consecutive flights?----
time_between_flights <- flights %>%
  filter(!is.na(air_time)) %>%
  mutate(dep_time_in_hours = ((dep_time %/% 100*60 + dep_time %% 100) %% 1440)*60,
         arr_time_in_hours = ((arr_time %/% 100 + arr_time %% 60) %% 1440)*60) %>%
  gorup_by(month, day, flight) %>%
  summarise(diff_time = lag())


#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.-----

flights1 <- flights %>%
  filter(!is.na(dest),!is.na(arr_delay)) %>%
  group_by(month,dest) %>%
  summarise(total_delay = sum(arr_delay))

View(flights1)







