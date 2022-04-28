#Problem 1 ----
#1.1
data <- tq_get(c("AMZN","FB", "NFLX"), 
               get = "stock.prices", 
               from ="2019-01-01", 
               to ="2021-04-01") %>%
  select(symbol, date, adjusted)

#1.2
Dates <- data.frame(Date = rep(seq.Date(from=ymd("2019-01-01"), 
                                     to =ymd("2021-04-01"), 
                                     by="day"), 3),
                    Symbol = c(rep("AMZN", 822), 
                               rep("FB", 822), 
                               rep("NFLX", 822)))
Final <- Dates %>%
  dplyr::left_join(data, by=c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup") 

#1.3
new_data <- Final %>%
  filter( (Symbol == "AMZN" | Symbol == "FB") & 
          ((Date > "2019-01-01" & Date < "2019-07-01") | (Date > "2020-04-01" & Date < "2020-07-01")) ) %>%
  arrange(Symbol, desc(Date))


#1.4
new_data1 <- new_data %>%
  group_by(Symbol) %>%
  arrange(Date) %>%
  slice(c(1, n()))
  

#1.5
new_data2 <- new_data %>%
  mutate(year_month = lubridate::floor_date(Date, unit = "month")) %>%
  group_by(year_month, Symbol) %>%
  arrange(Date) %>%
  slice(n())


#Problem 2 ----
d <- Final %>%
  mutate(SMA10 = SMA(adjusted, 10), 
           SMA26 = SMA(adjusted, 26),
           signal = ifelse(is.na(SMA26), 0, ifelse(SMA26 > SMA10, 1, 0))) %>%
  group_by(Symbol) %>%
  arrange(Date, .by_group = TRUE)

#Find intersection (if 1 - SMA10 crossed above, if -1 - SMA10 crossed below)
d1 <- d %>%
  mutate(position = signal - lag(signal))

#How many times did the 10 day SMA line cross 26 day SMA line from below?
buy <- d1 %>%
  filter(position == 1) %>%
  summarise(SMA26_below = n())

#How many times did the 10 day SMA line cross 26 day SMA line from above?
sell <- d1 %>%
  filter(position == -1) %>%
  summarise(SMA26_above = n())


