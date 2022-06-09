# Download data for a stock of your choice and do the following:
# Calculate the 20 day SMA of the stock price and define upper and
# lower bounds around it which are equal to SMA +-2 standard deviation
# the past observations used to calculate the SMA.
# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the price goes above the upper bound - sell.
# If the price goes below the lower bound - buy.

data <- tq_get("NFLX",
               get = "stock.prices",
               from="2019-04-01",
               to="2021-04-01") %>%
  select(symbol, date, adjusted)

Dates <- data.frame(Date = seq.Date(from = lubridate::ymd("2019-04-01"),
                                    to = lubridate::ymd("2021-04-01"), 
                                    by="day"),
                    Symbol = "NFLX")

final <- Dates %>%
  left_join(data, by=c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup")


new_data <- final %>%
  mutate(SMA20 = SMA(adjusted, 20)) %>%
  filter(!is.na(SMA20)) %>%
  select(SMA20) %>%
  summarise(sd = sd(SMA20),
            mean = mean(SMA20),
            n = n(),
            margin_of_error = 1.96*(sd/sqrt(n)),
            lower = mean - margin_of_error,
            upper = mean + margin_of_error)


baseline <- final %>%
  mutate(signal = ifelse(adjusted > 415.8038, "sell", ifelse(adjusted < 402.8708, "buy", NA)))
  

# Calculate the RSI using the instruction about the formula from here:

RSI_fun <- function(price, n){

gain = c()
loss = c()

for(i in 1:length(price)){
  difference = round(price[i+1] - price[i], 2)
  
  if (difference == 0) {
    gain = c(gain, 0)
    loss = c(loss, 0)
  } else {
    if (difference > 0) {
      gain = c(gain, difference)
      loss = c(loss, 0)
    } else {
      gain = c(gain, 0)
      loss = c(loss, abs(difference))
    }
  }
}
AvgGain = sum(gain[1:14])/14
AvgLoss = sum(loss[1:14])/14

avg_gain = (AvgGain * 13 + gain[15:length(price)]) / 14
avg_loss = (AvgLoss * 13 + loss[15:length(price)]) / 14


RS = avg_gain/avg_loss
RSI = 100 - 100/(1+RS)

}


########################

RSIfun <- function(price, n) {
  N <- length(price)
  U <- rep(0, N)
  D <- rep(0, N)
  rsi <- rep(NA, N)
  Lprice <- lag(price, 1)
  for(i in 2:N){
    if(price[i] >= Lprice[i]){
      U[i] <- 1
    } else {
      D[i] <- 1
    }
    if(i > n){
      AvgUp <- mean(U[(i-n+1):i])
      AvgDn <- mean(D[(i-n+1):i])
      rsi[i] <- AvgUp/(AvgUp+AvgDn)*100
    }
  }
  rsi <- reclass(rsi, price)
  return(rsi)
}


# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the RSI above 65 - sell.
# If the price goes below 35 - buy.

strategy <- final %>%
  mutate(RSI = RSI(adjusted, 14),
         RSI_signal = ifelse(RSI > 65, "sell", ifelse(RSI < 35, "buy", NA)),
         baseline = ifelse(adjusted > 415.8038, "sell", ifelse(adjusted < 402.8708, "buy", NA)))


