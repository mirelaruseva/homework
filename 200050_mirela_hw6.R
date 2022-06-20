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

#my initial thought process and understanding 
new_data <- final %>%
  mutate(SMA20 = SMA(adjusted, 20),
         SD20 = sd(SMA20),
         lower = SMA20 - 1.96*SD20,
         upper = SMA20 + 1.96*SD20,
         signal = ifelse(adjusted > upper & lag(adjusted) < upper, "sell", ifelse(adjusted < lower & lag(adjusted) > lower, "buy", NA)),
         RSI = RSI(adjusted, 14),
         RSI_signal = ifelse(RSI > 65, "sell", ifelse(RSI < 35, "buy", NA)))
  
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


####

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


#correct answer (not entirely completed)
fin_data1 <- final %>%
  mutate(SMA20 = SMA(adjusted, n = 20),
         SD20 = RcppRoll::roll_sd(adjusted, n = 20, align = "right"),
         Upper = SMA20 + 2*SD20,
         Lower = SMA20 - 2*SD20,
         buy_sell = case_when(lag(adjusted) < lag(Upper) & adjusted > Upper ~ "sell",
                              lag(adjusted) > lag(Lower) & adjusted < Lower ~ "buy")) %>%
  mutate(ProfitCoeff = adjusted/lag(adjusted),
         ProfitCoeff = ifelse(is.na(ProfitCoeff), 1, ProfitCoeff))

fin_data2 <- fin_data1 %>%
  mutate(gain_loss = (adjusted/lag(adjusted)) - 1,
         gain_loss = if_else(is.na(gain_loss), 0, gain_loss),
         average_gain1 = case_when(gain_loss >= 0 ~ gain_loss,
                                   gain_loss < 0 ~ 0),
         average_gain = SMA(average_gain1, 14),
         average_loss1 = case_when(gain_loss <= 0 ~ abs(gain_loss),
                                   gain_loss > 0 ~ 0),
         average_loss = SMA(average_loss1, 14),
         RSI1 = 100 - (100/(1+(average_gain/average_loss))),
         RSI = 100 - (100/(1+((13*lag(average_gain) + average_gain1)/(1)))),
         buy_sell = case_when(RSI > 65 ~ "sell",
                              RSI < 35 ~ "buy"),
         ProfitCoeff = adjusted/lag(adjusted),
         ProfitCoeff = if_else(is.na(ProfitCoeff), 1, ProfitCoeff),
         BencmarkMoney = 100 * cumprod(ProfitCoeff),
         buy_sell_for_comparison = buy_sell) %>%
  fill(buy_sell_for_comparison, .direction = "down") %>%
  mutate(buy_sell_for_comparison = if_else(is.na(buy_sell_for_comparison)),
         ProfitCoeff_strategy = case_when(buy_sell_for_comparison == ,
                                          buy_sell_for_comparison == ),
         StrategyMoney = 100 * cumprod(ProfitCoeff_strategy))
