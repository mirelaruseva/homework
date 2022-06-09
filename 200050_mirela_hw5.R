#Problem 1 ----
#1.1 - A function which replicates the SMA function. It should calculate the SMA
# for a vector to the left

SMAfun <- function(x, k){
  stopifnot(k > 0 && k <= length(x))
  
  if(k == length(x)){
    
    results <- sum(x)/k 
    NAs <- rep(NA,(k-1)) 
    results <- c(NAs, results) 
  
    } else {
      
    y <- cumsum(x)
    results <- y[(k+1):length(y)] - y[1:(length(y) - k)]
    results <- c(y[k], results)
    results <- results/k #compute SMA
    NAs <- rep(NA, (k-1))
    results <- c(NAs, results)
  }
  return(results)
    }

#test  
x <- seq(1:10)
SMA(x, 3)
SMAfun(x, 3)

#1.2 - A function, which calculates the correlation coefficient between two vectors.
# It should replicate the cor function from the base package.


cor_fun <- function(x,y){
  stopifnot(length(x)==length(y))
  xm <- mean(x)
  ym <- mean(y)
  dividend <- c()
  divisor <- c()
  divisor2 <- c()
  for(i in 1:length(x)){
    xi = x[i]
    yi = y[i]
    r=(xi-xm)*(yi-ym)
    dividend=c(dividend,r)
    r1=(xi-xm)^2
    divisor=c(divisor,r1)
    r2=(yi-ym)^2
    divisor2=c(divisor2,r2)
  }
  results = (sum(dividend))/sqrt(sum(divisor)*sum(divisor2))
  return(results)
  }

#test
a=rnorm(100, mean=4, sd=2)
b=rnorm(100, mean=3, sd=1)

cor(a,b, method = "pearson")
cor_fun(a,b)


#Problem 2 ----
#Find all prime numbers less than 100, using for/while loops.

#find if it is a prime number
is.primetest <- function(k){
  if(k>=2){ 
  if(k == 2){
      print(paste(k,"is prime"))
 } else {
  for(i in 2:(k-1)){
      if((k%%i) == 0){
        print(paste(k,"is not prime"))
        break
      } else {
        print(paste(k,"is prime"))
      }
    }
  }
  } else {
  stop("Should be at least 2")
  }
}

#test
is.primetest(125)


#all prime numbers
prime_numbers <- function(n) {
    if (n >= 2) {
      x <- seq(2, n)
      prime_nums = NULL
      for (i in seq(2,n)) {
        if (any(x == i)) {
          prime_nums = c(prime_nums, i)
          x = c(x[(x %% i) != 0], i)
        }
      }
      return(prime_nums)
    }
    else 
    {
      stop()
    }
  } 
  
  
#test
prime_numbers(100)

#Problem 3 ----
# Download data for a stock of your choice 
data <- tq_get("FB",
               get = "stock.prices",
               from="2019-01-01",
               to="2021-01-01") %>%
  select(symbol, date, adjusted)

Dates <- data.frame(Date = seq.Date(from = lubridate::ymd("2019-01-01"),
                                    to = lubridate::ymd("2021-01-01"), 
                                    by="day"),
                    Symbol = "FB")

final <- Dates %>%
  left_join(data, by=c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup")

# 1.Calculate the 26-period EMA(use the EMA function from tidyquant)
# 2.Calculate the 12-period EMA.
# 3.Calculate the MACD line(12-period EMA minus 26-period EMA)
# 4.Calculate the signal line - this is the 9-period EMA of the MACD.
# 5.Calculate the buy/sell signals. This means create a new column which tell
# us if we should buy or sell. When the MACD line crosses the signal line
# from above(MACD is above signal then MACD is below signal) this is a sell signal. 
# If it crosses from below (MACD is below signal then MACD is above signal) this is a buy signal.

new_data = final %>%
  mutate(EMA26 = EMA(adjusted, 26),
         EMA12 = EMA(adjusted, 12),
         MACD = EMA12 - EMA26,
         Signal = EMA(MACD, 9),
         Buy_Sell = ifelse(lag(Signal)>lag(MACD) & Signal<MACD, "buy", ifelse(lag(Signal)<lag(MACD) & Signal>MACD, "sell", NA)))
           

# 6. Simulate how the strategy preforms and compare it to a benchmark strategy
# of just buying and holding the stock.
# In order to do this start with a portfolio of 100$ invested in the stock on the first day
# and see how it performs. Example:
# I start with 100$ and a stock which costs 100$ at the beginning of my time period.
# I get a buy signal when the stock price is 90. I buy the stock.
# I get a sell signal to sell the stock when the price is 110. I sell it and 
# and don't get any more signals.I end up with 100 * 110 / 90 = 122.22 
# The benchmark portfolio is I buy the stock at 100 at the beginning and at
# the end of the period the stock price is 120. I end up with 120.
# 122.22 > 120. so the MACD strategy was beating the market.


budget = 100
price = seq(from=10, to=200, by=10)
for(i in 1:2){
stockprice = sample(price, 1)
  if(stockprice < budget){
    budget = budget - stockprice
  } else {
    budget = budget + stockprice
  }
 print(budget)
}






















