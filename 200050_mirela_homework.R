#Problem 1 ----
x <- seq(1,10)

for(i in x){
  print(c(x*3))
}



#Problem 2 ----
y <- rnorm(10,0,1)
print(y)
for(i in 1:10){
  if(y[i] > 1)
    print(y[i])
}



#Problem 3 ----
samplespace <- c(rep("man",6),rep("woman",8))
comb <- function(n,k){factorial(n)/factorial(n-k)/factorial(k)}
hyp.dis <- function(N,r,w,n,k){(comb(r,k)*comb(w,n-k))/comb(N,n)}

for(i in 1:10){
  i <- sample(samplespace, 5)
  print(i)
}

hyp.dis(14,6,8,5,3)

#Problem 4 ----

strike_price=120
results=0
profit=0


for(i in 1:1000){

price=100
for(j in 1:100){
  price=price + rnorm(1,0,7)
}

if (price > strike_price){
 profit = price - strike_price
  print(profit)
 results = results + profit }

}

print(results/1000)