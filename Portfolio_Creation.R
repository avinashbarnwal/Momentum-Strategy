#Acquisition Approximation
#monthly_adjusted[105,is.na(monthly_adjusted[105,])]<-monthly_adjusted[104,is.na(monthly_adjusted[105,])]
library(PerformanceAnalytics)
size<-16
Portfolio<-function(data_old,value,cash,data,monthly_adjusted,i)
{

  Invested_value_old <-sum(as.numeric(value)*as.numeric(monthly_adjusted[i,data_old]))
  Portfolio_value<-Invested_value_old+cash
  new_composition<- as.numeric(floor(Portfolio_value/(size*monthly_adjusted[i,data])))
  stock_compostion <- data
  Invested_value<-sum(as.numeric(new_composition)*as.numeric(monthly_adjusted[i,data]))
  print(Invested_value)
  cash<-Portfolio_value-Invested_value
  results <- list()
  results$first <- new_composition
  results$second<-cash
  results$third<-Invested_value
  return(results) 
}

data<-rank_data[1,1:size]  

value <- as.numeric(floor(1000000/(size*monthly_adjusted[12,data])))
Invested_value<-sum(value*monthly_adjusted[12,data])
cash <- 1000000 - Invested_value
value_data<-matrix(0,nrow=1,ncol=size)
stock_composition<-matrix(0,nrow=1,ncol=size)
stock_composition[1,] <- as.numeric(data)
cash_data<- vector()
Portfolio_Value<-vector()
value_data[1,]<-value
cash_data[1]<-cash
Portfolio_Value[1]<-1000000
nrow(monthly_adjusted)
for(i in 13:nrow(monthly_adjusted))
{
  print(i)
  data_old<-rank_data[i-12,1:size]
  data<-    rank_data[i-11,1:size]
  value<-   value_data[i-12,]
  cash<-    cash_data[i-12]
  result<-Portfolio(data_old,value,cash,data,monthly_adjusted,i)
  print(result)
  value_data<-rbind(value_data,result$first)
  cash_data[i-11]<-result$second
  Portfolio_Value[i-11]<-result$second+result$third
  stock_composition<-rbind(stock_composition,as.numeric(data))
}

date<-index(monthly_adjusted[13:nrow(monthly_adjusted),1])
return <- diff(log(Portfolio_Value))
return<-xts(return,date)
cagr<-Return.annualized(return)
sd_an<-sqrt(12)*sd(return)
sharpe_ratio<-cagr/sd_an
print(cagr)
print(sd_an)
print(sharpe_ratio)
plot(return)

n<-length(return)

z.test = function(a, mu, var){
  zeta = (mean(a) - mu) / (sqrt(var/length(a)))
  return(zeta)
}

test_statistics <- z.test(return,0.01,var(return))


Portfolio_Value_5<-Portfolio_Value
Portfolio_Value_16<-Portfolio_Value

return_5<-diff(log(Portfolio_Value_5))
return_16<-diff(log(Portfolio_Value_16))

var.test(return_5,return_16,alternative="two.sided",conf.level = 0.95)

sp<- sqrt((var(return_5)+var(return_16))/2)

z.two_samplez.test<-function(a,b, mu1,mu2,var1,var2){
  sp_2<- (var(a)+var(b))/2
  zeta = (mean(a)-mean(b)-(mu1-mu2))/(sqrt(sp_2/(2*length(a))))
  return(zeta)
}

test_statistics <- z.two_samplez.test(return_5,return_16,0.01,0.01,var(return_5),var(return_16))
