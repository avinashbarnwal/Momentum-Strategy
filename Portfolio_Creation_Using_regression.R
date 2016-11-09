#Acquisition Approximation
library(PerformanceAnalytics)
#Parameters
size<-16
Mom_length<-12
Portfolio<-function(data_old,value,cash,data,monthly_adjusted,i)
{
  
  Invested_value_old <-sum(as.numeric(value)*as.numeric(monthly_adjusted[i,data_old]))
  #print(Invested_value_old)
  #print(cash)
  Portfolio_value<-Invested_value_old+cash
  lm.sel<-vector()
  #print(data)
  #regression input and output data selection
  k<-0
  for (j in 1:length(data))
  {
    number<-as.numeric(data[,j])
    flag_rank_data   <- final_rank_data[(i-1-12):(i-13-12),number]
    #print(flag_rank_data)
    flag_return_data <- return_data[(i-12):(i-12-12),number]
    lm.res<-summary(lm(flag_return_data~flag_rank_data))
    #print(lm.res)
    p_value<-coef(lm.res)[2,4]
    if(p_value<0.05){
      k<-k+1
      lm.sel[k]<-number
     #print(data[,j])
    }
  }
 #print(data)
 #print(lm.sel)
 #colnames(monthly_adjusted[i,lm.sel])
  no <- length(lm.sel)
  new_composition <- as.numeric(floor(Portfolio_value/(no*monthly_adjusted[i,lm.sel])))
  stock_compostion <- lm.sel
  Invested_value <-sum(as.numeric(new_composition)*as.numeric(monthly_adjusted[i,lm.sel]))
  print(Invested_value)
  cash<-Portfolio_value-Invested_value
  results <- list()
  results$first  <- new_composition
  results$second <- cash
  results$third  <- Invested_value
  results$fourth <- stock_compostion
  return(results) 
}

i=25
data<-rank_data[i-12,1:size]  
lm.sel  <- vector()
p_value <- vector()
#print(data)
#regression input and output data selection
k<-0
for (j in 1:length(data))
{
  number<-as.numeric(data[,j])
  flag_rank_data   <- final_rank_data[(i-1-12):(i-12-12),number]
  #print(flag_rank_data)
  flag_return_data <- return_data[(i-12):(i-11-12),number]
  lm.res<-summary(lm(flag_return_data~flag_rank_data))
  #print(lm.res)
  p_value[j]<-coef(lm.res)[2,4]
  if(p_value[j]<0.05){
    k<-k+1
    lm.sel[k]<-number
  #print(data[,j])
  }
}

print(data)
print(lm.sel)
#print(monthly_adjusted[24,lm.sel])
#print(monthly_adjusted[24,data])
no<-length(lm.sel)
value <- as.numeric(floor(1000000/(no*monthly_adjusted[i,lm.sel])))
Invested_value<-sum(value*monthly_adjusted[i,lm.sel])
cash <- 1000000 - Invested_value
stock_composition <- list(lm.sel)
cash_data<- vector()
Portfolio_Value<-vector()
Portfolio_size<-vector()
value_data<-list(value)
cash_data[1]<-cash
Portfolio_size[1]<-no
Portfolio_Value[1]<-1000000
nrow(monthly_adjusted)
print(value)
print(cash)
#print()

#for(i in 25:nrow(monthly_adjusted))
for(i in 26:nrow(monthly_adjusted))
{
  print(i)
  data_old <-    stock_composition[[i-25]]
  data     <-    rank_data[i-12,1:size]
  value    <-    value_data[[i-25]]
  cash     <-    cash_data[i-25]
  #if()
  result<-Portfolio(data_old,value,cash,data,monthly_adjusted,i)
  #print(result)
  value_data[[i-24]]<-result$first
  cash_data[i-24]<-result$second
  Portfolio_Value[i-24]<-result$second+result$third
  stock_composition[[i-24]]<-result$fourth
}

date<-index(monthly_adjusted[25:nrow(monthly_adjusted),1])
Portfolio_Value<-xts(Portfolio_Value,date)
return <- diff(log(Portfolio_Value))
#return<-xts(return,date)
cagr<-Return.annualized(return)
sd_an<-sqrt(12)*sd(!is.na(return))
sharpe_ratio<-cagr/sd_an
print(cagr)
print(sd_an)
print(sharpe_ratio)
plot(return,type="l")
plot(Portfolio_Value,type="l")

#length(date)
#length(return)


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


lm.res<-summary(lm(return_data1[2:13,number]~final_rank_data[1:12,number]))
