#Acquisition Approximation
library(PerformanceAnalytics)
library(ggplot2)
#Parameters
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
    flag_rank_data   <- final_rank_data[(i-13):(i-24),number]
    #print(flag_rank_data)
    flag_return_data <- as.numeric(return_data[(i-12):(i-23),number])
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


Portfolio_style_regression<-function(size,capital)
{
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
  flag_rank_data   <- final_rank_data[(i-13):(i-24),number]
  #print(flag_rank_data)
  flag_return_data <- as.numeric(return_data[(i-12):(i-23),number])
  lm.res<-summary(lm(flag_return_data~flag_rank_data))
  #print(lm.res)
  p_value[j]<-coef(lm.res)[2,4]
  if(p_value[j]<0.05){
    k<-k+1
    lm.sel[k]<-number
  #print(data[,j])
  }
}



no<-length(lm.sel)
value <- as.numeric(floor(capital/(no*monthly_adjusted[i,lm.sel])))
Invested_value<-sum(value*monthly_adjusted[i,lm.sel])
cash <- capital - Invested_value
stock_composition <- list(lm.sel)
cash_data<- vector()
Portfolio_Value<-vector()
Portfolio_size<-vector()
value_data<-list(value)
cash_data[1]<-cash
Portfolio_size[1]<-no
Portfolio_Value[1]<-capital
nrow(monthly_adjusted)
print(value)
print(cash)

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
return<-return[!is.na(return)]
print(return)
cagr<-Return.annualized(return)
sd_an<-sqrt(12)*sd(!is.na(return))
sharpe_ratio<-cagr/sd_an
print(cagr)
print(sd_an)
print(sharpe_ratio)
plot(return,type="l")
plot(Portfolio_Value,type="l")
assign(paste("Portfolio_Value_reg",size, sep=""),Portfolio_Value,envir=.GlobalEnv)
assign(paste("return_reg",size, sep=""),return,envir=.GlobalEnv)
cagr<-Return.annualized(return)
assign(paste("cagr_reg",size, sep=""),cagr,envir=.GlobalEnv)
sd_an<-sqrt(12)*sd(return)
assign(paste("sd_an_reg",size, sep=""),sd_an,envir=.GlobalEnv)
sharpe_ratio<-cagr/sd_an
assign(paste("sharpe_ratio_reg",size, sep=""),sharpe_ratio,envir=.GlobalEnv)

}
length(date)
length(return)
Portfolio_style_regression(5,1000000)
Portfolio_style_regression(10,1000000)
Portfolio_style_regression(16,1000000)
Portfolio_style_regression(20,1000000)
Portfolio_style_regression(25,1000000)
Portfolio_style_regression(30,1000000)



par(mfrow=c(1,1))
sd<-c(sd_an_reg5,sd_an_reg10,sd_an_reg16,sd_an_reg20,sd_an_reg25,sd_an_reg30)
cagr<-c(cagr_reg5,cagr_reg10,cagr_reg16,cagr_reg20,cagr_reg25,cagr_reg30)
size_no<-c(5,10,16,20,25,30)
plot(sd,cagr,ylim=c(0.0,0.2),xlab="Annualised Standard Deviation",ylab="Annualised Return",main="With Regression Filter")
text(sd, cagr, labels=size_no, cex= 0.7,pos=3)



size_1<- cbind(rep(5,length(return_reg5)),  return_5)
size_2<- cbind(rep(10,length(return_10)),return_10)
size_3<- cbind(rep(16,length(return_16)),return_16)
size_4<- cbind(rep(20,length(return_20)),return_20)
size_5<- cbind(rep(25,length(return_25)),return_25)
size_6<- cbind(rep(30,length(return_30)),return_30)
size_7<- cbind(rep(35,length(return_35)),return_35)
size_8<- cbind(rep(40,length(return_40)),return_40)
size_9<- cbind(rep(45,length(return_45)),return_45)
size_testing<-rbind(size_1,size_2,size_3,size_4,size_5,size_6,size_7,size_8,size_9)

colnames(size_testing)<-c("size_no","return")

return_data<-as.numeric(size_testing$return)
size_data<-as.character(size_testing$size_no)

fit <- aov(return_data~size_data)
summary(fit)


bartlett.test(return_data, size_data)


z.two_samplez.test<-function(a,b, mu1,mu2,var1,var2){
  sp_2<- (var(a)+var(b))/2
  zeta = (mean(a)-mean(b)-(mu1-mu2))/(sqrt(sp_2/(2*length(a))))
  return(zeta)
}

test_statistics <- z.two_samplez.test(return_5,return_16,0.01,0.01,var(return_5),var(return_16))

test_statistics <- z.two_samplez.test(return_35,return_reg25,0.01,0.01,var(return_35),var(return_reg25))


date1<-index(monthly_adjusted[25:nrow(monthly_adjusted),1])
plot(date1,as.numeric(Portfolio_Value_reg5),col="red",type="l",xlab="Date",ylab="Portfolio Value",main="With Regression Filter")
lines(date1,as.numeric(Portfolio_Value_reg10),col="blue")
lines(date1,as.numeric(Portfolio_Value_reg16),col="green")
lines(date1,as.numeric(Portfolio_Value_reg20),col="grey")
lines(date1,as.numeric(Portfolio_Value_reg25),col="pink")
lines(date1,as.numeric(Portfolio_Value_reg30),col="orange")
legend("topleft",
       c("size=5",
         "size=10",
         "size=16",
         "size=20",
         "size=25",
         "size=30"),lty=c(1,1,1,1,1,1),cex=0.8,pt.cex = 1,
       col=c("red","blue","green","grey","pink","orange"),bty = "n") # gives the legend lines the correct color and width


Date<-date1
Reg5<-as.numeric(Portfolio_Value_reg5)
Reg10<-as.numeric(Portfolio_Value_reg10)
Reg16<-as.numeric(Portfolio_Value_reg16)
Reg20<-as.numeric(Portfolio_Value_reg20)
Reg25<-as.numeric(Portfolio_Value_reg25)
Reg30<-as.numeric(Portfolio_Value_reg30)
temp_data<-data.frame(Date,Reg5,Reg10,Reg16,Reg20,Reg25,Reg30)




ggplot(data=temp_data, aes(x=Date)) +
  geom_line(aes(y = Reg5, colour="Size = 5"))+
  geom_line(aes(y = Reg10,colour="Size =10"))+
  geom_line(aes(y = Reg16,colour="Size =16"))+
  geom_line(aes(y = Reg20,colour="Size =20"))+
  geom_line(aes(y = Reg25,colour="Size =25"))+
  geom_line(aes(y = Reg30,colour="Size =30"))+ylab("Portfolio Value")+labs(title = "With Regression Portfolio")+theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_discrete(name  ="Size",
                              labels=c("Size = 5", "Size = 10","Size = 16","Size = 20","Size = 25","Size = 30"))