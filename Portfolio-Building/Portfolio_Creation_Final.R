#Acquisition Approximation
library(PerformanceAnalytics)
Portfolio<-function(data_old,value,cash,data,monthly_adjusted,i,size)
{
  Invested_value_old <-sum(as.numeric(value)*as.numeric(monthly_adjusted[i,data_old]))
  Portfolio_value<-Invested_value_old+cash
  equal_amount<-Portfolio_value/size
  price<-as.numeric(monthly_adjusted[i,data])
  new_composition<- floor(equal_amount/price)
  stock_compostion <- data
  Invested_value<-sum(as.numeric(new_composition)*as.numeric(monthly_adjusted[i,data]))
  cash<-Portfolio_value-Invested_value
  results <- list()
  results$first <- new_composition
  results$second<-cash
  results$third<-Invested_value
  return(results) 
}

Portfolio_Style<-function(size,capital){
  
  data<-rank_data[1,1:size]  
  value <- as.numeric(floor(capital/(size*monthly_adjusted[12,data])))
  Invested_value<-sum(value*monthly_adjusted[12,data])
  cash <- capital - Invested_value
  value_data<-matrix(0,nrow=1,ncol=size)
  stock_composition<-matrix(0,nrow=1,ncol=size)
  stock_composition[1,] <- as.numeric(data)
  cash_data<- vector()
  Portfolio_Value<-vector()
  value_data[1,]<-value
  cash_data[1]<-cash
  Portfolio_Value[1]<-capital
  for(i in 13:nrow(monthly_adjusted))
  {
    
    print(i)
    data_old<-rank_data[i-12,1:size]
    data<-    rank_data[i-11,1:size]
    value<-   value_data[i-12,]
    cash<-    cash_data[i-12]
    result<-Portfolio(data_old,value,cash,data,monthly_adjusted,i,size)
    print(result)
    value_data<-rbind(value_data,result$first)
    cash_data[i-11]<-result$second
    Portfolio_Value[i-11]<-result$second+result$third
    stock_composition<-rbind(stock_composition,as.numeric(data))
  }
  
  date<-index(monthly_adjusted[13:nrow(monthly_adjusted),1])
  date1<-index(monthly_adjusted[12:nrow(monthly_adjusted),1])
  Portfolio_Value <- xts(Portfolio_Value,date1)
  assign(paste("Portfolio_Value_",size, sep=""),Portfolio_Value,envir=.GlobalEnv)
  return <- diff(log(Portfolio_Value))
  return<-return[!is.na(return)]
  return<-xts(return,date)
  assign(paste("return_",size, sep=""),return,envir=.GlobalEnv)
  cagr<-Return.annualized(return)
  assign(paste("cagr_",size, sep=""),cagr,envir=.GlobalEnv)
  sd_an<-sqrt(12)*sd(return)
  assign(paste("sd_an_",size, sep=""),sd_an,envir=.GlobalEnv)
  sharpe_ratio<-cagr/sd_an
  assign(paste("sharpe_ratio_",size, sep=""),sharpe_ratio,envir=.GlobalEnv)
  print(cagr)
  print(sd_an)
  print(sharpe_ratio)
  par(mfrow=c(2,1))
  plot(return)
  plot(Portfolio_Value)
}

#plot(cash_data)

Portfolio_Style(5,1000000)
Portfolio_Style(10,1000000)
Portfolio_Style(16,1000000)
Portfolio_Style(20,1000000)
Portfolio_Style(25,1000000)
Portfolio_Style(30,1000000)
Portfolio_Style(35,1000000)
Portfolio_Style(40,1000000)
Portfolio_Style(45,1000000)
Portfolio_Style(50,1000000)
Portfolio_Style(55,1000000)

z.two_samplez.pooled_test<-function(a,b, mu1,mu2,var1,var2){
  sp_2<- (var(a)+var(b))/2
  zeta = (mean(a)-mean(b)-(mu1-mu2))/(sqrt(sp_2/(2*length(a))))
  return(zeta)
}

sd<-c(sd_an_5,sd_an_10,sd_an_16,sd_an_20,sd_an_25,sd_an_30,sd_an_35,sd_an_40,sd_an_45)
cagr<-c(cagr_5,cagr_10,cagr_16,cagr_20,cagr_25,cagr_30,sd_an_35,sd_an_40,sd_an_45)
size_no<-c(5,10,16,20,25,30,35,40,45)
plot(sd,cagr,ylim=c(0.0,0.2),xlab="Annualised Standard Deviation",ylab="Annualised Return",main="Vanilla Portfolio")
text(sd, cagr, labels=size_no, cex= 0.7,pos=3)



size_1<- cbind(rep(5,length(return_5)),  return_5)
size_2<- cbind(rep(10,length(return_10)),return_10)
size_3<- cbind(rep(16,length(return_16)),return_16)
size_4<- cbind(rep(20,length(return_20)),return_20)
size_5<- cbind(rep(25,length(return_25)),return_25)
size_6<- cbind(rep(30,length(return_30)),return_30)
size_7<- cbind(rep(35,length(return_35)),return_35)
size_8<- cbind(rep(40,length(return_40)),return_40)
size_9<- cbind(rep(45,length(return_45)),return_45)
#size_10<- cbind(rep(50,length(return_50)),return_50)
#size_11<- cbind(rep(55,length(return_55)),return_55)

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
  print(mean(a))
  print(mean(b))
  print(var(a))
  print(var(b))
  return(zeta)
}

test_statistics_5_16 <- z.two_samplez.test(return_5,return_16,0.01,0.01,var(return_5),var(return_16))
test_statistics_5_16

test_statistics_5_10 <- z.two_samplez.test(return_5,return_10,0.01,0.01,var(return_5),var(return_10))
test_statistics_5_10

test_statistics_5_35 <- z.two_samplez.test(return_5,return_35,0.01,0.01,var(return_5),var(return_35))
test_statistics_5_35

test_statistics_2_16 <- z.two_samplez.test(return_2,return_16,0.01,0.01,var(return_2),var(return_16))
test_statistics_2_16

plot(as.numeric(return_5),col="red",type="l")
lines(as.numeric(return_35))

date1<-index(monthly_adjusted[12:nrow(monthly_adjusted),1])
plot(date1,as.numeric(Portfolio_Value_5),col="red",type="l",ylab="Portfolio Value",main="Vanilla Portfolio")
lines(date1,as.numeric(Portfolio_Value_10),col="blue")
lines(date1,as.numeric(Portfolio_Value_16),col="green")
lines(date1,as.numeric(Portfolio_Value_20),col="grey")
lines(date1,as.numeric(Portfolio_Value_25),col="pink")
lines(date1,as.numeric(Portfolio_Value_30),col="orange")
lines(date1,as.numeric(Portfolio_Value_35),col="yellow")
lines(date1,as.numeric(Portfolio_Value_40),col="violet")
lines(date1,as.numeric(Portfolio_Value_45),col="black")
legend("topleft",
c("size=5",
"size=10",
"size=16",
"size=20",
"size=25",
"size=30",
"size=35",
"size=40",
"size=45"),lty=c(1,1,1,1,1,1,1,1,1),cex=0.4,
col=c("red","blue","green","grey","pink","orange","yellow","violet","black")) # gives the legend lines the correct color and width

date1<-index(monthly_adjusted[12:nrow(monthly_adjusted),1])
Date<-date1
Van5<-as.numeric(Portfolio_Value_5)
Van10<-as.numeric(Portfolio_Value_10)
Van16<-as.numeric(Portfolio_Value_16)
Van20<-as.numeric(Portfolio_Value_20)
Van25<-as.numeric(Portfolio_Value_25)
Van30<-as.numeric(Portfolio_Value_30)
Van35<-as.numeric(Portfolio_Value_35)
Van40<-as.numeric(Portfolio_Value_40)
Van45<-as.numeric(Portfolio_Value_45)
temp_data<-data.frame(Date,Van5,Van10,Van16,Van20,Van25,Van30,Van35,Van40,Van45)

ggplot(data=temp_data, aes(x=Date)) +
  geom_line(aes(y = Van5,colour="Size = 5"))+
  geom_line(aes(y = Van10,colour="Size =10"))+
  geom_line(aes(y = Van16,colour="Size =16"))+
  geom_line(aes(y = Van20,colour="Size =20"))+
  geom_line(aes(y = Van25,colour="Size =25"))+
  geom_line(aes(y = Van30,colour="Size =30"))+
  geom_line(aes(y = Van35,colour="Size =35"))+
  geom_line(aes(y = Van40,colour="Size =40"))+
  geom_line(aes(y = Van45,colour="Size =45"))+ylab("Portfolio Value")+labs(title = "Vanilla Portfolio")+theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_discrete(name  ="Size",
                        labels=c("Size = 5", "Size = 10","Size = 16","Size = 20","Size = 25","Size = 30","Size = 35","Size = 40","Size = 45"))