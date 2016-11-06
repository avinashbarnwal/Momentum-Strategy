#For First Stock
library(xts)
monthly_adjusted<- apply.monthly(dataset_adjusted[,1], tail, 1)
time <- length(monthly_adjusted)
return_data <- vector()

for(i in 1:(time-11))
{
  return_data[i+11]<- (as.numeric(monthly_adjusted[i+11])/as.numeric(monthly_adjusted[i]))-1
}
date<-index(monthly_adjusted[,1])
return_data<-xts(return_data,date)
colnames(return_data)<-colnames(dataset_adjusted[,1])

#For Rest Stock Market
for(j in 2:ncol(dataset_adjusted))
{
  print(j)
  data_monthly<- apply.monthly(dataset_adjusted[,j], tail, 1)
  time <- length(data_monthly)
  return <- vector()
  monthly_adjusted<-cbind(monthly_adjusted,data_monthly)
  for(i in 1:(time-11))
  {
    return[i+11]<- (as.numeric(data_monthly[i+11])/as.numeric(data_monthly[i]))-1
  }
  date<-index(monthly_adjusted[,1])
  return<-xts(return,date)
  colnames(return)<-colnames(dataset_adjusted[,j])
  return_data<-cbind(return_data,return)
}  

for(i in 12:time)
{
  if(i==12)
  {
    print(i)
    rank_data<-t(order(-return_data[i,]))
    date<-index(return_data[i,])
    rank_data<-xts(rank_data,date)
  }
  if(i>12)
  {
    print(i)
    rank<-t(order(-return_data[i,]))
    date<-index(return_data[i,])
    rank<-xts(rank,date)
    rank_data<-rbind(rank_data,rank)
  }
  
}


rank_data1<-data.frame(rank_data)
rank_data2=t(rank_data1)
inds = which(rank_data2 == 1, arr.ind=TRUE)
final_rank_data<-as.numeric(inds[,1])

for (i in 2:ncol(rank_data))
{
  inds = which(rank_data2 == i, arr.ind=TRUE)
  first_rank_data<-as.numeric(inds[,1])
  final_rank_data<-cbind(final_rank_data,first_rank_data)
}

colnames(final_rank_data)<-colnames(return_data)

date<-index(return_data[12:nrow(return_data[,1]),1])
final_rank_data<-xts(final_rank_data,date)
return_data1<-return_data[12:nrow(return_data[,1]),]


# Regression Part
number<-14

par(mar=c(5,4,4,4)+.1)
plot(x=date[2:13],y=final_rank_data[2:13,number],main=colnames(final_rank_data)[number],xlab="Time",ylab="Rank",col="red",type="l")
par(new = TRUE)
plot(return_data1[1:12,number],main="",xaxt="n",yaxt="n",xlab="",ylab="",col="blue",type="l")
axis(4)
mtext("Return",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Rank","Return"))
abline(lm(return_data1[2:13,number]~final_rank_data[1:12,number]))

summary(lm(return_data1[2:13,number]~final_rank_data[1:12,number]))
#abline(lm(height ~ bodymass))

54
23
21
128
41
9
158
68
30
27
32
137
34
1
57
126
22
100
14





  