#For First Stock
library(xts)

monthly_adjusted<- apply.monthly(dataset_adjusted[,1], tail, 1)
time <- length(monthly_adjusted)
return_data <- vector()
#Run this statement always when you rerun the procedure again

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

monthly_adjusted[105,is.na(monthly_adjusted[105,])]<-monthly_adjusted[104,is.na(monthly_adjusted[105,])]
monthly_adjusted[106,is.na(monthly_adjusted[106,])]<-monthly_adjusted[105,is.na(monthly_adjusted[106,])]
monthly_adjusted[107,is.na(monthly_adjusted[107,])]<-monthly_adjusted[106,is.na(monthly_adjusted[107,])]

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

return_data<-return_data[12:nrow(return_data),]
colnames(final_rank_data)<-colnames(return_data)

assign('final_rank_data',final_rank_data,envir=.GlobalEnv)
assign('return_data',return_data,envir=.GlobalEnv)
assign('rank_data',rank_data,envir=.GlobalEnv)
assign('monthly_adjusted',monthly_adjusted,envir=.GlobalEnv)


