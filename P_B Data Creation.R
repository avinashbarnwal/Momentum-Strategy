library(quantmod)
library(sqldf)

#Source 
P_B_Data <- read.csv(file="/Users/avinashbarnwal/Desktop/Fall-2016/Data Analysis-1/Project/P_B_DATA.csv",header=T,stringsAsFactors =FALSE )

P_B_Data<-P_B_Data[,-2]
Column<-P_B_Data[,1]
P_B_Data<-P_B_Data[,-1]
P_B_Data_t<- t(P_B_Data)
colnames(P_B_Data_t)<-Column
flag<-rownames(P_B_Data_t)
temp<-strsplit(flag, "X")
temp_matrix<-matrix(unlist(temp), ncol=2, byrow=TRUE)
date<-temp_matrix[,2]

date<-as.Date(date,"%m.%d.%Y")
P_B_final_Data<- xts(P_B_Data_t,date)

#Massaging for 2011-12-30 as data is not available

dataset_adjusted_P_B<-dataset_adjusted[,-150]
add<-t(as.numeric(dataset_adjusted_P_B["2011-12-30"]))
add<-xts(add,as.Date("2011-12-31","%Y-%m-%d"))
colnames(add)<-colnames(dataset_adjusted_P_B)
dataset_adjusted_P_B<-rbind(dataset_adjusted_P_B,add)

#Creating similar stock space

P_B_Selected_LARGE<-P_B_final_Data[,colnames(dataset_adjusted_P_B)]
colnames(P_B_Selected_LARGE)==colnames(dataset_adjusted_P_B)
P_data_subset<-dataset_adjusted_P_B[index(P_B_Selected_LARGE)]

book_data<-matrix(0,nrow=nrow(P_data_subset),ncol=ncol(P_data_subset))

for(i in 1:ncol(P_data_subset))
{
  book_data[,i]<-P_data_subset[,i]/P_B_Selected_LARGE[,i]
}

colnames(book_data)<-colnames(P_data_subset)
date<-index(P_data_subset)
book_data<-xts(book_data,date)
colnames(book_data)<-paste(colnames(book_data),"_b",sep="")

#Creating Monthly P/B Data from Yearly Monthly Data

book_data$Year <- as.numeric(format(index(book_data[,1]),"%Y"))
dataset_adjusted_P_B$Year <- as.numeric(format(index(dataset_adjusted_P_B[,1]),"%Y"))
dataset_adjusted_P_B$Date <- 10000*as.numeric(format(index(dataset_adjusted_P_B[,1]),"%Y"))+100*as.numeric(format(index(dataset_adjusted_P_B[,1]),"%m"))+as.numeric(format(index(dataset_adjusted_P_B[,1]),"%d"))
dataset_adjusted_P_B1<-as.data.frame(dataset_adjusted_P_B)
book_data1<-as.data.frame(book_data)
flag1<-ncol(dataset_adjusted_P_B)-3

P_B_Data_Created <- sqldf("select a.*,b.* from dataset_adjusted_P_B1 a left join book_data1 b on a.Year = b.Year")

temp<-ncol(P_B_Data_Created)

for(i in 1:flag1)
{
  j<- 167+i
  namecol<-colnames(dataset_adjusted_P_B1)[i]
  symbol<-paste(namecol,"_p_b",sep="")
  nocol<-ncol(P_B_Data_Created)
  P_B_Data_Created[,(nocol+1)]<-P_B_Data_Created[,i]/P_B_Data_Created[,j]
  print(P_B_Data_Created[,temp+1])
  colnames(P_B_Data_Created)[nocol+1]<-symbol
}

P_B_Data_Created1<-P_B_Data_Created[,c(166,333:496)]
date<-as.Date(as.character(P_B_Data_Created$Date),"%Y%m%d")
P_B_Data_Created1<-xts(P_B_Data_Created1,date)

monthly_adjusted<- apply.monthly(P_B_Data_Created1, tail, 1)

for(i in 1:nrow(monthly_adjusted))
{
  
  if(i==1)
  {
    print(i)
    rank_data<-t(order(monthly_adjusted[i,]))
    date<-index(monthly_adjusted[i,])
    rank_data<-xts(rank_data,date)
  }
  if(i>=2)
  {
    print(i)
    rank<-t(order(monthly_adjusted[i,]))
    date<-index(monthly_adjusted[i,])
    rank<-xts(rank,date)
    rank_data<-rbind(rank_data,rank)
  }
}


