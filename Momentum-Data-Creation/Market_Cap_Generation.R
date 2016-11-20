library(quantmod)
#Source 
data_cap <- read.csv(file="/Users/avinashbarnwal/Desktop/Market_Cap.csv",header=T,stringsAsFactors =FALSE )

#Large Cap - 5000
#Mid Cap   - 1000
#Small Cap - <1000

for (i in 1:665)
{
  if(data_cap[i,3] >= 10000  & !is.na(data_cap[i,3]) &  
     data_cap[i,4] >= 10000  & !is.na(data_cap[i,4]) &
     data_cap[i,5] >= 10000  & !is.na(data_cap[i,5]) &
     data_cap[i,6] >= 10000  & !is.na(data_cap[i,6]) &
     data_cap[i,7] >= 10000  & !is.na(data_cap[i,7]) & 
     data_cap[i,8] >= 10000  & !is.na(data_cap[i,8]) & 
     data_cap[i,9] >= 10000  & !is.na(data_cap[i,9]) &
     data_cap[i,10] >= 10000  & !is.na(data_cap[i,10]))
  {
    data_cap[i,11]="L"
  }
  
  
}

for (i in 1:665)
{
  if(data_cap[i,3] >= 2000  & !is.na(data_cap[i,3]) &  data_cap[i,3] < 10000 &
     data_cap[i,4] >= 2000  & !is.na(data_cap[i,4]) &  data_cap[i,4] < 10000 &
     data_cap[i,5] >= 2000  & !is.na(data_cap[i,5]) &  data_cap[i,5] < 10000 &
     data_cap[i,6] >= 2000  & !is.na(data_cap[i,6]) &  data_cap[i,6] < 10000 &
     data_cap[i,7] >= 2000  & !is.na(data_cap[i,7]) &  data_cap[i,7] < 10000 &
     data_cap[i,8] >= 2000  & !is.na(data_cap[i,8]) &  data_cap[i,8] < 10000 &
     data_cap[i,9] >= 2000  & !is.na(data_cap[i,9]) &  data_cap[i,9] < 10000 &
     data_cap[i,10] >= 2000  & !is.na(data_cap[i,10]) & data_cap[i,10] < 10000 )
  {
    data_cap[i,11]="M"
  }  
}


for (i in 1:665)
{
  if( !is.na(data_cap[i,3]) &  data_cap[i,3] < 2000 &
      !is.na(data_cap[i,4]) &  data_cap[i,4] < 2000 &
      !is.na(data_cap[i,5]) &  data_cap[i,5] < 2000 &
      !is.na(data_cap[i,6]) &  data_cap[i,6] < 2000 &
      !is.na(data_cap[i,7]) &  data_cap[i,7] < 2000 &
      !is.na(data_cap[i,8]) &  data_cap[i,8] < 2000 &
      !is.na(data_cap[i,9]) &  data_cap[i,9] < 2000 &
      !is.na(data_cap[i,10]) &  data_cap[i,10] < 2000 )
  {
    data_cap[i,11]="S"
  }  
}

#table(data_cap[,11])

LARGE_CAP <- data_cap[data_cap[,11]=="L" & !is.na(data_cap[,11]),1]
MID_CAP   <- data_cap[data_cap[,11]=="M" & !is.na(data_cap[,11]),1]
