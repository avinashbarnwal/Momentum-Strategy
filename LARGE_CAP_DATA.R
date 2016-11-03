sp500 <- new.env()
dataset <- getSymbols(LARGE_CAP[1], env=sp500,from="2008-01-01", src='yahoo',auto.assign=FALSE)
dataset_adjusted <- dataset[,6]
colnames(dataset_adjusted) <- LARGE_CAP[1]
n <- length(LARGE_CAP)

# Actual loop

for(i in 2:n) {
  symbol <- LARGE_CAP[i]
  # specify the "from" date to desired start date
  tryit <- try(getSymbols(symbol,env=sp500,from="2008-01-01", src='yahoo',auto.assign=FALSE))
  if(inherits(tryit, "try-error")){
    paste(symbol," data can't download")
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, env=sp500,from="2008-01-01", src='yahoo',auto.assign=FALSE)
    dataset <- merge(dataset, data)
    data_adjusted <- data[,6]
    colnames(data_adjusted) <- symbol
    dataset_adjusted <- merge(dataset_adjusted,data_adjusted)
    rm(symbol)
  }
 # setTxtProgressBar(pb, i)
}

write.table(dataset,file="/Users/avinashbarnwal/Desktop/Fall-2016/Data Analysis-1/dataset.csv",sep=",")
write.table(dataset_adjusted,file="/Users/avinashbarnwal/Desktop/Fall-2016/Data Analysis-1/dataset_adjusted.csv",sep=",")




