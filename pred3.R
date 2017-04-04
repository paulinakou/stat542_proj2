#Walmart data
library(data.table)
library(forecast)

predict3 <- function(){
  
  store = sort(unique(train$Store))
  n.store = length(store)
  dept = sort(unique(train$Dept))
  n.dept = length(dept)
  
  # Find the month and year we want to predict for
  if(t+2<=12){
    month=t+2
    year=2011
  }else{
    month=(t+2)-12
    year=2012
  }
  
  temp <-train[,1:5]
  fit <- setDT(temp)[, list(AR = list(auto.arima(Weekly_Sales))), by = .(Store,Dept)]
  
  for (s in 1:n.store){
    cat("Prediction 3, Store: ", store[s], "\n")
    for (d in 1:n.dept){
      if(sum(train$Dept==dept[d] & train$Store==store[s])!=0){

        #  find the data for (store, dept) = (s, d)
        #train.id = which(train$Store == store[s] & train$Dept == dept[d])
        #train.temp = train[train.id, ]
        test.id = which(test$Store == store[s] & test$Dept == dept[d] & month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y")))
        if(length(test.id)!=0){
          fit.id <- which(fit$Store== store[s] & fit$Dept == dept[d])
          if(fit.id>0){
            # number of steps ahead to forecast
            horizon = length(test.id)  
            fc <- forecast(fit$AR[[fit.id]],h=horizon)
            pred <- as.numeric(fc$mean)
            #test[test.id,"Weekly_Pred3"]<-pred
            test[test.id,"Weekly_Pred3"] <<- pred
          }else{
            test[test.id,"Weekly_Pred3"]<<-0
          }
        }
      }
    }
  }
}
