#Walmart data

#change Date from factor class to date class

predict2 <- function(){
  train$Date = as.Date(train$Date, '%Y-%m-%d')
  test$Date = as.Date(test$Date, '%Y-%m-%d')
  
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
  
  if(t<10){
    simple_pred()
  }else{
    all.stores = unique(test$Store)
    num.stores = length(all.stores)
    train.dates = unique(train$Date)
    num.train.dates = length(train.dates)
    train.frame = data.frame(Date=rep(train.dates, num.stores),
                             Store=rep(all.stores, each=num.train.dates))

    # Dimension reduction using SVD.
    n.comp = 12 # keep first 12 components
    preprocess.svd = function(train, n.comp){
      train[is.na(train)] = 0
      z = svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
      s = diag(z$d[1:n.comp])
      train[, 2:ncol(train)] = z$u %*% s %*% t(z$v)
    }
    
    for(d in 1:99){  # first department
      tr.d = train.frame
      tr.d = join(tr.d, train[train$Dept==d, c('Store','Date','Weekly_Sales')])  # perform a left join.
      tr.d = cast(tr.d, Date ~ Store)  # row is Date, col is Store, entries are the sales
      # apply SVD for tr.d
      tr.d = preprocess.svd(tr.d, n.comp)   
      
      test.dates = unique(test$Date)
      num.test.dates = length(test.dates)
      forecast.frame = data.frame(Date=rep(test.dates, num.stores),
                                  Store=rep(all.stores, each=num.test.dates))
      fc.d = forecast.frame
      fc.d$Weekly_Sales = 0
      fc.d = cast(fc.d, Date ~ Store)  # similar as tr.d
      
      horizon = length(unique(test[which(month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y"))),"Date"]))  # number of steps ahead to forecast
      for(j in 2:ncol(tr.d)){ # loop over stores
        s = ts(tr.d[, j], frequency = 52)  # convert sales to time series. 
        fc = stlf(s, h=horizon, s.window=3, method='arima', ic='bic')
        #arma_fit <- auto.arima(s)
        #arma_forecast <- forecast(arma_fit,h=horizon)
        pred = as.numeric(fc$mean)
        id = which(test$Store==j-1 & test$Dept==d & 12==as.numeric(format(test$Date,"%m")) & 2012==as.numeric(format(test$Date,"%Y")))
        test[id,"Weekly_Pred2"]<<-pred
        #test[id,"Weekly_Pred3"]<-pred
        #test[id,"Weekly_Pred3"]
      }
    }
  }
}

