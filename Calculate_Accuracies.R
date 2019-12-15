# import the time series library
library(fpp2)

# set the working directory
setwd("/home/hendra/Datasets/WDI_Indonesia/")

# read the Y dataset
df.WDI.Y <- read.csv( "df_Y_WDI_with_year_as_rownames.csv" )

# divide the dataset into a train set and test set
WDI.train.1 <- window(ts(df.WDI.Y[,1], start = 1962), start = 1962, end = 2010)
WDI.test.1 <- window(ts(df.WDI.Y[,1], start = 1962), start = 2011)

# train the average method 
WDI.train.1.fit1 <- meanf(WDI.train.1, h = 3)

# train the random walk method
WDI.train.1.fit2 <- rwf(WDI.train.1, h = 3)

# train seasonal naive method
WDI.train.1.fit3 <- snaive(WDI.train.1, h = 3)

# train the drift method
WDI.train.1.fit4 <- rwf(WDI.train.1, h = 3, drift = TRUE)

# Evaluating forecast accuracy 
accuracy(WDI.train.1.fit1, WDI.test.1) # metode rata-rata
accuracy(WDI.train.1.fit2, WDI.test.1) # metode prediksi random walk atau disebut juga naive
accuracy(WDI.train.1.fit3, WDI.test.1) # metode seasonal naive
accuracy(WDI.train.1.fit4, WDI.test.1) # metode drift

# divide the dataset into a train set
WDI.train.2 <- window(ts(df.WDI.Y[,2], start = 1962), start = 1962, end = 2010)
WDI.test.2 <- window(ts(df.WDI.Y[,2], start = 1962), start = 2011)

# train the average method 
WDI.train.2.fit1 <- meanf(WDI.train.2, h = 3)

# train the random walk method
WDI.train.2.fit2 <- rwf(WDI.train.2, h = 3)

# train seasonal naive method
WDI.train.2.fit3 <- snaive(WDI.train.2, h = 3)

# train the drift method
WDI.train.2.fit4 <- rwf(WDI.train.2, h = 3, drift = TRUE)

# Evaluating forecast accuracy 
accuracy(WDI.train.2.fit1, WDI.test.2) 
accuracy(WDI.train.2.fit2, WDI.test.2)
accuracy(WDI.train.2.fit3, WDI.test.2)
accuracy(WDI.train.2.fit4, WDI.test.2)

# Plot all baselines methods
autoplot( WDI.train.1 ) +
  autolayer( WDI.train.1.fit1, series="Mean", PI=FALSE ) +
  autolayer( WDI.train.1.fit2, series="Naive", PI=FALSE ) +
  autolayer( WDI.train.1.fit3, series="Seasonal", PI=FALSE ) +
  autolayer( WDI.train.1.fit4, series = "Drift", PI=FALSE ) +
  xlab( "Tahun" ) + ylab( "Rupiah" ) +
  ggtitle("Prediksi GDP dari Metode baselines") +
  guides( colour = guide_legend(title="Forecast") )






