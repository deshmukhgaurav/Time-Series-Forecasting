require(forecast)

getdata <- function(x){
	# Reads data from the file passed to the function
	#
	# args:
	# x - Contains the file name with extension to be read
	#
	# returns:
	# 	A matrix containing all the sales value for each day of
	# all 100 products. The first row contains the overall for 
	# each day.
    data<-as.matrix(read.table(x,header=FALSE))
    
    # Transpose the original data for the convinience
    trans<-t(data)

    # Create a new matrix to also hold the overall sales
    train <- matrix(data=NA, nrow=nrow(trans)-1, ncol=ncol(trans)+1)

    # Sum up the sales for each day in first row
    for(i in c(1:nrow(trans)-1)){
    	train[,1][i] <- sum(data[,i+1])
    }

    # Simply copy the data
    for(i in c(1:ncol(trans))){
    	train[,i+1] <- trans[,i][2:nrow(trans)]
    }
    return(train)
}

get_key_id <- function(x){
	# Reads key product ids
	#
	# args:
	# x - Contains the file name with extension to be read
	#
	# returns:
	#	A matrix containing key product ids.
	data<-as.matrix(read.table(x,header=FALSE))
	return(data)
}

forecast.stl<-function(x, n.ahead=28) {
	# Computes data-trend prediction using STL with ETS method
	#
	# args:
	# x - Contains list of sales for each 118 days for a key product
	#
	# returns - List of sales for next 28 days

	# Data preprocessing to remove the 0s. So that we can perform 
	# logarithmic operation on the data to reduce it for greater 
	# accuracy.
	for(i in c(1:length(x)))
			x[i]=x[i]+2.71

	# Creating a time series object of the preprocessed data by 
	# performing log on the data.
	# Frequency is set to 30 as the it is a daily data for couple of months.
	# Therefore frequency = 30/1 = 30
	myTs<-ts(log(x), start=1, frequency=30)

	# Performing Seasonal Decomposition of Time Series by Loess (STL) and Error Trend Seasonal (ETS) method
	# stlf() combines stlm and forecast.stlm. It takes a ts argument, 
	# applies an STL decomposition, models the seasonally adjusted data, 
	# reseasonalizes, and returns the forecasts.
	fc<-stlf(myTs, 
             h=n.ahead, 
             s.window=2, 
             method='ets',
             ic='bic',
             opt.crit='mae')

	# As we had performed logarithmic operation to get the original forecast data we have to perform
	# exponentail operation.
	pred <- exp(fc$mean)

	# Post Processing of data making sure that negative values are set to 0.
	# Subtracting the value of constant e added while data preprocessing.
	# Rounding up the value to the closesd whole number.
  	for(i in c(1:n.ahead)){
  		pred[i]=pred[i]-2.71
  		pred[i]=round(pred[i]/1)*1
  		if(pred[i]<0)
  			pred[i]=0
  	}
	return(pred)
}

get_final <- function(result, key_id, nrow, ncol){
	# Compute the final output matrix containing key product id as the first column and
	# predicted data in rest of the matrix
	#
	# args:
	# result - A matrix containing predicted data for the overall sales and all products 
	# for each days.
	# key_id - A list containing all the key product id
	# nrow - number of rows in the final output matrix
	# ncol - number of columns in the final output matrix
	#
	# returns:
	# Final output matrix containing key product id as the first column and predcited data
	# in rest of the matrix
	
	# Initializing the output matrix
	output<-matrix(data=0, nrow=nrow, ncol=ncol)

	# Inserting key product ids in the first column of output matrix
	for(i in c(1:100)){
		output[,1][i+1]<-key_id[,1][i]
	}

	# Inserting predicted data into the output matrix
	tresult<-t(result)
	for(i in c(1:101)){
		for(j in c(1:28))
		output[,j+1][i]<-tresult[,j][i]
	}
	return(output)
}