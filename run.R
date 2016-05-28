source('data_trend_pred.R')

# Retrieve the sales data for 118 days for each 100 key products
# And calulate the overall sales for each day
train <- getdata("product_distribution_training_set.txt")

# Get the id for all the key products
key_id <- get_key_id("key_production_IDs.txt")

# Construct a result martix of 28 rows for each day and 101 columns for each
# 100 products (1 for the overall prediction)
result<- matrix(NA, nrow=28, ncol=101)

# Performing STL and ETS method on each row of the input data 
for(i in c(1:101)){
	result[,i]<-forecast.stl(train[,i])
}

# Getting the final output matrix containing key product ids and predicted data
output<-get_final(result, key_id, nrow=101, ncol=29)

# Writing the final output matrix to a file
write.table(output, "output.txt", row.names=FALSE, col.names=FALSE)