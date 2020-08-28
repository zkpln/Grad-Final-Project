################################################
##### Summer Project
##### By Zak Kaplan, Jason Klotz
###############################################

rm(list=ls())
setwd("/Users/Zak/Documents/R")

library("forecast")
library("xts")
library("coop")

###################### Loading the Data

calendar <- read.delim("calendar.csv", sep = ",", header = TRUE)
head(calendar)

sales_train_validation <- read.delim("sales_train_validation.csv", sep = ",", header = TRUE)
dim(sales_train_validation)

sales_train_evaluation <- read.delim("sales_train_evaluation.csv", sep = ",", header = TRUE)
dim(sales_train_evaluation)
head(sales_train_evaluation)

sell_prices <- read.delim("sell_prices.csv", sep = ",", header = TRUE)
dim(sell_prices)
head(sell_prices)

load("medians.RData")
load("means.RData")
load("ranges.RData")
load("summaries.RData")
load("pred.RData")
load("pred_neural.RData")
load("rmse.RData")
load("rmse_neural.RData")
load("pred_actual.RData")
load("rmse_actual.RData")
load("pred_bats.RData")
load("rmse_bats.RData")
load("pred_bats3.RData")
load("rmse_bats3.RData")
load("rmse_bats4.RData")
load("pred_bats4.RData")
load("rmse_bats5.RData")
load("pred_bats5.RData")



############## Looking at a random item

plot(c(1:1913),sales_train_validation[2457,7:1919],type = "l", ylab = 'Units sold', xlab = 'Day')

dim(sales_train_validation)

item_2457 <- c(t(sales_train_evaluation[2457,7:1919]))


ts_item_2457 <- ts(item_2457, frequency = 365, start = c(2011,1), end = c(2016,6))

plot(ts_item_2457, ylab = 'Units sold', xlab = 'Year')

plot(forecast(item_2457))
fit <- bats(item_2457)
plot(fit)
fc <- forecast(item_2457, h= 20, model=fit)
fc


fit <- Arima(item_2457, order = c(1,0,3))
fc <- forecast(item_2457, h= 20, model=fit)
fc

fit <- auto.arima(item_2457)
fc <- forecast(item_2457, h= 20, model=fit)
fc

fit <- tbats(item_2457)
fc <- forecast(item_2457, h= 20, model=fit)
fc


fit <- nnetar(item_2457)
fc <- forecast(item_2457, h= 28, model=fit)
fc

plot(c(1:28), sales_train_evaluation[2457,1920:1947], type = 'p', ylim = c(0,10), xlab = 'Days', ylab = 'Units Sold', main = 'Item 2457 Predicted vs Actual')
lines(c(1:28), sales_train_evaluation[2457,1920:1947], col="black",lty=1)
points(c(1:28), fc$mean, col="red", pch="*")
lines(c(1:28), fc$mean, col="red",lty=2)
legend(1, 9, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

names(sales_train_evaluation)
names(fc)
fc$mean

library(ggplot2)
c(t(item_2457)) %>%
auto.arima %>%
forecast(h=1000) %>%
autoplot

library(ggplot2)
c(t(item_2457)) %>%
nnetar %>%
forecast(h=1000) %>%
autoplot



library(ggplot2)
WWWusage %>%
Arima(order=c(3,1,0)) %>%
forecast(h=20) %>%
autoplot

median(c(t(sales_train_evaluation[1,7:1919])))

################ finding highest mean

medians <- c()
for (i in c(1:30490))
{

m.i <- median(c(t(sales_train_evaluation[i,7:1919])))

medians <- c(medians, m.i)


}

load("medians.RData")

medians

max(medians)

which.max(medians)

#8413 is max median

dim(sales_train_evaluation)

################# Looking at item with highest mean


plot(c(1:1913),sales_train_validation[8413,7:1919],type = "l", xlab = 'Days', ylab = 'Units Sold', main = 'Item 8413')

dim(sales_train_validation)

item_8413 <- c(t(sales_train_evaluation[8413,1836:1919]))


ts_item_8413 <- ts(item_8413, frequency = 365, start = c(2011,1), end = c(2016,6))

plot(ts_item_8413, ylab = 'Units sold', xlab = 'Year')

plot(forecast(item_8413))
fit <- bats(item_8413)
plot(fit)
fc <- forecast(item_8413, h= 20, model=fit)
fc


fit <- Arima(item_8413, order = c(1,0,3))
fc <- forecast(item_8413, h= 20, model=fit)
fc

fit <- auto.arima(item_8413)
fc <- forecast(item_8413, h= 20, model=fit)
fc

fit <- tbats(item_8413)
fc <- forecast(item_8413, h= 28, model=fit)
fc
plot(fit)
plot(fc, main = 'TBATS Example')

fit <- nnetar(item_8413)
fc <- forecast(item_8413, h= 28, model=fit)
fc


plot(c(1:28), sales_train_evaluation[8413,1920:1947], type = 'p', ylim = c(0,600), xlab = 'Days', ylab = 'Units Sold',main = 'Item 8413 Prediction vs Actual')
lines(c(1:28), sales_train_evaluation[8413,1920:1947], col="black",lty=1)
points(c(1:28), fc$mean, col="red", pch="*")
lines(c(1:28), fc$mean, col="red",lty=2)
legend(1, 500, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)


item_8413_100 <- c(t(sales_train_evaluation[8413,1819:1919]))

plot(c(1:101),item_8413_100, type = 'l')

plot(forecast(item_8413_100))
fit <- bats(item_8413_100)
plot(fit)
fc <- forecast(item_8413_100, h= 20, model=fit)
fc

fit <- auto.arima(item_8413_100)
fc <- forecast(item_8413_100, h= 20, model=fit)
fc

fit <- tbats(item_8413_100)
fc <- forecast(item_8413_100, h= 28, model=fit)
fc


fit <- nnetar(item_8413_100)
fc <- forecast(item_8413_100, h= 28, model=fit)
fc

plot(c(1:28), sales_train_evaluation[8413,1920:1947], type = 'p', ylim = c(0,600), xlab = 'Days', ylab = 'Units Sold',main = 'Item 8413 Prediction vs Actual')
lines(c(1:28), sales_train_evaluation[8413,1920:1947], col="black",lty=1)
points(c(1:28), fc$mean, col="red", pch="*")
lines(c(1:28), fc$mean, col="red",lty=2)
legend(1, 500, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)


rmse.error <- 0
for (i in 1:length(sales_train_evaluation[8413,1920:1947])){
	rmse.error <- rmse.error + ((sales_train_evaluation[8413,1919+i] - fc$mean[i])**2)/28
	
}
rmse.error**.5


item_2457_100 <- c(t(sales_train_evaluation[2457,1819:1919]))

plot(c(1:101),item_2457_100, type = 'l')


plot(forecast(item_2457_100))
fit <- bats(item_2457_100)
plot(fit)
fc <- forecast(item_2457_100, h= 20, model=fit)
fc

fit <- auto.arima(item_2457_100)
fc <- forecast(item_2457_100, h= 20, model=fit)
fc

fit <- tbats(item_2457_100)
fc <- forecast(item_2457_100, h= 28, model=fit)
fc


fit <- nnetar(item_2457_100)
fc <- forecast(item_2457_100, h= 28, model=fit)
fc

plot(c(1:28), sales_train_evaluation[2457,1920:1947], type = 'p', ylim = c(0,10), xlab = 'Days', ylab = 'Units Sold',main = 'Item 8413 Prediction vs Actual')
lines(c(1:28), sales_train_evaluation[2457,1920:1947], col="black",lty=1)
points(c(1:28), fc$mean, col="red", pch="*")
lines(c(1:28), fc$mean, col="red",lty=2)
legend(1, 8, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)


rmse.error <- 0
for (i in 1:length(sales_train_evaluation[8413,1920:1947])){
	rmse.error <- rmse.error + ((sales_train_evaluation[8413,1919+i] - fc$mean[i])**2)/28
	
}
rmse.error**.5

item_2457_14 <- c(t(sales_train_evaluation[2457,1822:1919]))


days_item_2457 <- matrix(0,1,7)
for ( i in 1:length(item_2457_14)){

	if (i%%7 == 0){
	days_item_2457[1, 1] <- days_item_2457[1,1] + item_2457[i]
}

if (i%%7 == 1){
	days_item_2457[1,2] <- days_item_2457[1,2] + item_2457[i]
}

if (i%%7 == 2){
	days_item_2457[1,3] <- days_item_2457[1,3] + item_2457[i]
}

if (i%%7 == 3){
	days_item_2457[1,4] <- days_item_2457[1,4] + item_2457[i]
}

if (i%%7 == 4){
	days_item_2457[1,5] <- days_item_2457[1,5] + item_2457[i]
}

if (i%%7 == 5){
	days_item_2457[1,6] <- days_item_2457[1,6] + item_2457[i]
}

if (i%%7 == 6){
	days_item_2457[1,7] <- days_item_2457[1,7] + item_2457[i]
}
	



}


days_item_2457 <- matrix(0,1,7)
for ( i in 1:length(item_2457_14)){

	for (j in 1:7){

		if ((i-1)%%7 == j-1){
		days_item_2457[1, j] <- days_item_2457[1,j] + item_2457_14[i]
		}
	}
}

week_breakdown <- function(item,week) {
	days <- 7*week

	days_item <- rep(0, days)
		
	for ( i in 1:length(item)){

		for (j in 1:days){

			if ((i-1)%%days == (j-1)){
				
				days_item[j] <- days_item[j] + item[i]
				
			}
		}
	}


	maxi <- max(days_item)
		
	for ( i in 1:length(days_item)) {

		days_item[i] <- days_item[i]/maxi

	}
	
	return(days_item)

}

x <- week_breakdown(item_2457_14,1)


fit <- tbats(item_2457_14)
fc <- forecast(item_2457_14, h= 28, model=fit)
fc

item_guess <- function(fc,days_item_1, days_item_2, days_item_4){

	item.guess <- c()

	for ( i in 1:28){

		factor <- (days_item_1[((i-1)%%7)+1] + days_item_2[((i-1)%%14)+1] + days_item_4[i])/3
	
		guess <- fc$upper[i,2]*factor

		item.guess <- c(item.guess, guess)
}


return(item.guess)




}

item_2457_12 <- c(t(sales_train_evaluation[2457,1836:1919]))
item_2457_5 <- c(t(sales_train_evaluation[2457,100:1919]))

fit <- tbats(item_2457_12)
fc <- forecast(item_2457_12, h= 28, model=fit)
fc


x <- week_breakdown(item_2457_12,1)

y <- week_breakdown(item_2457_12,2)

z <- week_breakdown(item_2457_12,4)

a <- week_breakdown(item_2457_5,52)

item.2457.guess <- item_guess(fc,x,y,z)





item.2457.guess <- c()

for ( i in 1:28){
	
	guess <- fc$upper[i,2]*days_item_2457[1,((i-1)%%7)+1]

	item.2457.guess <- c(item.2457.guess, guess)
}

plot(c(1:28), sales_train_evaluation[2457,1920:1947], type = 'p', ylim = c(0,10), xlab = 'Days', ylab = 'Units Sold',main = 'Item 2457 Prediction vs Actual')
lines(c(1:28), sales_train_evaluation[2457,1920:1947], col="black",lty=1)
points(c(1:28), item.2457.guess, col="red", pch="*")
lines(c(1:28), item.2457.guess, col="red",lty=2)
legend(1, 9, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

plot(c(1:28), sales_train_evaluation[2457,1920:1947], type = 'p', ylim = c(0,10), xlab = 'Days', ylab = 'Units Sold',main = 'Item 2457 Prediction vs Actual')
lines(c(1:28), sales_train_evaluation[2457,1920:1947], col="black",lty=1)
points(c(1:28), fc$mean, col="red", pch="*")
lines(c(1:28), fc$mean, col="red",lty=2)
legend(1, 9, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)


rmse.error <- 0
for (i in 1:length(sales_train_evaluation[2457,1920:1947])){
	rmse.error <- rmse.error + ((sales_train_evaluation[2457,1919+i] - item.2457.guess[i])**2)/28
	
}
rmse.error**.5







pred <- matrix(0,30490,28)

rmse.errors = c()

for (i in c(1:30490)){

	item <- c(t(sales_train_validation[i,1836:1919]))

	if ( item == rep(0,84)){

		item.guess = rep(0,28)


	}

	else{
	
		x <- week_breakdown(item,1)

		y <- week_breakdown(item,2)

		z <- week_breakdown(item,4)
	
		fit <- tbats(item)
	
		fc <- forecast(item, h= 28, model=fit)

		item.guess <- item_guess(fc,x,y,z)
	
	}
	
	rmse.error <- 0

	for (j in 1:length(sales_train_evaluation[i,1920:1947])){
		rmse.error <- rmse.error + ((sales_train_evaluation[i,1919+j] - item.guess[j])**2)/28
	
}

	rmse.errors <- c(rmse.errors, rmse.error**.5)

	pred[i,] <- item.guess



}


item_guess <- function(fc,days_item_1, days_item_2, days_item_4,days_item_5) {

	item.guess <- c()

	for ( i in 1:28){

		factor <- (days_item_1[((i-1)%%7)+1] + days_item_2[((i-1)%%14)+1] + days_item_4[i]+days_item_5[i])/4
	
		guess <- fc$upper[i,2]*factor

		item.guess <- c(item.guess, guess)
}


return(item.guess)




}




item_2457_12 <- c(t(sales_train_evaluation[26851,1836:1919]))
item_2457_5 <- c(t(sales_train_evaluation[26851,100:1919]))


fit <- tbats(item_2457_12)
fc <- forecast(item_2457_12, h= 28, model=fit)


x <- week_breakdown(item_2457_12,1)

y <- week_breakdown(item_2457_12,2)

z <- week_breakdown(item_2457_12,4)

a <- week_breakdown(item_2457_5,52)

item.2457.guess <- item_guess(fc,x,y,z,a)


plot(c(1:28), sales_train_evaluation[26851,1920:1947], type = 'p', xlab = 'Days', ylab = 'Units Sold',main = 'Item 2457 Prediction vs Actual')
lines(c(1:28), sales_train_evaluation[26851,1920:1947], col="black",lty=1)
points(c(1:28), item.2457.guess, col="red", pch="*")
lines(c(1:28), item.2457.guess, col="red",lty=2)
legend(1, 9, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)


rmse.error <- 0
for (i in 1:length(sales_train_evaluation[26851,1920:1947])){
	rmse.error <- rmse.error + ((sales_train_evaluation[26851,1919+i] - item.2457.guess[i])**2)/28
	
}
rmse.error**.5




pred_bats <- matrix(0,30490,28)

rmse.errors_bats = c()

for (i in c(1:30490)){

	item <- c(t(sales_train_validation[i,1836:1919]))
	item_5 <- c(t(sales_train_validation[i,100:1919]))

	if ( item == rep(0,84)){

		item.guess = rep(0,28)


	}

	else{
	
		x <- week_breakdown(item,1)

		y <- week_breakdown(item,2)

		z <- week_breakdown(item,4)

		a <- week_breakdown(item_5,52)
	
		fit <- tbats(item)
	
		fc <- forecast(item, h= 28, model=fit)

		item.guess <- item_guess(fc,x,y,z,a)
	
	}
	
	rmse.error <- 0

	for (j in 1:length(sales_train_evaluation[i,1920:1947])){
		rmse.error <- rmse.error + ((sales_train_evaluation[i,1919+j] - item.guess[j])**2)/28
	
}

	rmse.errors_bats <- c(rmse.errors_bats, rmse.error**.5)

	pred_bats[i,] <- item.guess



}







##################### Neural networks for high median items

boxplot(medians)

hi.medians = c()
for (i in c(1:30490))
{

if( medians[i] > 0){

hi.medians <- c(hi.medians, i)
}




}

hi.medians

length(hi.medians)



save(pred, file = 'pred.RData')
save(rmse.errors, file = 'rmse.RData')




pred_neural <- matrix(0,30490,28)

rmse.errors_neural = c()




for (i in c(1:30490)){

	
	item <- c(t(sales_train_validation[i,1819:1919]))

	fit <- nnetar(item)
	fc <- forecast(item, h= 28, model=fit)

	rmse.error <- 0

	for (j in 1:length(sales_train_evaluation[i,1920:1947])){
		rmse.error <- rmse.error + ((sales_train_evaluation[i,1919+j] - fc$mean[j])**2)/28
	
	}

	rmse.errors_neural <- c(rmse.errors_neural, rmse.error**.5)

	pred_neural[i,] <- fc$mean





	}







summary(rmse.errors_neural)

pred_actual <- matrix(0,30490,29)

rmse.errors_actual = c()

for (i in c(1:30490)){

	if( rmse.errors[i] < rmse.errors_neural[i]){
		pred_actual[i,] <- c(pred[i,],'bats')
		rmse.errors_actual <- c(rmse.errors_actual, rmse.errors[i])



	}
	
	else{
		pred_actual[i,] <- c(pred_neural[i,],'neural')
		rmse.errors_actual <- c(rmse.errors_actual, rmse.errors_neural[i])



	}


}

boxplot(rmse.errors_actual)
boxplot(rmse.errors)
boxplot(rmse.errors_neural)

save(pred_neural, file = 'pred_neural.RData')
save(rmse.errors_neural, file = 'rmse_neural.RData')

save(pred_actual, file = 'pred_actual.RData')
save(rmse.errors_actual, file = 'rmse_actual.RData')


bats.count <- 0
neural.count <- 0

for (i in c(1:30490)){
	
	if (pred_actual[i,29] == 'bats'){

		bats.count <- bats.count + 1	

	}
	
	else {

		neural.count <- neural.count +1

	}

	




}


ranges <- c()
for (i in c(1:30490))
{

r.i <- max(c(t(sales_train_validation[i,7:1919]))) - min(c(t(sales_train_validation[i,7:1919])))

ranges <- c(ranges, r.i)


}

means <- c()
for (i in c(1:30490))
{

m.i <- mean(c(t(sales_train_validation[i,7:1919])))

means <- c(means, m.i)


}

summaries <- matrix(0,30490,3)

for (i in c(1:30490)){

	summaries[i,] <- c(medians[i],means[i],ranges[i])


}


save(means, file = 'means.RData')
save(ranges, file = 'ranges.RData')
save(summaries, file = 'summaries.RData')


head(summaries)
colnames(summaries) <- c('median','mean','range')

hist(medians)
hist(ranges)
hist(means)

summary(medians)
summary(means)
summary(ranges)

neural_means <- c()
bats_means <- c()
neural_medians <- c()
bats_medians <- c()
neural_ranges <- c()
bats_ranges <- c()

for (i in c(1:30490)){

	if ( pred_actual[i,29] == 'neural'){


		neural_means <- c(neural_means, means[i])
		neural_medians <- c(neural_medians, medians[i])
		neural_ranges <- c(neural_ranges, ranges[i])

	}

	else {
		
		bats_means <- c(bats_means, means[i])
		bats_medians <- c(bats_medians, medians[i])
		bats_ranges <- c(bats_ranges, ranges[i])




	

	}

}

summary(neural_medians)


sales_train_validation[1,1:8]

sales_train_validation$store_id


bats_stores <- matrix(0,1,10)
colnames(bats_stores) <- c('CA_1','CA_2','CA_3', 'CA_4', 'TX_1','TX_2','TX_3','WI_1','WI_2','WI_3')

neural_stores <- matrix(0,1,10)
colnames(neural_stores) <- c('CA_1','CA_2','CA_3', 'CA_4', 'TX_1','TX_2','TX_3','WI_1','WI_2','WI_3')


store_ids <- c('CA_1','CA_2','CA_3', 'CA_4', 'TX_1','TX_2','TX_3','WI_1','WI_2','WI_3')



for (i in c(1:30940)){

	for (j in c(1:10)){
	
		if (toString(sales_train_validation$store_id[i]) == store_ids[j]){
			
		
			if (pred_actual[i,29] == 'bats'){

				bats_stores[1,j] <- bats_stores[1,j] +1

			}
			
			if (pred_actual[i,29] == 'neural'){

				neural_stores[1,j] <- neural_stores[1,j] +1

			}
		
		}

	
	}





}

library(ggplot2)
library(reshape2)

bats.stores <- bats_stores[1,,1]
neural.stores <- neural_stores[1,,1]


df1 <- data.frame(bats.stores, neural.stores, store_ids)
df2 <- melt(df1, id.vars='store_ids')
head(df2)

ggplot(df2, aes(x=store_ids, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')



colnames(sales_train_validation)

sales_train_validation$cat_id

bats_cat <- matrix(0,1,3)
colnames(bats_cat) <- c('FOODS','HOBBIES','HOUSEHOLD')

neural_cat <- matrix(0,1,3)
colnames(neural_cat) <- c('FOODS','HOBBIES','HOUSEHOLD')


cat_ids <- c('FOODS','HOBBIES','HOUSEHOLD')




for (i in c(1:30940)){

	for (j in c(1:3)){
	
		if (toString(sales_train_validation$cat_id[i]) == cat_ids[j]){
			
		
			if (pred_actual[i,29] == 'bats'){

				bats_cat[1,j] <- bats_cat[1,j] +1

			}
			
			if (pred_actual[i,29] == 'neural'){

				neural_cat[1,j] <- neural_cat[1,j] +1

			}
		
		}

	
	}





}


bats_cat <- outer(bats_cat,1/length(bats_means))
neural_cat <- outer(neural_cat,1/length(neural_means))

bats.cat <- bats_cat[1,,1]
neural.cat <- neural_cat[1,,1]


df1 <- data.frame(bats.cat, neural.cat, cat_ids)
df2 <- melt(df1, id.vars='cat_ids')
head(df2)

ggplot(df2, aes(x=cat_ids, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')


sales_train_validation$dept_id

bats_dept <- matrix(0,1,7)
colnames(bats_dept) <- c('FOODS_1','FOODS_2', 'FOODS_3','HOBBIES_1','HOBBIES_2','HOUSEHOLD_1','HOUSEHOLD_2')

neural_dept <- matrix(0,1,7)
colnames(neural_dept) <- c('FOODS_1','FOODS_2', 'FOODS_3','HOBBIES_1','HOBBIES_2','HOUSEHOLD_1','HOUSEHOLD_2')



dept_ids <- c('FOODS_1','FOODS_2', 'FOODS_3','HOBBIES_1','HOBBIES_2','HOUSEHOLD_1','HOUSEHOLD_2')







for (i in c(1:30940)){

	for (j in c(1:7)){
	
		if (toString(sales_train_validation$dept_id[i]) == dept_ids[j]){
			
		
			if (pred_actual[i,29] == 'bats'){

				bats_dept[1,j] <- bats_dept[1,j] +1

			}
			
			if (pred_actual[i,29] == 'neural'){

				neural_dept[1,j] <- neural_dept[1,j] +1

			}
		
		}

	
	}





}


bats_dept <- outer(bats_dept,1/length(bats_means))
neural_dept <- outer(neural_dept,1/length(neural_means))

bats.dept <- bats_dept[1,,1]
neural.dept <- neural_dept[1,,1]


df1 <- data.frame(bats.dept, neural.dept, dept_ids)
df2 <- melt(df1, id.vars='dept_ids')
head(df2)

ggplot(df2, aes(x=dept_ids, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')

problems <- c()

for (i in c(1:30490)){

	if (rmse.errors_actual[i] >5){
	
		problems <- c(problems, i)

	}

}

which.max(rmse.errors_actual)

item_26851 <- c(t(sales_train_validation[26851,7:1919]))

plot(item_26851)
lines(c(1:1913), item_26851, col="black",lty=1)

sales_train_validation[26851,1:6]

dim(sales_train_evaluation)

item_26851 <- c(t(sales_train_evaluation[26851,1920:1947]))

plot(item_26851, main = 'TBATS with scaling')
lines(c(1:28), item_26851, col="black",lty=1)
points(c(1:28), pred_actual[26851,1:28], col="red", pch="*")
lines(c(1:28), pred_actual[26851,1:28], col="red",lty=2)
legend(1, 120, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)


plot(item_26851, main = 'TBATS without scaling')
lines(c(1:28), item_26851, col="black",lty=1)
points(c(1:28), pred[26851,1:28], col="red", pch="*")
lines(c(1:28), pred[26851,1:28], col="red",lty=2)
legend(1, 120, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)




pred_actual[26851,29]

item_26851_1 <- c(t(sales_train_evaluation[26851,1190:1217]))

plot(item_26851)
lines(c(1:28), item_26851, col="black",lty=1)
points(c(1:28), item_26851_1, col="red", pch="*")
lines(c(1:28), item_26851_1, col="red",lty=2)
legend(1, 120, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

save(pred_bats, file = 'pred_bats.RData')
save(rmse.errors_bats, file = 'rmse_bats.RData')

for (i in c(1:30490)){
	
	if (rmse.errors_bats[i] < rmse.errors_actual[i]){


		rmse.errors_actual[i] <- rmse.errors_bats[i]
		pred_actual[i,] <- c(pred_bats[i,], 'bats2')



	}



}


neural_means <- c()
bats_means <- c()
bats2_means <- c()
neural_medians <- c()
bats_medians <- c()
bats2_medians <- c()
neural_ranges <- c()
bats_ranges <- c()
bats2_ranges <- c()

for (i in c(1:30490)){

	if ( pred_actual[i,29] == 'neural'){


		neural_means <- c(neural_means, means[i])
		neural_medians <- c(neural_medians, medians[i])
		neural_ranges <- c(neural_ranges, ranges[i])

	}

	if (pred_actual[i,29] == 'bats') {
		
		bats_means <- c(bats_means, means[i])
		bats_medians <- c(bats_medians, medians[i])
		bats_ranges <- c(bats_ranges, ranges[i])




	

	}


	if (pred_actual[i,29] == 'bats2') {
		
		bats2_means <- c(bats2_means, means[i])
		bats2_medians <- c(bats2_medians, medians[i])
		bats2_ranges <- c(bats2_ranges, ranges[i])





}

}

save(pred_actual, file = 'pred_actual.RData')
save(rmse.errors_actual, file = 'rmse_actual.RData')



problems <- c()

for (i in c(1:30490)){

	if (rmse.errors_actual[i] >5){
	
		problems <- c(problems, i)

	}

}




day_factor <- function(item,week){
	days <- 7*week
	
	day <- 0

	week_factor <- rep(0, days)

	total_factor <- rep(0,days)

		
	
	for ( i in 1:length(item)){
	
		day <- day + 1
		
		week_factor[day] <- item[i]

		if (day == 7) {
			
			maxi <- which.max(week_factor)
			total_factor[maxi] <- total_factor[maxi] +1
			week_factor <- rep(0,days)
			day <- 0
			

		}	
	

	}

return(max(total_factor)/sum(total_factor))

}

item_2457_12 <- c(t(sales_train_evaluation[2457,1836:1919]))

a <- day_factor(item_2457_12,1)

a

item_guess <- function(item,item_5,fc) {

	item.guess <- c()

	factor.1 <- day_factor(item,1)
	
	factor.2 <- day_factor(item,2)

	factor.4 <- day_factor(item,4)

	factor.5 <- day_factor(item_5,52)

	days_item_1 <- week_breakdown(item,1)

	days_item_2 <- week_breakdown(item,2)

	days_item_4 <- week_breakdown(item,4)

	days_item_5 <- week_breakdown(item_5,52)
	

	for ( i in 1:28){

		factor <- (days_item_1[((i-1)%%7)+1]*factor.1 + days_item_2[((i-1)%%14)+1]*factor.2 + days_item_4[i]*factor.4+days_item_5[i]*factor.5)/(factor.1 + factor.2 + factor.4 + factor.5)
	
		guess <- fc$upper[i,2]*factor

		item.guess <- c(item.guess, guess)
}


return(item.guess)




}


item_2457_12 <- c(t(sales_train_evaluation[26851,1836:1919]))
item_2457_5 <- c(t(sales_train_evaluation[26851,100:1919]))


fit <- tbats(item_2457_12)
fc <- forecast(item_2457_12, h= 28, model=fit)



item_guess(item_2457_12,item_2457_5, fc)




pred_bats3 <- matrix(0,30490,28)

rmse.errors_bats3 = c()

for (i in c(1:30490)){

	item <- c(t(sales_train_validation[i,1836:1919]))
	item_5 <- c(t(sales_train_validation[i,100:1919]))

	if ( item == rep(0,84)){

		item.guess = rep(0,28)


	}

	else{
	
		fit <- tbats(item)
	
		fc <- forecast(item, h= 28, model=fit)

		item.guess <- item_guess(item,item_5,fc)
	
	}
	
	rmse.error <- 0

	for (j in 1:length(sales_train_evaluation[i,1920:1947])){
		rmse.error <- rmse.error + ((sales_train_evaluation[i,1919+j] - item.guess[j])**2)/28
	
}

	rmse.errors_bats3 <- c(rmse.errors_bats3, rmse.error**.5)

	pred_bats3[i,] <- item.guess



}



for (i in c(1:30490)){
	
	if (rmse.errors_bats3[i] < rmse.errors_actual[i]){


		rmse.errors_actual[i] <- rmse.errors_bats3[i]
		pred_actual[i,] <- c(pred_bats3[i,], 'bats3')



	}



}



neural_means <- c()
bats_means <- c()
bats2_means <- c()
bats3_means <- c()
neural_medians <- c()
bats_medians <- c()
bats2_medians <- c()
bats3_medians <- c()
neural_ranges <- c()
bats_ranges <- c()
bats2_ranges <- c()
bats3_ranges <- c()






for (i in c(1:30490)){

	if ( pred_actual[i,29] == 'neural'){


		neural_means <- c(neural_means, means[i])
		neural_medians <- c(neural_medians, medians[i])
		neural_ranges <- c(neural_ranges, ranges[i])

	}

	if (pred_actual[i,29] == 'bats') {
		
		bats_means <- c(bats_means, means[i])
		bats_medians <- c(bats_medians, medians[i])
		bats_ranges <- c(bats_ranges, ranges[i])




	

	}


	if (pred_actual[i,29] == 'bats2') {
		
		bats2_means <- c(bats2_means, means[i])
		bats2_medians <- c(bats2_medians, medians[i])
		bats2_ranges <- c(bats2_ranges, ranges[i])





}

if (pred_actual[i,29] == 'bats3') {
		
		bats3_means <- c(bats3_means, means[i])
		bats3_medians <- c(bats3_medians, medians[i])
		bats3_ranges <- c(bats3_ranges, ranges[i])





}

}


save(pred_actual, file = 'pred_actual.RData')
save(rmse.errors_actual, file = 'rmse_actual.RData')


save(pred_bats3, file = 'pred_bats3.RData')
save(rmse.errors_bats, file = 'rmse_bats3.RData')


problems <- c()

for (i in c(1:30490)){

	if (rmse.errors_actual[i] >5){
	
		problems <- c(problems, i)

	}

}




problem.methods <- c()

for ( i in problems){

	problem.methods <- c(problem.methods,pred_actual[i,29])


}


day_factor <- function(item,week){
	days <- 7*week
	
	day <- 0

	week_factor <- rep(0, days)

	total_factor_max <- rep(0,days)

	total_factor_min <- rep(0,days)

		
	
	for ( i in 1:length(item)){
	
		day <- day + 1
		
		week_factor[day] <- item[i]

		if (day == 7) {
			
			maxi <- which.max(week_factor)
			mini <- which.min(week_factor)
			total_factor_max[maxi] <- total_factor_max[maxi] +1
			total_factor_min[mini] <- total_factor_min[mini] +1
			week_factor <- rep(0,days)
			day <- 0
			

		}	
	

	}

a <- max(total_factor_max)/sum(total_factor_max)

b <- max(total_factor_min)/sum(total_factor_min)


return((a + b)/2)

}



pred_bats4 <- matrix(0,30490,28)

rmse.errors_bats4 = c()

for (i in c(1:30490)){

	item <- c(t(sales_train_validation[i,1836:1919]))
	item_5 <- c(t(sales_train_validation[i,100:1919]))

	if ( item == rep(0,84)){

		item.guess = rep(0,28)


	}

	else{
	
		fit <- tbats(item)
	
		fc <- forecast(item, h= 28, model=fit)

		item.guess <- item_guess(item,item_5,fc)
	
	}
	
	rmse.error <- 0

	for (j in 1:length(sales_train_evaluation[i,1920:1947])){
		rmse.error <- rmse.error + ((sales_train_evaluation[i,1919+j] - item.guess[j])**2)/28
	
}

	rmse.errors_bats4 <- c(rmse.errors_bats4, rmse.error**.5)

	pred_bats4[i,] <- item.guess



}

save(rmse.errors_bats4, file = 'rmse_bats4.RData')
save(pred_bats4, file = 'pred_bats4.RData')

length(neural_means)


plot(c(1:28), sales_train_evaluation[8413,1920:1947], type = 'p', xlab = 'Days', ylab = 'Units Sold',main = 'Item 8413 Prediction vs Actual: Neural')
lines(c(1:28), sales_train_evaluation[8413,1920:1947], col="black",lty=1)
points(c(1:28), pred_actual[8413,1:28], col="red", pch="*")
lines(c(1:28), pred_actual[8413,1:28], col="red",lty=2)
legend(1, 3.5, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

plot(c(1:28), sales_train_evaluation[18056,1920:1947], type = 'p', xlab = 'Days', ylab = 'Units Sold',main = 'Item 18056 Prediction vs Actual: TBATS')
lines(c(1:28), sales_train_evaluation[18056,1920:1947], col="black",lty=1)
points(c(1:28), pred_actual[18056,1:28], col="red", pch="*")
lines(c(1:28), pred_actual[18056,1:28], col="red",lty=2)
legend(1, 3.5, legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)






bats.ids <- c()
neural.ids <- c()

for (i in c(1:30490)){

	if (pred_actual[i,29] == 'bats'){

	bats.ids <- c(bats.ids,i)

	}

	else {

	neural.ids <- c(neural.ids, i)

	}


}

which.max(neural_means)
neural.ids[6966]



day_factor <- function(item,week){
	days <- 7*week
	
	data.table <- t(matrix(item,nrow = 84/days, ncol = days,  byrow = TRUE))

	cosine.table <- cosine(data.table)

	cosine.values <- c(cosine.table)
			
	return(sum(cosine.values)/length(cosine.values)[1])
}

day_factor_5 <- function(item_5){
	
	data.table <- t(matrix(item_5,nrow = 5, ncol = 52*7, byrow = TRUE))

	cosine.table <- cosine(data.table)

	cosine.values <- c(cosine.table)
	
	return(sum(cosine.values)/length(cosine.values)[1])


	



}

test <- c(t(sales_train_validation[8413,1836:1919]))

test8 <- day_factor_5(test_5)

test_5 <- c(t(sales_train_validation[8413,100:1919]))

item_guess <- function(item,item_5,fc) {

	item.guess <- c()

	factor.1 <- day_factor(item,1)
	
	factor.2 <- day_factor(item,2)

	factor.4 <- day_factor(item,4)

	factor.5 <- day_factor_5(item_5)

	days_item_1 <- week_breakdown(item,1)

	days_item_2 <- week_breakdown(item,2)

	days_item_4 <- week_breakdown(item,4)

	days_item_5 <- week_breakdown(item_5,52)
	

	for ( i in 1:28){

		factor <- (days_item_1[((i-1)%%7)+1]*factor.1 + days_item_2[((i-1)%%14)+1]*factor.2 + days_item_4[i]*factor.4+days_item_5[i]*factor.5)/(factor.1 + factor.2 + factor.4 + factor.5)
	
		guess <- fc$upper[i,2]*factor

		item.guess <- c(item.guess, guess)
}


return(item.guess)




}

test



week_breakdown <- function(item,week) {
	days <- 7*week

	days_item <- rep(0, days)
		
	for ( i in 1:length(item)){

		for (j in 1:days){

			if ((i-1)%%days == (j-1)){
				
				days_item[j] <- days_item[j] + item[i]
				
			}
		}
	}


	maxi <- max(days_item)
		
	for ( i in 1:length(days_item)) {

		days_item[i] <- days_item[i]/maxi

	}
	
	return(days_item)

}


		fit <- tbats(test)
	
		fc <- forecast(test, h= 28, model=fit)

		item.guess <- item_guess(test,test_5,fc)






pred_bats5 <- matrix(0,30490,28)

rmse.errors_bats5 = c()


for (i in c(1:30490)){

	item <- c(t(sales_train_validation[i,1836:1919]))
	item_5 <- c(t(sales_train_validation[i,100:1919]))

	if ( item == rep(0,84)){

		item.guess <-  rep(0,28)


	}

	else{
	
		fit <- tbats(item)
	
		fc <- forecast(item, h= 28, model=fit)

		item.guess <- item_guess(item,item_5,fc)
	
	}
	
	rmse.error <- 0

	for (j in 1:length(sales_train_evaluation[i,1920:1947])){
		rmse.error <- rmse.error + ((sales_train_evaluation[i,1919+j] - item.guess[j])**2)/28
	
}

	rmse.errors_bats5 <- c(rmse.errors_bats5, rmse.error**.5)

	pred_bats5[i,] <- item.guess



}

save(rmse.errors_bats5, file = 'rmse_bats5.RData')
save(pred_bats5, file = 'pred_bats5.RData')


pred_bats5

test <- c(t(sales_train_validation[3569,1836:1919]))

day_factor(test,1)



day_factor <- function(item,week){
	days <- 7*week
	
	data.table <- t(matrix(item,nrow = 84/days, ncol = days,  byrow = TRUE))

	cosine.table <- cosine(data.table)

	cosine.values <- c(cosine.table)

	nans <- is.nan(cosine.values)

	for(i in 1:length(cosine.values)){

		if (nans[i] == TRUE){
	
			cosine.values[i] = 0

		}

	}
		
		
	return(sum(cosine.values)/length(cosine.values)[1])
}



day_factor_5 <- function(item_5){
	
	data.table <- t(matrix(item_5,nrow = 5, ncol = 52*7, byrow = TRUE))

	cosine.table <- cosine(data.table)

	cosine.values <- c(cosine.table)

	nans <- is.nan(cosine.values)

	for (i in 1:length(cosine.table)){

		if (nans[i] == TRUE){

			cosine.values[i] = 0
		
		}

	}
	
	return(sum(cosine.values)/length(cosine.values)[1])


	



}


day_factor(test,1)




plot(c(1:1941), sales_train_evaluation[29465,7:1947], type = 'l', xlab = 'Days', ylab = 'Units Sold',main = 'Item 29465')
lines(c(1:1941), sales_train_evaluation[29465,7:1947], col="black",lty=1)

fit <- tbats(USAccDeaths)
plot(forecast(fit), main = 'Time Series Analysis')



for (i in c(1:30490)){
	
	if (rmse.errors_bats4[i] < rmse.errors_actual[i]){


		rmse.errors_actual[i] <- rmse.errors_bats4[i]
		pred_actual[i,] <- c(pred_bats4[i,], 'bats4')



	}



}

for (i in c(1:30490)){
	
	if (rmse.errors_bats5[i] < rmse.errors_actual[i]){


		rmse.errors_actual[i] <- rmse.errors_bats5[i]
		pred_actual[i,] <- c(pred_bats5[i,], 'bats5')



	}



}

for (i in c(1:30490)){
	
	if (rmse.errors_bats5[i] < rmse.errors_actual[i]){


		rmse.errors_actual[i] <- rmse.errors_bats5[i]
		pred_actual[i,] <- c(pred_bats5[i,], 'bats5')



	}



}

bats.items <- c()
neural.items <- c()

for (i in c(1:30490)){

	if (pred_actual[i,29] != 'neural'){
	
		bats.items <- c(bats.items,i)

	}

	else {

		neural.items <- c(neural.items,i)

	}

}


bats_stores <- matrix(0,1,10)
colnames(bats_stores) <- c('CA_1','CA_2','CA_3', 'CA_4', 'TX_1','TX_2','TX_3','WI_1','WI_2','WI_3')

neural_stores <- matrix(0,1,10)
colnames(neural_stores) <- c('CA_1','CA_2','CA_3', 'CA_4', 'TX_1','TX_2','TX_3','WI_1','WI_2','WI_3')


store_ids <- c('CA_1','CA_2','CA_3', 'CA_4', 'TX_1','TX_2','TX_3','WI_1','WI_2','WI_3')



for (i in c(1:30940)){

	for (j in c(1:10)){
	
		if (toString(sales_train_validation$store_id[i]) == store_ids[j]){
			
		
			if (pred_actual[i,29] != 'neural'){

				bats_stores[1,j] <- bats_stores[1,j] +1

			}
			
			if (pred_actual[i,29] == 'neural'){

				neural_stores[1,j] <- neural_stores[1,j] +1

			}
		
		}

	
	}





}

library(ggplot2)
library(reshape2)


bats_stores <- outer(bats_stores,1/length(bats.items))
neural_stores <- outer(neural_stores,1/length(neural.items))



bats.stores <- bats_stores[1,,1]
neural.stores <- neural_stores[1,,1]


df1 <- data.frame(bats.stores, neural.stores, store_ids)
df2 <- melt(df1, id.vars='store_ids')
head(df2)

ggplot(df2, aes(x=store_ids, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')



colnames(sales_train_validation)

sales_train_validation$cat_id

bats_cat <- matrix(0,1,3)
colnames(bats_cat) <- c('FOODS','HOBBIES','HOUSEHOLD')

neural_cat <- matrix(0,1,3)
colnames(neural_cat) <- c('FOODS','HOBBIES','HOUSEHOLD')


cat_ids <- c('FOODS','HOBBIES','HOUSEHOLD')




for (i in c(1:30940)){

	for (j in c(1:3)){
	
		if (toString(sales_train_validation$cat_id[i]) == cat_ids[j]){
			
		
			if (pred_actual[i,29] != 'neural'){

				bats_cat[1,j] <- bats_cat[1,j] +1

			}
			
			if (pred_actual[i,29] == 'neural'){

				neural_cat[1,j] <- neural_cat[1,j] +1

			}
		
		}

	
	}





}


bats_cat <- outer(bats_cat,1/length(bats.items))
neural_cat <- outer(neural_cat,1/length(neural.items))

bats.cat <- bats_cat[1,,1]
neural.cat <- neural_cat[1,,1]


df1 <- data.frame(bats.cat, neural.cat, cat_ids)
df2 <- melt(df1, id.vars='cat_ids')
head(df2)

ggplot(df2, aes(x=cat_ids, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')


sales_train_validation$dept_id

bats_dept <- matrix(0,1,7)
colnames(bats_dept) <- c('FOODS_1','FOODS_2', 'FOODS_3','HOBBIES_1','HOBBIES_2','HOUSEHOLD_1','HOUSEHOLD_2')

neural_dept <- matrix(0,1,7)
colnames(neural_dept) <- c('FOODS_1','FOODS_2', 'FOODS_3','HOBBIES_1','HOBBIES_2','HOUSEHOLD_1','HOUSEHOLD_2')



dept_ids <- c('FOODS_1','FOODS_2', 'FOODS_3','HOBBIES_1','HOBBIES_2','HOUSEHOLD_1','HOUSEHOLD_2')







for (i in c(1:30940)){

	for (j in c(1:7)){
	
		if (toString(sales_train_validation$dept_id[i]) == dept_ids[j]){
			
		
			if (pred_actual[i,29] != 'neural'){

				bats_dept[1,j] <- bats_dept[1,j] +1

			}
			
			if (pred_actual[i,29] == 'neural'){

				neural_dept[1,j] <- neural_dept[1,j] +1

			}
		
		}

	
	}





}


bats_dept <- outer(bats_dept,1/length(bats.items))
neural_dept <- outer(neural_dept,1/length(neural.items))

bats.dept <- bats_dept[1,,1]
neural.dept <- neural_dept[1,,1]


df1 <- data.frame(bats.dept, neural.dept, dept_ids)
df2 <- melt(df1, id.vars='dept_ids')
head(df2)

ggplot(df2, aes(x=dept_ids, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')

item_18056 <- c(t(sales_train_evaluation[18056,1920:1947]))

item.18056.full <- c(t(sales_train_evaluation[18056,1836:1919]))




fit <- tbats(item.18056.full)
	
fc <- forecast(item.18056.full, h= 28, model=fit)

summary(fc)
names(fc)

plot(item_18056, main = 'TBATS Changed vs Original', ylab = 'Units Sold')
lines(c(1:28), item_18056, col="black",lty=1)
points(c(1:28), pred_actual[18056,1:28], col="red", pch="*")
lines(c(1:28), pred_actual[18056,1:28], col="red",lty=2)
points(c(1:28), fc$mean, col = "blue", pch = 0)
lines(c(1:28), fc$mean, col = "blue", lty = 3)


legend(1, 120, legend=c("Actual", "TBATS Changed","TBATS Unchanged"),
       col=c("black", "red",'blue'), lty=1:3, cex=0.8)

bats.errors <- c()

for (i in c(1:30490)){

	if (pred_actual[i,29] != 'neural'){

		bats.errors <- c(bats.errors, rmse.errors_actual[i])
	

}



}

problems <- c()

for (i in c(1:30490)){

	if (rmse.errors_actual[i] > 5){

		problems <- c(problems,i)


}




}



