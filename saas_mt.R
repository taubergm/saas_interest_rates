if (!require(rgl)) {
  install.packages("rgl", repos="http://cran.us.r-project.org")
}
library(rgl)
if (!require(Metrics)) {
  install.packages("Metrics", repos="http://cran.us.r-project.org")
}
library(Metrics)

# https://stackoverflow.com/questions/33848918/fitting-a-3d-surface-to-a-dataset-of-points-r


saas_multiples = read.csv('saas_mt2.csv')
saas_multiples_data = saas_multiples[,4:25]

normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}
saas_multiples_normalized = t(apply(saas_multiples_data, 1, normalize))
saas_multiples_normalized_average = rev(colMeans(saas_multiples_normalized, na.rm = TRUE)) # reverse to go increasing in time

saas_averages = read.csv('saas_averages.csv')
saas_averages = cbind(saas_averages, time)
cor(saas_averages)



ten_yr = saas_averages$X10yr
thirty_yr = saas_averages$X30yr
fed_rate = saas_averages$effective.fed.funds.rat
time = seq(1,length(saas_multiples_normalized_average),1)
multiples = saas_averages$avg_multiple

# basic plots to check for correlation
plot(time, multiples)
plot(ten_yr, multiples)
plot(thirty_yr, multiples)
plot(fed_rate, multiples)

saas_averages_normalized = as.data.frame(apply(saas_averages, 2, normalize))
ten_yr_normalized = saas_averages_normalized$X10yr
thirty_yr_rnormalized = saas_averages_normalized$X30yr
fed_rate_rnormalized = saas_averages_normalized$effective.fed.funds.rat
multiples_normalized = saas_multiples_normalized_average

plot(time, multiples_normalized)
plot(ten_yr, multiples_normalized)
plot(thirty_yr, multiples_normalized)
plot(fed_rate, multiples_normalized)

cor(time, ten_yr)
cor(time, saas_multiples_normalized_average)
cor(ten_yr, saas_multiples_normalized_average)
cor(thirty_yr, saas_multiples_normalized_average)
cor(fed_rate, saas_multiples_normalized_average)




model_time = lm(multiples_normalized ~ time)
model_time_squared = lm(multiples_normalized ~ poly(time,2))

model_fed = lm(multiples_normalized ~ fed_rate)
model_fed_squared = lm(multiples_normalized ~ poly(fed_rate,2))
model_fed_cubic = lm(multiples_normalized ~ poly(fed_rate,3))

model_tenyr = lm(multiples_normalized ~ ten_yr)
model_tenyr_squared = lm(multiples_normalized ~ poly(ten_yr,2))

model_thirtyyr = lm(multiples_normalized ~ thirty_yr)
model_thirtyyr_squared = lm(multiples_normalized ~ poly(thirty_yr,2))

model_tenyr_thirtyyr_fed = lm(multiples_normalized ~ thirty_yr + ten_yr + fed_rate)

model_time_tenyr_thirtyyr_fed = lm(multiples_normalized ~ thirty_yr + ten_yr + time + fed_rate)
model_time_fed_squared_tenyr_squared = lm(multiples_normalized ~ time + poly(fed_rate,2) + poly(ten_yr,2))


predicted_model_time = predict(model_time)
predicted_model_time_squared = predict(model_time_squared)
predicted_model_fed = predict(model_fed)
predicted_model_fed_2 = predict(model_fed_squared)
predicted_model_fed_3 = predict(model_fed_cubic)
predicted_model_ten = predict(model_tenyr)
predicted_model_ten_2 = predict(model_tenyr_squared)
predicted_model_thirty = predict(model_thirtyyr)
predicted_model_thirty_2 = predict(model_thirtyyr_squared)
predicted_model_tenyr_thirtyyr_fed = predict(model_thirtyyr_squared)
predicted_model_time_ten_thirty_fed = predict(model_time_ten_thirty_fed)
predicted_model_time_fed_2_ten_2 = predict(model_time_fed_squared_tenyr_squared)


model_data = as.data.frame(cbind(saas_multiples_normalized_average,
                                 predicted_model_time_squared,
                                 predicted_model_fed,
                                 predicted_model_fed_2,
                                 predicted_model_fed_3,
                                 predicted_model_ten,
                                 predicted_model_ten_2,
                                 predicted_model_thirty,
                                 predicted_model_thirty_2,
                                 predicted_model_tenyr_thirtyyr_fed,
                                 predicted_model_time_ten_thirty_fed,
                                 predicted_model_time_fed_2_ten_2
                                 ))

cor(model_data)

# Done again for the lols
model_time = lm(multiples_normalized ~ time)
model_fed_cubic = lm(multiples_normalized ~ poly(fed_rate,3))
model_tenyr_cubic = lm(multiples_normalized ~ poly(ten_yr,3))
model_thirtyyr_cubic = lm(multiples_normalized ~ poly(thirty_yr,3))
model_tenyr_thirtyyr_fed = lm(multiples_normalized ~ thirty_yr + ten_yr + fed_rate)
model_time_tenyr_thirtyyr_fed = lm(multiples_normalized ~ thirty_yr + ten_yr + time + fed_rate)
model_thirtyyr_squared_fed_squared_tenyr_squared = lm(multiples_normalized ~ poly(thirty_yr,2) + poly(fed_rate,2) + poly(ten_yr,2))
model_time_thirtyyr_squared_fed_squared_tenyr_squared = lm(multiples_normalized ~ time + poly(thirty_yr,2) + poly(fed_rate,2) + poly(ten_yr,2))

predicted_model_time = predict(model_time)
predicted_model_fed_cubic = predict(model_fed_cubic)
predicted_model_tenyr_cubic = predict(model_tenyr_cubic)
predicted_model_thirtyyr_cubic = predict(model_thirtyyr_cubic)
predicted_model_tenyr_thirtyyr_fed = predict(model_tenyr_thirtyyr_fed)
predicted_model_time_tenyr_thirtyyr_fed = predict(model_time_tenyr_thirtyyr_fed)
predicted_model_thirtyyr_squared_fed_squared_tenyr_squared = predict(model_thirtyyr_squared_fed_squared_tenyr_squared)
predicted_model_model_time_thirtyyr_squared_fed_squared_tenyr_squared = predict(model_time_thirtyyr_squared_fed_squared_tenyr_squared)

error_model_time = rmse(multiples_normalized, predicted_model_time)
error_model_fed_cubic = rmse(multiples_normalized, predicted_model_fed_cubic)
error_model_tenyr_cubic = rmse(multiples_normalized, predicted_model_tenyr_cubic)
error_model_thirtyyr_cubic = rmse(multiples_normalized, predicted_model_thirtyyr_cubic)
error_model_tenyr_thirtyyr_fed = rmse(multiples_normalized, predicted_model_tenyr_thirtyyr_fed)
error_model_tenyr_thirtyyr_fed = rmse(multiples_normalized, predicted_model_tenyr_thirtyyr_fed)
error_model_time_tenyr_thirtyyr_fed = rmse(multiples_normalized, predicted_model_time_tenyr_thirtyyr_fed)
error_model_thirtyyr_squared_fed_squared_tenyr_squared = rmse(multiples_normalized, predicted_model_thirtyyr_squared_fed_squared_tenyr_squared)
error_model_time_thirtyyr_squared_fed_squared_tenyr_squared = rmse(multiples_normalized, predicted_model_model_time_thirtyyr_squared_fed_squared_tenyr_squared)


plot(time, multiples_normalized, col="red")
points(time, predicted_model_time, col="blue")

plot(time, multiples_normalized, col="red")
points(time, predicted_model_fed_cubic, col="blue")

plot(time, multiples_normalized, col="red")
points(time, predicted_model_thirtyyr_squared_fed_squared_tenyr_squared, col="blue")

