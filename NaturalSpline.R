library(lubridate)
library(geosphere)
library(dplyr)
library(splines)
library(boot)

#read data
train_dat <- read.csv("W22P1_train.csv", header = TRUE)
test_dat <- read.csv("W22P1_test.csv", header = TRUE)

#extract hour from pickup_time
train_dat$hour = hour(hms(train_dat$pickup_time))
test_dat$hour = hour(hms(test_dat$pickup_time))

#pre-processing the data
amended_train_dat = train_dat[5:10]
amended_train_dat = mutate(amended_train_dat, distance = distHaversine
                           (cbind(amended_train_dat$dropoff_longitude,
                                  amended_train_dat$dropoff_latitude),
                             cbind(amended_train_dat$pickup_longitude,
                                   amended_train_dat$pickup_latitude)))
amended_test_dat = test_dat[5:9]
amended_test_dat = mutate(amended_test_dat, distance = distHaversine
                          (cbind(amended_test_dat$dropoff_longitude,
                                 amended_test_dat$dropoff_latitude),
                            cbind(amended_test_dat$pickup_longitude,
                                  amended_test_dat$pickup_latitude)))

#create a natural spline model
all_dfs=c(1:20)
cv_errors = rep(0,length(all_dfs))
for (id in all_dfs) {
  fit.ns=glm(log(trip_duration) ~ ns(distance, df = id),data = amended_train_dat)
  cv_errors[id] = cv.glm(amended_train_dat, fit.ns, K=10)$delta[1]
}
plot(all_dfs, cv_errors, xlab = "Degree", ylab = "CV MSE", type = "l")
points(which.min(cv_errors), cv_errors[which.min(cv_errors)], col = "red", cex = 3, pch = 20)

# minimum degree of freedom and error
which.min(cv_errors)+1
cv_errors[which.min(cv_errors)]

#fit model for minimal df
fit.df=lm(log(trip_duration) ~ ns(distance , df = 17),data = amended_train_dat)

# predict log duration
log_pred <- predict(fit.df, amended_test_dat)

#write to a submission file
outDat = data.frame(id = test_dat$id, trip_duration = exp(log_pred))

write.csv(outDat, "W22P1_sample_submission.csv", row.names = F)