library(lubridate)
library(geosphere)
library(dplyr)
library(ranger)

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


#create a random forest model
rf_model <- ranger(log(amended_train_dat$trip_duration) ~ .,data = amended_train_dat[-5], num.trees =  100)

# predict log duration
log_pred <- predict(rf_model, amended_test_dat)

#write to a submission file
outDat = data.frame(id = test_dat$id, trip_duration = exp(log_pred$predictions))

write.csv(outDat, "W22P1_sample_submission.csv", row.names = F)
