library(lubridate)
library(geosphere)
library(dplyr)
library(xgboost)
library(caret)

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


#create a xgBoost model
trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## some of the values below are default values in the sklearn-api. 
                       eta = 0.05,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 0.5)
xgb_model = train(
  amended_train_dat[-5], log(amended_train_dat$trip_duration),  
  trControl = trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

# predict log duration
log_pred <- predict(xgb_model, amended_test_dat)

#write to a submission file
outDat = data.frame(id = test_dat$id, trip_duration = exp(log_pred))

write.csv(outDat, "W22P1_sample_submission.csv", row.names = F)
