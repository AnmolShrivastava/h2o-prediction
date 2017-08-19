library(jsonlite)
library(stringr)
library(data.table)
library(dplyr)
library(splitstackshape)

json_train <- fromJSON("train_data.json")



train_data <- data.table(ID = unlist(names(json_train)))
train_data[, `:=` (
  genres = unlist(lapply(json_train, '[', 1)),
  titles = unlist(lapply(json_train, '[', 2)),
  cities = unlist(lapply(json_train, '[', 3)),
  segment = unlist(lapply(json_train, '[', 4)),
  dow = unlist(lapply(json_train, '[', 5)),
  tod = unlist(lapply(json_train, '[', 6))
)]

train_data$segment <- as.factor(train_data$segment)

json_test <- fromJSON("test_data.json")

test_data <- data.table(ID = unlist(names(json_test)))
test_data[, `:=` (
  genres = unlist(lapply(json_test, '[', 1)),
  titles = unlist(lapply(json_test, '[', 2)),
  cities = unlist(lapply(json_test, '[', 4)),
  dow = unlist(lapply(json_test, '[', 5)),
  tod = unlist(lapply(json_test, '[', 3))
)]
test_data$segment <- 0


total_data <- rbind(train_data, test_data)
dow <- subset(total_data, select = c("ID", "dow"))
dow <- cSplit(dow, splitCols = "dow", sep = ",", "long")
dow <- as.data.table(dow)
dow[, day := substr(dow, 1, str_locate(dow, ":") - 1),]
dow[, dow_dur := gsub(".*:", "", dow), ]
dow$dow_dur <- as.numeric(dow$dow_dur)
top_dow <-
  dow[, rank := .(rank(-(dow_dur), ties.method = "first")) , by = .(ID)]
top_dow <- top_dow[rank == 1, ]
head(top_dow)
head(tod)
tod <- subset(total_data, select = c("ID", "tod"))
tod <- cSplit(tod, splitCols = "tod", sep = ",", "long")
tod <- as.data.table(tod)
tod[, hour := substr(tod, 1, str_locate(tod, ":") - 1),]
tod[, tod_dur := gsub(".*:", "", tod), ]
tod$tod_dur <- as.numeric(tod$tod_dur)
top_tod <-
  tod[, rank := .(rank(-(tod_dur), ties.method = "first")) , by = .(ID)]
top_tod <- top_tod[rank == 1, ]

cities <- subset(total_data, select = c("ID", "cities"))
cities <- cSplit(cities, splitCols = "cities", sep = ",", "long")
cities <- as.data.table(cities)
cities[, city := substr(cities, 1, str_locate(cities, ":") - 1),]
cities[, city_dur := gsub(".*:", "", cities), ]
cities$city_dur <- as.numeric(cities$city_dur)
top_cities <-
  cities[, rank := .(rank(-(city_dur), ties.method = "first")) , by = .(ID)]
top_cities <- top_cities[rank == 1, ]

total_data <-
  cSplit(total_data,
         splitCols = "genres",
         sep = ",",
         direction = "long")
total_data[, genre_duration := substr(total_data$genres,
                                      (str_locate(total_data$genres, ":")[, 1]) + 1,
                                      length(total_data$genres)), ]
total_data[, genres := gsub(":.*", "", total_data$genres) , ]
total_data[, movie_count := str_count(total_data$titles, ":"), ]
total_data[, tod_count := str_count(total_data$tod, ":"), ]
total_data[, dow_count := str_count(total_data$dow, ":"), ]
total_data[, city_count := str_count(total_data$cities, ":"), ]
total_data1 <- subset(total_data, select = c(1, 2, 5, 8, 9, 10, 11, 12))
total_data2 <- melt(total_data1, measure.vars = "genres")
str(c(genre_column))
str(total_data3[, genre_column])
total_data3 <-
  dcast.data.table(
    total_data2,
    ID + segment + dow_count + tod_count + movie_count + city_count ~ value +
      variable
  )
genre_column <- c(grep(".*_genres", colnames(total_data3)))

total_data3[is.na(total_data3), ] <- 0
total_data3 <- as.data.frame(total_data3)

total_data3[, 7:41] <- data.matrix(total_data3[, 7:41])

total_data3[is.na(total_data3)] <- 1
head(total_data3[is.na(total_data3[, genre_column]) == F, ])
total_data3 <- merge(total_data3, top_dow[, c(1, 3, 4)], by = "ID")
total_data3 <- merge(total_data3, top_tod[, c(1, 3, 4)], by = "ID")
total_data3 <- merge(total_data3, top_cities[, c(1, 3, 4)], by = "ID")
total_data3$day <- as.factor(total_data3$day)
total_data3$city <- as.factor(total_data3$city)

total_data3$hour <- as.factor(total_data3$hour)
final_test <-
  total_data3[c(which(total_data3$ID %in% test_data$ID)), ]
final_train <-
  total_data3[-c(which(total_data3$ID %in% test_data$ID)), ]

final_test$segment <- NULL
final_train$segment <- as.factor(final_train$segment)
summary(final_train)
library(caret)
?preProcess
#prep <- preProcess(segment ~ ., x = final_train[, -1], method = "nzv")
#final_train <- predict(prep, final_train)
#final_test <- predict(prep, final_test)
library(h2o)
summary(pre_predi)
ncol(pre_predi)
localH2O <- h2o.init(nthreads = -1)
final_trainh2o <- as.h2o(final_train)
final_testh2o <- as.h2o(final_test)

mod1 <- glm(segment ~ ., data = pre_predi, family = "binomial")
pred1 <- predict(mod1, newdata = final_test[, -1], type = "response")
colnames(final_trainh2o)
y <- 2
x <- c(3:25)
str(pred1)
modh2o <-
  h2o.randomForest(
    x = x,
    y = y,
    training_frame = final_trainh2o,
    nfolds = 5
  )
predh2o <- as.data.frame(h2o.predict(modh2o,final_testh2o))
str(predh2o$pos)

pred_h2o <- data.frame(ID = final_test$ID,segment = predh2o$pos)
write.csv(pred_h2o,"pred_h2orf.csv",row.names = F)
library(randomForest)

mod2 <- randomForest(segment~.,data = final_train[,-c(1,28)])
grid.rf <-
  h2o.grid(
    "gbm",
    x = x,
    y = y,
    training_frame = final_trainh2o,
    
    hyper_params = list(ntrees = c(50, 100, 150), max_depth = c(7,8,9))
  )


gbm_h2o <-
  h2o.gbm(
    y = y,
    x = x,
    training_frame = final_trainh2o,
    nfolds = 5,
    ntrees = 150,max_depth = 9,stopping_rounds = 10
  )


predict.gbm <- as.data.frame(h2o.predict(gbm_h2o, final_testh2o))
pred_h2o <- data.frame(ID = final_test$ID,segment = predict.gbm$pos )
write.csv(pred_h2o,"pred_h2oGBM.csv",row.names = F)


str(final_train)
select A,B,C, CASE WHEN (A = B) AND (B=C) AND ((A+B) > C) AND ((B+C) > A) AND ((C+A) > B) THEN 'Equilateral'
WHEN (A = B) AND (B != C) AND ((A+B) > C) AND ((B+C) > A) AND ((C+A) > B) THEN 'Isosceles'
WHEN (A !=B) AND (B!= C) AND (C != A) AND ((A+B) > C) AND ((A+B) < C) AND ((B+C) > A) AND ((C+A) > B) THEN 'Scalene'
WHEN ((A+B) < C) OR ((B+C) < A) OR ((C+A) < B) THEN 'Not A Triangle' FROM TRIANGLES