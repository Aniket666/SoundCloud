library(rjson)
library(tibble)
library(jsonlite)
library(rpart)
library(randomForest)

#data import
data <- fromJSON(txt = "/Users/aniket/Downloads/sc_data_science_challenge.json", flatten = TRUE)
head(data, 10)

dummy <- rbind(data$columns, data$data)
dummy <- as.data.frame(dummy)
names(dummy) <- data$columns
dummy <- dummy[-1,]
sc_data$listener_prev_month_avg_daily_tracks_listened <- dummy$listener_prev_month_avg_daily_tracks_listened

sc_data <- rbind(data$columns, data$data)
sc_data <- as.data.frame(sc_data)
names(sc_data) <- data$columns
sc_data <- sc_data[-1,]
head(sc_data)


#check number of missing values in each column
colSums(is.na(sc_data))

#create skip column
sc_data$skip <- NA

#create a category change column in which value is 1 if recommented track genre and user's top genre are different
sc_data$category_change <- NA


#implement skip logic
#convert data from factor to integer
sc_data$listen_duration <- as.numeric(sc_data$listen_duration)
sc_data$track_duration <- as.numeric(sc_data$track_duration)
sc_data$listener_prev_month_avg_daily_tracks_listened <- as.numeric(sc_data$listener_prev_month_avg_daily_tracks_listened)
sc_data$skip <- ifelse(sc_data$listen_duration < (0.5*sc_data$track_duration), 1, 0)

#impute NA with median
prev.median <- median(sc_data$listener_prev_month_avg_daily_tracks_listened, na.rm = TRUE)
sc_data$listener_prev_month_avg_daily_tracks_listened[is.na(sc_data$listener_prev_month_avg_daily_tracks_listened)] = prev.median

time.median <- median(sc_data$listener_prev_month_listening_time, na.rm = TRUE)
sc_data$listener_prev_month_listening_time[is.na(sc_data$listener_prev_month_listening_time)] = time.median

#implement category change logic
for(i in 1:nrow(sc_data)){
  sc_data$category_change[i] <- ifelse(is.na(match(sc_data$track_genre_category[i], sc_data$listener_top_genre_category_listened[i])) == TRUE, 1, 0)
}



nrow(subset(sc_data, sc_data$skip == 1))
nrow(subset(sc_data, sc_data$skip == 0))
sc_data$skip <- as.factor(sc_data$skip)
sc_data$listener_prev_month_avg_daily_tracks_listened <- as.numeric(sc_data$listener_prev_month_avg_daily_tracks_listened)
sc_data$listener_prev_month_listening_time <- as.numeric(sc_data$listener_prev_month_listening_time)



#Decision Tree
dtm <- rpart(skip~category_change
             +listening_context
             +listener_prev_month_avg_daily_tracks_listened
             +listener_prev_month_listening_time, data = train, method = "class")
summary(dtm)

dtm_prediction <- predict(dtm, newdata = test, type = "class")
#dtm_prediction <- ifelse(dtm_prediction > 0.5, 1, 0)
table(test$skip, dtm_prediction)
mer <- mean(dtm_prediction != test$skip)
accuracy <- 1 - mer

#random forest
rf <- randomForest(skip~category_change
                   +listening_context
                   +listener_prev_month_avg_daily_tracks_listened
                   +listener_prev_month_listening_time, data = train, ntree = 500)
dtm_prediction <- predict(rf, newdata = test, type = "class")
table(test$skip, dtm_prediction)
