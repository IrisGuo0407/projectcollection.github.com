# Library two packages
library(Hmisc)
library(data.table)

# Read three csv files
airbnb_train_x <- fread("/users/leiguo/Desktop/Data Mining and Predictive Analytics (758T)/Project/airbnb_train_x.csv")
airbnb_train_y <- fread("/users/leiguo/Desktop/Data Mining and Predictive Analytics (758T)/Project/airbnb_train_y.csv")
airbnb_test_x <- fread("/users/leiguo/Desktop/Data Mining and Predictive Analytics (758T)/Project/airbnb_test_x.csv")



# Combine train_x and train_y
airbnb_train_x$V1 <- as.numeric(airbnb_train_x$V1)
airbnb_test_x$V1 <- as.numeric(airbnb_test_x$V1)+100000
data_set <- rbind(airbnb_train_x,airbnb_test_x)


# Drop rows
data_set = subset(data_set, !(V1 %in% c(548,16246,30584,47615,56281,65792,72540,75208,75600,92585,95793,96068,100775,110274)))


#2 access: Drop column

#3 accommodates: as.factor
data_set$accommodates <- as.numeric(data_set$accommodates)

#4 amenities: count

f=function(x,output) {length(unlist(strsplit(x,",")))}
data_set$amenities=apply(data_set[,4],1,f)
data_set$amenities <- as.numeric(data_set$amenities)



#5 availability_30: as.numeric
data_set$availability_30 <- as.numeric(data_set$availability_30)

#6 availability_365: as.numeric
data_set$availability_365 <- as.numeric(data_set$availability_365)

#7 availability_60: as.numeric
data_set$availability_60 <- as.numeric(data_set$availability_60)

#8 availability_90: as.numeric
data_set$availability_90 <- as.numeric(data_set$availability_90)


#  Data Transformation
data_set$availability_30_to_60 <- data_set$availability_60-data_set$availability_30

data_set$availability_60_to_90 <- data_set$availability_90-data_set$availability_60

data_set$availability_90_to_365 <- data_set$availability_365-data_set$availability_90



#9 bathrooms: replace missing values with 1.0 (mode), as.numeric
data_set$bathrooms[data_set$bathrooms == ""] <- "1.0"
data_set$bathrooms <- as.numeric(data_set$bathrooms)

#10 bed_type: as.factor
data_set$bed_type <- as.factor(data_set$bed_type)

#11 bedrooms: replace missing values with "1.0" (mode), as.numeric
data_set$bedrooms[data_set$bedrooms == ""] <- "1.0"
data_set$bedrooms <- as.numeric(data_set$bedrooms)

#12 beds: replace missing values with "1.0" (mode), as.numeric
data_set$beds[data_set$beds == ""] <- "1.0"
data_set$beds <- as.numeric(data_set$beds)


# Feature creation: beds/(bedrooms+1)
data_set$beds_per_room <- data_set$beds/(data_set$bedrooms+1)

#13 cancellation_policy: as.factor
data_set$cancellation_policy[data_set$cancellation_policy == "no_refunds"] <- "strict"
data_set$cancellation_policy[data_set$cancellation_policy == "super_strict_30"] <- "strict"
data_set$cancellation_policy[data_set$cancellation_policy == "super_strict_60"] <- "strict"
data_set$cancellation_policy <- as.factor(data_set$cancellation_policy)


#14 city: drop column (location)

#15 city_name: as.factor
data_set$city_name <- as.factor(data_set$city_name)

#16 cleaning_fee: replace missing values with 0, as.numeric
data_set$cleaning_fee[data_set$cleaning_fee == ""] <- "$0.00"
data_set$cleaning_fee = gsub("\\$", "", data_set$cleaning_fee)
data_set$cleaning_fee <- as.numeric(data_set$cleaning_fee)
data_set$cleaning_fee <- impute(data_set$cleaning_fee,0)
data_set$cleaning_fee <- as.numeric(data_set$cleaning_fee)


#17 country: as.factor
data_set$country <- as.factor(data_set$country)


#18 country_code: delete column (same as column "country")


#19 description: delete column (text)


#20 experience_offered: delete column (all are none value)


#21 extra_people: as.numeric
data_set$extra_people = gsub("\\$", "", data_set$extra_people)
data_set$extra_people <- as.numeric(data_set$extra_people)


#22 first_review: convert to days

now <- as.Date("2018-05-04")
data_set$first_review <- as.Date(data_set$first_review,"%Y-%m-%d")
data_set$first_review <- now-data_set$first_review
data_set$first_review <- as.numeric(data_set$first_review)


#23 guests_included: use this column, no need to clean this column


#24 host_about: delete column (text)


#25 host_acceptance_rate: Drop (too many NA)
table(data_set$host_acceptance_rate)


#26 host_has_profile_pic: replace missing values with "t", as.factor
data_set$host_has_profile_pic [data_set$host_has_profile_pic == ""] <- "t"
data_set$host_has_profile_pic <- as.factor(data_set$host_has_profile_pic)


#27 host_identity_verified: replace missing values with "t", as.factor
data_set$host_identity_verified [data_set$host_identity_verified == ""] <- "t"
data_set$host_identity_verified <- as.factor(data_set$host_identity_verified)

#28 host_is_superhost: replace missing values with "f", as.factor
data_set$host_is_superhost [data_set$host_is_superhost == ""] <- "f"
data_set$host_is_superhost <- as.factor(data_set$host_is_superhost)


#29 host_listings_count: replace missing values with "1.0", as.numeric
data_set$host_listings_count [data_set$host_listings_count == ""] <- "1.0"
data_set$host_listings_count <- as.numeric(data_set$host_listings_count)


#30 host_location: delete column (text)


#31 host_name: delete column (not related)


#32 host_neighbourhood: delete column 


#33 host_response_rate: replace missing values with "100%", as.numeric
data_set$host_response_rate [data_set$host_response_rate == ""] <- "100%"
data_set$host_response_rate = gsub("\\%", "", data_set$host_response_rate)
data_set$host_response_rate <- as.numeric(data_set$host_response_rate)

#34 host_response_time: put missing values into a new category "Not given", as.factor
data_set$host_response_time[data_set$host_response_time == ""]<- "Not given"
data_set$host_response_time <- as.factor(data_set$host_response_time)

#35 host_since: convert to days 
data_set$host_since <- as.Date(data_set$host_since,"%Y-%m-%d")
data_set$host_since <- now-data_set$host_since
data_set$host_since <- as.numeric(data_set$host_since)
data_set$host_since <- impute(data_set$host_since,median)
data_set$host_since <- as.numeric(data_set$host_since)

# Feature Creation: host_since - first_review

data_set$period_difference <- data_set$host_since-data_set$first_review


#36 host_total_listing_count: delete column (high correlation with host_listing_count)
data_set$host_total_listings_count [data_set$host_total_listings_count == ""] <- "1.0"
data_set$host_total_listings_count <- as.numeric(data_set$host_total_listings_count)



#37 host_verifications: convert to count
data_set$host_verifications=apply(data_set[,37],1,f)
data_set$host_verifications <- as.numeric(data_set$host_verifications)


#38 house_rules: delete column (text)
f_2=function(x,output) {lengths(gregexpr("\\W+", x)) + 1}
data_set$house_rules=apply(data_set[,38],1,f_2)


#39 instant_bookable: replace missing values with "f", as.factor
data_set$instant_bookable [data_set$instant_bookable == ""] <- "f"
data_set$instant_bookable= as.factor(data_set$instant_bookable)


#40 interacion: delete column (text)


#41 is_business_travel_ready: put missing values into a new category "Not given", as.factor
data_set$is_business_travel_ready[data_set$is_business_travel_ready == ""] <- "Not given"
data_set$is_business_travel_ready= as.factor(data_set$is_business_travel_ready)


#42 is_location_exact: replace missing values with "t", as.factor
data_set$is_location_exact [data_set$is_location_exact == ""] <- "t"
data_set$is_location_exact= as.factor(data_set$is_location_exact)


#43 jurisdiction_names: delete column (text)

#44 latitude: delelte column (location)

#45 license: categorical: categorize data as "Not given" or "Given" (text need to be improved)

data_set$license = ifelse (data_set$license == "", "Not given", "Given")
data_set$license <- as.factor(data_set$license)


#46 longtitude: delete column (location)


#47 market: don't use, same as location



#48 maximum_nights: replace value >=1125 with 1125, as.numeric
data_set$maximum_nights <- as.numeric(data_set$maximum_nights)
data_set$maximum_nights[data_set$maximum_nights>=1125] = 1125
data_set$maximum_nights <- impute(data_set$maximum_nights,1125)
data_set$maximum_nights <- as.numeric(data_set$maximum_nights)


#49 minimum_nights: replace value >=1250 with 1250, as.numeric 
data_set$minimum_nights <- as.numeric(data_set$minimum_nights)
data_set$minimum_nights[data_set$minimum_nights>=1250] = 1250
data_set$minimum_nights <- impute(data_set$minimum_nights,2)
data_set$minimum_nights <- as.numeric(data_set$minimum_nights)



#55 price: replace missing values with median, as.numeric
data_set$price = gsub("\\$", "", data_set$price)
data_set$price <- as.numeric(data_set$price)
data_set$price=impute(data_set$price,median)
data_set$price <- as.numeric(data_set$price)

#50 monthly_price: convert to categorical
data_set$monthly_price = gsub("[$,]", "", data_set$monthly_price)
data_set$monthly_price <- as.numeric(data_set$monthly_price)
data_set$monthly_price[which(is.na(data_set$monthly_price))] <- data_set$price[which(is.na(data_set$monthly_price))]*30



#51 name: delete column (non-related)


#52 neighborhood_overview: delete column (text)


#53 neighbourhood: do not use


#54 notes: delete column (text)




#56 property_type: replace missing values with "Apartment" (mode)
data_set$property_type [data_set$property_type != "Apartment" & data_set$property_type != "House"] <- "Other"
data_set$property_type <- as.factor(data_set$property_type)


#57 require_guest_phone_verification: replace missing values with "f", as.factor
data_set$require_guest_phone_verification[data_set$require_guest_phone_verification == ""] <- "f"
data_set$require_guest_phone_verification <- as.factor(data_set$require_guest_phone_verification)


#58 require_guest_profile_picture: replace missing values with "f", as.factor
data_set$require_guest_profile_picture[data_set$require_guest_profile_picture == ""] <- "f"
data_set$require_guest_profile_picture <- as.factor(data_set$require_guest_profile_picture)


#59 requires_license: replace missing values with "f", as.factor
data_set$requires_license[data_set$requires_license == ""] <- "f"
data_set$requires_license <- as.factor(data_set$requires_license)


#60 room_type: replace missing value with mode
data_set$room_type [data_set$room_type == ""] <- "Entire home/apt"
data_set$room_type= as.factor(data_set$room_type)


#61 security_deposit: drop too many NAs
table(data_set$security_deposit)


#62 smart_location: delete column (location)

#63 space: delete column (text)

#64 square_feet: delete column (too much noe value)

#65 state: as.factor
data_set$state[data_set$state == ""] <- "CA"
data_set$state[data_set$state == "Ca"] <- "CA"
data_set$state[data_set$state == "MP"] <- "NY"
data_set$state[data_set$state == "NJ"] <- "NY"
data_set$state[data_set$state == "New York"] <- "NY"
data_set$state[data_set$state == "Ny"] <- "NY"
data_set$state[data_set$state == "ca"] <- "CA"
data_set$state[data_set$state == "il"] <- "IL"
data_set$state[data_set$state == "ny"] <- "NY"
data_set$state <- as.factor(data_set$state)



#66 street: delete column (location text)

#67 summary: delete column (text)

#68 transit: delete column (text)

#69 weekly_price: convert to categorical, as.factor
data_set$weekly_price = gsub("[$,]", "", data_set$weekly_price)
data_set$weekly_price <- as.numeric(data_set$weekly_price)
data_set$weekly_price[which(is.na(data_set$weekly_price))] <- data_set$price[which(is.na(data_set$weekly_price))]*7


#70 Zip Code: delete column (location)



# Generate new_train_x
new_train_x=data_set[c(1:99988),c(3:13,15:16,21:23,26:29,33:35,37:39,41:42,45,48:50,55:60,65,69,71:75)]

# Create new_train_y
new_train_y <- subset(airbnb_train_y,!(V1 %in% c(548,16246,30584,47615,56281,65792,72540,75208,75600,92585,95793,96068)))

# Create new_test_x
new_test_x =data_set[c(99989:112194),c(3:13,15,16,21:23,26:29,33:35,37:39,41:42,45,48:50,55:60,65,69,71:75)]


# Actual booking rate process
actual_booking_rate <- new_train_y$high_booking_rate
actual_booking_rate <- impute(actual_booking_rate,0)
actual_booking_rate <- as.factor(actual_booking_rate)


new_train_x$high_booking_rate <-actual_booking_rate

install.packages("randomForest")
library(randomForest)


# Partition data
set.seed(3299)
num_insts = nrow(new_train_x)
train_insts <- sample(num_insts,.7*num_insts)
rf_train <- new_train_x[train_insts,]
rf_test <- new_train_x[-train_insts,]

# Build model 
set.seed(19297)
rf_ntree <- randomForest(high_booking_rate~.,data=rf_train[,c(1:3,7:46)],ntree=300)
rf_pred <- predict(rf_ntree,newdata=rf_test[,c(1:3,7:46)])
rf_table <-table(rf_test$high_booking_rate,rf_pred)
accuracy_rf <- sum(diag(rf_table))/sum(rf_table)

# Parameters tuning (we discovered the best mtry is 12)
rf_accu <-c()
i=1
for (a in c(1:20)) {
  set.seed(19297)
  rf_ntree <- randomForest(high_booking_rate~.,data=rf_train[,c(1:3,7:46)],mtry=a,ntree=300)
  rf_pred <- predict(rf_ntree,newdata=rf_test[,c(1:3,7:46)])
  rf_table <-table(rf_test$high_booking_rate,rf_pred)
  accuracy_rf <- sum(diag(rf_table))/sum(rf_table)
  rf_accu[i] <- accuracy_rf
  i=i+1
}

# Build model with all the training data to predict testing data
set.seed(19297)
rf_ntree_test <- randomForest(high_booking_rate~.,data=new_train_x[,c(1:3,7:46)],mtry=12,ntree=300)
rf_pred_test <- predict(rf_ntree_test,newdata=new_test_x[,c(1:3,7:45)])
write.csv(rf_pred_test,"submission_booking_rate_group8.csv")
