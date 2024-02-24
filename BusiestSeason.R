winter<-read.csv("/Users/katyaliber/Winter/winter.csv")
fall<-read.csv("/Users/katyaliber/Fall/fall.csv")
summer<-read.csv("/Users/katyaliber/Summer/summer.csv")
spring<-read.csv("/Users/katyaliber/Spring/spring.csv")
all<-read.csv("/Users/katyaliber/AllSeasons/all.csv")

numWinter <- length(winter$ride_id)
numFall <- length(fall$ride_id)
numSpring <-length(spring$ride_id)
numSummer <-length(summer$ride_id)
numAll <- length(all)

library(lubridate)
df <- data.frame(
  start_time = as.POSIXct(all$started_at, format = "%Y-%m-%dT%H:%M:%SZ"),
  end_time = as.POSIXct(all$ended_at, format = "%Y-%m-%dT%H:%M:%SZ")
)

# Calculate ride time in df
df$ride_time <- as.numeric(difftime(df$end_time, df$start_time, units = "hours"))

# Add ride_time from df to winter dataframe
all$ride_time <- df$ride_time * 60 

# Print the updated dataframe
print(all$ride_time)



headings <- c("rideable_type","ride_time")

new_df<-subset(all, select =headings)
View(new_df)
selected_values = df %>%
  filter(rideable_type=="electric_bike")
elcMean<-median(new_df$ride_time[new_df$rideable_type=="electric_bike"])
claMean<-median(new_df$ride_time[new_df$rideable_type=="classic_bike"])
plot(new_df$ride_time)
clean_data<-new_df[new_df$ride_time>0,]
clean_data<-clean_data[clean_data$ride_time<240,]
clean_data <- dplyr::group_by(clean_data, rideable_type)  %>% 
  summarize(averageTime = mean(ride_time))

library(tidyverse)

p<-ggplot(data = clean_data, aes(x = rideable_type, y = averageTime, fill = rideable_type)) + 
  geom_col(width=0.5) +
  labs( title="Comparison of Average Ride Times by Bike Type",x="Bike Type", y ="Average Ride Time",fill="Bike Type")
new_names <-c("Classic Bike","Docked Bike", "Electric Bike")
p+scale_x_discrete(label=new_names)



headings2 <- c("member_casual","ride_time")

memVSTime<-subset(all, select =headings2)

memMean<-median(new_df$ride_time[memVSTime$member_casual=="member"])
casMean<-median(new_df$ride_time[memVSTime$member_casual=="casual"])

clean_data3<-memVSTime[memVSTime$ride_time>0,]
clean_data3<-clean_data3[clean_data3$ride_time<240,]

clean_data3 <- dplyr::group_by(clean_data3, member_casual)  %>% 
  summarize(averageTime = mean(ride_time))

ggplot(data = clean_data3, aes(x = member_casual, y = averageTime, fill = member_casual)) +
  geom_col(width=0.5) +
    labs( title="Comparison of Average Ride Times by Membership Type",x="Member type", y ="Average Ride Time",fill="Membership type")


# timeStr = substr(all$ended_at, 12,19)
# time<-substr(timeStr,4,5)
# intEnd1 <-as.numeric(time)
# 
# 
# timeStr = substr(all$started_at, 12,19)
# time<-substr(timeStr,4,5)
# intStart1<-as.numeric(time)
# 
# my_vector <- vector(mode="numeric")
# for(i in 1:length(intStart1)){
#   if(intEnd1[i]>=intStart1[i]){
#     difference<-intEnd1[i]-intStart1[i]
#     my_vector[i]= difference
#   }
#   else{
#     difference<-abs(intStart1[i]-60)+intEnd1[i]
#     my_vector[i]= difference
#   }
# }
# my_vector
# summary(my_vector)
# View(all)
# 
# 
# electricRideTime = vector(mode="numeric")
# classicRideTime = vector(mode="numeric")
# 
# 
# bikeType<-(all$rideable_type)
# 
# for(i in 1:length(bikeType)){
#   if(bikeType[i] == "electric_bike"){
#     electricRideTime[i] = my_vector[i]
#   } else if(bikeType[i] == "classic_bike"){
#     classicRideTime[i] = my_vector[i]
#   }
# }
# 
# elec<-dplyr::select(all,rideable_type,"electric_bike")
# 
# summary(electricRideTime)
# summary(classicRideTime)
# plot(my_vector,electricRideTime, xlab="Times", "electicRideTime")
#  