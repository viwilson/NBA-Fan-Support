

library(ggplot2)
ggplot(data_long,aes(x=Month,y=Popularity,colour=Team,group=Team)) + 
  
  geom_line()


aggregate( Popularity ~ Team, NBATrendsData, mean)

NumMonth = substr(as.character(Month), 6,7)

lm(Popularity ~ Team + NumMonth)

levels(NBATrendsData$MonthNum) <- c("January", "February", "March", "April", "May", "June", "July", "August", 
                                    "September", "October", "November", "December")

boxplot(Popularity~MonthNum,data=NBATrendsData, main="Search Popularity by Month", 
        xlab="Month", ylab="Popularity (0-100)")

tapply(NBATrendsData$Popularity, NBATrendsData$Month,
       mean)

tapply(NBATrendsData$Popularity, NBATrendsData$Month, median)


attach(NBATrendsData)
model1 <- lm(Popularity ~ Team + Month)
model1

ggplot(NBATrendsData, aes(Month, Popularity)) + geom_point(color = "blue")

ggplot(NBATrendsData, aes(Month, Popularity)) + geom_boxplot()

library(tidyverse)

NBATrendsData %>%
  group_by(Month) %>%
  summarize_at(vars(Popularity), funs(mean, median)) %>%
  ggplot() + geom_point(aes(x = Month, y = mean), col = "red") +
  geom_point(aes(x = Month, y = median), col = "blue")



