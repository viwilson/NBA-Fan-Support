#Calculating mean popularity score for each NBA team
aggregate( Popularity ~ Team, NBATrendsData, mean)

#Creating new variable of NumMonth from existing Month variable
NumMonth = substr(as.character(Month), 6,7)

#Setting Levels of MonthNum Variable to correspond to month of year
levels(NBATrendsData$Month)

levels(NBATrendsData$Month) <- c("January", "February", "March", 
                                    "April", "May", "June", "July", 
                                    "August", "September", "October", 
                                    "November", "December")

#Calculating overall mean for each month
tapply(NBATrendsData$Popularity, NBATrendsData$Month,
       mean)
#Calculating overall median for each month
tapply(NBATrendsData$Popularity, NBATrendsData$Month, median)


#Boxplot of Search Popularity by Month
ggplot(NBATrendsData, aes(Month, Popularity)) + geom_boxplot()

library(tidyverse)

#Creating Scatterplot with mean and median popularity score by month
# Red = median, blue = mean
NBATrendsData %>%
  group_by(Month) %>%
  summarize_at(vars(Popularity), funs(mean, median)) %>%
  ggplot() + geom_point(aes(x = Month, y = mean), col = "red") +
  geom_point(aes(x = Month, y = median), col = "blue") 

# In this plot, the red dots represent the mean popularity score across
# all 30 NBA teams for each month, whereas the blue dots represent the
# median popularity score. We can see that for each month the mean
# Google Trends search popularity score is lower than the median. This is
# to be expected, since the data is skewed to the right, with the few
# outlying popularity scores pulling up the mean. This difference between
# the mean and median is even more pronounced in the months of April,
# May, and June. This also makes sense because this during the NBA Playoff
# season, and the popularity scores for the teams that make the playoff
# skyrocket to near 100, causing the median to be largely inflated.

#Scatterplot of mean/median popularity for each team
NBATrendsData %>%
  group_by(Team) %>%
  summarize_at(vars(Popularity), funs(mean, median)) %>%
  ggplot() + geom_point(aes(x = Team, y = mean), col = "red") +
  geom_point(aes(x = Team, y = median), col = "blue") + 
  scale_x_discrete(labels = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE",
                              "DAL", "DEN", "DET", "GSW", "HOU", "IND",
                              "LAC", "LAL", "MEM", "MIA", "MIL", "MIN",
                              "NOP", "NYK", "OKC", "ORL", "PHI", "PHX",
                              "POR", "SAC", "SAS", "TOR", "UTA", "WSH"))

# From this scatterplot, we see that the Sacramento kings have the highest
# median and mean popularity score, by a large margin. The Portland
# Trailblazers, Los Angeles Lakers, and Chicago Bulls are not far behind
# either. The teams that appear to have the lowest mean and median
# popularity scores are the Toronto Raptors, Oklahoma City Thunder, and
# Houston Rockets.


with( NBATrendsData[ NBATrendsData$Team=="AtlantaHawks",  ] , 
      plot(Month , Popularity  ) )

with( NBATrendsData[ NBATrendsData$Team=="Boston.Celtics",  ] , 
      plot(Month , Popularity  ) )

with( NBATrendsData[ NBATrendsData$Team=="Brooklyn.Nets",  ] , 
      plot(Month , Popularity  ) )

uniq_teams <- unique(NBATrendsData$Team)

for (i in 1:30) {
  with(NBATrendsData[NBATrendsData$Team == uniq_teams[i], ],
    plot(Month, Popularity))
}




# Loop


for (i in uniq_teams) {
  
  temp_plot = ggplot(data= subset(NBATrendsData, Team == i)) + 
    geom_point(size=3, aes(x=Month, y=Popularity )) +
    ggtitle(i) 
  
  ggsave(temp_plot, file=paste0("plot_", i,".png"), 
         width = 14, height = 10)
}








write.csv(NBATrendsData, "NBATrendsData.csv")
