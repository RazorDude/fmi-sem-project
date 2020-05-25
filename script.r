library(ggplot2)
library(dplyr)
library(reshape2)

data <- read.csv('data/Video_Games_Sales_as_at_22_Dec_2016.csv')
View(data)

# Task 2.1 - location and dispersion of sales by genre
dataByGenre <- group_by(data, Genre)
dataByGenre <- summarise(
	dataByGenre,
	total = sum(Global_Sales),
	mean = mean(Global_Sales),
	range = paste(range(Global_Sales)[1], ' - ', range(Global_Sales)[2]),
	median = median(Global_Sales)
)
View(dataByGenre) # Location data
View(quantile(dataByGenre$total)) # Total sales quantile
var(dataByGenre$total) # Dispersion variability


# Task 2.2
boxplot(data$Global_Sales ~ data$Genre, xlab='Genre', ylab='Global Sales', main='Global Sales By Genre')
t.test(data$Global_Sales[data$Genre=='Strategy'], data$Global_Sales[data$Genre=='Shooter'])
# One-way ANOVA
model<-aov(data$Global_Sales ~ data$Genre)
summary(model)
anova(model)

plot(TukeyHSD(aov(Global_Sales ~ as.factor(Genre), data)))

while(1==1) {
	Sys.sleep(10000)
}
