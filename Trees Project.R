#######################################################################
##Reading and tidying provided data
#######################################################################

library(xlsx)
library(readr)
setwd("W:/ ")
dat <- read.xlsx("sork weather data.xlsx", 1)
library("openxlsx")
mydf <- read.xlsx("sork weather data.xlsx", sheet = 1, startRow = 2, colNames = TRUE)
#Timestamp = 1 is midnight Jan 1st 1900
library(dplyr)
mydf <- mydf[c(-1,-2),]
mydf$TIMESTAMP <- as.Date(as.numeric(mydf$TIMESTAMP), origin = "1899-12-30")
rain.data <- mydf %>% select(TIMESTAMP,Rain_mm_Tot)
#removes n/a's
rain.data<-na.omit(rain.data)
#removes NAN's
rain.data<-rain.data[rain.data$Rain_mm_Tot != "NAN",]
#aggregates total rainfall in mm by month
monthly.rain <- rain.data %>%
  mutate(month = format(TIMESTAMP, "%m"), year = format(TIMESTAMP, "%Y")) %>%
  group_by(year,month) %>%
  summarise(total = sum(as.numeric(Rain_mm_Tot)))







weather <- monthly.rain



#puts sept-dec to following year 
for(i in 1:116){
  if(weather$month[i]==9 |weather$month[i]==10 |weather$month[i]==11 |weather$month[i]==12)weather$year[i]<-weather$year[i]+1
}

#I imported acorn_data using 'import dataset' so i could set the first row as names
acorn.data<- acorn_data
#lowercase 'Year' so we can merge by year
colnames(acorn.data)[1] <- "year"


#joins weather and acorn.data by year, throws our rows that do not have same year in each
acorn.rain <- inner_join(x=weather,y=acorn.data)



#flattens out the months as multiple variables instead of multiple rows
library(tidyr)
acorn.rain<-spread(acorn.rain,month,total)

#gets rid of 2008 data (no acorn counts)
acorn.rain <- acorn.rain[-c(1:100),]


write.csv(acorn.rain, "acorn.rain.csv")




######################################################################
##a few exploritory models
##you can start here if you download acorn.rain.csv
######################################################################

acorn.rain <- read.csv("acorn.rain.csv")
#testing if fall rainfall has significant predictive power (it doesn't)
model1 <- lm(MnAcorns~factor(flmaxstate) + X9+X10+ X11, data=acorn.rain)
summary(model1)


#LASSO regression
library(glmnet)
acornm1 <- acorn.rain[,c(2,19,23,26,27,28,29,30,31,32,33,34,35,36,37)]
acornm1<-na.omit(acornm1)
mx = model.matrix(MnAcorns~.,data=acornm1) #turns factors into coded numerical values
my = acornm1$MnAcorns
cv.out=cv.glmnet(mx,my,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
model.lasso=glmnet(mx, my,alpha=1,lambda=bestlam)
coef(model.lasso)


#best subsets 'bic'
library(leaps)
exhaust=regsubsets (MnAcorns~.,data=acornm1,nvmax=17)
plot(exhaust,scale="Cp")
exhaust.summary <- summary(exhaust)
plot(exhaust,scale="bic")
plot(1:9,summary(exhaust)$bic)
lines(1:9,summary(exhaust)$bic)
(model.bic <- coef(exhaust ,5))





#########################################################################
##turns date into numeric
#########################################################################
dates <- data.frame(year=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016), tx_start=c("2007-01-01","2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01"))

trees.raw.data <- merge(acorn.rain, dates, by ="year")

trees.raw.data$flstate2first <-as.numeric( as.Date(as.character(trees.raw.data$flstate2first), format="%m/%d/%y")- as.Date(as.character(trees.raw.data$tx_start), format="%Y-%m-%d"))

flstate2sum <- trees.raw.data %>% 
  drop_na(flstate2first) %>% 
  group_by(year) %>% 
  mutate(meanflstate2first = mean(flstate2first)) %>%
  mutate(totrain = X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12) %>%
  ungroup() %>% 
  mutate(timingflstate2first = mean(flstate2first)-flstate2first)


##a couple plots

#this shows response variable is skewed
ggplot(flstate2sum, aes(MnAcorns,color=flstate2first)) + geom_histogram() + scale_colour_gradient(low = "red", high = "green")
#days after jan1st
ggplot(flstate2sum, aes(x=flstate2first,y=MnAcorns,color=year))+ scale_colour_gradient(low = "red", high = "green")  + geom_point()
#total rain
ggplot(flstate2sum, aes(x=flstate2first,y=MnAcorns,color=totrain))+ scale_colour_gradient(low = "red", high = "green")  + geom_point()
#mean fl state
ggplot(flstate2sum, aes(x=meanflstate2first,y=MnAcorns,color=year))+ scale_colour_gradient(low = "red", high = "green")  + geom_point()
#days before/after mean
ggplot(flstate2sum, aes(x=timingflstate2first,y=MnAcorns,color=year))+ scale_colour_gradient(low = "red", high = "green")  + geom_point()





#####################################################################
##Due to skewed data (cause by trees that never produce),
##we need to remove all trees with mean production less than 10
#####################################################################
#group by tree, filter out low prodcting trees
#these two lines work but badly written, probably could be condense into one using mutat and ungroup
tree.group <- acorn.rain %>% group_by(Tree.Num) %>% summarise(acorns = sum(MnAcorns)/8) %>% filter(acorns >= 10)
producing.trees <- acorn.rain %>% filter(Tree.Num %in% tree.group$Tree.Num)

#plots the production of each tree by year (each tree is a different line)
ggplot(data = producing.trees, aes(x=year, y=MnAcorns)) + geom_line(aes(colour=as.factor(Tree.Num)))

#turns the tibble back into a legacy data.frame
tree.group <- as.data.frame(tree.group)

#loop that turns each tree into a differenced time series and plots them
plot(x=c(2009,2010,2011,2012,2013,2014,2015,2016), y=rep(0,8), type = "l",ylim=c(-60,60))
for(i in 1:52){
  plot.df <- producing.trees %>% filter(Tree.Num == tree.group[i,1])
  plot.ts <- ts(plot.df$MnAcorns, start = c(2009,1), frequency = 1)
  plot.ts <- diff(plot.ts)
  lines(plot.ts)
}


#turning flstate2 into numberic for new data frame

##
#####we should consider making this scaleable for any variable (or dataframe)
##
dates <- data.frame(year=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016), tx_start=c("2007-01-01","2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01"))

trees.raw.data <- merge(producing.trees, dates, by ="year")

trees.raw.data$flstate2first <-as.numeric( as.Date(as.character(trees.raw.data$flstate2first), format="%m/%d/%y")- as.Date(as.character(trees.raw.data$tx_start), format="%Y-%m-%d"))

flstate2sum <- trees.raw.data %>% 
  drop_na(flstate2first) %>% 
  group_by(year) %>% 
  mutate(meanflstate2first = mean(flstate2first)) %>%
  mutate(totrain = X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12) %>%
  ungroup() %>% 
  mutate(timingflstate2first = mean(flstate2first)-flstate2first)

#same plots as before but using only productive trees

#days after jan1st
ggplot(flstate2sum, aes(x=flstate2first,y=MnAcorns,color=year))+ scale_colour_gradient(low = "red", high = "green")  + geom_point()
#total rain
ggplot(flstate2sum, aes(x=flstate2first,y=MnAcorns,color=totrain))+ scale_colour_gradient(low = "red", high = "green")  + geom_point()
#mean fl state
ggplot(flstate2sum, aes(x=meanflstate2first,y=MnAcorns,color=year))+ scale_colour_gradient(low = "red", high = "green")  + geom_point()
#days before/after mean
ggplot(flstate2sum, aes(x=timingflstate2first,y=MnAcorns,color=year))+ scale_colour_gradient(low = "red", high = "green")  + geom_point()

