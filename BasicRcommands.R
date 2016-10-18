#Intro to R

#testings 

# Resources
#http://egret.psychol.cam.ac.uk/statistics/R/basicstats.html
#http://www.statmethods.net/stats/frequencies.html
#https://www.zoology.ubc.ca/~schluter/R/fit-model/


######## Work with Spider dataset ###############
#first, set working directory using files tab on the lower right side of R Studio. 
#1. Navigate to the folder with your data. (use ... button on right side)
#2. select "more". 
#3. Select "set as working directory"
setwd("~/Box Sync/Iowa State University/Teaching/Rstats/IntroToR")


#Next, load dataset. 
spider<-read.csv("spider.csv", header=T)

####Check class of data ####
str(spider)

############Summary statistics
mean(spider$tot_webs,na.rm=T)
median(spider$tot_webs, na.rm=T)
summary(spider$tot_webs, na.rm=T) # Summarizes model fits, and simple data (for which it gives minimum, first quartile, median, mean, third quartile, maximum).
min(spider$tot_webs, na.rm=T)
max(spider$tot_webs, na.rm=T)
quantile(spider$tot_webs, na.rm=T) # calculates any quantile (not just the default of 0%, 25%, 50%, 75%, 100%)
var(spider$tot_webs, na.rm=T) # sample variance
sd(spider$tot_webs, na.rm=T) # sample standard deviation

########### Counts, boxplots, histograms etc.- use to examine data ##########
table(spider$island, spider$tot_webs) # shows counts of each value in x (producing a unidimensional list of frequencies with the values as dimnames)
table(spider$tot_webs)/length(spider$tot_webs) # shows relative frequencies
#barplot(spider, table(tot_webs)) # by virtue of the way table() produces its results, this produces a frequency histogram
hist(spider$tot_webs) # a more general way of doing a histogram (also deals with continuous data).
# Use "prob=TRUE" to plot relative frequencies (so the area under the graph is 1).
# The "breaks" option can be used for manual control of the number of bins.
boxplot(spider$tot_webs) # Box-and-whisker plot
boxplot(spider$tot_webs~spider$island) #boxplot separated by group
plot(spider$birddens, spider$tot_webs) #scatterplot, but bird_dens varies by island and only 4 islands, so it looks strange

######graphing with ggplot2########
#http://blog.echen.me/2012/01/17/quick-introduction-to-ggplot2/

library(ggplot2) #gives ggplot functions for graphing. 

ggplot(spider, aes(x=island, y=tot_webs))+
      geom_boxplot()+
      xlab("Island")+
      ylab("Number of webs")+ 
      ggtitle("Spider webs and Island")

# `Orange` is a built-in data frame that describes the growth of orange trees.
str(Orange)
summary(Orange)

ggplot(Orange, aes(x=age, y=circumference))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
