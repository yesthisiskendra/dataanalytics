---
  title: "HW_5"
author: "Kendra Osburn"
date: "5/13/2019"
output: html_document
---
  
  ```{r}
crime <- read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.csv', sep=",", header=TRUE)
plot(crime$murder, crime$burglary)
```

PROBLEM: The outlier on the far right is skewing the graph
SOLUTION: Remove that outlier to get a better understanding of the data as a whole
NOTE: This does not mean we are ignoring Washington, DC. This means we will return to it later

```{r}
crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]
plot(crime2$murder, crime2$burglary)
```

PROBLEM: Axis should start at zero
SOLUTION: Set x-axis(1-10) y-axis(0-1200)

```{r}
plot(crime2$murder, crime2$burglary, xlim = c(0,10), ylim = c(0,1200))
```

PROBLEM: We could use more clarity
SOLUTION: Add a LOESS curve with scatter,smooth()

```{r}
scatter.smooth(crime2$murder, crime2$burglary, xlim = c(0,10), ylim = c(0,1200))
```

PROBLEM: This is still ugly
SOLUTION: Fix it in Illustrator (if time permits)

```{r}
scatter.smooth(crime2$murder, crime2$burglary, xlim = c(0,10), ylim = c(0,1200))
```

<h3>Exploring More Variables </h3>
  ```{r}
plot(crime2[,2:9])
```
PROBLEM: We want curves!
  SOLUTION: add panel.smooth

## 6.9
```{r}
plot(crime2[,2:9], panel=panel.smooth)
```

PROBLEM: It's ugly!
SOLUTION: Fix it in Illustrator (if time permits)

Tip from Nathan:
1. Decide what part of the story you want to tell
2. Design graphic to highlight this area
3. Do not obscure facts

```{r}
crime <- read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.tsv', sep="\t", header=TRUE)
symbols(crime$murder, crime$burglary, circles=crime$population)
```
PROBLEM: The circle sizes are incorrectly proportional to the radius, not the area
SOLUTION: use sqrt( crime$population, pi)

```{r}
radius <- sqrt( crime$population / pi)
symbols(crime$murder, crime$burglary, circles=radius)
```
PROBLEM: The circles are huge!
SOLUTION: Scale the circles down with the inches argument

```{r}
symbols(crime$murder, crime$burglary, circles=radius, inches=0.35, fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate")
```

NOW WITH SQUARES!
```{r}
symbols(crime$murder, crime$burglary, squares=sqrt(crime$population), inches=0.5, xlab="Murder Rate", ylab="Burglary Rate")
```

PROBLEM: We can see a relationship, but we have no idea which bubble is which state
SOLUTION: add labels with text()

```{r}
symbols(crime$murder, crime$burglary, circles=radius, inches=0.35, fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate")
text(crime$murder, crime$burglary, crime$state, cex=0.5)
```

PROBLEM: It's ugly and Georgia is behind Texas!
  SOLUTION: Fix it in Illustrator (if time permits)

```{r}
birth <- read.csv('http://datasets.flowingdata.com/birth-rate.csv')
stem(birth$X2008)
```
```{r}
hist(birth$X2008)
```

## 6.24
```{r}
hist(birth$X2008, main="Global Distribution of Birth Rates", xlab="Live births per 1,000 population", ylab="Country Count", col = "purple", border = "white")
```
PROBLEM: Even with that clean up, it's still unattractive.
SOLUTION: Fix it in Illustrator (if time permits)

## 6.32
NOTE: Density plots cannot have missing values. The first step is always remove the missing values.
```{r}
birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <-density(birth2008)
plot(d2008, type="n")
# type="n" means "no plotting"
polygon(d2008, col="#821122", border="#cccccc")
```

```{r}
# plot(d2008, type="n")
library(lattice)
# histogram(birth$X2008,breaks=10)
# lines(d2008)
```

## MULTIPLE DISTRIBUTIONS
NOTE: Nathan suggests using python (which is awesome) but since we can do this easily right here in R, I'm doing it right here in R

```{r message=FALSE, warning=FALSE}
library(reshape)
mdata <- melt(birth)
melted <- mdata[,2:3]
colnames(melted) <- c('year', 'rate')
histogram(~rate | year, data=melted, layout=c(10,5))
```


PROBLEM: Messy! Reads the wrong direction! An outlier is skewing the data!
  SOLUTION: First, remove outlier

```{r}
melted <- melted[melted$rate <132,]
histogram(~rate | year, data=melted, layout=c(10,5))
```

```{r}
melted$year <- as.character(melted$year)
if(nchar(melted$year[1])>4){
  melted$year <- substring(melted$year, 2)  
}
h <- histogram(~rate | year, data=melted, layout=c(10,5))
update(h, index.cond=list(c(41:50, 31:40, 21:30, 11:20, 1:10)))
```


PROBLEM: Much better! But still, busy and needs some help
SOLUTION: Illustrator if time permits!
  
  
  ## 6.38 & 6.40
  ```{r}
tvs <- read.table('http://datasets.flowingdata.com/tv_sizes.txt', sep="\t", header=TRUE)
# Filter outliers
tvs <- tvs[tvs$size < 80,]
tvs <- tvs[tvs$size > 10,]
# Set breakfs for histograms
breaks = seq(10, 80, by=5)
# Set layout
par(mfrow=c(4,2))
# Draw histograms one by one
hist(tvs[tvs$year == 2009,]$size, breaks=breaks)
hist(tvs[tvs$year == 2008,]$size, breaks=breaks)
hist(tvs[tvs$year == 2007,]$size, breaks=breaks)
hist(tvs[tvs$year == 2006,]$size, breaks=breaks)
hist(tvs[tvs$year == 2005,]$size, breaks=breaks)
hist(tvs[tvs$year == 2004,]$size, breaks=breaks)
hist(tvs[tvs$year == 2003,]$size, breaks=breaks)
hist(tvs[tvs$year == 2002,]$size, breaks=breaks)

```

```{r}
# And now, as a loop
par(mfrow=c(4,2))
for (year in 2002:max(tvs$year)){
  hist(tvs[tvs$year == year,]$size, breaks=breaks)
}
```

```{r}
setwd("/Users/kendraosburn/syracuse/719")
happiness <- read.csv('happiness_project.csv', sep=",", header=TRUE)
par(mfrow=c(3,1))
happiness_melt <- melt(happiness, id=c('year', 'happiness_score'))
hist(happiness_melt[happiness_melt$year == 2015,]$happiness_score, xlim=c(0,10))
abline(v=mean(happiness_melt[happiness_melt$year == 2015,]$happiness_score),col="blue")
hist(happiness_melt[happiness_melt$year == 2016,]$happiness_score, xlim=c(0,10))
abline(v=mean(happiness_melt[happiness_melt$year == 2016,]$happiness_score),col="blue")
hist(happiness_melt[happiness_melt$year == 2017,]$happiness_score, xlim=c(0,10))
abline(v=mean(happiness_melt[happiness_melt$year == 2017,]$happiness_score),col="blue")
# print(mean(happiness_melt[happiness_melt$year == 2015,]$happiness_score))
# print(mean(happiness_melt[happiness_melt$year == 2016,]$happiness_score))
# print(mean(happiness_melt[happiness_melt$year == 2017,]$happiness_score))
# And now, as a loop
# for (year in 2015:2017){
#   hist(happiness_melt[happiness_melt$year == year,]$size, breaks=breaks)
# }
```
```{r}
par(mfrow=c(3,3))
regions <- unique(happiness_melt$region)
for (region in regions) {
  hist(happiness_melt[happiness_melt$region == region,]$happiness_score, 
       xlim=c(3,8), main = paste(region), xlab="Score", ylab="Number of Countries")
}
```

END