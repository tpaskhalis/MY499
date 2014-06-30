# setwd("~/Dropbox/Git/MY499")
setwd("/media/tom/New Volume/Dropbox/Git/MY499")
# setwd("D:/Dropbox/Git/MY499")

##########################################################################
#Plotting timeseries for Twitter and YouGov data
##########################################################################
require(zoo)
require(Amelia)

# Twitter data
twitter <- read.csv("data/followers_count.csv")

ts <- zoo(cbind(twitter$BNP, twitter$UKIP, twitter$Conservative, twitter$Labour, twitter$LibDem), 
          as.Date(twitter$Date, "%d/%m/%Y"))

# dev.off()
# layout(rbind(1,2), heights=c(9,1))
# colours <- c("blue", "purple", "cyan", "red", "gold")
# plot.zoo(ts, plot.type = "single", xlab = NA, ylab = "Number of followers",
#          col = colours, lwd = 2)
# par(mar=c(0, 0, 0, 0))
# plot.new()
# legend("center",legend=c("BNP","UKIP","Cons","Lab","LibDem"), 
#        lwd=2, col=colours, ncol = 5, bty ="n")

# Twitter percentages
total <- apply(twitter[,2:6], 1, sum)
twitterp <- cbind(twitter$Date, twitter[,2:6]/total)
names(twitterp)[1] <- "Date" 

ts2 <- zoo(cbind(twitterp$UKIP, twitterp$Conservative, twitterp$Labour, twitterp$LibDem), 
          as.Date(twitterp[,1], "%d/%m/%Y"))

# dev.off()
# layout(rbind(1,2), heights=c(9,1))
# colours <- c("blue", "purple", "cyan", "red", "gold")
# plot.zoo(ts2, plot.type = "single", xlab = NA, ylab = "Percentage of followers",
#          col = colours, lwd = 2)
# par(mar=c(0, 0, 0, 0))
# plot.new()
# legend("center",legend=c("BNP","UKIP","Cons","Lab","LibDem"), 
#        lwd=2, col=colours, ncol = 5, bty ="n")

# YouGov data
yougov <- read.csv("data/voting_intention.csv")
tspattern <- "[0-9]{2}/[0-9]{2}/[0-9]{4}-" 
yougov$Date <- gsub(tspattern, "", yougov$Date)

ts3 <- zoo(cbind(yougov$UKIP, yougov$Conservative, yougov$Labour, yougov$LibDem), 
          as.Date(yougov$Date, "%d/%m/%Y"))

# dev.off()
# layout(rbind(1,2), heights=c(9,1))
# colours <- c("purple", "cyan", "red", "gold")
# plot.zoo(ts3, plot.type = "single", xlab = NA, ylab = "Voting intention",
#          col = colours, lwd = 2, lty=2)
# par(mar=c(0, 0, 0, 0))
# plot.new()
# legend("center",legend=c("UKIP","Cons","Lab","LibDem"), 
#        lwd=2, lty=2, col=colours, ncol = 4, bty ="n")

# YouGov percentages

total <- apply(yougov[,3:6], 1, sum)
yougovp <- cbind(yougov$Date, yougov[,2:6]/total)
names(yougovp)[1] <- "Date"

ts4 <- zoo(cbind(yougovp$UKIP, yougovp$Conservative, yougovp$Labour, yougovp$LibDem), 
          as.Date(yougov[,1], "%d/%m/%Y"))

# dev.off()
# layout(rbind(1,2), heights=c(9,1))
# colours <- c("purple", "cyan", "red", "gold")
# plot.zoo(ts4, plot.type = "single", xlab = NA, ylab = "Percentage of respondents",
#          col = colours, lwd = 2, lty=2)
# par(mar=c(0, 0, 0, 0))
# plot.new()
# legend("center",legend=c("UKIP","Cons","Lab","LibDem"), 
#        lwd=2, lty=2, col=colours, ncol = 4, bty ="n")

# Overlaying Twitter data with YouGov data

youtwit <- merge(twitterp[,-2], yougovp[,-2], by = union("Date", "Date"), all=TRUE)
youtwit$Date <- as.POSIXlt(youtwit$Date, tz = "GMT", "%d/%m/%Y")

complete <- youtwit[complete.cases(youtwit[-1]),]


# small <- youtwit[(youtwit$Date >= as.POSIXlt("25/03/2014", tz = "GMT", "%d/%m/%Y")) & 
#                  (youtwit$Date <= as.POSIXlt("18/04/2014", tz = "GMT", "%d/%m/%Y")),]
# 
# a.out <- amelia(youtwit)

ts5 <- zoo(cbind(complete$UKIP.x, complete$UKIP.y, 
                 complete$Conservative.x, complete$Conservative.y,
                 complete$Labour.x, complete$Labour.y,
                 complete$LibDem.x, complete$LibDem.y),
           complete$Date)

dev.off()
layout(rbind(1,2), heights=c(9,1))
colours <- c("purple", "purple", "cyan", "cyan", "red", "red", "gold", "gold")
plot.zoo(ts5, plot.type = "single", xlab = NA, ylab = "Percentage", 
         col = colours, lwd = 2, lty = c(1,2,1,2,1,2,1,2))
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center",legend=c("UKIP(T)","UKIP(Y)", "Cons(T)","Cons(Y)", "Lab(T)","Lab(Y)", "LibDem(T)", "LibDem(Y)"), 
       lwd=2, lty = c(1,2,1,2,1,2,1,2), col=colours, ncol = 4, bty ="n")

##########################################################################
#Plotting maps for Twitter
##########################################################################
require(maptools)
require(rgdal)
require(ggplot2)
require(gridExtra)
require(plyr)

europe <- readOGR(dsn = "./data/map", layer = "european_region_region")
proj4string(europe) <- CRS("+init=epsg:27700")
europe.wgs84 <- spTransform(europe, CRS("+init=epsg:4326"))

followers <- read.csv("./data/ukip_geocoded.csv")
followers <- followers[followers$bnp == 0 & followers$libdem == 0 & followers$cons == 0 & followers$lab == 0,]
location <- followers[,c(17,18)]
points <- location[complete.cases(location),]
coordinates(points) <- ~lon + lat
proj4string(points) <- CRS("+init=epsg:4326")

overlaid <- over(points, europe.wgs84)
countfolls <- na.omit(count(overlaid, "NAME"))
countfolls$freq <- countfolls$freq/sum(countfolls$freq)

respondents <- read.csv("./data/ukip_bes.csv")
countres <- count(respondents, "q1")
countres$freq <- countres$freq/sum(countres$freq)
levels(countres$q1) <- c("Eastern Euro Region", "East Midlands Euro Region", "London Euro Region",
                         "North East Euro Region", "North West Euro Region", "Scotland Euro Region",
                         "South East Euro Region", "South West Euro Region", "Wales Euro Region",
                         "West Midlands Euro Region", "Yorkshire and the Humber Euro Region", 
                         "Yorkshire and the Humber Euro Region")
countres$freq[11] <- sum(countres$freq[c(11,12)])
countres <- countres[-12,]

europe.f <- fortify(europe, region = "NAME")
europe.f1 <- merge(europe.f, countres, by.x = "id", by.y = "q1")

europe.f2 <- merge(europe.f, countfolls, by.x = "id", by.y = "NAME")

Map1 <- ggplot(europe.f1, aes(long, lat, group = group, fill = freq)) +
  geom_polygon() + coord_equal() + theme(axis.text = element_blank(),
                                         axis.ticks = element_blank(),
                                         axis.line = element_blank(),
                                         panel.background = element_blank(),
                                         panel.grid = element_blank()) +
  scale_fill_gradient(low = "plum1", high = "purple4") +
  labs(x = NULL, y = NULL, fill = "UKIP %") +
  ggtitle("UKIP Respondents Distribution")

Map2 <- ggplot(europe.f2, aes(long, lat, group = group, fill = freq)) +
  geom_polygon() + coord_equal() + theme(axis.text = element_blank(),
                                         axis.ticks = element_blank(),
                                         axis.line = element_blank(),
                                         panel.background = element_blank(),
                                         panel.grid = element_blank()) +
  scale_fill_gradient(low = "plum1", high = "purple4") +
  labs(x = NULL, y = NULL, fill = "UKIP %") +
  ggtitle("UKIP Followers Distribution")
grid.arrange(Map1, Map2, ncol = 2)

##########################################################################
#Geocoding Twitter user location
##########################################################################

require(ggmap)

metadata_uk <- read.csv("data/metadata_uk.csv", na.strings = "", stringsAsFactors = FALSE)
geocoded <- lapply(metadata_uk[1:2500,'location'], geocode)
metadata_uk[1:2500,'lon'] <- sapply(geocoded, function(x) {as.numeric(x[1])})
metadata_uk[1:2500,'lat'] <- sapply(geocoded, function(x) {as.numeric(x[2])})
write.csv(metadata_uk, file = "data/metadata_uk_geocoded.csv", row.names = FALSE)
geocodeQueryCheck()

metadata_uk <- read.csv("data/metadata_uk_geocoded.csv", na.strings = "", stringsAsFactors = FALSE)
geocoded <- lapply(metadata_uk[6001:nrow(metadata_uk),'location'], geocode)
metadata_uk[6001:nrow(metadata_uk),'lon'] <- sapply(geocoded, function(x) {as.numeric(x[1])})
metadata_uk[6001:nrow(metadata_uk),'lat'] <- sapply(geocoded, function(x) {as.numeric(x[2])})
write.csv(metadata_uk, file = "data/metadata_uk_geocoded.csv", row.names = FALSE)

##########################################################################
#Estimating probability of gender
##########################################################################

require(gender)

metadata <- read.csv("data/followers_metadata_22052014.csv", na.strings = "", stringsAsFactors = FALSE)
metadata <- read.csv("data/metadata_gender.csv", na.strings = "", stringsAsFactors = FALSE)

metadata$name1 <- sapply(sapply(metadata$name,strsplit, split = " ", perl = TRUE), '[', 1)
metadata$name1 <- sapply(metadata$name1, tolower)
metadata$name2 <- sapply(sapply(metadata$name,strsplit, split = " ", perl = TRUE), '[', 2)
metadata$name2 <- sapply(metadata$name2, tolower)
metadata$gender1 <- sapply(lapply(metadata$name1, gender), '[[', 4)
metadata$gender2 <- sapply(lapply(metadata$name2, gender), '[[', 4)
write.csv(metadata, file = "data/metadata_gender.csv", row.names = FALSE)

##########################################################################
#Statistical Analysis
##########################################################################

metadata_uk <- metadata[metadata$timezone == 'London' & (metadata$lang == 'en' | metadata$lang == 'en-gb' | metadata$lang == 'en-GB') 
                        & metadata$protected == 'False' & metadata$location != 'NA' & (metadata$gender1 == 'male' | metadata$gender1 == 'female')
                        & !(metadata$snp == 1 & metadata$ukip == 0 & metadata$libdem == 0 & metadata$cons == 0 & metadata$lab == 0 & metadata$bnp == 0),]
metadata_ukip <- metadata_uk[metadata_uk$ukip == 1,]
write.csv(metadata_uk, file = "data/metadata_uk.csv", row.names = FALSE)
write.csv(metadata_ukip, file = "data/metadata_ukip.csv", row.names = FALSE)

metadata_uk[,c(2:8, 20)] <- data.frame(apply(metadata_uk[,c(2:8, 20)], 2, as.factor))

glm.1 <- glm(ukip ~ gender1 + bnp + lab + cons + libdem, data = metadata_uk, family = binomial())
glm.2 <- glm(bnp ~ gender1 + ukip + lab + cons + libdem, data = metadata_uk, family = binomial())
summary(glm.1)
summary(glm.2)
