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

pdf(file = "./plots/Rplot1.pdf")
layout(rbind(1,2), heights=c(9,1))
colours <- c("blue", "purple", "cyan", "red", "gold")
plot.zoo(ts, plot.type = "single", xlab = NA, ylab = "Number of followers",
         col = colours, lwd = 2)
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center",legend=c("BNP","UKIP","Cons","Lab","LibDem"), 
       lwd=2, col=colours, ncol = 5, bty ="n")
dev.off()

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
yougov <- read.csv("data/yougov_voting_intention.csv")
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

pdf(file = "./plots/Rplot2.pdf")
layout(rbind(1,2), heights=c(9,1))
colours <- c("purple", "purple", "cyan", "cyan", "red", "red", "gold", "gold")
plot.zoo(ts5, plot.type = "single", xlab = NA, ylab = "Percentage", 
         col = colours, lwd = 2, lty = c(1,2,1,2,1,2,1,2))
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center",legend=c("UKIP(T)","UKIP(Y)", "Cons(T)","Cons(Y)", "Lab(T)","Lab(Y)", "LibDem(T)", "LibDem(Y)"), 
       lwd=2, lty = c(1,2,1,2,1,2,1,2), col=colours, ncol = 4, bty ="n")
dev.off()

##########################################################################
#Summary statistics
##########################################################################
require(maptools)
require(rgdal)
require(ggplot2)
require(gridExtra)
require(plyr)

europe <- readOGR(dsn = "./data/map", layer = "european_region_region")
proj4string(europe) <- CRS("+init=epsg:27700")
europe.wgs84 <- spTransform(europe, CRS("+init=epsg:4326"))

metadata_uk <- read.csv("./data/metadata_uk_region.csv")
metadata_uk <- metadata_uk[!is.na(metadata_uk$region),]

twitter <- count(metadata_uk, "region")
population <- read.csv("data/ons_population.csv")
demography <- merge(twitter, population, by = "region")
demography$region <- gsub("_EURO_REGION", "", demography$region)
# demography$freq <- log(demography$freq)
# demography$population <- log(demography$population)
population.plot1 <- ggplot(demography, aes(x = population, y = freq, label = region)) + 
    xlab("Population size(ONS)") + ylab("Sample size(Twitter dataset)") +
    geom_point(colour = "black") + geom_text(colour = "black", size = 4) +
    geom_text(data = data.frame(), 
              aes(8e+06, 9e+03, label = paste0("Pearson-R = ", as.character(round(cor(demography$population, demography$freq), 2))))) +
    geom_abline(intercept = coef(lm(freq ~ population, data = demography))[1], 
                slope = coef(lm(freq ~ population, data = demography))[2])

population.plot2 <- ggplot(demography[-c(3,6),], aes(x = population, y = freq, label = region)) + 
  xlab("Population size(ONS)") + ylab("Sample size(Twitter dataset)") +
  geom_point(colour = "black") + geom_text(colour = "black", size = 4) +
  geom_text(data = data.frame(), 
            aes(8e+06, 4e+03, label = paste0("Pearson-R = ", as.character(round(cor(demography[-c(3,6),]$population, demography[-c(3,6),]$freq), 2))))) +
  geom_abline(intercept = coef(lm(freq ~ population, data = demography[-c(3,6),]))[1], 
              slope = coef(lm(freq ~ population, data = demography[-c(3,6),]))[2])

pdf(file = "./plots/Rplot33.pdf")
population.plot1
dev.off()

pdf(file = "./plots/Rplot34.pdf")
population.plot2
dev.off()

twitter$ukip <- count(metadata_uk, "region", "ukip")[,2]
results <- read.csv("data/epe_results_2014.csv")
election <- merge(twitter, results, by = "region")
election$region <- gsub("_EURO_REGION", "", election$region)
election$ukip.freq <- election$ukip.x/election$freq

election.plot1 <- ggplot(election, aes(x = ukip.freq, y = ukip.perc, label = region)) + 
  xlab("UKIP regional share(Twitter)") + ylab("UKIP regional result(EP2014)") +
  geom_point(colour = "purple") + geom_text(colour = "purple", size = 4) +
  geom_text(data = data.frame(), 
            aes(0.125, 0.2, label = paste0("Pearson-R = ", as.character(round(cor(election$ukip.freq, election$ukip.perc), 2))))) +
  geom_abline(intercept = coef(lm(ukip.perc ~ ukip.freq, data = election))[1], 
              slope = coef(lm(ukip.perc ~ ukip.freq, data = election))[2])

election.plot2 <- ggplot(election[-6,], aes(x = ukip.freq, y = ukip.perc, label = region)) + 
  xlab("UKIP regional share(Twitter)") + ylab("UKIP regional result(EP2014)") +
  geom_point(colour = "purple") + geom_text(colour = "purple", size = 4) +
  geom_text(data = data.frame(), 
              aes(0.125, 0.2, label = paste0("Pearson-R = ", as.character(round(cor(election[-6,]$ukip.freq, election[-6,]$ukip.perc), 2))))) +
  geom_abline(intercept = coef(lm(ukip.perc ~ ukip.freq, data = election[-6,]))[1], 
              slope = coef(lm(ukip.perc ~ ukip.freq, data = election[-6,]))[2])

pdf(file = "./plots/Rplot43.pdf")
election.plot1
dev.off()

pdf(file = "./plots/Rplot44.pdf")
election.plot2
dev.off()

gender <- count(metadata_uk$gender)
gender$ons <- c((41039 * 0.51), (41039 * 0.49))
gender$ipsos <- c((41039 * 0.42), (41039 * 0.58))
colnames(gender)[1:2] <- c("gender", "twitter")
gender.m <- melt(gender)
colnames(gender.m) <- c("gender", "source", "followers")
gender.plot <- ggplot(data=gender.m, aes(x=gender, y=followers, fill=source)) + geom_bar(stat="identity", position=position_dodge())

pdf(file = "./plots/Rplot53.pdf")
gender.plot
dev.off()

followers <- read.csv("./data/metadata_uk_region.csv")
followers <- followers[followers$bnp == 0 & followers$libdem == 0 & followers$cons == 0 & followers$lab == 0,]
countfolls <- na.omit(count(followers, "region"))
countfolls$freq <- countfolls$freq/sum(countfolls$freq)
levels(countfolls$region) <- c("Eastern Euro Region", "East Midlands Euro Region", "London Euro Region",
                         "North East Euro Region", "North West Euro Region", "Scotland Euro Region",
                         "South East Euro Region", "South West Euro Region", "Wales Euro Region",
                         "West Midlands Euro Region", "Yorkshire and the Humber Euro Region")

voters <- read.csv("./data/epe_results_2014.csv")
countvots <- data.frame(c("Eastern Euro Region", "East Midlands Euro Region", "London Euro Region",
                          "North East Euro Region", "North West Euro Region", "Scotland Euro Region",
                          "South East Euro Region", "South West Euro Region", "Wales Euro Region",
                          "West Midlands Euro Region", "Yorkshire and the Humber Euro Region"), voters$ukip/sum(voters$ukip))

colnames(countvots) = c("region", "freq")

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

europe.f2 <- merge(europe.f, countfolls, by.x = "id", by.y = "region")

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

europe.f3 <- merge(europe.f, countvots, by.x = "id", by.y = "region")

Map3 <- ggplot(europe.f3, aes(long, lat, group = group, fill = freq)) +
  geom_polygon() + coord_equal() + theme(axis.text = element_blank(),
                                         axis.ticks = element_blank(),
                                         axis.line = element_blank(),
                                         panel.background = element_blank(),
                                         panel.grid = element_blank()) +
  scale_fill_gradient(low = "plum1", high = "purple4") +
  labs(x = NULL, y = NULL, fill = "UKIP %") +
  ggtitle("UKIP Voters Distribution")

countfollsres <- merge(countfolls, countres, by.x = "region", by.y = "q1")
countfollsres$region <- gsub(" Euro Region", "", countfollsres$region)

Scatterplot1 <- ggplot(countfollsres, aes(x = freq.x, y = freq.y, label = region)) + 
               xlab("UKIP Followers share") + ylab("UKIP Respondents share") +
               geom_point(colour = "purple") + geom_text(colour = "purple", size = 4) +
               geom_text(data = data.frame(), 
                         aes(0.12, 0.175, label = paste0("Pearson-R = ", as.character(round(cor(countfollsres$freq.x, countfollsres$freq.y), 2))))) +
               geom_abline(intercept = coef(lm(freq.y ~ freq.x, data = countfollsres))[1], 
                           slope = coef(lm(freq.y ~ freq.x, data = countfollsres))[2])

Scatterplot2 <- ggplot(countfollsres[-c(3,6),], aes(x = freq.x, y = freq.y, label = region)) + 
              xlab("UKIP Followers share") + ylab("UKIP Respondents share") +
              geom_point(colour = "purple") + geom_text(colour = "purple", size = 4) +
              geom_text(data = data.frame(), 
                        aes(0.12, 0.175, label = paste0("Pearson-R = ", as.character(round(cor(countfollsres[-c(3,6),]$freq.x, countfollsres[-c(3,6),]$freq.y), 2))))) +
              geom_abline(intercept = coef(lm(freq.y ~ freq.x, data = countfollsres[-c(3,6),]))[1], 
                          slope = coef(lm(freq.y ~ freq.x, data = countfollsres[-c(3,6),]))[2])

countfollsvots <- merge(countfolls, countvots, by = "region")
countfollsvots$region <- gsub(" Euro Region", "", countfollsvots$region)

Scatterplot3 <- ggplot(countfollsvots, aes(x = freq.x, y = freq.y, label = region)) + 
  xlab("UKIP Followers share") + ylab("UKIP Voters share") +
  geom_point(colour = "purple") + geom_text(colour = "purple", size = 4) +
  geom_text(data = data.frame(), 
            aes(0.12, 0.175, label = paste0("Pearson-R = ", as.character(round(cor(countfollsvots$freq.x, countfollsvots$freq.y), 2))))) +
  geom_abline(intercept = coef(lm(freq.y ~ freq.x, data = countfollsvots))[1], 
              slope = coef(lm(freq.y ~ freq.x, data = countfollsvots))[2])

Scatterplot4 <- ggplot(countfollsvots[-c(3,6),], aes(x = freq.x, y = freq.y, label = region)) + 
  xlab("UKIP Followers share") + ylab("UKIP Voters share") +
  geom_point(colour = "purple") + geom_text(colour = "purple", size = 4) +
  geom_text(data = data.frame(), 
            aes(0.12, 0.175, label = paste0("Pearson-R = ", as.character(round(cor(countfollsvots[-c(3,6),]$freq.x, countfollsvots[-c(3,6),]$freq.y), 2))))) +
  geom_abline(intercept = coef(lm(freq.y ~ freq.x, data = countfollsvots[-c(3,6),]))[1], 
              slope = coef(lm(freq.y ~ freq.x, data = countfollsvots[-c(3,6),]))[2])


pdf(file = "./plots/Rplot3.pdf")
grid.arrange(Map1, Map2, ncol = 2)
dev.off()

pdf(file = "./plots/Rplot31.pdf")
Map1
dev.off()

pdf(file = "./plots/Rplot32.pdf")
Map2
dev.off()

pdf(file = "./plots/Rplot4.pdf")
grid.arrange(Scatterplot1, Scatterplot2, nrow = 2)
dev.off()

pdf(file = "./plots/Rplot41.pdf")
Scatterplot1
dev.off()

pdf(file = "./plots/Rplot42.pdf")
Scatterplot2
dev.off()

pdf(file = "./plots/Rplot5.pdf")
grid.arrange(Map3, Map4, ncol = 2)
dev.off()

pdf(file = "./plots/Rplot51.pdf")
Map3
dev.off()

pdf(file = "./plots/Rplot6.pdf")
grid.arrange(Scatterplot3, Scatterplot4, nrow = 2)
dev.off()

pdf(file = "./plots/Rplot61.pdf")
Scatterplot3
dev.off()

pdf(file = "./plots/Rplot62.pdf")
Scatterplot4
dev.off()

##########################################################################
#Geocoding Twitter follower location
##########################################################################
require(ggmap)

metadata_uk <- read.csv("data/metadata_uk.csv", stringsAsFactors = FALSE)
geocoded <- lapply(metadata_uk[1:2500,'location'], geocode)
metadata_uk[1:2500,'lon'] <- sapply(geocoded, function(x) {as.numeric(x[1])})
metadata_uk[1:2500,'lat'] <- sapply(geocoded, function(x) {as.numeric(x[2])})
write.csv(metadata_uk, file = "data/metadata_uk_geocoded.csv", row.names = FALSE)
geocodeQueryCheck()

metadata_uk <- read.csv("data/metadata_uk_geocoded.csv", stringsAsFactors = FALSE)
geocoded <- lapply(metadata_uk[0000:nrow(metadata_uk),'location'], geocode)
metadata_uk[0000:nrow(metadata_uk),'lon'] <- sapply(geocoded, function(x) {as.numeric(x[1])})
metadata_uk[0000:nrow(metadata_uk),'lat'] <- sapply(geocoded, function(x) {as.numeric(x[2])})
write.csv(metadata_uk, file = "data/metadata_uk_geocoded.csv", row.names = FALSE)

##########################################################################
#Estimating region of Twitter followers
##########################################################################
require(maptools)
require(rgdal)

metadata_uk <- read.csv("data/metadata_uk_geocoded.csv", stringsAsFactors = FALSE)
metadata_uk <- metadata_uk[!is.na(metadata_uk$lat) & !is.na(metadata_uk$lon),]
points <- metadata_uk[,c(21,22)]
coordinates(points) <- ~lon + lat
proj4string(points) <- CRS("+init=epsg:4326")

europe <- readOGR(dsn = "./data/map", layer = "european_region_region")
proj4string(europe) <- CRS("+init=epsg:27700")
europe.wgs84 <- spTransform(europe, CRS("+init=epsg:4326"))

overlaid <- over(points, europe.wgs84)
metadata_uk$region <- overlaid$FILE_NAME
write.csv(metadata_uk, file = "data/metadata_uk_region.csv", row.names = FALSE)
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
#Dimension reduction and simplification
##########################################################################
metadata_uk <- read.csv("data/metadata_uk_region.csv", stringsAsFactors = FALSE)
metadata_uk <- metadata_uk[!is.na(metadata_uk$region),]

metadata_uk[,20] <- replace(metadata_uk[,20], metadata_uk[,20] == 'female', 0)
metadata_uk[,20] <- replace(metadata_uk[,20], metadata_uk[,20] == 'male', 1)

metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'EASTERN_EURO_REGION', 0)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'EAST_MIDLANDS_EURO_REGION', 1)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'LONDON_EURO_REGION', 2)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'NORTH_EAST_EURO_REGION', 3)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'NORTH_WEST_EURO_REGION', 4)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'SCOTLAND_EURO_REGION', 5)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'SOUTH_EAST_EURO_REGION', 6)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'SOUTH_WEST_EURO_REGION', 7)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'WALES_EURO_REGION', 8)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'WEST_MIDLANDS_EURO_REGION', 9)
metadata_uk[,23] <- replace(metadata_uk[,23], metadata_uk[,23] == 'YORKSHIRE_AND_THE_HUMBER_EURO_REGION', 10)

simple_metadata <- metadata_uk[,c(1:6,20,23)]
write.csv(simple_metadata, file = "data/simple_metadata.csv", row.names = FALSE)

##########################################################################
#Continuous random sampling
##########################################################################
simple_metadata <- read.csv("data/simple_metadata.csv")

samples <- NULL
for (i in list.files("./data/samples/raw/", recursive = TRUE)) {
  samples <- c(samples, paste("./data/samples/raw/", i, sep = ""))
}
for (i in samples) {
  sam <- read.csv(i)
  simple_metadata <- simple_metadata[!(simple_metadata$id %in% sam$id),]
}
for (i in (length(samples)+1):(length(samples)+2)) {
  newsam <- simple_metadata[sample(nrow(simple_metadata), 1000),]
  write.csv(newsam, file = paste("./data/samples/raw/sample", as.character(i), ".csv", sep = ""), row.names = FALSE)
  simple_metadata <- simple_metadata[!(simple_metadata$id %in% newsam$id),]
}

##########################################################################
#Statistical Analysis(gender, region)
##########################################################################
metadata_uk <- metadata[metadata$timezone == 'London' & (metadata$lang == 'en' | metadata$lang == 'en-gb' | metadata$lang == 'en-GB') 
                        & metadata$protected == 'False' & metadata$location != 'NA' & (metadata$gender1 == 'male' | metadata$gender1 == 'female')
                        & !(metadata$snp == 1 & metadata$ukip == 0 & metadata$libdem == 0 & metadata$cons == 0 & metadata$lab == 0 & metadata$bnp == 0),]
write.csv(metadata_uk, file = "data/metadata_uk.csv", row.names = FALSE)

metadata_gender <- read.csv("./data/metadata_gender.csv", na.strings = "")
metadata_gender <- metadata_gender[!(metadata_gender$snp == 1 & metadata_gender$ukip == 0 & metadata_gender$libdem == 0 & 
                                    metadata_gender$cons == 0 & metadata_gender$lab == 0 & metadata_gender$bnp == 0),]
metadata_gender$gender1 <- factor(metadata_gender$gender1, levels = c("female", "either", "NA", "male"))

plot1 <- qplot(metadata_gender$gender1) + labs(title = "gender")
plot2 <- qplot(metadata_gender[metadata_gender$ukip == 1,]$gender1) + labs(title = "ukip gender")
plot3 <- qplot(metadata_gender[metadata_gender$ukip == 1 & metadata_gender$libdem == 0 & metadata_gender$cons == 0 & metadata_gender$lab == 0 & metadata_gender$bnp == 0,]$gender1) + labs(title = "ukip only gender")

pdf(file = "./plots/Rplot00.pdf")
grid.arrange(plot1, plot2, plot3, ncol = 3)
dev.off()

metadata_uk <- read.csv("./data/metadata_uk_region.csv")
metadata_uk <- metadata_uk[!is.na(metadata_uk$region),]

glm.1 <- glm(ukip ~ gender, data = metadata_uk, family = binomial())
glm.2 <- glm(ukip ~ gender + region, data = metadata_uk, family = binomial())
glm.3 <- glm(ukip ~ gender + region + bnp + cons + lab + libdem, data = metadata_uk, family = binomial())
glm.4 <- glm(bnp ~ gender, data = metadata_uk, family = binomial())
glm.5 <- glm(bnp ~ gender + region, data = metadata_uk, family = binomial())
glm.6 <- glm(bnp ~ gender + region + ukip + cons + lab + libdem, data = metadata_uk, family = binomial())

glm.7 <- glm(cons ~ gender, data = metadata_uk, family = binomial())
glm.8 <- glm(cons ~ gender + region, data = metadata_uk, family = binomial())
glm.9 <- glm(cons ~ gender + region + ukip + bnp + lab + libdem, data = metadata_uk, family = binomial())

glm.10 <- glm(lab ~ gender, data = metadata_uk, family = binomial())
glm.11 <- glm(lab ~ gender + region, data = metadata_uk, family = binomial())
glm.12 <- glm(lab ~ gender + region + ukip + bnp + cons + libdem, data = metadata_uk, family = binomial())

glm.13 <- glm(libdem ~ gender, data = metadata_uk, family = binomial())
glm.14 <- glm(libdem ~ gender + region, data = metadata_uk, family = binomial())
glm.15 <- glm(libdem ~ gender + region + ukip + bnp + cons + lab, data = metadata_uk, family = binomial())

require(stargazer)
stargazer(glm.1, glm.2, glm.3, title = "Models of following UKIP", align=TRUE, dep.var.labels=c("Following UKIP Twitter account"),
          covariate.labels=c("Gender(ref:female)","East Midlands(ref:East)",
                             "London","North East","North West","Scotland", "South East", "South West",
                             "Wales", "West Midlands", "Yorkshire and the Humber", "BNP(ref:not)", "Conservative", 
                             "Labour", "Liberal Democrat"), no.space = TRUE)

stargazer(glm.4, glm.5, glm.6, title = "Models of following BNP", align=TRUE, dep.var.labels=c("Following BNP Twitter account"),
          covariate.labels=c("Gender(ref: female)","East Midlands(ref: East)",
                             "London","North East","North West","Scotland", "South East", "South West",
                             "Wales", "West Midlands", "Yorkshire and the Humber", "UKIP(ref:not)", "Conservative", 
                             "Labour", "Liberal Democrat"), no.space = TRUE)

stargazer(glm.7, glm.8, glm.9, title = "Models of following Conservative", align=TRUE, dep.var.labels=c("Following Conservative Twitter account"),
          covariate.labels=c("Gender(ref: female)","East Midlands(ref: East)",
                             "London","North East","North West","Scotland", "South East", "South West",
                             "Wales", "West Midlands", "Yorkshire and the Humber", "UKIP(ref:not)", "BNP", 
                             "Labour", "Liberal Democrat"), no.space = TRUE)

stargazer(glm.10, glm.11, glm.12, title = "Models of following Labour", align=TRUE, dep.var.labels=c("Following Labour Twitter account"),
          covariate.labels=c("Gender(ref: female)","East Midlands(ref: East)",
                             "London","North East","North West","Scotland", "South East", "South West",
                             "Wales", "West Midlands", "Yorkshire and the Humber", "UKIP(ref:not)", "BNP", 
                             "Conservative", "Liberal Democrat"), no.space = TRUE)

stargazer(glm.13, glm.14, glm.15, title = "Models of following Liberal Democrat", align=TRUE, dep.var.labels=c("Following Liberati Democrat Twitter account"),
          covariate.labels=c("Gender(ref: female)","East Midlands(ref: East)",
                             "London","North East","North West","Scotland", "South East", "South West",
                             "Wales", "West Midlands", "Yorkshire and the Humber", "UKIP(ref:not)", "BNP", 
                             "Conservative", "Labour"), no.space = TRUE)


##########################################################################
#Statistical Analysis(friends, PART 1)
##########################################################################

sample1 <- read.csv('/home/tom/data/sample1_mod.csv', check.names = FALSE)
total <- apply(sample1[,9:ncol(sample1)], 2, sum)
sample1.10 <- sample1[,c(rep(TRUE, 8), total > 10)]
write.csv(sample1.10, file = "data/samples/sample1_10.csv", row.names = FALSE)

sample2 <- read.csv('/home/tom/data/sample2_mod.csv', check.names = FALSE)
total <- apply(sample2[,9:ncol(sample2)], 2, sum)
sample2.10 <- sample2[,c(rep(TRUE, 8), total > 10)]
write.csv(sample2.10, file = "data/samples/sample2_10.csv", row.names = FALSE)

sample1.10 <- read.csv("data/samples/sample1_10.csv", check.names = FALSE)
sample2.10 <- read.csv("data/samples/sample2_10.csv", check.names = FALSE)

require(grplasso)
friends <- data.frame(apply(sample1.10[,9:ncol(sample1.10)], 2, factor))
friends <- friends[,-grep("X358204197", colnames(friends))]
friends$ukip <- as.numeric(as.character(sample1.10$ukip))
fit.friends <- grplasso(ukip ~ ., data = friends, model = LogReg(), lambda = 10, center = TRUE, standardize = TRUE)

friends2 <- data.frame(apply(sample2.10[,9:ncol(sample2.10)], 2, factor))
friends2 <- friends2[,-grep("X358204197", colnames(friends2))]
friends2$ukip <- as.numeric(as.character(sample2.10$ukip))
fit.friends2 <- grplasso(ukip ~ ., data = friends2, model = LogReg(), lambda = 10, center = TRUE, standardize = TRUE)

intersect(names(fit.friends$coefficients[fit.friends$coefficients > 0.1,]), 
          names(fit.friends2$coefficients[fit.friends2$coefficients > 0.1,]))

common <- intersect(names(sample1.10[,9:ncol(sample1.10)]), names(sample2.10[,9:ncol(sample2.10)]))
sam <- merge(sample1.10, sample2.10, by = c("id", "ukip", "libdem", "cons", "lab", "bnp", "gender1", "region", common), all = TRUE)
sam[is.na(sam)] <- 0

friends3 <- data.frame(apply(sam[,9:ncol(sam)], 2, factor))
friends3 <- friends3[,-grep("X358204197", colnames(friends3))]
friends3$ukip <- as.numeric(as.character(sam$ukip))
fit.friends3 <- grplasso(ukip ~ ., data = friends3, model = LogReg(), lambda = 10, center = TRUE, standardize = TRUE)

##########################################################################
#Statistical Analysis(friends, PART 2)
##########################################################################

samplefiles <- NULL

# for (i in list.files("/home/tom/data/", recursive = TRUE)) {
#   samplefiles <- c(samplefiles, paste("/home/tom/data/", i, sep = ""))
# }
# samplefiles <- samplefiles[grepl("_mod", samplefiles)]

for (i in list.files("D:/data/transformed/", recursive = TRUE)) {
  samplefiles <- c(samplefiles, paste("D:/data/transformed/", i, sep = ""))
}
samplefiles <- samplefiles[grepl("_friends", samplefiles)]

for (i in samplefiles) {
  csv <- read.csv(i, check.names = FALSE)
  total <- apply(csv[,9:ncol(csv)], 2, sum)
  csv10 <- csv[,c(rep(TRUE, 8), total > 10)]
  write.csv(csv10, file = sub("_friends", "_10", i), row.names = FALSE)
}

sample10files <- NULL

for (i in list.files("D:/data/transformed/", recursive = TRUE)) {
  sample10files <- c(sample10files, paste("D:/data/transformed/", i, sep = ""))
}
sample10files <- sample10files[grepl("_10", sample10files)]

csv <- read.csv("D:/data/transformed/sample1_10.csv", check.names = FALSE)
csv <- cbind(csv[,1:8], sample = 1, csv[,9:ncol(csv)])
for (i in 2:length(sample10files)) {
  csv1 <- read.csv(sample10files[i], check.names = FALSE)
  csv1 <- cbind(csv1[,1:8], sample = i, csv1[,9:ncol(csv1)])
  common <- intersect(names(csv[,10:ncol(csv)]), names(csv1[,10:ncol(csv1)]))
  csv <- merge(csv, csv1, by = c("id", "ukip", "libdem", "cons", "lab", "bnp", "gender1", "region", "sample", common), all = TRUE)
}
for (i in 1:length(samplefiles)) {
  csvf <- read.csv(samplefiles[i], check.names = FALSE)
  nacols <- sapply(csv[csv$sample == i,], function(x)all(is.na(x)))
  common <- intersect(names(csvf[,10:ncol(csvf)]), names(nacols[nacols == TRUE]))
  csv[csv$sample == i,common] <- csvf[,common]
}
csv[is.na(csv)] <- 0
write.csv(csv, file = "D:/data/transformed/complete_samples.csv", row.names = FALSE)

csv <- read.csv("data/complete_samples.csv", check.names = FALSE)
popukip <- apply(csv[csv$ukip == 1, 10:ncol(csv)], 2, sum)
popbnp <- apply(csv[csv$bnp == 1, 10:ncol(csv)], 2, sum)
poplab <- apply(csv[csv$lab == 1, 10:ncol(csv)], 2, sum)

require(grplasso)
friends <- data.frame(apply(csv[,10:ncol(csv)], 2, factor))
friends <- friends[,-grep("X358204197", colnames(friends))]
friends$ukip <- as.numeric(as.character(csv$ukip))
fit.friends <- grplasso(ukip ~ ., data = friends, model = LogReg(), lambda = 100, center = TRUE, standardize = TRUE)

friends <- data.frame(apply(csv[,10:ncol(csv)], 2, factor))
friends <- friends[,-grep("X279157541", colnames(friends))]
friends$bnp <- as.numeric(as.character(csv$bnp))
fit.friends <- grplasso(bnp ~ ., data = friends, model = LogReg(), lambda = 40, center = TRUE, standardize = TRUE)

require(xtable)
fit.friends.df <- data.frame(fit.friends$coefficients[fit.friends$coefficients < 0,])
colnames(fit.friends.df) <- "coefficient"
fit.friends.df$id <- rownames(fit.friends.df)
rownames(fit.friends.df) <- NULL
fit.friends.df <- fit.friends.df[order(fit.friends.df$coefficient),]
print(xtable(fit.friends.df), include.rownames=FALSE)
