# setwd("~/Dropbox/Git/MY499")
setwd("/media/tom/New Volume/Dropbox/Git/MY499")
# setwd("D:/Dropbox/Git/MY499")

library(zoo)
library(Amelia)
library(mice)

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