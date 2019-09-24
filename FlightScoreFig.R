library(readr)
library(lme4)
library(arm)
library(ggplot2)
library(dplyr)
library(plyr)
library(stringr)
library(chron)
library(data.table)


setwd("S:/projects/2016_BRP_SSW_S1067/_Analysis/from_159NAS/WOTH Selection Tables - 9 to 11")
filelist = list.files(path = ".", recursive = TRUE, 
                      pattern = "\\.txt$", 
                      full.names = TRUE)
DTW <- rbindlist(sapply(filelist, fread, simplify = FALSE), 
                use.names = TRUE, idcol = "FileName", fill = TRUE)

DTW$Species <- as.character(DTW$Species)


###ADD site column to data set
DTW$Site <- str_sub(DTW$FileName, 20, 21)
DTW$Site <- as.factor(DTW$Site)

##Add in date column (0605 only, when flights occur at 10:22, 10:25, 10:37)
DTW$Date <- str_sub(DTW$FileName, 31,38)
DTW$Date<- as.factor(DTW$Date)


##Hour
DTW$Hour <- str_sub(DTW$`Begin Path`, -12,-9)
DTW$Hour[DTW$Site == "23" & DTW$Date == "20170711" &DTW$`File Offset (s)` > 3600] <- 1100
DTW$Hour <- as.factor(DTW$Hour)
levels(DTW$Hour)

##Min
DTW$Min <- DTW$`File Offset (s)`/60


##Continous time since 0900
DTW9 <- DTW[which(DTW$Hour == "0900"| DTW$Hour == "0500"),]
DTW9$Min <- as.numeric(DTW9$Min)
DTW9$Time <- DTW9$Min + 60

DTW10 <- DTW[which(DTW$Hour == "1000" | DTW$Hour == "0600"),]
DTW10$Min <- as.numeric(DTW10$Min)
DTW10$Time <- DTW10$Min + 120

DTW11 <- DTW[which(DTW$Hour == "1100" | DTW$Hour == "0700"),]
DTW11$Min <- as.numeric(DTW11$Min)
DTW11$Time <- DTW11$Min + 180

d <- rbind(DTW9, DTW10, DTW11)

{d$TimeBin[d$Time > 60 & d$Time < 70] <- "5:00 - 5:10AM"
  d$TimeBin[d$Time > 70 & d$Time < 80] <- "5:10 - 5:20AM"
  d$TimeBin[d$Time > 80 & d$Time < 90] <- "5:20 - 5:30AM"
  d$TimeBin[d$Time > 90 & d$Time < 100] <- "5:30 - 5:40AM"
  d$TimeBin[d$Time > 100 & d$Time < 110] <- "5:40 - 5:50AM"
  d$TimeBin[d$Time > 110 & d$Time < 120] <- "5:50 - 6:00AM"
  d$TimeBin[d$Time > 120 & d$Time < 130] <- "6:00 - 6:10AM"
  d$TimeBin[d$Time > 130 & d$Time < 140] <- "6:10 - 6:20AM"
  d$TimeBin[d$Time > 140 & d$Time < 150] <- "6:20 - 6:30AM"
  d$TimeBin[d$Time > 150 & d$Time < 160] <- "6:30 - 6:40AM"
  d$TimeBin[d$Time > 160 & d$Time < 170] <- "6:40 - 6:50AM"
  d$TimeBin[d$Time > 170 & d$Time < 180] <- "6:50 - 7:00AM"
  d$TimeBin[d$Time > 180 & d$Time < 190] <- "7:00 - 7:10AM"
  d$TimeBin[d$Time > 190 & d$Time < 200] <- "7:10 - 7:20AM"
  d$TimeBin[d$Time > 200 & d$Time < 210] <- "7:20 - 7:30AM"
  d$TimeBin[d$Time > 210 & d$Time < 220] <- "7:30 - 7:40AM"
  d$TimeBin[d$Time > 220 & d$Time < 230] <- "7:40 - 7:50AM"
  d$TimeBin[d$Time > 230 & d$Time < 240] <- "7:50 - 8:00AM"
}
d$TimeBin <- as.factor(d$TimeBin)


##SongNum column
countsTOT <- d[, .(SongNum = .N), by = c("TimeBin", "Date")]
countsTOT <- na.omit(countsTOT)
countsTOT$TimeBin <- as.factor(countsTOT$TimeBin)
countsTOT$Date <- as.factor(countsTOT$Date)


##Add in zeros where necessary
counts0503 <- countsTOT[which(countsTOT$Date == "20170503"),] 
df0503 <- data.frame("TimeBin" = c( "5:00 - 5:10AM","5:10 - 5:20AM"),
                     "SongNum" = c(0,0), 
                     "Date" = "20170503")
counts0503z <- rbind(counts0503, df0503)

counts0504 <- countsTOT[which(countsTOT$Date == "20170504"),] 
df0504 <- data.frame("TimeBin" = "5:00 - 5:10AM",
                     "SongNum" = 0, 
                     "Date" = "20170504")
counts0504z <- rbind(counts0504, df0504)

counts0508 <- countsTOT[which(countsTOT$Date == "20170508"),]
df0508 <- data.frame("TimeBin" = "5:00 - 5:10AM",
                     "SongNum" = 0, 
                     "Date" = "20170508")
counts0508z <- rbind(counts0508, df0508)

counts0509 <- countsTOT[which(countsTOT$Date == "20170509"),] ##Doesn't need any zeros
counts0515 <- countsTOT[which(countsTOT$Date == "20170515"),]##Doesn't need any zeros
counts0517 <- countsTOT[which(countsTOT$Date == "20170517"),] ##Doesn't need any zeros
counts0518 <- countsTOT[which(countsTOT$Date == "20170518"),]##Doesn't need any zeros

counts0605 <- countsTOT[which(countsTOT$Date == "20170605"),] ##Needs zeros after 6:30
df0605 <- data.frame("TimeBin" = c( "6:40 - 6:50AM","6:50 - 7:00AM", 
                               "7:00 - 7:10AM", "7:10 - 7:20AM", 
                               "7:20 - 7:30AM", "7:30 - 7:40AM", 
                               "7:40 - 7:50AM", "7:50 - 8:00AM"),
                 "SongNum" = c(0,0,
                               0,0,0,
                               0,0,0),
                 "Date" = "20170605")
counts0605z <- rbind(counts0605, df0605)

counts0711 <- countsTOT[which(countsTOT$Date == "20170711"),] ##Needs zeros at 5:00
df0711 <- data.frame("TimeBin" = "5:00 - 5:10AM",
                     "SongNum" = 0,
                     "Date" = "20170711")
counts0711z <- rbind(counts0711, df0711)

fcounts <- rbind(counts0503z, counts0504z, counts0508z, counts0509, 
                 counts0515, counts0517, counts0518, counts0605z, counts0711z)

##Add in columns for flight number during current bin, and the three 10 min bins before
{fcounts$FlightT <- 0
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "5:50 - 6:00AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170605" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 2
  fcounts$FlightT[fcounts$Date == "20170605" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170605" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170711" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170711" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170711" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  
  fcounts$FlightTminus1<- 0
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170605" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 2
  fcounts$FlightTminus1[fcounts$Date == "20170605" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170605" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170711" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170711" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170711" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  
  
  fcounts$FlightTminus2<- 0
  fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus2[fcounts$Date == "20170605" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 2
  fcounts$FlightTminus2[fcounts$Date == "20170605" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus2[fcounts$Date == "20170605" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus2[fcounts$Date == "20170711" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightTminus2[fcounts$Date == "20170711" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus2[fcounts$Date == "20170711" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  
  fcounts$FlightTminus3<- 0
  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170605" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 2
  fcounts$FlightTminus3[fcounts$Date == "20170605" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170605" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170711" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170711" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170711" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  
  fcounts$FlightTminus4<- 0
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170605" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 2
  fcounts$FlightTminus4[fcounts$Date == "20170605" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170605" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170711" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170711" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170711" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus5<- 0
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170605" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 2
  fcounts$FlightTminus5[fcounts$Date == "20170605" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170605" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170711" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170711" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170711" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus6<- 0
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170605" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 2
  fcounts$FlightTminus6[fcounts$Date == "20170605" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170711" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170711" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170711" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus7<- 0
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170605" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 2
  fcounts$FlightTminus7[fcounts$Date == "20170605" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170711" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170711" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus8<- 0
  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170605" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 2
  fcounts$FlightTminus8[fcounts$Date == "20170605" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170711" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170711" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus9<- 0
  fcounts$FlightTminus9[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170605" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 2
  fcounts$FlightTminus9[fcounts$Date == "20170711" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170711" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus10<- 0
  fcounts$FlightTminus10[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus10[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus10[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  fcounts$FlightTminus10[fcounts$Date == "20170711" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1

  fcounts$FlightTminus11<- 0
  fcounts$FlightTminus11[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus11[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  fcounts$FlightTminus11[fcounts$Date == "20170711" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus12<- 0
  fcounts$FlightTminus12[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
}
  



##CreateFlightScore variable
fcounts$FlightScore <- fcounts$FlightT+ 
  fcounts$FlightTminus1 + 
  (fcounts$FlightTminus2)*.91 + 
  (fcounts$FlightTminus3)*.82 +
  (fcounts$FlightTminus4)*.73 + 
  (fcounts$FlightTminus5)*.64 + 
  (fcounts$FlightTminus6)*.55+ 
  (fcounts$FlightTminus7)*.46 + 
  (fcounts$FlightTminus8)*.37 +
  (fcounts$FlightTminus9)*.28 + 
  (fcounts$FlightTminus10)*.19 + 
  (fcounts$FlightTminus11)*.10 + 
  (fcounts$FlightTminus12)*.01 

###Create plot for song number v. flight score
ggplot(data = fcounts, aes(x = FlightScore, y = SongNum, color = Date)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Song Number Per 10 min", 
        x = "Flight Impact Score") + 
  theme_classic(base_size = 18)  




##Models of flight score, zero-inflated poisson for count data
library(pscl)
library(boot)
library(MuMIn)
library(glmm)

fcounts$Date <- as.factor(fcounts$Date)
levels(fcounts$Date)

##Poisson model
pois <- glm(SongNum ~ FlightScore, data = fcounts, family = poisson)
summary(pois)

1 - pchisq(summary(pois)$deviance, 
           summary(pois)$df.residual
)



##Zero inflated
zip.null <- zeroinfl(SongNum ~ Date, data = fcounts)
zip.flightonly <- zeroinfl(SongNum ~ FlightScore, data = fcounts)
zip <- zeroinfl(SongNum ~ FlightScore + Date, data = fcounts)
summary(zip.null)
summary(zip)

out.put <- model.sel(zip, zip.null, zip.flightonly)

nd = data.frame(Trt = fcounts$FlightScore)
cbind(nd, 
      Count = predict(zip, newdata = nd, type = "count"),
      Zero = predict(zip, newdata = nd, type = "zero")
)

confint(zip, level = 0.95)


options(na.action = "na.fail")
all_dredge <- dredge(flight)
subset(all_dredge, delta < 1050)
