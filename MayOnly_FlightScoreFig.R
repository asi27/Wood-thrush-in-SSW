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
filelist
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

####MAY ONLY, DIDN'T INCLUDE COUNT DATA FROM JUNE AND JULY
fcounts <- rbind(counts0503z, counts0504z, counts0508z, counts0509, 
                 counts0515, counts0517, counts0518)

##Add in columns for flight number during current bin, and the three 10 min bins before
{fcounts$FlightT <- 0
  fcounts$FlightT[fcounts$Date == "20170503" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170503" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170503" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  
  fcounts$FlightT[fcounts$Date == "20170504" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170504" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170504" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightT[fcounts$Date == "20170508" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170508" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1

  fcounts$FlightT[fcounts$Date == "20170509" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 2
  
  fcounts$FlightT[fcounts$Date == "20170515" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170515" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "5:40 - 5:50AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "5:50 - 6:00AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  
  fcounts$FlightT[fcounts$Date == "20170518" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 2
  fcounts$FlightT[fcounts$Date == "20170518" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightT[fcounts$Date == "20170518" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1

  
  fcounts$FlightTminus1<- 0
  fcounts$FlightTminus1[fcounts$Date == "20170503" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170503" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170503" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  
  fcounts$FlightTminus1[fcounts$Date == "20170504" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170504" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170504" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus1[fcounts$Date == "20170508" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170508" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  
  fcounts$FlightTminus1[fcounts$Date == "20170509" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 2
  
  fcounts$FlightTminus1[fcounts$Date == "20170515" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170515" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "5:50 - 6:00AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  
  fcounts$FlightTminus1[fcounts$Date == "20170518" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 2
  fcounts$FlightTminus1[fcounts$Date == "20170518" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus1[fcounts$Date == "20170518" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
 
   fcounts$FlightTminus2<- 0
   fcounts$FlightTminus2[fcounts$Date == "20170503" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170503" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170503" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
   
   fcounts$FlightTminus2[fcounts$Date == "20170504" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170504" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170504" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
   
   fcounts$FlightTminus2[fcounts$Date == "20170508" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170508" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
   
   fcounts$FlightTminus2[fcounts$Date == "20170509" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 2
   
   fcounts$FlightTminus2[fcounts$Date == "20170515" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170515" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
   
   fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:00 - 6:10AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
   
   fcounts$FlightTminus2[fcounts$Date == "20170518" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 2
   fcounts$FlightTminus2[fcounts$Date == "20170518" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
   fcounts$FlightTminus2[fcounts$Date == "20170518" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
 
    fcounts$FlightTminus3<- 0
  fcounts$FlightTminus3[fcounts$Date == "20170503" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170503" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170503" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  
  fcounts$FlightTminus3[fcounts$Date == "20170504" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170504" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus3[fcounts$Date == "20170508" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170508" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  
  fcounts$FlightTminus3[fcounts$Date == "20170509" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 2
  
  fcounts$FlightTminus3[fcounts$Date == "20170515" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170515" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1

  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "6:10 - 6:20AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  
  fcounts$FlightTminus3[fcounts$Date == "20170518" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 2
  fcounts$FlightTminus3[fcounts$Date == "20170518" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus3[fcounts$Date == "20170518" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  
  
  fcounts$FlightTminus4<- 0
  fcounts$FlightTminus4[fcounts$Date == "20170503" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170503" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170503" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  
  fcounts$FlightTminus4[fcounts$Date == "20170504" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170504" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus4[fcounts$Date == "20170508" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170508" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  
  fcounts$FlightTminus4[fcounts$Date == "20170509" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 2
  
  fcounts$FlightTminus4[fcounts$Date == "20170515" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170515" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "6:20 - 6:30AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus4[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  
  fcounts$FlightTminus4[fcounts$Date == "20170518" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 2
  fcounts$FlightTminus4[fcounts$Date == "20170518" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  
 
   fcounts$FlightTminus5<- 0
  fcounts$FlightTminus5[fcounts$Date == "20170503" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170503" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170503" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  
  fcounts$FlightTminus5[fcounts$Date == "20170504" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170504" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus5[fcounts$Date == "20170508" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170508" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  
  fcounts$FlightTminus5[fcounts$Date == "20170509" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 2
  
  fcounts$FlightTminus5[fcounts$Date == "20170515" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170515" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "6:30 - 6:40AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus5[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  
  fcounts$FlightTminus5[fcounts$Date == "20170518" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 2
  fcounts$FlightTminus5[fcounts$Date == "20170518" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus6<- 0
  fcounts$FlightTminus6[fcounts$Date == "20170503" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170503" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170503" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  
  fcounts$FlightTminus6[fcounts$Date == "20170504" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus6[fcounts$Date == "20170508" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170508" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  
  fcounts$FlightTminus6[fcounts$Date == "20170509" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 2
  
  fcounts$FlightTminus6[fcounts$Date == "20170515" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170515" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "6:40 - 6:50AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus6[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus6[fcounts$Date == "20170518" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 2
  fcounts$FlightTminus6[fcounts$Date == "20170518" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  
  fcounts$FlightTminus7<- 0
  fcounts$FlightTminus7[fcounts$Date == "20170503" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170503" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170503" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus7[fcounts$Date == "20170504" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus7[fcounts$Date == "20170508" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170508" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  
  fcounts$FlightTminus7[fcounts$Date == "20170509" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 2
  
  fcounts$FlightTminus7[fcounts$Date == "20170515" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170515" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "6:50 - 7:00AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus7[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus7[fcounts$Date == "20170518" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 2
  fcounts$FlightTminus7[fcounts$Date == "20170518" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus8<- 0
  fcounts$FlightTminus8[fcounts$Date == "20170503" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170503" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170503" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus8[fcounts$Date == "20170508" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170508" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  
  fcounts$FlightTminus8[fcounts$Date == "20170509" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 2
  
  fcounts$FlightTminus8[fcounts$Date == "20170515" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170515" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1

  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:00 - 7:10AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus8[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus8[fcounts$Date == "20170518" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 2
  
 
  
  fcounts$FlightTminus9<- 0
  fcounts$FlightTminus9[fcounts$Date == "20170503" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170503" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170503" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus9[fcounts$Date == "20170508" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170508" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus9[fcounts$Date == "20170515" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170515" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus9[fcounts$Date == "20170517" & fcounts$TimeBin == "7:10 - 7:20AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus9[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  
  fcounts$FlightTminus10<- 0
  fcounts$FlightTminus10[fcounts$Date == "20170503" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus10[fcounts$Date == "20170503" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus10[fcounts$Date == "20170508" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus10[fcounts$Date == "20170508" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus10[fcounts$Date == "20170515" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  
  fcounts$FlightTminus10[fcounts$Date == "20170517" & fcounts$TimeBin == "7:20 - 7:30AM"] <- 1
  fcounts$FlightTminus10[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus10[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus10[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  
 
  fcounts$FlightTminus11<- 0
  fcounts$FlightTminus11[fcounts$Date == "20170503" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus11[fcounts$Date == "20170508" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus11[fcounts$Date == "20170515" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
  
  fcounts$FlightTminus11[fcounts$Date == "20170517" & fcounts$TimeBin == "7:30 - 7:40AM"] <- 1
  fcounts$FlightTminus11[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
  fcounts$FlightTminus11[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
 
 
   fcounts$FlightTminus12<- 0
   fcounts$FlightTminus12[fcounts$Date == "20170517" & fcounts$TimeBin == "7:40 - 7:50AM"] <- 1
   fcounts$FlightTminus12[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
   
   fcounts$FlightTminus13<- 0
   fcounts$FlightTminus13[fcounts$Date == "20170517" & fcounts$TimeBin == "7:50 - 8:00AM"] <- 1
   
   
}
  



##CreateFlightScore variable
fcounts$FlightScore <- fcounts$FlightT+ 
  fcounts$FlightTminus1 + 
  (fcounts$FlightTminus2)*.92 + 
  (fcounts$FlightTminus3)*.84 +
  (fcounts$FlightTminus4)*.76 + 
  (fcounts$FlightTminus6)*.68 + 
  (fcounts$FlightTminus6)*.60+ 
  (fcounts$FlightTminus7)*.52 + 
  (fcounts$FlightTminus8)*.44 +
  (fcounts$FlightTminus9)*.36 + 
  (fcounts$FlightTminus10)*.28 + 
  (fcounts$FlightTminus11)*.2 + 
  (fcounts$FlightTminus12)*.12 + 
  (fcounts$FlightTminus13)*.04

fcounts <- fcounts[which(fcounts$Date == "20170503" |
                          fcounts$Date == "20170504" | 
                           fcounts$Date == "20170508" |
                           fcounts$Date == "20170509" |
                           fcounts$Date == "20170515" |
                           fcounts$Date == "20170517"|
                        fcounts$Date == "20170518"),]



###Add weather data
fcounts$HighTemp[fcounts$Date == "20170503"] <- "Above50"
fcounts$HighTemp[fcounts$Date == "20170504"] <- "Above50"
fcounts$HighTemp[fcounts$Date == "20170508"] <- "Below50"
fcounts$HighTemp[fcounts$Date == "20170509"] <- "Below50"
fcounts$HighTemp[fcounts$Date == "20170515"] <- "Above50"
fcounts$HighTemp[fcounts$Date == "20170517"] <- "Above50"
fcounts$HighTemp[fcounts$Date == "20170518"] <- "Above50"
 
fcounts$Temp[fcounts$Date == "20170503"] <- 59
fcounts$Temp[fcounts$Date == "20170504"] <- 53
fcounts$Temp[fcounts$Date == "20170508"] <- 44
fcounts$Temp[fcounts$Date == "20170509"] <- 48
fcounts$Temp[fcounts$Date == "20170515"] <- 59
fcounts$Temp[fcounts$Date == "20170517"] <- 68
fcounts$Temp[fcounts$Date == "20170518"] <- 88

fcounts$NumSites[fcounts$Date == "20170503"] <- 3
fcounts$NumSites[fcounts$Date == "20170504"] <- 2
fcounts$NumSites[fcounts$Date == "20170508"] <- 5
fcounts$NumSites[fcounts$Date == "20170509"] <- 1
fcounts$NumSites[fcounts$Date == "20170515"] <- 6
fcounts$NumSites[fcounts$Date == "20170517"] <- 3
fcounts$NumSites[fcounts$Date == "20170518"] <- 4

fcounts$AvgSongNum <- fcounts$SongNum/fcounts$NumSites

###Create plot for song number v. flight score
ggplot(data = fcounts, aes(x = FlightScore, y = SongNum, color = HighTemp)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Song Number Per 10 min", 
        x = "Flight Impact Score") + 
  theme_classic(base_size = 18)  


###Create plot for average song number v. flight score
ggplot(data = fcounts, aes(x = FlightScore, y = AvgSongNum)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Average Song Num per site, per 10 mins", 
        x = "Flight Impact Score") + 
  theme_classic(base_size = 18)  




###Create plot for average song number v. flight score including only data for 0600h
fcounts6 <- fcounts[which(fcounts$TimeBin == "6:00 - 6:10AM" | 
                              fcounts$TimeBin == "6:10 - 6:20AM" |
                              fcounts$TimeBin == "6:20 - 6:30AM" |
                              fcounts$TimeBin == "6:30 - 6:40AM" |
                              fcounts$TimeBin == "6:40 - 6:50AM" |
                              fcounts$TimeBin == "6:50 - 7:00AM"),]
ggplot(data = fcounts6, aes(x = FlightScore, y = AvgSongNum)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Average Song Num per site, per 10 mins", 
        x = "Flight Impact Score") + 
  theme_classic(base_size = 18)  


ggplot(data = fcounts6, aes(x = FlightScore, y = AvgSongNum, color = HighTemp)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Average Song Num per site, per 10 mins", 
        x = "Flight Impact Score") + 
  theme_classic(base_size = 18)  

ggplot(data = fcounts6, aes(x = Temp, y = AvgSongNum)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Average Song Num per site, per 10 mins", 
        x = "High Daily Temperature") + 
  theme_classic(base_size = 18)  


###Create plot for average song number v. flight score, NOT including data from before 5:20
fcounts520 <- fcounts[which(fcounts$TimeBin != "5:00 - 5:10AM" | 
                            fcounts$TimeBin == "5:10 - 5:20AM"),]
ggplot(data = fcounts520, aes(x = FlightScore, y = AvgSongNum)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Average Song Num per site, per 10 mins", 
        x = "Flight Impact Score") + 
  theme_classic(base_size = 18)  

###Create plot for average song number v. flight score, NOT including data from before 5:40 (peak)
fcounts540 <- fcounts[which(fcounts$TimeBin != "5:00 - 5:10AM" & 
                              fcounts$TimeBin != "5:10 - 5:20AM" &
                              fcounts$TimeBin != "5:20 - 5:30AM" & 
                              fcounts$TimeBin != "5:30 - 5:40AM"),]
ggplot(data = fcounts540, aes(x = FlightScore, y = AvgSongNum)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Average Song Num per site, per 10 mins", 
        x = "Flight Impact Score") + 
  theme_classic(base_size = 18)  



###Create plot for song number v. flight score
ggplot(data = fcounts, aes(x = FlightScore, y = SongNum, color = Date)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Song Number Per 10 min", 
        x = "Flight Impact Score") + 
  theme_classic(base_size = 18)  

###Song Num Per Time bin (no data on flights, just looking at wood thrush singing patterns over time)
ggplot(data = fcounts, aes(x = TimeBin, y = SongNum)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Song Number Per 10 min") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=18)) + 
  theme(axis.text.y = element_text( hjust = 1, size = 12),
        axis.title.y  = element_text(size=18)) 


###Flight score v time (nothing about wood thrush song)
ggplot(data = fcounts, aes(x = TimeBin, y = FlightScore)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( y = "Flight Score", 
        X = "Time Bin") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x  = element_text(size=18)) + 
  theme(axis.text.y = element_text( hjust = 1, size = 12),
        axis.title.y  = element_text(size=18)) 


####Add song data and flight data to the same graph
ggplot(data = fcounts, aes(x = TimeBin)) +
  geom_point(aes(y = SongNum, colour = "Song Number")) +
  geom_point(aes(y = FlightScore*200, colour = "Flight Score")) + 
  scale_y_continuous(sec.axis = sec_axis(~./200,name = "Flight Score")) +
  scale_colour_manual(values = c("blue", "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x  = element_text(size=18)) 


###Create counts data set with SIte so that I can add error bars to graph (site is replicate on any given day)
countsSite <- d[, .(SongNum = .N), by = c("TimeBin", "Date", "Site")]

###############May 3rd
countsSite0503 <- countsSite[which(countsSite$Date == "20170503"),]
eb.0503 <- ddply(countsSite0503, c("TimeBin"), summarise,
                   N    = sum(!is.na(SongNum)),
                   mean = mean(SongNum, na.rm=TRUE),
                   sd   = sd(SongNum, na.rm=TRUE),
                   se   = sd / sqrt(N))

##Graph with error bars for 0503
ggplot(data = eb.0503, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0503", y = "Song Number per 10 min bin") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)

############May 4th 
countsSite0504 <- countsSite[which(countsSite$Date == "20170504"),]
eb.0504 <- ddply(countsSite0504, c("TimeBin"), summarise,
                 N    = sum(!is.na(SongNum)),
                 mean = mean(SongNum, na.rm=TRUE),
                 sd   = sd(SongNum, na.rm=TRUE),
                 se   = sd / sqrt(N))

##Graph with error bars for 0504
ggplot(data = eb.0504, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0504", y = "Song Number per 10 min bin") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)



############May 8th 
countsSite0508 <- countsSite[which(countsSite$Date == "20170508"),]
eb.0508 <- ddply(countsSite0508, c("TimeBin"), summarise,
                 N    = sum(!is.na(SongNum)),
                 mean = mean(SongNum, na.rm=TRUE),
                 sd   = sd(SongNum, na.rm=TRUE),
                 se   = sd / sqrt(N))

##Graph with error bars for 0508
ggplot(data = eb.0508, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0508", y = "Song Number per 10 min bin") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)



############May 15th 
countsSite0515 <- countsSite[which(countsSite$Date == "20170515"),]
eb.0515 <- ddply(countsSite0515, c("TimeBin"), summarise,
                 N    = sum(!is.na(SongNum)),
                 mean = mean(SongNum, na.rm=TRUE),
                 sd   = sd(SongNum, na.rm=TRUE),
                 se   = sd / sqrt(N))

##Graph with error bars for 0515
ggplot(data = eb.0515, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0515", y = "Song Number per 10 min bin") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)


############May 17th 
countsSite0517<- countsSite[which(countsSite$Date == "20170517"),]
eb.0517 <- ddply(countsSite0517, c("TimeBin"), summarise,
                 N    = sum(!is.na(SongNum)),
                 mean = mean(SongNum, na.rm=TRUE),
                 sd   = sd(SongNum, na.rm=TRUE),
                 se   = sd / sqrt(N))

##Graph with error bars for 0517
ggplot(data = eb.0517, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0517", y = "Song Number per 10 min bin") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)



############May 18th 
countsSite0518 <- countsSite[which(countsSite$Date == "20170518"),]
eb.0518 <- ddply(countsSite0518, c("TimeBin"), summarise,
                 N    = sum(!is.na(SongNum)),
                 mean = mean(SongNum, na.rm=TRUE),
                 sd   = sd(SongNum, na.rm=TRUE),
                 se   = sd / sqrt(N))

##Graph with error bars for 0518
ggplot(data = eb.0518, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0518", y = "Song Number per 10 min bin") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)


###Counts 
countsDate<- d[, .(SongNum = .N), by = "Date"]
countsDateMay <- countsDate[which(countsDate$Date == "20170503" |
                                    countsDate$Date == "20170508" |
                                    countsDate$Date == "20170515" |
                                    countsDate$Date == "20170518"),]
countsDateMay$Flights <- c(3, 2, 2, 4)

ggplot(data = countsDateMay, aes(x = Flights, y = SongNum)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.25, size = 1.5) + 
  labs( title = "Wood Thrush", 
        y = "Daily number of wood thrush during dawn chorus", 
        x = "Number of flights during dawn chorus") + 
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
