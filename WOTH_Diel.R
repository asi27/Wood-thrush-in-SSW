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
DT <- rbindlist(sapply(filelist, fread, simplify = FALSE), 
                use.names = TRUE, idcol = "FileName", fill = TRUE)
DTW <- DT[which(DT$Species == "WOTH"),]

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


##Continous time since 0800
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

range(d$Time)
###Create bins
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

##Calculate # songs per tme bin 
d0505 <- d[which(d$Date == "20170505"),]
counts05 <- d0505[, .(SongNum = .N), by = c("TimeBin", "Site")]
df05<- data.frame("TimeBin" = c("5:00 - 5:10AM","5:10 - 5:20AM","5:20 - 5:30AM",
                               "5:00 - 5:10AM","5:10 - 5:20AM","5:20 - 5:30AM","5:30 - 5:40AM", 
                               "7:00 - 7:10AM", 
                               "7:10 - 7:20AM", "7:20 - 7:30AM", "7:30 - 7:40AM", "7:40 - 7:50AM", "7:50 - 8:00AM",
                               "7:10 - 7:20AM", "7:20 - 7:30AM", "7:30 - 7:40AM", "7:40 - 7:50AM", "7:50 - 8:00AM"), 
                 "SongNum" = c(0,0,0,
                               0,0,0,0,
                               0,
                               0,0,0,0,0,
                               0,0,0,0,0), 
                 "Site" = c("26", "26", "26", 
                            "09", "09", "09", "09",
                            "09", 
                            "26", "26", "26", "26", "26",
                            "09", "09", "09", "09", "09"))

counts05a <- rbind(counts05,df05)
counts05a <- na.omit(counts05a)

counts05a$songrate <- counts05a$SongNum/10

counts05a$TimeBin<- as.character(counts05a$TimeBin)

eb05.data <- ddply(counts05a, c("TimeBin"), summarise,
                  N    = sum(!is.na(songrate)),
                  mean = mean(songrate, na.rm=TRUE),
                  sd   = sd(songrate, na.rm=TRUE),
                  se   = sd / sqrt(N))

ggplot(data = eb05.data, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0505", y = "Song Rate (songs/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)



##Calculate # songs per tme bin 
d0517 <- d[which(d$Date == "20170517"),]
counts3 <- d0517[, .(SongNum = .N), by = c("TimeBin", "Site")]
df3<- data.frame("TimeBin" = c("5:50 - 6:00AM","6:00 - 6:10AM", "6:10 - 6:20AM", "7:30 - 7:40AM", "7:40 - 7:50AM",
  "6:40 - 6:50AM","6:50 - 7:00AM","6:40 - 6:50AM","6:50 - 7:00AM"), 
                "SongNum" = c(0,0,0,0,0,
                  0,0,0,0), 
                "Site" = c("22","22", "22", "22","22",
"26", "26", "23", "23"))

counts3a <- rbind(counts3,df3)
counts3a <- na.omit(counts3a)

counts3a$songrate <- counts3a$SongNum/10

counts3a$TimeBin<- as.character(counts3a$TimeBin)


eb3a.data <- ddply(counts3a, c("TimeBin"), summarise,
                   N    = sum(!is.na(songrate)),
                   mean = mean(songrate, na.rm=TRUE),
                   sd   = sd(songrate, na.rm=TRUE),
                   se   = sd / sqrt(N))


eb3b.data <- ddply(counts3a, c("TimeBin"), summarise,
                   N    = sum(!is.na(SongNum)),
                   mean = mean(SongNum, na.rm=TRUE),
                   sd   = sd(SongNum, na.rm=TRUE),
                   se   = sd / sqrt(N))
##Graph with error bars
ggplot(data = eb3b.data, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0517", y = "Song Number") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)


##Graph with error bars
ggplot(data = eb3a.data, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0517", y = "Song Rate (songs/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)


##Graph split by site
ggplot(data = counts3a, aes(x = TimeBin, y = songrate, fill = Site)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0517", y = "Song Rate (songs/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14))


##Calculate # songs per tme bin 
d0605 <- d[which(d$Date == "20170605"),]
counts0605 <- d0605[, .(SongNum = .N), by = c("TimeBin", "Site")]
levels(d0605$TimeBin)
levels(d0605$Site)

df <- data.frame("TimeBin" = c("5:50 - 6:00AM", "6:00 - 6:10AM","6:30 - 6:40AM",
                                "6:40 - 6:50AM","6:50 - 7:00AM", 
                               "7:00 - 7:10AM", "7:10 - 7:20AM", 
                               "7:20 - 7:30AM", "7:30 - 7:40AM", 
                               "7:40 - 7:50AM", "7:50 - 8:00AM",
                               "6:40 - 6:50AM","6:50 - 7:00AM", 
                               "7:00 - 7:10AM", "7:10 - 7:20AM", 
                               "7:20 - 7:30AM", "7:30 - 7:40AM", 
                               "7:40 - 7:50AM", "7:50 - 8:00AM",
                               "6:40 - 6:50AM","6:50 - 7:00AM", 
                               "7:00 - 7:10AM", "7:10 - 7:20AM", 
                               "7:20 - 7:30AM", "7:30 - 7:40AM", 
                               "7:40 - 7:50AM", "7:50 - 8:00AM"),
                 "SongNum" = c(0,0,0,
                               0,0,0,0,0,0,0,0, 
                               0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0), 
                 "Site" = c("26","26","26",
                   "22", "22","22","22","22","22","22","22",
                 "25","25","25","25","25","25","25","25", 
                   "26","26","26","26","26","26","26","26"))


counts1 <- rbind(counts0605,df)

counts1$songrate <- counts1$SongNum/10
counts1$TimeBin<- as.character(counts1$TimeBin)
counts1 <- na.omit(counts1)

eb1.data <- ddply(counts1, c("TimeBin"), summarise,
                   N    = sum(!is.na(songrate)),
                   mean = mean(songrate, na.rm=TRUE),
                   sd   = sd(songrate, na.rm=TRUE),
                   se   = sd / sqrt(N))

##Graph with error bars
ggplot(data = eb1.data, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0605", y = "Song Rate (songs/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)


ebSN2.data <- ddply(counts1, c("TimeBin"), summarise,
                   N    = sum(!is.na(SongNum)),
                   mean = mean(SongNum, na.rm=TRUE),
                   sd   = sd(SongNum, na.rm=TRUE),
                   se   = sd / sqrt(N))

ggplot(data = ebSN2.data, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0605", y = "Number of Songs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)

##Plot with sites split
ggplot(data = counts1, aes(x = TimeBin, y = songrate, fill = Site)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0605", y = "Song Rate (songs/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14))



##Calculate # songs per tme bin 
d0711 <- d[which(d$Date == "20170711"),]
counts2 <- d0711[, .(SongNum = .N), by = c("TimeBin", "Site")]

df <- data.frame("TimeBin" = c(  "5:00 - 5:10AM",  "5:00 - 5:10AM",  "5:00 - 5:10AM", "5:10 - 5:20AM",
                                 "6:20 - 6:30AM","6:30 - 6:40AM", "6:40 - 6:50AM", "6:50 - 7:00AM", "7:00 - 7:10AM", "7:10 - 7:20AM", "7:20 - 7:30AM", "7:30 - 7:40AM", "7:40 - 7:50AM", "7:50 - 8:00AM",
  "6:30 - 6:40AM","6:40 - 6:50AM"), 
                 "SongNum" = c(0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,
                   0,0), 
                 "Site" = c("12", "23", "26", "12",
                            "12", "12", "12", "12","12", "12","12","12","12", "12",
                            "23", "23"))
counts2a <- rbind(df,counts2)
counts2a$songrate <- counts2a$SongNum/10
counts2a$TimeBin <- as.character(counts2a$TimeBin)
counts2a <- na.omit(counts2a)


###Plot with error bars

eb2a.data <- ddply(counts2a, c("TimeBin"), summarise,
                  N    = sum(!is.na(songrate)),
                  mean = mean(songrate, na.rm=TRUE),
                  sd   = sd(songrate, na.rm=TRUE),
                  se   = sd / sqrt(N))

ggplot(data = eb2a.data, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0711", y = "Song Rate (songs/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)

ebSN.data <- ddply(counts2a, c("TimeBin"), summarise,
                   N    = sum(!is.na(SongNum)),
                   mean = mean(SongNum, na.rm=TRUE),
                   sd   = sd(SongNum, na.rm=TRUE),
                   se   = sd / sqrt(N))

ggplot(data = ebSN.data, aes(x = TimeBin, y = mean)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax=mean + sd), 
                width=.2, position = position_dodge(.9)) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0711", y = "Number of Songs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14)) + 
  geom_hline(yintercept = 0)


##Plot with sites split
ggplot(data = counts2a, aes(x = TimeBin, y = songrate, fill = Site)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0711", y = "Song Rate (songs/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14))


##Calculate # songs per tme bin 
d0717 <- d[which(d$Date == "20170717"),]
counts4 <- d0717[, .(SongNum = .N), by = c("TimeBin", "Site")]


counts4$songrate <- counts4$SongNum/10
counts4$TimeBin<- as.character(counts4$TimeBin)
counts4 <- na.omit(counts4)

ggplot(data = counts4, aes(x = TimeBin, y = songrate, fill = Site)) +       
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() + 
  labs( title = "Wood Thrush, 0717", y = "Song Rate (songs/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x =element_text(size=14)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size=14))

