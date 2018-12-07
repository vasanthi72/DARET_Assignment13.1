COBRA.YTD2017 <- read.csv("E:/kamagyana/Computing/DARET/Assignments/COBRA-YTD2017.csv", stringsAsFactors=FALSE)
View(COBRA.YTD2017)
str(COBRA.YTD2017)
sum(is.na(COBRA.YTD2017))
crimedata <- COBRA.YTD2017
colnames(crimedata)[22] <- "longitude"
colnames(crimedata)[23] <- "lattitude"
colnames(crimedata)[19] <- "CrimeType"
colnames(crimedata)[21] <- "NeiPlgUnit"
library(lubridate)
crimedata$rpt_date <- mdy(crimedata$rpt_date)
crimedata$occur_date <- mdy(crimedata$occur_date)
crimedata$poss_date <- mdy(crimedata$poss_date)
crimedata$occur_time <- hms(crimedata$occur_time)
crimedata$poss_time <- hms(crimedata$poss_time)
numcrime <- cbind(crimedata[,c(2,22,23)])
str(numcrime)
cor(numcrime, method = "pearson", use = "complete.obs")
numcrime <- cbind(crimedata[,c(1,2,8,12,13,15,18,22,23)])
numcrime <- numcrime[,-5]
numcrime$MI_PRINX <- as.numeric(numcrime$MI_PRINX)
numcrime$beat <- as.numeric(numcrime$beat)
numcrime$MinOfucr <- as.numeric(numcrime$MinOfucr)
numcrime$dispo_code <- as.numeric(numcrime$dispo_code)
numcrime$loc_type <- as.numeric(numcrime$loc_type)
str(numcrime)
cor(numcrime, method = "pearson", use = "complete.obs")
crime <-as.data.frame(table(crimedata$CrimeType))
colnames(crime)[1] <- "Crime"
crime
crime[order(-crime$Freq),]
NeiPlgUnit <-as.data.frame(table(crimedata$NeiPlgUnit)); colnames(NeiPlgUnit)[1] <- "NeiPlgUnit" ; NeiPlgUnit[order(-NeiPlgUnit$Freq),]
crimedata <- mutate(crimedata, occur_day = day(occur_date))
crimedata <- mutate(crimedata, occur_month = month(occur_date))
crimedata <- mutate(crimedata, Shift_code = ifelse(Shift == "Day",1,ifelse(Shift == "Morn",2,ifelse(Shift == "Eve",3,"NA"))))
cor.test(crimedata$offense_id,as.numeric(crimedata$Shift_code), method = "spearman", use = "complete.obs")
cor(crimedata$offense_id, crimedata$beat,  method = "spearman", use = "complete.obs")
crimedata <- mutate(crimedata, Day_code = ifelse(Avg.Day == "Sun",1,ifelse(Avg.Day == "Mon",2,ifelse(Avg.Day == "Tue",3, ifelse(Avg.Day == "Wed",4, ifelse(Avg.Day == "Thu",5,ifelse(Avg.Day == "Fri", 6, ifelse (Avg.Day == "Sat", 7, "NA"))))))))
numcrime <- cbind(crimedata[,c(1,2,8,12,15,18,22,23,24,25,26,27)])
numcrime$MI_PRINX <- as.numeric(numcrime$MI_PRINX)
numcrime$beat <- as.numeric(numcrime$beat)
numcrime$MinOfucr <- as.numeric(numcrime$MinOfucr)
numcrime$loc_type <- as.numeric(numcrime$loc_type)
numcrime$MaxOfnum_victims <- as.numeric(numcrime$MaxOfnum_victims)
numcrime$occur_day <- as.numeric(numcrime$occur_day)
numcrime$Shift_code <- as.numeric(numcrime$Shift_code)
numcrime$Day_code <- as.numeric(numcrime$Day_code)
cor(numcrime, method = "pearson", use= "complete.obs")
cor.test(crimedata$offense_id, as.numeric(crimedata$occur_time), method = "spearman", use = "complete.obs")
cor.test(crimedata$offense_id, as.numeric(crimedata$beat), method = "spearman", use = "complete.obs")
cor.test(crimedata$offense_id, as.numeric(crimedata$occur_day), method = "spearman", use = "complete.obs")
cor.test(crimedata$offense_id, as.numeric(crimedata$occur_month), method = "spearman", use = "complete.obs")
cor.test(crimedata$offense_id, as.numeric(crimedata$Day_code), method = "spearman", use = "complete.obs")
cor.test(crimedata$offense_id, as.numeric(crimedata$Shift_code), method = "spearman", use = "complete.obs")
DayCrime <- as.matrix(table(crimedata$CrimeType,crimedata$occur_day))
DayCrime <- DayCrime[-c(5,8,9,11),]
chisq.test(DayCrime)
MonthCrime <- as.matrix(table(crimedata$CrimeType,crimedata$occur_month))
MonthCrime <- MonthCrime[-c(5,9),]
chisq.test(MonthCrime)


