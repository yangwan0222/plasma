################### Update available ####################
library(Epi)
library(lubridate)

d <- read.csv("C://Users//wanya//Desktop//update available+//ALIQUOT_LIST_02_13_22.csv")[,c(1:6,14)]
d <- d[d$PLAVOL!=0,]
d[d$CSFVOL != 0,]$CSFVOL <- "Yes"
d[d$CSFVOL == 0,]$CSFVOL <- NA
names(d)[7] <- "CSF"
names(d)[5] <- "PLA_AVAILABLE"
t <- as.data.frame(table(d$RID))
names(t) <- c("RID","LONGITUDINAL_N")
d <- merge(d,t,by="RID",all.x = TRUE)

d1 <- read.csv("C://Users//wanya//Desktop//update available+//results.csv")
names(d1) <- c("RID","Phase","EXAMDATE")
d1 <- unique(d1)
d1$EXAMDATE <- dmy(d1$EXAMDATE)
d <- merge(d,d1,by=c("RID","EXAMDATE"))

# ApoE - Results [ADNI1,GO,2,3]
apoe <- read.csv("C://Users//wanya//Desktop//update available+//ApoE.csv")
apoe <- apoe[,c(3,9,10)]
d <- merge(d,apoe,by="RID",all.x = TRUE)

# University of Gothenburg Longitudinal Plasma P-tau181 [ADNI1,GO,2]
ptau <- read.csv("C://Users//wanya//Desktop//update available+//PLASMAPTAU181.csv")[,c(1,4,6)]
ptau$EXAMDATE <- ymd(ptau$EXAMDATE)
d$EXAMDATE <- mdy(d$EXAMDATE)

d$PLASMAPTAU181 <- NA
for(i in seq(nrow(d)))
{
  t <- ptau[ptau$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    next
  } 
  t$diff <- abs(t$EXAMDATE-d$EXAMDATE[i])
  if(min(t$diff)>=90)
  {
    next
  } 
  t <- t[t$diff == min(t$diff),]
  d$PLASMAPTAU181[i] <- t$PLASMAPTAU181[1]
}

# Subject Demographics [ADNI1,GO,2,3]
d1 <- read.csv("C://Users//wanya//Desktop//update available+//demographics.csv")[,c("RID","PTDOBMM","PTDOBYY","PTGENDER","PTHAND","PTMARRY","PTEDUCAT","PTETHCAT","PTRACCAT","PTPLANG","PTHOME","PTWORK")]
d1 = d1[complete.cases(d1),]
d1 = d1[match(unique(d1$RID), d1$RID),]
names(d1) = c("RID","DOB","AGE","GENDER","HAND","MARRY","EDU","ETHNIC","RACE","LANG","HOME","WORK")
d1$GENDER = factor(d1$GENDER, levels=1:2, labels=c("Male","Female"))
d1$HAND   = factor(d1$HAND,   levels=1:2, labels=c("Right","Left"))
d1$MARRY  = factor(d1$MARRY,  levels=1:5, labels=c("Married","Widowed","Divorced","Never","Unknown"))
d1$HOME   = factor(d1$HOME,   levels=1:8, labels=c("House","Condo","Apartment","Mobile","Retirement Cmty","Assisted Lvng","Nursing","Other"))
d1$LANG   = factor(d1$LANG,   levels=1:3, labels=c("English","Spanish","Other"))    
d1$ETHNIC = factor(d1$ETHNIC, levels=1:3, labels=c("Latino","Not Latino","Unknown"))
d1$RACE   = factor(d1$RACE,   levels=1:7, labels=c("Native American","Asian","Pacific","Black","White","More than one","Unknown"))

d1 <- d1[,c(1,3,4,7,9)]
d <- merge(d,d1,by="RID",all.x = TRUE)

# UC Berkeley - AV45 Analysis [ADNIGO,2,3]
d1 <- read.csv("C://Users//wanya//Desktop//update available+//FBP.csv")[,c(1,4,19)]
names(d1)[3] <- "FBP"
d1$EXAMDATE <- ymd(d1$EXAMDATE)

d$FBP <- NA
for(i in seq(nrow(d)))
{
  t <- d1[d1$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    d$FBP[i] <- NA
    next
  } 
  t$diff <- abs(t$EXAMDATE-d$EXAMDATE[i])
  if(min(t$diff)>=90)
  {
    next
  } 
  t <- t[t$diff == min(t$diff),]
  d$FBP[i] <- t$FBP[1]
}

# Diagnostic Summary [ADNI1,GO,2,3]
d1 = read.csv("C://Users//wanya//Desktop//update available+//DX.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]   
d1$DXCURREN[is.na(d1$DXCURREN)] = d1$DXCHANGE[is.na(d1$DXCURREN)]
d1$DXCURREN[is.na(d1$DXCURREN)] = d1$DIAGNOSIS[is.na(d1$DXCURREN)]
d1$DX= Relevel(factor(d1$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d1 <- d1[,c(2,4,9)]
d1$EXAMDATE <- ymd(d1$EXAMDATE)
d1$DX <- as.character(d1$DX)

d$DX <- NA
for(i in seq(nrow(d)))
{
  t <- d1[d1$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    next
  } 
  t$diff <- abs(t$EXAMDATE-d$EXAMDATE[i])
  t <- na.omit(t)
  if(min(t$diff)>=90)
  {
    next
  } 
  t <- t[t$diff == min(t$diff),]
  d$DX[i] <- t$DX[1]
}

# Arm [ADNI1,GO,2]
d1 = read.csv("C://Users//wanya//Desktop//update available+//ARM.csv")[,c("RID","USERDATE","ARM")]
d1$ARM = Relevel(factor(d1$ARM), list(NL=c(1,4,7), SMC=c(11), EMCI=c(10), MCI=c(2,5,8), AD=c(3,6,9)))
d1$USERDATE <- ymd(d1$USERDATE)
names(d1)[2] <- "EXAMDATE"
d1$ARM <- as.character(d1$ARM)

d$ARM <- NA
for(i in seq(nrow(d)))
{
  t <- d1[d1$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    next
  } 
  t$diff <- abs(t$EXAMDATE-d$EXAMDATE[i])
  t <- na.omit(t)
  if(min(t$diff)>=180)
  {
    next
  } 
  t <- t[t$diff == min(t$diff),]
  d$ARM[i] <- t$ARM[1]
}

# UC Berkeley - AV1451 PVC Analysis [ADNI2,3]
d1 <- read.csv("C://Users//wanya//Desktop//update available+//TAUPET.csv")[,c(1,4)]
d1$EXAMDATE <- ymd(d1$EXAMDATE)
d1 <- na.omit(d1)

d$TAUPET <- NA
for(i in seq(nrow(d)))
{
  t <- d1[d1$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    next
  } 
  t$diff <- abs(t$EXAMDATE-d$EXAMDATE[i])
  t <- na.omit(t)
  if(min(t$diff)>=90)
  {
    next
  } 
  d$TAUPET[i] <- "Yes"
}

# Bateman Lab Plasma Abeta42/Abeta40 Ratio as a Predictor of Brain Amyloidosis [ADNIGO,2]
d1 <- read.csv("C://Users//wanya//Desktop//update available+//BAT.csv")[,c(1,4)]
d1$EXAMDATE <- ymd(d1$EXAMDATE)
d1 <- na.omit(d1)

d$BAT <- NA
for(i in seq(nrow(d)))
{
  t <- d1[d1$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    next
  } 
  t$diff <- abs(t$EXAMDATE-d$EXAMDATE[i])
  t <- na.omit(t)
  if(min(t$diff)>=90)
  {
    next
  } 
  d$BAT[i] <- "Bateman17"
}

# Bateman Lab Plasma Abeta42/Abeta40 Ratio as a Predictor of Brain Amyloidosis [ADNI1,2,GO]
d1 <- read.csv("C://Users//wanya//Desktop//update available+//BAT3.csv")[,c(1,4)]
d1$EXAMDATE <- mdy(d1$EXAMDATE)
d1 <- na.omit(d1)

d$BAT3 <- NA
for(i in seq(nrow(d)))
{
  t <- d1[d1$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    next
  } 
  t$diff <- abs(t$EXAMDATE-d$EXAMDATE[i])
  t <- na.omit(t)
  if(min(t$diff)>=90)
  {
    next
  } 
  d$BAT3[i] <- "Bateman19"
}

# Mayo (Jack Lab) - ADNI MRI MCH [ADNIGO,2,3]
d1 <- read.csv("C://Users//wanya//Desktop//update available+//MRI.csv")[,c(3,6,20)]
d1 <- d1[d1$NOFINDINGS == 0,]
d1 <- na.omit(d1)
names(d1)[2] <- "EXAMDATE"
d1$EXAMDATE <- ymd(d1$EXAMDATE)

d$MRI <- NA
for(i in seq(nrow(d)))
{
  t <- d1[d1$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    next
  } 
  t$diff <- abs(t$EXAMDATE-d$EXAMDATE[i])
  t <- na.omit(t)
  if(min(t$diff)>=90)
  {
    next
  } 
  d$MRI[i] <- "Yes"
}

######################################################

d <- d[d$PLA_AVAILABLE!=0,]
x <- d

x1 <- d[!is.na(d$FBP),]
t1 <- data.frame(table(x1$RID))
names(t1) <- c("RID","FBP_Freq")
x1 <- merge(x1,t1,by="RID",all.x = TRUE)
x1 <- x1[!is.na(x1$FBP_Freq) & x1$FBP_Freq >= 3,]

x2 <- x1[!is.na(x1$TAUPET),]
t2 <- data.frame(table(x2$RID))
names(t2) <- c("RID","TAUPET_Freq")
x1 <- merge(x1,t2,by="RID",all.x = TRUE)
x1 <- x1[!is.na(x1$TAUPET_Freq),]

x <- x1
x <- x[order(x[,1], x[,2]),]

x_3 <- x[x$FBP_Freq==3,]
x_3 <- x_3[order(x_3[,1], x_3[,2]),]

x_4 <- x[x$FBP_Freq==4,]
x_4 <- x_4[order(x_4[,1], x_4[,2]),]
x_4 <- rbind(x_4[1:2,],x_4)
x_4 <- x_4[-(seq(4,to=nrow(x_4),by=4)),]
x_4 <- x_4[-c(1,2),]

x_5 <- x[x$FBP_Freq==5,]
x_5 <- x_5[order(x_5[,1], x_5[,2]),]
x_5 <- rbind(x_5[1:3,],x_5)
x_5 <- x_5[-(seq(5,to=nrow(x_5),by=5)),]
x_5 <- x_5[-c(1:3),]
x_5 <- rbind(x_5[1,],x_5)
x_5 <- x_5[-(seq(4,to=nrow(x_5),by=4)),]
x_5 <- x_5[-c(1),]

x_6 <- x[x$FBP_Freq==6,]
x_6 <- x_6[order(x_6[,1], x_6[,2]),]
x_6 <- rbind(x_6[1:4,],x_6)
x_6 <- x_6[-(seq(6,to=nrow(x_6),by=6)),]
x_6 <- x_6[-c(1:4),]
x_6 <- rbind(x_6[1:2,],x_6)
x_6 <- x_6[-(seq(5,to=nrow(x_6),by=5)),]
x_6 <- x_6[-c(1:2),]
x_6 <- rbind(x_6[1,],x_6)
x_6 <- x_6[-(seq(4,to=nrow(x_6),by=4)),]
x_6 <- x_6[-c(1),]

write.csv(x,"C://Users//wanya//Desktop//update available+//3minFBP_1minTAUPET.csv")
write.csv(rbind(x_3,x_4,x_5,x_6),"C://Users//wanya//Desktop//update available+//3samplesForEach.csv")

