# Final_CS112
### WISCONSIN 2008 EXPERIMENT
### Dan Hopkins
### Replication code: 2/24/2016

install.packages("arm")
install.packages("texreg")
install.packages("mice")
install.packages("lattice")
install.packages("Zelig")
install.packages("survival")
install.packages("sampleSelection")
install.packages("maxLik")
install.packages("xtable")
install.packages("randomForest")
install.packages("apsrtable")
install.packages("rgenoud")


### Load libraries
library(foreign)
library(xtable)
library(texreg)
library(mice)
library(Zelig)
library(MASS)
library(sampleSelection)
library(arm)
library(Matching)
library(randomForest)
library(apsrtable)
library(rgenoud)

### Set working directory
setwd("~/Downloads")

load("unpersuaded.RData")
### load data set (with proprietary variables)
dta <- dta.sub

#selecting turn out score and assigning a variable to it 
dta$turnout.score.c <- NA
dta$turnout.score.c[dta$turnout.score==0] <- 0
dta$turnout.score.c[dta$turnout.score==1/9] <- 1
dta$turnout.score.c[dta$turnout.score==2/9] <- 2
dta$turnout.score.c[dta$turnout.score==3/9] <- 3
dta$turnout.score.c[dta$turnout.score==4/9] <- 4
dta$turnout.score.c[dta$turnout.score==5/9] <- 5
dta$turnout.score.c[dta$turnout.score==6/9] <- 6
dta$turnout.score.c[dta$turnout.score==7/9] <- 7
dta$turnout.score.c[dta$turnout.score==8/9] <- 8
dta$turnout.score.c[dta$turnout.score==9/9] <- 9

dta$treat000 <- 0
dta$treat000[dta$treat=="c0m0p0"] <- 1

dta$treat001 <- 0
dta$treat001[dta$treat=="c0m0p1"] <- 1

dta$treat100 <- 0
dta$treat100[dta$treat=="c1m0p0"] <- 1

dta$treat101 <- 0
dta$treat101[dta$treat=="c1m0p1"] <- 1

dta$treat010 <- 0
dta$treat010[dta$treat=="c0m1p0"] <- 1

dta$treat011 <- 0
dta$treat011[dta$treat=="c0m1p1"] <- 1

dta$treat110 <- 0
dta$treat110[dta$treat=="c1m1p0"] <- 1

dta$treat111 <- 0
dta$treat111[dta$treat=="c1m1p1"] <- 1

#### CREATE INDICATOR VARIABLES
for(i in 1:9){
  
  txt1 <- paste("dta$turnout.score.",i,"<- 0",sep="")
  eval(parse(text=txt1))
  txt15 <- paste("dta$turnout.score.",i,"<- 1*(dta$turnout.score.c==",i,")",sep="")
  eval(parse(text=txt15))
  txt2 <- paste("dta$canvass.i.score.",i,"<- 0",sep="")
  eval(parse(text=txt2))
  txt16 <- paste("dta$canvass.i.score.",i,"<- 1*(dta$turnout.score.c==",i," & (dta$canvass==1))",sep="")
  eval(parse(text=txt16))
  txt3 <- paste("dta$phonecall.i.score.",i,"<- 0",sep="")
  eval(parse(text=txt3))
  txt17 <- paste("dta$phonecall.i.score.",i,"<- 1*(dta$turnout.score.c==",i,"& (dta$phonecall==1))",sep="")
  eval(parse(text=txt17))	
  
}



dta.sv20 <- dta[! dta$q_phonematchscore=="",]

#### subset variables for public availability
dta.sub <- subset(dta,select=c("survey","svy_result","obama","canvass","phonecall","mail","black","hispanic","turnout.score.c",
                               "male","protestant","catholic","q_age","vh_02g","vh_04p","vh_04g","vh_06p","vh_06g","vh_08p","vh_08g","q_phonematchscore"))

save(dta.sub,file="wi-08-public-data.Rdata")


##### subset by survey results
dta.sv <- dta[! dta$dv_sen %in% c(NA),]
dta.sv2 <- dta[! dta$svy_result %in% c(""),]
dta.sv3 <- dta[! dta$svy_result %in% c("","20 DeclinedToParticipate"),]
dta.sv4 <- dta[! dta$svy_result %in% c("","20 DeclinedToParticipate","24 Already voted"),]
dta.sv5 <- dta[! dta$svy_result %in% c("","21 Do not call","92 Invalid","90 Not in service","86 Tri-Tone","35 Privacy Manager"),]
dta.sv6 <- dta[! dta$svy_result %in% c("80 Wrong number","31 Language barrier","32 Deceased","","21 Do not call","92 Invalid","90 Not in service","86 Tri-Tone","35 Privacy Manager"),]
dta.sv7 <- dta[! dta$svy_result %in% c("20 DeclinedToParticipate","80 Wrong number","31 Language barrier","32 Deceased","","21 Do not call","92 Invalid","90 Not in service","86 Tri-Tone","35 Privacy Manager"),]
dta.sv8 <- dta[! dta$svy_result %in% c("24 Already voted","20 DeclinedToParticipate","80 Wrong number","31 Language barrier","32 Deceased","","21 Do not call","92 Invalid","90 Not in service","86 Tri-Tone","35 Privacy Manager"),]
dta.sv9 <- dta[! dta$svy_result %in% c("30 Early hangup","24 Already voted","20 DeclinedToParticipate","80 Wrong number","31 Language barrier","32 Deceased","","21 Do not call","92 Invalid","90 Not in service","86 Tri-Tone","35 Privacy Manager"),]
dta.sv10 <- dta[! dta$svy_result %in% c("04 Refused","30 Early hangup","24 Already voted","20 DeclinedToParticipate","80 Wrong number","31 Language barrier","32 Deceased","","21 Do not call","92 Invalid","90 Not in service","86 Tri-Tone","35 Privacy Manager"),]
dta.sv10a <- dta[dta$svy_result %in% c("04 Refused"),]

dta.sv11 <- dta[! dta$obama %in% c(NA),]

dta.sv5 <- dta[! dta$svy_result %in% c("","21 Do not call","92 Invalid","90 Not in service","86 Tri-Tone","35 Privacy Manager"),]

ivs <- c("q_age","black","male","hispanic","vh_02g","vh_04p","vh_04g","vh_06p","vh_06g","vh_08p","turnout.score.c","q_supportscore","catholic","protestant","q_pres04d","ncec_dem_perf","q_medianhhincomet","q_percentsingleparents","q_percentinpoverty","q_percentcollegegrads","q_percenthomeowners","q_percenturban","q_percentwhitecollar","q_percentunemployed","q_percenthispanic","q_percentasian","q_percentafricanamerican","q_percent65andolder")

rmatc <- matrix(NA,length(ivs),8)
rownames(rmatc) <- ivs
colnames(rmatc) <- c("Mean, C=1","Mean, C=0","P-value","Mean, C=1","Mean, C=2","P-value","N","N.survey")

for(i in 1:length(ivs)){
  txt <- paste("tout <- t.test(dta$",ivs[i],"[dta$canvass==1],dta$",ivs[i],"[dta$canvass==0])",sep="")
  eval(parse(text=txt))
  rmatc[i,1] <- tout$estimate[1]
  rmatc[i,2] <- tout$estimate[2]
  rmatc[i,3] <- tout$p.value
  
  txt <- paste("tout2 <- t.test(dta.sv11$",ivs[i],"[dta.sv11$canvass==1],dta.sv11$",ivs[i],"[dta.sv11$canvass==0])",sep="")
  eval(parse(text=txt))
  
  rmatc[i,4] <- tout2$estimate[1]
  rmatc[i,5] <- tout2$estimate[2]
  rmatc[i,6] <- tout2$p.value
  
  txt3 <- paste("hold <- length(na.omit(dta$",ivs[i],"))",sep="")
  eval(parse(text=txt3))
  rmatc[i,7] <- hold
  
  txt4 <- paste("hold <- length(na.omit(dta.sv11$",ivs[i],"))",sep="")
  eval(parse(text=txt4))
  rmatc[i,8] <- hold
  
  
}
round(rmatc,digits=3)
library(xtable)

### appendix table A2
xtable(rmatc[,c(1:3,7)],digits=c(0,rep(3,3),0))

### table 1
xtable(rmatc[,c(4:6,8)],digits=c(0,3,3,3,0))


#### appendix table A2, balance in random assignment

ivs <- c("q_age","black","male","hispanic","vh_02g","vh_04p","vh_04g","vh_06p","vh_06g","vh_08p","turnout.score.c","q_supportscore","catholic","protestant","q_pres04d","ncec_dem_perf","q_medianhhincomet","q_percentsingleparents","q_percentinpoverty","q_percentcollegegrads","q_percenthomeowners","q_percenturban","q_percentwhitecollar","q_percentunemployed","q_percenthispanic","q_percentasian","q_percentafricanamerican","q_percent65andolder")

rmatc <- matrix(NA,length(ivs),8)
rownames(rmatc) <- ivs
colnames(rmatc) <- c("Mean, C=1","Mean, C=0","P-value","Mean, C=1","Mean, C=2","P-value","N","N.survey")

for(i in 1:length(ivs)){
  txt <- paste("tout <- t.test(dta$",ivs[i],"[dta$canvass==1],dta$",ivs[i],"[dta$canvass==0])",sep="")
  eval(parse(text=txt))
  rmatc[i,1] <- tout$estimate[1]
  rmatc[i,2] <- tout$estimate[2]
  rmatc[i,3] <- tout$p.value
  
  txt <- paste("tout2 <- t.test(dta.sv11$",ivs[i],"[dta.sv11$canvass==1],dta.sv11$",ivs[i],"[dta.sv11$canvass==0])",sep="")
  eval(parse(text=txt))
  
  rmatc[i,4] <- tout2$estimate[1]
  rmatc[i,5] <- tout2$estimate[2]
  rmatc[i,6] <- tout2$p.value
  
  txt3 <- paste("hold <- length(na.omit(dta$",ivs[i],"))",sep="")
  eval(parse(text=txt3))
  rmatc[i,7] <- hold
  
  txt4 <- paste("hold <- length(na.omit(dta.sv11$",ivs[i],"))",sep="")
  eval(parse(text=txt4))
  rmatc[i,8] <- hold
  
  
}
round(rmatc,digits=3)
library(xtable)

### Appendix Table A2
xtable(rmatc[,c(1:3,7)],digits=c(0,rep(3,3),0))
#xtable(rmatc[,c(4:6,8)],digits=c(0,3,3,3,0))

### overall response rate by canvassing assignment
table(dta$survey,dta$canvass)

###### appendix table A3, phone treatment and mail treatment
####

ivs <- c("q_age","black","male","hispanic","vh_02g","vh_04p","vh_04g","vh_06p","vh_06g","vh_08p","turnout.score.c","q_supportscore","catholic","protestant","q_pres04d","ncec_dem_perf","q_medianhhincomet","q_percentsingleparents","q_percentinpoverty","q_percentcollegegrads","q_percenthomeowners","q_percenturban","q_percentwhitecollar","q_percentunemployed","q_percenthispanic","q_percentasian","q_percentafricanamerican","q_percent65andolder")

rmatp <- matrix(NA,length(ivs),7)
rownames(rmatp) <- ivs
colnames(rmatp) <- c("Mean, P=1","Mean, P=0","P-value","Mean, M=1","Mean, M=0","P-value","N")

for(i in 1:length(ivs)){
  txt <- paste("tout <- t.test(dta.sv11$",ivs[i],"[dta.sv11$phonecall==1],dta.sv11$",ivs[i],"[dta.sv11$phonecall==0])",sep="")
  eval(parse(text=txt))
  rmatp[i,1] <- tout$estimate[1]
  rmatp[i,2] <- tout$estimate[2]
  rmatp[i,3] <- tout$p.value
  
  txt <- paste("tout2 <- t.test(dta.sv11$",ivs[i],"[dta.sv11$mail==1],dta.sv11$",ivs[i],"[dta.sv11$mail==0])",sep="")
  eval(parse(text=txt))
  
  rmatp[i,4] <- tout2$estimate[1]
  rmatp[i,5] <- tout2$estimate[2]
  rmatp[i,6] <- tout2$p.value
  
  txt3 <- paste("hold <- length(na.omit(dta.sv11$",ivs[i],"))",sep="")
  eval(parse(text=txt3))
  rmatp[i,7] <- hold
  
}
round(rmatp,digits=3)

#### Appendix Table A3
xtable(rmatp[,1:6],digits=c(0,rep(3,6)))


#### Table 2
dataset <- c("dta","dta.sv2","dta.sv5","dta.sv7","dta.sv11")

rmat30 <- rmat20 <- rmat10 <- matrix(NA,length(dataset),5)

for(i in 1:length(dataset)){
  
  txt1 <- paste("dta.sub <- ",dataset[i])
  eval(parse(text=txt1))
  tout1 <- t.test(dta.sub$turnout.score[dta.sub$canvass==1],dta.sub$turnout.score[dta.sub$canvass==0])
  
  rmat10[i,1] <- tout1$estimate[1]
  rmat10[i,2] <- tout1$estimate[2]
  rmat10[i,3] <- tout1$estimate[1]-tout1$estimate[2]
  rmat10[i,4] <- tout1$p.value
  rmat10[i,5] <- dim(dta.sub)[1]
  
  tout2 <- t.test(dta.sub$turnout.score[dta.sub$phonecall==1],dta.sub$turnout.score[dta.sub$phonecall==0])
  
  rmat20[i,1] <- tout2$estimate[1]
  rmat20[i,2] <- tout2$estimate[2]
  rmat20[i,3] <- tout2$estimate[1]-tout2$estimate[2]
  rmat20[i,4] <- tout2$p.value
  rmat20[i,5] <- dim(dta.sub)[1]
  
  tout3 <- t.test(dta.sub$turnout.score[dta.sub$mail==1],dta.sub$turnout.score[dta.sub$mail==0])
  
  rmat30[i,1] <- tout3$estimate[1]
  rmat30[i,2] <- tout3$estimate[2]
  rmat30[i,3] <- tout3$estimate[1]-tout3$estimate[2]
  rmat30[i,4] <- tout3$p.value
  rmat30[i,5] <- dim(dta.sub)[1]
}
library(xtable)

### stages of non-response: canvass
xtable(rmat10,digits=c(0,3,3,3,3,0))

xtable(rmat20,digits=c(0,3,3,3,3,0))
xtable(rmat30,digits=c(0,3,3,3,3,0))

#### FIGURE 1

ConfIntervals    = matrix(NA, 10, 4)
for(ii in 1:length(unique(dta$turnout.score.c))){
  turn <- sort(unique(dta$turnout.score.c))[ii]
  zzx     = lm(dta$survey[dta$turnout.score.c==turn]~dta$canvass[dta$turnout.score.c==turn])
  summary(zzx)
  ConfIntervals[ii, 1]         = coefficients(zzx)[2]
  ConfIntervals[ii, 2:3]     = confint(zzx)[2, 1:2]
  ConfIntervals[ii, 4]         = length(resid(zzx))
}
plot(0:9, c(min(ConfIntervals[,2:3]), max(ConfIntervals[,2:3]), rep(0, 8)), type="n", xlab = "", ylab = "", xaxt='n', yaxt='n')
axis(1, at = seq(0, 9,by=1), labels =  seq(0, 9,by=1), tick = T,
     cex.axis = .8, mgp = c(2,.4,0))
axis(2, tick = T, cex.axis = .8, mgp = c(2,.7,0))
mtext("Prior turnout level",             side = 1, line = 1.7, cex = 0.9)
mtext("Effect of canvass on survey response rate",     side = 2, line = 2.2, cex = 0.9)
for(ii in 1:length(unique(dta$turnout.score.c))){
  lines(c(ii-1, ii-1), ConfIntervals[ii,2:3])
  lines(c(ii-1.06, ii-0.94), c(ConfIntervals[ii,2], ConfIntervals[ii,2]))
  lines(c(ii-1.06, ii-0.94), c(ConfIntervals[ii,3], ConfIntervals[ii,3]))
  abline(h = 0, lty = 3, col = "grey") # add horiontal line
  #points(ii-1, ConfIntervals[ii,1], pch = 16)
  points(ii-1, ConfIntervals[ii,1], pch = 16, cex = 15*ConfIntervals[ii,4]/sum(ConfIntervals[,4]))
}




### Table 3

all.sub <- lm(vh_08g ~ canvass + phonecall+mail,data=dta)
svy.sub <- lm(vh_08g ~ canvass + phonecall+mail,data=dta.sv11)

texreg(list(all.sub,svy.sub),digits=3,stars=0.05)


###
### multiple imputation

interactions <- c("canvass.i.score.1","phonecall.i.score.1","canvass.i.score.2", "phonecall.i.score.2","canvass.i.score.3","phonecall.i.score.3","canvass.i.score.4","phonecall.i.score.4","canvass.i.score.5","phonecall.i.score.5","canvass.i.score.6","phonecall.i.score.6","canvass.i.score.7","phonecall.i.score.7","canvass.i.score.8","phonecall.i.score.8","canvass.i.score.9","phonecall.i.score.9")

cn <- c("obama","mccain","canvass","phonecall","mail","q_supportscore","turnout.score.c","male","q_age","ncec_dem_perf","black","hispanic","protestant","catholic","q_medianhhincome","q_percentcollegegrads")
dta.sub <- subset(dta,select=c(cn,interactions))


m1 <- mice(dta.sub)

load("mi-dta-03012016.Rdata")

n1 <- complete(m1,action=1)
n2 <- complete(m1,action=2)
n3 <- complete(m1,action=3)
n4 <- complete(m1,action=4)
n5 <- complete(m1,action=5)

n1$canvass <- n2$canvass <- n3$canvass <- n4$canvass <- n5$canvass <- dta$canvass
n1$phonecall <- n2$phonecall <- n3$phonecall <- n4$phonecall <- n5$phonecall <- dta$phonecall
n1$mail <- n2$mail <- n3$mail <- n4$mail <- n5$mail <- dta$mail

pro.vec <- table(n1$turnout.score.c)/dim(n1)[1]
pro.zero <- rep(0,10)


##########################
#### check imputation ####
##########################

n.sims <- 5
rmat <- matrix(NA,n.sims,1)
set.seed(20007)
for(i in 1:n.sims){
  
  idx <- which(! dta$obama %in% c(NA))
  remove.obs <- sample(idx,size=500,replace=F)
  
  dta.hold <- dta
  dta.hold$obama[remove.obs] <- NA
  
  interactions <- c("canvass.i.score.1","phonecall.i.score.1","canvass.i.score.2", "phonecall.i.score.2","canvass.i.score.3","phonecall.i.score.3","canvass.i.score.4","phonecall.i.score.4","canvass.i.score.5","phonecall.i.score.5","canvass.i.score.6","phonecall.i.score.6","canvass.i.score.7","phonecall.i.score.7","canvass.i.score.8","phonecall.i.score.8","canvass.i.score.9","phonecall.i.score.9")
  
  cn <- c("obama","mccain","canvass","phonecall","mail","q_supportscore","turnout.score.c","male","q_age","ncec_dem_perf","black","hispanic","protestant","catholic","q_medianhhincome","q_percentcollegegrads")
  dta.sub <- subset(dta.hold,select=c(cn,interactions))
  
  m1.test <- mice(dta.sub)
  
  n1t <- complete(m1.test,action=1)
  n2t <- complete(m1.test,action=2)
  n3t <- complete(m1.test,action=3)
  n4t <- complete(m1.test,action=4)
  n5t <- complete(m1.test,action=5)
  
  m1 <- sum(diag(table(n1t$obama[remove.obs],dta$obama[remove.obs])))/500
  m2 <- sum(diag(table(n2t$obama[remove.obs],dta$obama[remove.obs])))/500
  m3 <- sum(diag(table(n3t$obama[remove.obs],dta$obama[remove.obs])))/500
  m4 <- sum(diag(table(n4t$obama[remove.obs],dta$obama[remove.obs])))/500
  m5 <- sum(diag(table(n5t$obama[remove.obs],dta$obama[remove.obs])))/500
  
  rmat[i,1] <- mean(c(m1,m2,m3,m4,m5))
}

#### remove phonematch

n1p <- n1[! dta$q_phonematch=="",] 
n2p <- n2[! dta$q_phonematch=="",] 
n3p <- n3[! dta$q_phonematch=="",] 
n4p <- n4[! dta$q_phonematch=="",] 
n5p <- n5[! dta$q_phonematch=="",] 


pro.vec.all <- table(n1$turnout.score.c)/dim(n1)[1]
pro.vec <- pro.vec.all[2:10]

##### low-turnout respondents

n1p <- n1[ dta$turnout.score.c < 3,] 
n2p <- n2[ dta$turnout.score.c < 3,] 
n3p <- n3[ dta$turnout.score.c < 3,] 
n4p <- n4[ dta$turnout.score.c < 3,] 
n5p <- n5[ dta$turnout.score.c < 3,] 


#####
#####

dta.sv5$WeakMatch <- 1*(dta.sv5$q_phonematchscore %in% c("Weak Match","Restricted Number - Weak Match"))
dta.sv5$MedMatch <- 1*(dta.sv5$q_phonematchscore %in% c("Medium Match","Restricted Number - Medium Match"))
dta.sv5$StrongMatch <- 1*(dta.sv5$q_phonematchscore %in% c("Restricted Number - Strong Match","Strong Match"))

# Estimate non-parametric selection model
# First generate propensity

PropensProbit = glm(survey ~ WeakMatch + MedMatch + StrongMatch + male + q_age + black + hispanic + protestant + catholic +
                      canvass.i.score.1 + canvass.i.score.2 + canvass.i.score.3 + canvass.i.score.4 + canvass.i.score.5 +
                      canvass.i.score.6 + canvass.i.score.7 + canvass.i.score.8 + canvass.i.score.9 + phonecall.i.score.1 + phonecall.i.score.2 + 
                      phonecall.i.score.3 + phonecall.i.score.4 + phonecall.i.score.5  + phonecall.i.score.6 + phonecall.i.score.7 + phonecall.i.score.8 + phonecall.i.score.9 +
                      canvass + phonecall + mail, na.action = na.exclude,
                      data = dta.sv5, family = binomial(link = "probit"))

summary(PropensProbit)
rf <- randomForest(survey ~ WeakMatch + MedMatch + StrongMatch + male + q_age + black + hispanic + protestant + catholic +
                     canvass.i.score.1 + canvass.i.score.2 + canvass.i.score.3 + canvass.i.score.4 + canvass.i.score.5 +
                     canvass.i.score.6 + canvass.i.score.7 + canvass.i.score.8 + canvass.i.score.9 + phonecall.i.score.1 + phonecall.i.score.2 + 
                     phonecall.i.score.3 + phonecall.i.score.4 + phonecall.i.score.5  + phonecall.i.score.6 + phonecall.i.score.7 + phonecall.i.score.8 + phonecall.i.score.9 +
                     canvass + phonecall + mail, na.action = na.exclude, data= dta.sv5, mtry=6, importance = TRUE)
importance(rf)
varImpPlot(rf)


summary(PropensProbit)
dta.sv5$Propensity  <- fitted(PropensProbit)        # includes NA's b/c of na.action = na.exclude command
dta.sv5$PropensitySq  <- fitted(PropensProbit )^2

ObamaWithPropensityNoIntera= lm(obama ~ male + q_age + black + hispanic + protestant + catholic + Propensity + PropensitySq +
                                  canvass + phonecall + mail, data = dta.sv5, na.action = na.exclude )       # turnout.score.c +


summary(ObamaWithPropensityNoIntera)

rf <- randomForest(obama ~ male + q_age + black + hispanic + protestant + catholic + Propensity + PropensitySq +
                     canvass + phonecall + mail, na.action = na.exclude, data= dta.sv5, mtry=6, importance = TRUE)
importance(rf)
varImpPlot(rf)

ObamaWithPropensityNonVotersNoIntera= lm(obama ~ male + q_age + black + hispanic + protestant + catholic +
                                          Propensity + PropensitySq + canvass + phonecall + mail, 
                                          data = dta.sv5, na.action = na.exclude )     # turnout.score.c +

summary(ObamaWithPropensityNonVotersNoIntera)

rf <- randomForest(obama ~ male + q_age + black + hispanic + protestant + catholic +
                     Propensity + PropensitySq + canvass + phonecall + mail, na.action = na.exclude, data= dta.sv5, mtry=6, importance = TRUE)
importance(rf)
varImpPlot(rf)

#apsrtable(ObamaWithPropensityNoIntera, ObamaWithPropensityNonVotersNoIntera, digits=4, stars = "default" )
apsrtable(ObamaWithPropensityNoIntera, ObamaWithPropensityNonVotersNoIntera, digits=4, stars =c(0.05) )


## Heckman selection model
dta.sv5b <- dta[! dta$svy_result %in% c("21 Do not call","92 Invalid","90 Not in service","86 Tri-Tone","35 Privacy Manager"),]

dta.sv5b$WeakMatch <- 1*(dta.sv5b$q_phonematchscore %in% c("Weak Match","Restricted Number - Weak Match"))
dta.sv5b$MedMatch <- 1*(dta.sv5b$q_phonematchscore %in% c("Medium Match","Restricted Number - Medium Match"))
dta.sv5b$StrongMatch <- 1*(dta.sv5b$q_phonematchscore %in% c("Restricted Number - Strong Match","Strong Match"))


HeckResults1MLE <- selection(survey ~ canvass + phonecall + mail + WeakMatch + MedMatch + StrongMatch ,
                             obama ~ canvass + phonecall + mail , data = dta.sv5b, method = "ml")    #dta.sv5 OR dta
summary(HeckResults1MLE)

HeckResults1a <- selection(survey ~ canvass + phonecall + mail + WeakMatch + MedMatch + StrongMatch + male + black + hispanic,
                           obama ~ canvass + phonecall + mail + male + black + hispanic, data = dta.sv5b)    #dta.sv5 OR dta
summary(HeckResults1a)

HeckResults1Limited <- selection(survey ~ canvass + phonecall + mail + male + black + hispanic + WeakMatch + MedMatch + StrongMatch,
                                 obama ~ canvass + phonecall + mail + male + black + hispanic, data = dta.sv5b)
summary(HeckResults1Limited)




#####
#####
#####

##### WEIGHTING

dta.sub <- subset(dta,select=c("survey","canvass","canvass.i.score.1","canvass.i.score.2","canvass.i.score.3","canvass.i.score.4","canvass.i.score.5","canvass.i.score.6","canvass.i.score.7","canvass.i.score.8","canvass.i.score.9","phonecall.i.score.1","phonecall.i.score.2","phonecall.i.score.3","phonecall.i.score.4","phonecall.i.score.5","phonecall.i.score.6","phonecall.i.score.7","phonecall.i.score.8","phonecall.i.score.9","turnout.score.1","turnout.score.2","turnout.score.3","turnout.score.4","turnout.score.5","turnout.score.6","turnout.score.7","turnout.score.8","turnout.score.9","mail","phonecall","male","black","hispanic","protestant","catholic","obama"))


pout <- glm(survey ~ canvass + canvass.i.score.1 + canvass.i.score.2 + canvass.i.score.3 + canvass.i.score.4 +
              canvass.i.score.5 + canvass.i.score.6 + canvass.i.score.7 + canvass.i.score.8 + canvass.i.score.9 +
              phonecall.i.score.1 + phonecall.i.score.2 + phonecall.i.score.3 + phonecall.i.score.4 + phonecall.i.score.5 +
              phonecall.i.score.6 + phonecall.i.score.7 + phonecall.i.score.8 + phonecall.i.score.9 + mail + phonecall + 
              male + black + hispanic + protestant + catholic, na.action = na.exclude, data=dta.sub)

summary(pout)
p1 <- predict(pout,newdata=dta.sub)

dta.sub$propensity1 <- p1
dta.sub$inv.propensity1 <- NA
dta.sub$inv.propensity1[dta.sub$survey==0] <- 1/(1-dta.sub$propensity1[dta.sub$survey==0])
dta.sub$inv.propensity1[dta.sub$survey==1] <- 1/(dta.sub$propensity1[dta.sub$survey==1])

lout1 <- lm(obama ~ canvass + phonecall + mail, weights=inv.propensity1, data=dta.sub)
set.seed(19104)
M <- 100000
x1 <- rnorm(M,mean=summary(lout1)$coef[2,1],sd=summary(lout1)$coef[2,2])
quantile(x1,c(0.025,.5,.975))




####
#Extending our analysis 
####

#Canvass
canvass_data <- data.frame(dta$survey, dta$obama, dta$black, dta$hispanic, dta$male, dta$protestant, dta$catholic, dta$q_age, dta$canvass)
final_canvass_data <- na.omit(canvass_data)
names(final_canvass_data) <- c("Survey", "Obama", "Black", "Hispanic", "Male", "Protestant", "Catholic", "Age", "Canvass")

canvass_linModel <- glm(final_canvass_data$Obama ~ final_canvass_data$Survey + final_canvass_data$Black + final_canvass_data$Hispanic + 
                    final_canvass_data$Male + final_canvass_data$Protestant + final_canvass_data$Catholic + final_canvass_data$Canvass +
                    final_canvass_data$Age, 
                    data = final_canvass_data, family = binomial)
canvass_linModel$coefficients 
summary(canvass_linModel)

glm?
confint(canvass_linModel, level = 0.95)

canvass_X  <- cbind(final_canvass_data$Survey, final_canvass_data$Black,final_canvass_data$Hispanic, final_canvass_data$Male,final_canvass_data$Protestant, final_canvass_data$Catholic)
canvass_Y  <- final_canvass_data$Obama
canvass_Tr  <- final_canvass_data$Canvass


canvass_genmatch <- GenMatch(Tr=canvass_Tr, X = canvass_X, estimand = "ATT", M=1, replace = TRUE, pop.size=10, max.generations=10, wait.generations=2, ties = FALSE)
canvass_match  <- Match(Y=canvass_Y, Tr=canvass_Tr, X=canvass_X, ties = FALSE, M = 1, Weight.matrix = canvass_genmatch)

canvass_balance <- MatchBalance(final_canvass_data$Obama ~ final_canvass_data$Survey + final_canvass_data$Black + final_canvass_data$Hispanic + 
                       final_canvass_data$Male + final_canvass_data$Protestant + final_canvass_data$Catholic + final_canvass_data$Canvass, 
                     data = final_canvass_data, match.out=canvass_match, nboots=500)


#Mail
mail_data <- data.frame(dta$survey, dta$obama, dta$black, dta$hispanic, dta$male, dta$protestant, dta$catholic, dta$mail)
final_mail_data <- na.omit(mail_data)
names(final_mail_data) <- c("Survey", "Obama", "Black", "Hispanic", "Male", "Protestant", "Catholic", "Mail")

mail_linModel <- glm(final_mail_data$Obama ~ final_mail_data$Survey + final_mail_data$Black + final_mail_data$Hispanic + 
                       final_mail_data$Male + final_mail_data$Protestant + final_mail_data$Catholic + final_mail_data$Mail, 
                        data = final_mail_data, family = binomial)
mail_linModel$coefficients 
summary(mail_linModel)

confint(mail_linModel, level = 0.95)



#Phonecall 

phonecall_data <- data.frame(dta$survey, dta$obama, dta$black, dta$hispanic, dta$male, dta$protestant, dta$catholic, dta$phonecall)
final_phonecall_data <- na.omit(phonecall_data)
names(final_phonecall_data) <- c("Survey", "Obama", "Black", "Hispanic", "Male", "Protestant", "Catholic", "Phonecall")

phonecall_linModel <- glm(final_phonecall_data$Obama ~ final_phonecall_data$Survey + final_phonecall_data$Black + final_phonecall_data$Hispanic + 
                            final_phonecall_data$Male + final_phonecall_data$Protestant + final_phonecall_data$Catholic + final_phonecall_data$Phonecall, 
                     data = final_phonecall_data, family = binomial)
phonecall_linModel$coefficients 
summary(phonecall_linModel)

confint(phonecall_linModel, level = 0.95)
