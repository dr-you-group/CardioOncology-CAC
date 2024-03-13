library(cmprsk)

sev <- read.csv("./")

sev$ctDate <- as.Date(sev$ctDate)

sev <- sev[sev$ctDate >= as.Date("2011-01-01") & sev$ctDate <= as.Date("2019-12-31"),]

sev$HTN[is.na(sev$HTN)] <- 0
sev$DM[is.na(sev$DM)] <- 0
sev$AF[is.na(sev$AF)] <- 0
sev$CRF[is.na(sev$CRF)] <- 0
sev$Dyslipidemia[is.na(sev$Dyslipidemia)] <- 0
sev$HF[is.na(sev$HF)] <- 0
sev$CAD[is.na(sev$CAD)] <- 0
sev$CVD[is.na(sev$CVD)] <- 0

sev$stroke <- sev$Stroke
sev$stroke[is.na(sev$stroke)] <- 0

sev$mi <- sev$MI
sev$mi[is.na(sev$mi)] <- 0

sev$death <- sev$Death
sev$death[is.na(sev$death)] <- 0

sev$stroke_date <- as.Date(sev$stroke_date)
sev$MI_date <- as.Date(sev$MI_date)
sev$death_date <- as.Date(sev$death_date)

sev$death[sev$death_date > "2020-12-31"] <- 0
sev$death_date[sev$death_date > "2020-12-31"] <- NA

sev$cvDeath <- ifelse(!is.na(sev$cvd_date_broad), sev$cvd_date_broad, 0)
sev$cvNar <- ifelse(!is.na(sev$cvd_date_narrow), sev$cvd_date_narrow, 0)
sev$mace <- ifelse(sev$stroke == 1 | sev$mi == 1 | sev$cvDeath == 1, 1, 0)
sev$composite <- ifelse(sev$stroke == 1 | sev$mi == 1, 1, 0)

sev$strokeCmp <- ifelse(sev$stroke == 1, 1, ifelse(sev$death == 1, 2, 0))
sev$miCmp <- ifelse(sev$mi == 1, 1, ifelse(sev$death == 1, 2, 0))
sev$compositeCmp <- ifelse(sev$composite == 1, 1, ifelse(sev$death == 1, 2, 0))
sev$maceCmp <- ifelse(sev$mace == 1, 1, ifelse(sev$death == 1, 2, 0))
sev$cvDeathCmp <- ifelse(sev$cvDeath == 1, 1, ifelse(sev$death == 1, 2, 0))
sev$cvNarCmp <- ifelse(sev$cvNar == 1, 1, ifelse(sev$death == 1, 2, 0))

sev$strokeSurv <- ifelse(sev$stroke == 1, as.character(sev$stroke_date), ifelse(sev$death == 1, as.character(sev$death_date), as.character("2020-12-31")))
sev$miSurv <- ifelse(sev$mi == 1, as.character(sev$MI_date), ifelse(sev$death == 1, as.character(sev$death_date), as.character("2020-12-31")))
sev$deathSurv <- ifelse(sev$death == 1, as.character(sev$death_date), as.character("2020-12-31"))
sev$maceSurv <- ifelse(sev$mace == 1, as.character(pmin(sev$stroke_date, sev$MI_date, sev$death_date, na.rm=T)), 
                       ifelse(sev$death == 1, as.character(sev$death_date),as.character("2020-12-31")))
sev$compositeSurv <- ifelse(sev$composite == 1, as.character(pmin(sev$stroke_date, sev$MI_date, na.rm=T)), 
                            ifelse(sev$death == 1, as.character(sev$death_date),as.character("2020-12-31")))

sev$strokeSurv <- as.Date(sev$strokeSurv)
sev$miSurv <- as.Date(sev$miSurv)
sev$maceSurv <- as.Date(sev$maceSurv)
sev$deathSurv <- as.Date(sev$deathSurv)
sev$compositeSurv <- as.Date(sev$compositeSurv)

sev$strokeSurvDate <- difftime(sev$strokeSurv, sev$ctDate, units = "days")
sev$miSurvDate <- difftime(sev$miSurv, sev$ctDate, units = "days")
sev$maceSurvDate <- difftime(sev$maceSurv, sev$ctDate, units = "days")
sev$deathSurvDate <- difftime(sev$deathSurv, sev$ctDate, units = "days")
sev$compositeSurvDate <- difftime(sev$compositeSurv, sev$ctDate, units = "days")

sev$ageFactor <- ifelse(sev$ageFactor1 == 3 | sev$ageFactor1 == 4, 5, sev$ageFactor1)
sev$stage <- ifelse(sev$Stage == 0, 1, sev$Stage)

sev$ageFactor <- as.factor(sev$ageFactor)
sev$sex <- as.factor(sev$sex)
sev$stage <- as.factor(sev$stage)
sev$HTN <- as.factor(sev$HTN)
sev$DM <- as.factor(sev$DM)
sev$CRF <- as.factor(sev$CRF)
sev$Dyslipidemia <- as.factor(sev$Dyslipidemia)
sev$AF <- as.factor(sev$AF)
sev$HF <- as.factor(sev$HF)
sev$CAD <- as.factor(sev$CAD)
sev$CVD <- as.factor(sev$CVD)
sev$chemo <- as.factor(sev$chemo)
sev$CAC_Total_AgatstonScore <- as.factor(sev$CAC_Total_AgatstonScore)

#### Survival date ####
summary(as.numeric(sev$strokeSurvDate))
summary(as.numeric(sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 0]))
summary(as.numeric(sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 1]))
summary(as.numeric(sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 2]))
summary(as.numeric(sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 3]))

summary(as.numeric(sev$miSurvDate))
summary(as.numeric(sev$miSurvDate[sev$CAC_Total_AgatstonScore == 0]))
summary(as.numeric(sev$miSurvDate[sev$CAC_Total_AgatstonScore == 1]))
summary(as.numeric(sev$miSurvDate[sev$CAC_Total_AgatstonScore == 2]))
summary(as.numeric(sev$miSurvDate[sev$CAC_Total_AgatstonScore == 3]))

summary(as.numeric(sev$maceSurvDate))
summary(as.numeric(sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 0]))
summary(as.numeric(sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 1]))
summary(as.numeric(sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 2]))
summary(as.numeric(sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 3]))

summary(as.numeric(sev$deathSurvDate))
summary(as.numeric(sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 0]))
summary(as.numeric(sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 1]))
summary(as.numeric(sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 2]))
summary(as.numeric(sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 3]))

summary(as.numeric(sev$compositeSurvDate))
summary(as.numeric(sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 0]))
summary(as.numeric(sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 1]))
summary(as.numeric(sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 2]))
summary(as.numeric(sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 3]))

#### Incidence rate#### 
tmpDF <- NULL
incidence1yr <- NULL

event <- c("stroke", "mi", "mace", "death", "composite", "cvDeath", "cvNar")

event_date <- c("strokeSurvDate", 
                "miSurvDate", 
                "maceSurvDate", 
                "deathSurvDate", 
                "compositeSurvDate", 
                "deathSurvDate", 
                "deathSurvDate")

event_df <- data.frame(event, event_date)

for (i in 1:nrow(event_df)) {
  
  event = as.character(event_df[i,1])
  event_date = as.character(event_df[i,2])
  
  tmpDF <- cbind(event)
  
  sev[paste0("1yr",event,"Date")] <- ifelse(sev[event_date] <= 365.25, as.numeric(sev[[event_date]])/365.25, 1)
  sev[paste0("1yr",event)] <- ifelse(sev[event_date] <= 365.25 & sev[event] == 1, 1, 0)
  
  for (j in c(0:3)) {
    
    assign(paste0("group",j), sev[sev$CAC_Total_AgatstonScore == j,])
    
    df <- get(paste0("group",j))
    case <- sum(df[paste0("1yr",event)])
    personYear <- sum(df[paste0("1yr",event,"Date")])
    
    assign(paste0("case_g",j), case)
    assign(paste0("personYear_g",j), personYear)
    
    
    tmpDF <- cbind(tmpDF,get(paste0("case_g",j)), get(paste0("personYear_g",j)))
    
  }
  
  tmpDF <- as.data.frame(tmpDF)
  names(tmpDF) <- c("event","case_g0", "personYear_g0", "case_g1", "personYear_g1","case_g2", "personYear_g2","case_g3", "personYear_g3")
  
  incidence1yr <- rbind(incidence1yr, tmpDF)
  
  
}

tmpDF <- NULL
incidence3yr <- NULL

event <- c("stroke", "mi", "mace", "death", "composite", "cvDeath", "cvNar")

event_date <- c("strokeSurvDate", 
                "miSurvDate", 
                "maceSurvDate", 
                "deathSurvDate", 
                "compositeSurvDate", 
                "deathSurvDate", 
                "deathSurvDate")

event_df <- data.frame(event, event_date)

for (i in 1:nrow(event_df)) {
  
  event = as.character(event_df[i,1])
  event_date = as.character(event_df[i,2])
  
  tmpDF <- cbind(event)
  
  sev[paste0("3yr",event,"Date")] <- ifelse(sev[event_date] <= 1095.75, as.numeric(sev[[event_date]])/365.25, 3)
  sev[paste0("3yr",event)] <- ifelse(sev[event_date] <= 1095.75 & sev[event] == 1, 1, 0)
  
  for (j in c(0:3)) {
    
    assign(paste0("group",j), sev[sev$CAC_Total_AgatstonScore == j,])
    
    df <- get(paste0("group",j))
    case <- sum(df[paste0("3yr",event)])
    personYear <- sum(df[paste0("3yr",event,"Date")])
    
    assign(paste0("case_g",j), case)
    assign(paste0("personYear_g",j), personYear)
    
    
    tmpDF <- cbind(tmpDF,get(paste0("case_g",j)), get(paste0("personYear_g",j)))
    
  }
  
  tmpDF <- as.data.frame(tmpDF)
  names(tmpDF) <- c("event","case_g0", "personYear_g0", "case_g1", "personYear_g1","case_g2", "personYear_g2","case_g3", "personYear_g3")
  
  incidence3yr <- rbind(incidence3yr, tmpDF)
  
  
}

tmpDF <- NULL
incidence5yr <- NULL

event <- c("stroke", "mi", "mace", "death", "composite", "cvDeath", "cvNar")

event_date <- c("strokeSurvDate", 
                "miSurvDate", 
                "maceSurvDate", 
                "deathSurvDate", 
                "compositeSurvDate", 
                "deathSurvDate", 
                "deathSurvDate")

event_df <- data.frame(event, event_date)

for (i in 1:nrow(event_df)) {
  
  event = as.character(event_df[i,1])
  event_date = as.character(event_df[i,2])
  
  tmpDF <- cbind(event)
  
  sev[paste0("5yr",event,"Date")] <- ifelse(sev[event_date] <= 1826.25, as.numeric(sev[[event_date]])/365.25, 5)
  sev[paste0("5yr",event)] <- ifelse(sev[event_date] <= 1826.25 & sev[event] == 1, 1, 0)
  
  for (j in c(0:3)) {
    
    assign(paste0("group",j), sev[sev$CAC_Total_AgatstonScore == j,])
    
    df <- get(paste0("group",j))
    case <- sum(df[paste0("5yr",event)])
    personYear <- sum(df[paste0("5yr",event,"Date")])
    
    assign(paste0("case_g",j), case)
    assign(paste0("personYear_g",j), personYear)
    
    
    tmpDF <- cbind(tmpDF,get(paste0("case_g",j)), get(paste0("personYear_g",j)))
    
  }
  
  tmpDF <- as.data.frame(tmpDF)
  names(tmpDF) <- c("event","case_g0", "personYear_g0", "case_g1", "personYear_g1","case_g2", "personYear_g2","case_g3", "personYear_g3")
  
  incidence5yr <- rbind(incidence5yr, tmpDF)
  
  
}

tmpDF <- NULL
incidence10yr <- NULL

event <- c("stroke", "mi", "mace", "death", "composite", "cvDeath", "cvNar")

event_date <- c("strokeSurvDate", 
                "miSurvDate", 
                "maceSurvDate", 
                "deathSurvDate", 
                "compositeSurvDate", 
                "deathSurvDate", 
                "deathSurvDate")

event_df <- data.frame(event, event_date)

for (i in 1:nrow(event_df)) {
  
  event = as.character(event_df[i,1])
  event_date = as.character(event_df[i,2])
  
  tmpDF <- cbind(event)
  
  sev[paste0("10yr",event,"Date")] <- ifelse(sev[event_date] <= 3652.5, as.numeric(sev[[event_date]])/365.25, 10)
  sev[paste0("10yr",event)] <- ifelse(sev[event_date] <= 3652.5 & sev[event] == 1, 1, 0)
  
  for (j in c(0:3)) {
    
    assign(paste0("group",j), sev[sev$CAC_Total_AgatstonScore == j,])
    
    df <- get(paste0("group",j))
    case <- sum(df[paste0("10yr",event)])
    personYear <- sum(df[paste0("10yr",event,"Date")])
    
    assign(paste0("case_g",j), case)
    assign(paste0("personYear_g",j), personYear)
    
    
    tmpDF <- cbind(tmpDF,get(paste0("case_g",j)), get(paste0("personYear_g",j)))
    
  }
  
  tmpDF <- as.data.frame(tmpDF)
  names(tmpDF) <- c("event","case_g0", "personYear_g0", "case_g1", "personYear_g1","case_g2", "personYear_g2","case_g3", "personYear_g3")
  
  incidence10yr <- rbind(incidence10yr, tmpDF)
  
  
}

incidence1yr$label <- "1yr"
incidence3yr$label <- "3yr"
incidence5yr$label <- "5yr"
incidence10yr$label <- "10yr"

inc <- rbind(incidence1yr, incidence3yr, incidence5yr, incidence10yr)

#write.csv(inc, "total_inc.csv", row.names = F)

#### Stroke ####
##### KM curve cumulative incidence plot #####

surv_object <- "Surv(time = sev$strokeSurvDate, event = sev$stroke)"

survfit <- survfit(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev)

survdiff(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev) #p-value

RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(survfit, col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), xlab = "Days", ylab = "Cumulative incidence", 
     xlim = c(0,3650), ylim = c(0,0.30), fun = function(x) 1-x)
# legend("bottomleft", inset = 0.01, legend =c("CACS = 0", "0 < CACS <= 100", "100 < CACS <= 400", "CACS > 400"), lty = 1,
#        col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), cex = 0.8) 
grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### Competing risk Cumulative incidence ##### 

cif <- cuminc(ftime = sev$strokeSurvDate, fstatus = sev$strokeCmp, group = sev$CAC_Total_AgatstonScore)
cif$Tests

temp1 <- data.frame(incidence = cif$`0 1`$est, time = cif$`0 1`$time, category = "CACS = 0")
temp2 <- data.frame(incidence = cif$`1 1`$est, time = cif$`1 1`$time, category = "0 < CACS <=100")
temp3 <- data.frame(incidence = cif$`2 1`$est, time = cif$`2 1`$time, category = "100 < CACS <= 400")
temp4 <- data.frame(incidence = cif$`3 1`$est, time = cif$`3 1`$time, category = "CACS > 400")

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(temp1$time, temp1$incidence, col = "#BC3C29FF", xlab = "Days", ylab = "Cumulative incidence",type = "l", lty = 1, xlim = c(0,3650), ylim = c(0,0.30))
lines(temp2$time, temp2$incidence, col = "#0072B5FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp3$time, temp3$incidence, col = "#E18727FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp4$time, temp4$incidence, col = "#20854EFF", type = "l", lty = 1, xlim = c(0,3650))

grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$strokeSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### cox regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

surv_object <- "Surv(time = sev$strokeSurvDate, event = sev$stroke)"

univariable <- NULL
for (covName in covariates) {
  
  result <- survival::coxph(as.formula(paste(surv_object, "~", covName)), data=sev)
  
  hazard <- summary(result)$coefficients[,2]
  ci <- exp(confint(result, level = 0.95))
  pval <- summary(result)$coefficients[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariable <- rbind(univariable, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")


coxResult <- survival::coxph(as.formula(paste(surv_object, "~", paste(covariates, collapse= "+"))), data=sev)

hazard <- summary(coxResult)$coefficients[,2]
ci <- exp(confint(coxResult, level = 0.95))
pval <- summary(coxResult)$coefficients[,5]

multivariable <- as.data.frame(cbind(hazard,ci,pval))


##### competing risk regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

univariableCmp <- NULL
for (covName in covariates) {
  
  formula <- as.formula(paste("~", covName))
  
  cov <- model.matrix(formula, data = sev)
  name <- colnames(cov)[-1]
  
  cov <- as.data.frame(cov[,-1])
  colnames(cov) <- name
  
  result <- cmprsk::crr(sev$strokeSurvDate, sev$strokeCmp, cov1 = cov, failcode = 1)
  
  hazard <- summary(result)$coef[,2]
  ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
  rownames(ci) <- rownames(summary(result)$conf.int)
  pval <- summary(result)$coef[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariableCmp <- rbind(univariableCmp, temp)
  
} 


covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

formula <- as.formula(paste("~", paste(covariates, collapse= "+")))

cov <- model.matrix(formula, data = sev)[,-1]

result <- cmprsk::crr(sev$strokeSurvDate, sev$strokeCmp, cov1 = cov, failcode = 1)

hazard <- summary(result)$coef[,2]
ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
rownames(ci) <- rownames(summary(result)$conf.int)
pval <- summary(result)$coef[,5]

temp <- as.data.frame(cbind(hazard, ci, pval))

multivariableCmp <-as.data.frame(cbind(hazard, ci, pval))

write.csv(univariable, "sevhis_stroke_cox_uni.csv")
write.csv(multivariable, "sevhis_stroke_cox_multi.csv")
write.csv(univariableCmp, "sevhis_stroke_cmprsk_uni.csv")
write.csv(multivariableCmp, "sevhis_stroke_cmprsk_multi.csv")


#### Myocardial infarction ####
##### KM curve cumulative incidence plot #####

surv_object <- "Surv(time = sev$miSurvDate, event = sev$mi)"

survfit <- survfit(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev)

survdiff(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev) #p-value

RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(survfit, col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), xlab = "Days", ylab = "Cumulative incidence", 
     xlim = c(0,3650), ylim = c(0,0.05), fun = function(x) 1-x)
# legend("bottomleft", inset = 0.01, legend =c("CACS = 0", "0 < CACS <= 100", "100 < CACS <= 400", "CACS > 400"), lty = 1,
#        col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), cex = 0.8) 
grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$miSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$miSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$miSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$miSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### Competing risk Cumulative incidence ##### 

cif <- cuminc(ftime = sev$miSurvDate, fstatus = sev$miCmp, group = sev$CAC_Total_AgatstonScore)
cif$Tests

temp1 <- data.frame(incidence = cif$`0 1`$est, time = cif$`0 1`$time, category = "CACS = 0")
temp2 <- data.frame(incidence = cif$`1 1`$est, time = cif$`1 1`$time, category = "0 < CACS <=100")
temp3 <- data.frame(incidence = cif$`2 1`$est, time = cif$`2 1`$time, category = "100 < CACS <= 400")
temp4 <- data.frame(incidence = cif$`3 1`$est, time = cif$`3 1`$time, category = "CACS > 400")

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(temp1$time, temp1$incidence, col = "#BC3C29FF", xlab = "Days", ylab = "Cumulative incidence",type = "l", lty = 1, xlim = c(0,3650), ylim = c(0,0.05))
lines(temp2$time, temp2$incidence, col = "#0072B5FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp3$time, temp3$incidence, col = "#E18727FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp4$time, temp4$incidence, col = "#20854EFF", type = "l", lty = 1, xlim = c(0,3650))

grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$miSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$miSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$miSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$miSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### cox regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

surv_object <- "Surv(time = sev$miSurvDate, event = sev$mi)"

univariable <- NULL
for (covName in covariates) {
  
  result <- survival::coxph(as.formula(paste(surv_object, "~", covName)), data=sev)
  
  hazard <- summary(result)$coefficients[,2]
  ci <- exp(confint(result, level = 0.95))
  pval <- summary(result)$coefficients[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariable <- rbind(univariable, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore") 

coxResult <- survival::coxph(as.formula(paste(surv_object, "~", paste(covariates, collapse= "+"))), data=sev)

hazard <- summary(coxResult)$coefficients[,2]
ci <- exp(confint(coxResult, level = 0.95))
pval <- summary(coxResult)$coefficients[,5]

multivariable <- as.data.frame(cbind(hazard,ci,pval))


##### competing risk regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

univariableCmp <- NULL
for (covName in covariates) {
  
  formula <- as.formula(paste("~", covName))
  
  cov <- model.matrix(formula, data = sev)
  name <- colnames(cov)[-1]
  
  cov <- as.data.frame(cov[,-1])
  colnames(cov) <- name
  
  result <- cmprsk::crr(sev$miSurvDate, sev$miCmp, cov1 = cov, failcode = 1)
  
  hazard <- summary(result)$coef[,2]
  ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
  rownames(ci) <- rownames(summary(result)$conf.int)
  pval <- summary(result)$coef[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariableCmp <- rbind(univariableCmp, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

formula <- as.formula(paste("~", paste(covariates, collapse= "+")))

cov <- model.matrix(formula, data = sev)[,-1]

result <- cmprsk::crr(sev$miSurvDate, sev$miCmp, cov1 = cov, failcode = 1)

hazard <- summary(result)$coef[,2]
ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
rownames(ci) <- rownames(summary(result)$conf.int)
pval <- summary(result)$coef[,5]

temp <- as.data.frame(cbind(hazard, ci, pval))

multivariableCmp <-as.data.frame(cbind(hazard, ci, pval))

write.csv(univariable, "sev_mi_cox_uni.csv")
write.csv(multivariable, "sev_mi_cox_multi.csv")
write.csv(univariableCmp, "sev_mi_cmprsk_uni.csv")
write.csv(multivariableCmp, "sev_mi_cmprsk_multi.csv")

#### MACE ####
##### KM curve cumulative incidence plot #####

surv_object <- "Surv(time = sev$maceSurvDate, event = sev$mace)"

survfit <- survfit(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev)

survdiff(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev) #p-value

RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(survfit, col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), xlab = "Days", ylab = "Cumulative incidence", 
     xlim = c(0,3650), fun = function(x) 1-x)
# legend("bottomleft", inset = 0.01, legend =c("CACS = 0", "0 < CACS <= 100", "100 < CACS <= 400", "CACS > 400"), lty = 1,
#        col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), cex = 0.8) 
grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

#### Competing risk Cumulative incidence #####

cif <- cuminc(ftime = sev$maceSurvDate, fstatus = sev$maceCmp, group = sev$CAC_Total_AgatstonScore)

temp1 <- data.frame(incidence = cif$`0 1`$est, time = cif$`0 1`$time, category = "CACS = 0")
temp2 <- data.frame(incidence = cif$`1 1`$est, time = cif$`1 1`$time, category = "0 < CACS <=100")
temp3 <- data.frame(incidence = cif$`2 1`$est, time = cif$`2 1`$time, category = "100 < CACS <= 400")
temp4 <- data.frame(incidence = cif$`3 1`$est, time = cif$`3 1`$time, category = "CACS > 400")

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(temp1$time, temp1$incidence, col = "#BC3C29FF", xlab = "Days", ylab = "Cumulative incidence",type = "l", lty = 1, xlim = c(0,3650), ylim = c(0,0.30))
lines(temp2$time, temp2$incidence, col = "#0072B5FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp3$time, temp3$incidence, col = "#E18727FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp4$time, temp4$incidence, col = "#20854EFF", type = "l", lty = 1, xlim = c(0,3650))

grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$maceSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### cox regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

surv_object <- "Surv(time = sev$maceSurvDate, event = sev$mace)"

univariable <- NULL
for (covName in covariates) {
  
  result <- survival::coxph(as.formula(paste(surv_object, "~", covName)), data=sev)
  
  hazard <- summary(result)$coefficients[,2]
  ci <- exp(confint(result, level = 0.95))
  pval <- summary(result)$coefficients[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariable <- rbind(univariable, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")


coxResult <- survival::coxph(as.formula(paste(surv_object, "~", paste(covariates, collapse= "+"))), data=sev)

hazard <- summary(coxResult)$coefficients[,2]
ci <- exp(confint(coxResult, level = 0.95))
pval <- summary(coxResult)$coefficients[,5]

multivariable <- as.data.frame(cbind(hazard,ci,pval))


#### competing risk regression #####
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

univariableCmp <- NULL
for (covName in covariates) {
  
  formula <- as.formula(paste("~", covName))
  
  cov <- model.matrix(formula, data = sev)
  name <- colnames(cov)[-1]
  
  cov <- as.data.frame(cov[,-1])
  colnames(cov) <- name
  
  result <- cmprsk::crr(sev$maceSurvDate, sev$maceCmp, cov1 = cov, failcode = 1)
  
  hazard <- summary(result)$coef[,2]
  ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
  rownames(ci) <- rownames(summary(result)$conf.int)
  pval <- summary(result)$coef[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariableCmp <- rbind(univariableCmp, temp)
  
}

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

formula <- as.formula(paste("~", paste(covariates, collapse= "+")))

cov <- model.matrix(formula, data = sev)[,-1]

result <- cmprsk::crr(sev$maceSurvDate, sev$maceCmp, cov1 = cov, failcode = 1)

hazard <- summary(result)$coef[,2]
ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
rownames(ci) <- rownames(summary(result)$conf.int)
pval <- summary(result)$coef[,5]

temp <- as.data.frame(cbind(hazard, ci, pval))

multivariableCmp <-as.data.frame(cbind(hazard, ci, pval))

write.csv(univariable, "sev_mace_cox_uni.csv")
write.csv(multivariable, "sev_mace_cox_multi.csv")
write.csv(univariableCmp, "sev_mace_cmprsk_uni.csv")
write.csv(multivariableCmp, "sev_mace_cmprsk_multi.csv")

#### CV Death ####
##### KM curve cumulative incidence plot #####

surv_object <- "Surv(time = sev$deathSurvDate, event = sev$cvDeath)"

survfit <- survfit(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev)

survdiff(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev) #p-value

RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(survfit, col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), xlab = "Days", ylab = "Cumulative incidence", 
     xlim = c(0,3650), fun = function(x) 1-x)
# legend("bottomleft", inset = 0.01, legend =c("CACS = 0", "0 < CACS <= 100", "100 < CACS <= 400", "CACS > 400"), lty = 1,
#        col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), cex = 0.8) 
grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

#### Competing risk Cumulative incidence #####

cif <- cuminc(ftime = sev$deathSurvDate, fstatus = sev$cvDeathCmp, group = sev$CAC_Total_AgatstonScore)

temp1 <- data.frame(incidence = cif$`0 1`$est, time = cif$`0 1`$time, category = "CACS = 0")
temp2 <- data.frame(incidence = cif$`1 1`$est, time = cif$`1 1`$time, category = "0 < CACS <=100")
temp3 <- data.frame(incidence = cif$`2 1`$est, time = cif$`2 1`$time, category = "100 < CACS <= 400")
temp4 <- data.frame(incidence = cif$`3 1`$est, time = cif$`3 1`$time, category = "CACS > 400")

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(temp1$time, temp1$incidence, col = "#BC3C29FF", xlab = "Days", ylab = "Cumulative incidence",type = "l", lty = 1, xlim = c(0,3650), ylim = c(0,0.30))
lines(temp2$time, temp2$incidence, col = "#0072B5FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp3$time, temp3$incidence, col = "#E18727FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp4$time, temp4$incidence, col = "#20854EFF", type = "l", lty = 1, xlim = c(0,3650))

grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### cox regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

surv_object <- "Surv(time = sev$deathSurvDate, event = sev$cvDeath)"

univariable <- NULL
for (covName in covariates) {
  
  result <- survival::coxph(as.formula(paste(surv_object, "~", covName)), data=sev)
  
  hazard <- summary(result)$coefficients[,2]
  ci <- exp(confint(result, level = 0.95))
  pval <- summary(result)$coefficients[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariable <- rbind(univariable, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")


coxResult <- survival::coxph(as.formula(paste(surv_object, "~", paste(covariates, collapse= "+"))), data=sev)

hazard <- summary(coxResult)$coefficients[,2]
ci <- exp(confint(coxResult, level = 0.95))
pval <- summary(coxResult)$coefficients[,5]

multivariable <- as.data.frame(cbind(hazard,ci,pval))


#### competing risk regression #####
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

univariableCmp <- NULL
for (covName in covariates) {
  
  formula <- as.formula(paste("~", covName))
  
  cov <- model.matrix(formula, data = sev)
  name <- colnames(cov)[-1]
  
  cov <- as.data.frame(cov[,-1])
  colnames(cov) <- name
  
  result <- cmprsk::crr(sev$deathSurvDate, sev$cvDeathCmp, cov1 = cov, failcode = 1)
  
  hazard <- summary(result)$coef[,2]
  ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
  rownames(ci) <- rownames(summary(result)$conf.int)
  pval <- summary(result)$coef[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariableCmp <- rbind(univariableCmp, temp)
  
}

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

formula <- as.formula(paste("~", paste(covariates, collapse= "+")))

cov <- model.matrix(formula, data = sev)[,-1]

result <- cmprsk::crr(sev$deathSurvDate, sev$cvDeathCmp, cov1 = cov, failcode = 1)

hazard <- summary(result)$coef[,2]
ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
rownames(ci) <- rownames(summary(result)$conf.int)
pval <- summary(result)$coef[,5]

temp <- as.data.frame(cbind(hazard, ci, pval))

multivariableCmp <-as.data.frame(cbind(hazard, ci, pval))

write.csv(univariable, "sev_cvDeath_cox_uni.csv")
write.csv(multivariable, "sev_cvDeath_cox_multi.csv")
write.csv(univariableCmp, "sev_cvDeath_cmprsk_uni.csv")
write.csv(multivariableCmp, "sev_cvDeath_cmprsk_multi.csv")

#### CV Death narrow ####
##### KM curve cumulative incidence plot #####

surv_object <- "Surv(time = sev$deathSurvDate, event = sev$cvNar)"

survfit <- survfit(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev)

survdiff(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev) #p-value

RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(survfit, col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), xlab = "Days", ylab = "Cumulative incidence", 
     xlim = c(0,3650), fun = function(x) 1-x)
# legend("bottomleft", inset = 0.01, legend =c("CACS = 0", "0 < CACS <= 100", "100 < CACS <= 400", "CACS > 400"), lty = 1,
#        col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), cex = 0.8) 
grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

#### Competing risk Cumulative incidence #####

cif <- cuminc(ftime = sev$deathSurvDate, fstatus = sev$cvNarCmp, group = sev$CAC_Total_AgatstonScore)

temp1 <- data.frame(incidence = cif$`0 1`$est, time = cif$`0 1`$time, category = "CACS = 0")
temp2 <- data.frame(incidence = cif$`1 1`$est, time = cif$`1 1`$time, category = "0 < CACS <=100")
temp3 <- data.frame(incidence = cif$`2 1`$est, time = cif$`2 1`$time, category = "100 < CACS <= 400")
temp4 <- data.frame(incidence = cif$`3 1`$est, time = cif$`3 1`$time, category = "CACS > 400")

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(temp1$time, temp1$incidence, col = "#BC3C29FF", xlab = "Days", ylab = "Cumulative incidence",type = "l", lty = 1, xlim = c(0,3650), ylim = c(0,0.15))
lines(temp2$time, temp2$incidence, col = "#0072B5FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp3$time, temp3$incidence, col = "#E18727FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp4$time, temp4$incidence, col = "#20854EFF", type = "l", lty = 1, xlim = c(0,3650))

grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### cox regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

surv_object <- "Surv(time = sev$deathSurvDate, event = sev$cvNar)"

univariable <- NULL
for (covName in covariates) {
  
  result <- survival::coxph(as.formula(paste(surv_object, "~", covName)), data=sev)
  
  hazard <- summary(result)$coefficients[,2]
  ci <- exp(confint(result, level = 0.95))
  pval <- summary(result)$coefficients[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariable <- rbind(univariable, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")


coxResult <- survival::coxph(as.formula(paste(surv_object, "~", paste(covariates, collapse= "+"))), data=sev)

hazard <- summary(coxResult)$coefficients[,2]
ci <- exp(confint(coxResult, level = 0.95))
pval <- summary(coxResult)$coefficients[,5]

multivariable <- as.data.frame(cbind(hazard,ci,pval))


#### competing risk regression #####
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

univariableCmp <- NULL
for (covName in covariates) {
  
  formula <- as.formula(paste("~", covName))
  
  cov <- model.matrix(formula, data = sev)
  name <- colnames(cov)[-1]
  
  cov <- as.data.frame(cov[,-1])
  colnames(cov) <- name
  
  result <- cmprsk::crr(sev$deathSurvDate, sev$cvNarCmp, cov1 = cov, failcode = 1)
  
  hazard <- summary(result)$coef[,2]
  ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
  rownames(ci) <- rownames(summary(result)$conf.int)
  pval <- summary(result)$coef[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariableCmp <- rbind(univariableCmp, temp)
  
}

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

formula <- as.formula(paste("~", paste(covariates, collapse= "+")))

cov <- model.matrix(formula, data = sev)[,-1]

result <- cmprsk::crr(sev$deathSurvDate, sev$cvNarCmp, cov1 = cov, failcode = 1)

hazard <- summary(result)$coef[,2]
ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
rownames(ci) <- rownames(summary(result)$conf.int)
pval <- summary(result)$coef[,5]

temp <- as.data.frame(cbind(hazard, ci, pval))

multivariableCmp <-as.data.frame(cbind(hazard, ci, pval))

write.csv(univariable, "sev_cvNar_cox_uni.csv")
write.csv(multivariable, "sev_cvNar_cox_multi.csv")
write.csv(univariableCmp, "sev_cvNar_cmprsk_uni.csv")
write.csv(multivariableCmp, "sev_cvNar_cmprsk_multi.csv")

#### death ####
##### KM curve cumulative incidence plot #####

surv_object <- "Surv(time = sev$deathSurvDate, event = sev$death)"

survfit <- survfit(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev)

survdiff(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev) #p-value

RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(survfit, col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), xlab = "Days", ylab = "Cumulative incidence", 
     xlim = c(0,3650), fun = function(x) 1-x)
# legend("bottomleft", inset = 0.01, legend =c("CACS = 0", "0 < CACS <= 100", "100 < CACS <= 400", "CACS > 400"), lty = 1,
#        col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), cex = 0.8) 
grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$deathSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)


##### cox regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

surv_object <- "Surv(time = sev$deathSurvDate, event = sev$death)"

univariable <- NULL
for (covName in covariates) {
  
  result <- survival::coxph(as.formula(paste(surv_object, "~", covName)), data=sev)
  
  hazard <- summary(result)$coefficients[,2]
  ci <- exp(confint(result, level = 0.95))
  pval <- summary(result)$coefficients[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariable <- rbind(univariable, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")


coxResult <- survival::coxph(as.formula(paste(surv_object, "~", paste(covariates, collapse= "+"))), data=sev)

hazard <- summary(coxResult)$coefficients[,2]
ci <- exp(confint(coxResult, level = 0.95))
pval <- summary(coxResult)$coefficients[,5]

multivariable <- as.data.frame(cbind(hazard,ci,pval))


write.csv(univariable, "sev_death_cox_uni.csv")
write.csv(multivariable, "sev_death_cox_multi.csv")

#### Composite ####
##### KM curve cumulative incidence plot #####

surv_object <- "Surv(time = sev$compositeSurvDate, event = sev$composite)"

survfit <- survfit(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev)

survdiff(as.formula(paste(surv_object, "~", "CAC_Total_AgatstonScore")), data = sev) #p-value

RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(survfit, col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), xlab = "Days", ylab = "Cumulative incidence", 
     xlim = c(0,3650), ylim = c(0,0.30), fun = function(x) 1-x)
# legend("bottomleft", inset = 0.01, legend =c("CACS = 0", "0 < CACS <= 100", "100 < CACS <= 400", "CACS > 400"), lty = 1,
#        col = c("#BC3C29FF", "#0072B5FF", "#E18727FF","#20854EFF"), cex = 0.8) 
grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### Competing risk Cumulative incidence ##### 

cif <- cuminc(ftime = sev$compositeSurvDate, fstatus = sev$compositeCmp, group = sev$CAC_Total_AgatstonScore)

temp1 <- data.frame(incidence = cif$`0 1`$est, time = cif$`0 1`$time, category = "CACS = 0")
temp2 <- data.frame(incidence = cif$`1 1`$est, time = cif$`1 1`$time, category = "0 < CACS <=100")
temp3 <- data.frame(incidence = cif$`2 1`$est, time = cif$`2 1`$time, category = "100 < CACS <= 400")
temp4 <- data.frame(incidence = cif$`3 1`$est, time = cif$`3 1`$time, category = "CACS > 400")

par(mfrow=c(1,1), mar = c(9,6,2,2))
plot(temp1$time, temp1$incidence, col = "#BC3C29FF", xlab = "Days", ylab = "Cumulative incidence",type = "l", lty = 1, xlim = c(0,3650), ylim = c(0,0.30))
lines(temp2$time, temp2$incidence, col = "#0072B5FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp3$time, temp3$incidence, col = "#E18727FF", type = "l", lty = 1, xlim = c(0,3650))
lines(temp4$time, temp4$incidence, col = "#20854EFF", type = "l", lty = 1, xlim = c(0,3650))

grid <- seq(0,3650, by = 1000)
mtext(RiskSetCount(grid, sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 0]), side = 1, line = 5, at = grid)
mtext(RiskSetCount(grid, sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 1]), side = 1, line = 6, at = grid)
mtext(RiskSetCount(grid, sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 2]), side = 1, line = 7, at = grid)
mtext(RiskSetCount(grid, sev$compositeSurvDate[sev$CAC_Total_AgatstonScore == 3]), side = 1, line = 8, at = grid)

##### cox regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

surv_object <- "Surv(time = sev$compositeSurvDate, event = sev$composite)"

univariable <- NULL
for (covName in covariates) {
  
  result <- survival::coxph(as.formula(paste(surv_object, "~", covName)), data=sev)
  
  hazard <- summary(result)$coefficients[,2]
  ci <- exp(confint(result, level = 0.95))
  pval <- summary(result)$coefficients[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariable <- rbind(univariable, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

coxResult <- survival::coxph(as.formula(paste(surv_object, "~", paste(covariates, collapse= "+"))), data=sev)

hazard <- summary(coxResult)$coefficients[,2]
ci <- exp(confint(coxResult, level = 0.95))
pval <- summary(coxResult)$coefficients[,5]

multivariable <- as.data.frame(cbind(hazard,ci,pval))


##### competing risk regression ##### 
covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

univariableCmp <- NULL
for (covName in covariates) {
  
  formula <- as.formula(paste("~", covName))
  
  cov <- model.matrix(formula, data = sev)
  name <- colnames(cov)[-1]
  
  cov <- as.data.frame(cov[,-1])
  colnames(cov) <- name
  
  result <- cmprsk::crr(sev$compositeSurvDate, sev$compositeCmp, cov1 = cov, failcode = 1)
  
  hazard <- summary(result)$coef[,2]
  ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
  rownames(ci) <- rownames(summary(result)$conf.int)
  pval <- summary(result)$coef[,5]
  
  temp <- as.data.frame(cbind(hazard, ci, pval))
  
  univariableCmp <- rbind(univariableCmp, temp)
  
} 

covariates <- c("ageFactor","sex","stage","HTN","DM","CRF","Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo","CAC_Total_AgatstonScore")

formula <- as.formula(paste("~", paste(covariates, collapse= "+")))

cov <- model.matrix(formula, data = sev)[,-1]

result <- cmprsk::crr(sev$compositeSurvDate, sev$compositeCmp, cov1 = cov, failcode = 1)

hazard <- summary(result)$coef[,2]
ci <- data.frame("lb" = summary(result)$conf.int[,c(3)], "ub" = summary(result)$conf.int[,c(4)])
rownames(ci) <- rownames(summary(result)$conf.int)
pval <- summary(result)$coef[,5]

temp <- as.data.frame(cbind(hazard, ci, pval))

multivariableCmp <-as.data.frame(cbind(hazard, ci, pval))

write.csv(univariable, "sevhis_composite_cox_uni.csv")
write.csv(multivariable, "sevhis_composite_cox_multi.csv")
write.csv(univariableCmp, "sevhis_composite_cmprsk_uni.csv")
write.csv(multivariableCmp, "sevhis_composite_cmprsk_multi.csv")

#### Baseline Characteristics ####
sev$cancerType <- as.factor(sev$cancerType)
sev$stroke <- as.factor(sev$stroke)
sev$mi <- as.factor(sev$mi)
sev$mace <- as.factor(sev$mace)
sev$death <- as.factor(sev$death)
sev$composite <- as.factor(sev$composite)
sev$cvDeath <- as.factor(sev$cvDeath)
sev$cvNar <- as.factor(sev$cvNar)


cac0 <- sev[sev$CAC_Total_AgatstonScore == 0,]
cac1 <- sev[sev$CAC_Total_AgatstonScore == 1,]
cac2 <- sev[sev$CAC_Total_AgatstonScore == 2,]
cac3 <- sev[sev$CAC_Total_AgatstonScore == 3,]

varlist <- c("ageFactor", "sex", "stage", "HTN", "DM", "CRF", "Dyslipidemia", "AF", "HF", "CAD", "CVD", "chemo", "cancerType", "stroke",
             "mi","mace", "death", "composite", "cvDeath", "cvNar")

baseline <- NULL


for (varname in varlist) {
  
  inter <- NULL
  
  for (i in as.numeric(levels(sev[[varname]]))) {
    
    var <- paste0(varname, i)
    
    n_overall <- sum(sev[[varname]] == i)
    pr_overall <- n_overall / nrow(sev)
    fn_overall <- paste0(n_overall, " (", round(pr_overall,3)*100, ")")
    
    n_cac0 <- sum(cac0[[varname]] == i)
    pr_cac0 <- n_cac0 / nrow(cac0)
    fn_cac0 <- paste0(n_cac0, " (", round(pr_cac0,3)*100, ")")
    
    n_cac1 <- sum(cac1[[varname]] == i)
    pr_cac1 <- n_cac1 / nrow(cac1)
    fn_cac1 <- paste0(n_cac1, " (", round(pr_cac1,3)*100, ")")
    
    n_cac2 <- sum(cac2[[varname]] == i)
    pr_cac2 <- n_cac2 / nrow(cac2)
    fn_cac2 <- paste0(n_cac2, " (", round(pr_cac2,3)*100, ")")
    
    n_cac3 <- sum(cac3[[varname]] == i)
    pr_cac3 <- n_cac3 / nrow(cac3)
    fn_cac3 <- paste0(n_cac3, " (", round(pr_cac3,3)*100, ")")
    
    
    tmpDF1 <- data.frame(var,fn_overall, fn_cac0, fn_cac1, fn_cac2, fn_cac3)
    
    inter <- rbind(inter, tmpDF1)
    
  }
  
  tmp_table <- xtabs(~ eval(as.name(varname)) + CAC_Total_AgatstonScore, sev)
  result <- chisq.test(tmp_table, correct = FALSE)
  chisq <- result$p.value
  
  result2 <- fisher.test(tmp_table, simulate.p.value = T)
  fisher <- result2$p.value
  
  expectedValue <- sum(result$expected < 5) / (length(levels(sev[[varname]])) * 4) * 100
  expectedValueUnder1 <- sum(result$expected < 1)
  pval_table <- inter
  
  pval_table$chisq <- chisq
  pval_table$fisher <- fisher
  pval_table$expectedValue <- expectedValue
  pval_table$expectedValueUnder1 <- expectedValueUnder1
  
  
  baseline <- rbind(baseline, pval_table)
}

overall_table <- data.frame("overall", nrow(sev), nrow(cac0), nrow(cac1), nrow(cac2), nrow(cac3), NA, NA, NA, NA)
names(overall_table) <- colnames(baseline)

final_baseline_table <- rbind(overall_table, baseline)

#write.csv(final_baseline_table, "total_baseline.csv", row.names = F)