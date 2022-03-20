### This script will run cross-sectional analysis for 3-5 types of MNAs to check whether they are priced ###
 
### objects to report globally:
### 3 plots of pre/postranking betas.
### 1 table (3 panels) with mean quintile returns.            t9
### 1 table (3 panels) with BJS regressions.                  t10
### 1 table (NFP only) with BJS loadings.                     t11
### 1 table (NFP only) with characteristics of portfolios.    t12
### 1 table (NFP only) with FM regression.                    t13

# do pmi, then retail, then nfp with t11, t12, t13.

library("data.table")
library("lubridate")
library("reshape2")
library("tidyr")
library("dplyr")
library(tidyverse)
library(rollRegres)
library(hms)
library(rlist)
library(stargazer)
library("DescTools")
library("psych")

options("scipen"=100, "digits"=5)
setwd("G:/Dropbox/SSD_20/data")

time1 <- Sys.time()

#################################################################

rf <- fread("mFF_RF_20.csv", select=c("date", "ymprd", "RF"))
rf[,ymprd:=ymprd+1]
prd_map <- fread("prd_map_19.csv")
prd_map[,date:=ymd(date)]
factors7 <- fread("mFF7_19.csv")
setnames(factors7, "ymprd", "prd")
bgnews <- fread("bg_macronews_EGH20.csv")
bgnews[,Date:=ymd(format(mdy(Date), "%Y-%m-%d"))]
spx0 <- fread("itd_spx_8320.csv")
spx0[,c("date","hm_prd"):=list(ymd(date),hour*60+minute)]

windoww <- c(20, 120)
beta_window <- 72
mkt_controls <- TRUE

###########
### PMI ###
###########

news_hmprd <- 600

spxcd <- spx0[hm_prd<=news_hmprd+windoww[2]]
spxcd <- spxcd[order(date), .SD[.N],by=date]
lastprd <- spxcd[hm_prd>=(windoww[2]-30)+news_hmprd]
lastprd <- prd_map[lastprd, on = "date", nomatch=0]
firstprd <- spx0[hm_prd>=news_hmprd-windoww[1]-5][hm_prd<=news_hmprd-windoww[1]]
firstprd <- prd_map[firstprd, on = "date", nomatch=0]
firstprd <- firstprd[order(hm_prd),.SD[.N],by=c("prd")]

spx <- firstprd[,.(prd, lspx=Close)][lastprd[,.(date, prd, ymprd, Time, spx=Close, year, hm_prd)], on=.(prd), nomatch=0]
spx[,spx_ret:=100*(spx-lspx)/lspx]

unr <- bgnews[Event %in% c("ISM Manufacturing")]
unr[,ymprd:=year(Date)*12+month(Date)-23500]
unr <- unr[Time == "10:00"]
# exclude 2000-08-01, when news was released early.
unr[,surprise := (Actual - Survey)]
setnames(unr, "Date", "date")
unr <- prd_map[unr, on = "date", nomatch=0]

# get taq with pmi dates #

taq <- fread("taq5m_pmi_9717.csv")
taq[,date:=ymd(date)]
taq <- prd_map[,.(date, prd)][taq, on="date",nomatch=0]
taq <- taq[hm_prd<=news_hmprd+windoww[2]]

lastprd <- taq[order(hm_prd),.SD[.N],by=c("PERMNO", "prd")]
lastprd <- lastprd[hm_prd>=(windoww[2]-30)+news_hmprd]
firstprd <- taq[hm_prd>=news_hmprd-windoww[1]-5][hm_prd<=news_hmprd-windoww[1]]
firstprd <- firstprd[order(hm_prd),.SD[.N],by=c("PERMNO", "prd")]
taq <- firstprd[,.(PERMNO, prd, lPRICE=PRICE)][lastprd, on=.(PERMNO, prd), nomatch=0]
taq[,ret:=100*(PRICE-lPRICE)/lPRICE]

unr <- unr[spx[,.(prd, spx_ret)], on="prd", nomatch=0]
unr_rets <- unr[,.(prd, Event, period, Time, surprise, spx_ret)][taq[,.(PERMNO, date, prd, hm_prd, ymprd, ret)], on="prd", nomatch=0]
setorder(unr_rets, PERMNO, prd)
unr_rets <- unr_rets[!is.na(surprise)]

if (mkt_controls == TRUE){
  beta_news <- make_beta_param_controls(copy(unr_rets[,.(PERMNO, surprise, ret, prd=ymprd, spx_ret)]), variable="surprise",
                                    retvar = "ret", window=beta_window, clean_window=beta_window, max_window=beta_window*1.2, controls = c("spx_ret"))
}

if (mkt_controls == FALSE){
  beta_news <- make_beta_param(copy(unr_rets[,.(PERMNO, surprise, ret, prd=ymprd, spx_ret)]), variable="surprise", retvar = "ret",
                           window=beta_window, clean_window=beta_window, max_window=beta_window*1.2)
}

# using spx controls, I will have the opposite sign. but i am not sure how to interpret controls for spx...
# beta wrt spx only during those days does not work.
setnames(beta_news, "beta", "beta_news")
beta_news[,beta_news:=winsor(beta_news, trim=0.01)]
beta_news[,c("ret", "spx_ret", "surprise"):=list(NULL)]
summary(beta_news[,beta_news])
setorder(beta_news, PERMNO, prd)

### ###

crsp <- fread("mCRSP_2619_p,rnm.csv")
setorder(crsp, PERMNO, prd)
crsp[, c("lme", "lprd"):=list(shift(me,1), shift(prd,1)), by=PERMNO]
crsp[(prd-lprd)>2,lme:=NA]
crsp <- crsp[!is.na(lme)]
crsp <- beta_news[crsp, on=.(PERMNO, prd), nomatch=0]

crsp[,lPRC:=shift(PRC,1), by="PERMNO"]
crsp <- crsp[!is.na(lme)][!is.na(beta_news)]
crsp[,c("size"):=list(log(lme))]
crsp <- crsp[year>=1998]

dcrsp <- fread("dTAQ_beta_sp500.csv", select=c("PERMNO", "ymprd", "beta"))
dcrsp <- dcrsp[PERMNO %fin% unique(crsp[,PERMNO])][ymprd>=476]
setnames(dcrsp, "ymprd", "prd")
crsp <- dcrsp[crsp, on=.(PERMNO, prd),nomatch=0]

temp <- crsp[,.(coef=coefficients(lm(beta_news~beta))[2], r2=summary(lm(beta_news~beta))$r.squared), by="prd"]
summary(temp)

crsp <- crsp[EXCHCD %in% c(1:3,31)]
nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_news, beta, RET)]

breaks_5 <- make_bpoints_d(copy(crsp), "beta_news", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_news", breaks_5)
breaks_5 <- make_bpoints_d(copy(crsp), "beta", 5)
crsp <- unisorter_var_d(copy(crsp), "beta", breaks_5)

crsp[order(beta_news_nt_5),.(beta_news=mean(beta_news), beta_mkt=mean(beta,na.rm=TRUE), size=mean(size), RET=mean(RET)), by=beta_news_nt_5]
crsp[order(beta_nt_5),.(beta_news=mean(beta_news), beta_mkt=mean(beta,na.rm=TRUE), size=mean(size), RET=mean(RET)), by=beta_nt_5]

# this is extremely crude way to eyeball something like noisy ew characteristics.

### beta_news ###

ptfs_vw <- characteristic_portfolios_d(copy(crsp), "beta_news_nt_5", 'RET', "value", "lme")
ptfs_vw[,LS:=beta_news_5 - beta_news_1]
ptfs_ew <- characteristic_portfolios_d(copy(crsp), "beta_news_nt_5", 'RET', "equal", "lme")
ptfs_ew[,LS:=beta_news_5 - beta_news_1]
invisible(gc())

ar_vw <- average_returns_calculator(data.frame(ptfs_vw[,-1]))
colnames(ar_vw) <- colnames(ptfs_vw[,-1])
ar_ew <- average_returns_calculator(data.frame(ptfs_ew[,-1]))
colnames(ar_ew) <- colnames(ptfs_ew[,-1])
table9_pmi <- rbind(ar_ew, ar_vw)
table9_pmi <- data.frame(table9_pmi)
rownames(table9_pmi) <- c("Mean ew", "T-stat ew", "Mean vw", "T-stat vw")
names(table9_pmi) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "LS")

table32_7 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table32_5 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table32_4_ <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, STR, prd)], TRUE, 2)
table32_4 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table32_3 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table32_1 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())

table10_pmi <- cbind(table32_1[11:12,1:3], table32_3[11:12,3], table32_4[11:12,3], table32_5[11:12,3], table32_7[11:12,3])
colnames(table10_pmi) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table5 <- table32_7
colnames(table5)[1] <- "Quintile"

### postranking betas ###

taqp <- crsp[,.(PERMNO, ymprd=prd, beta_news_nt_5, lme)][taq, on=.(PERMNO, ymprd),nomatch=0]
taqp[,prd:=NULL]
setnames(taqp, "ymprd", "prd")
ptfs_vw_ <- characteristic_portfolios_d(copy(taqp), "beta_news_nt_5", 'ret', "value", "lme")
ptfs_vw_[,LS:=beta_news_5 - beta_news_1]

ptfs_taq <- unr[,.(prd=ymprd, date, surprise, spx_ret)][ptfs_vw_, on="prd", nomatch=0]
ptfs_taq[,coefficients(lm(beta_news_1~surprise))[2]]
ptfs_taq <- melt(ptfs_taq, c("prd", 'surprise', "spx_ret", "date"))
setnames(ptfs_taq, c("variable", "value"), c("ptf", "ret"))

if (mkt_controls == TRUE){
  table5 <- ptfs_taq[,coefficients(lm(ret~surprise+spx_ret))[2], by=ptf]
}
if (mkt_controls == FALSE){
  table5 <- ptfs_taq[,coefficients(lm(ret~surprise))[2], by=ptf]
}

table9_pmi
table10_pmi


ptfs_vw_beta_news <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_news, lme, beta_news_nt_5)]), "beta_news_nt_5", 'beta_news', "value", "lme")
ptfs_vw_beta_news[,LS:=beta_news_5 - beta_news_1]
invisible(gc())

temptable <- data.frame(matrix(-88, 4, 6))
colnames(temptable) <- c(colnames(table9_pmi))
rownames(temptable) <- c("RET", "RET_tstat", "prebeta", "postbeta")

temptable[1:2,] <- table9_pmi[3:4,]
temptable[3,] <- colMeans(ptfs_vw_beta_news[,-1], na.rm=TRUE)
temptable[4,] <- table5$V1
temp <- t(data.table(temptable[3:4,]))
colnames(temp) <- c("Prebeta", "Postbeta")
temp <- temp[1:6,]
ggplot(data.frame(temp), aes(x=Prebeta, y=Postbeta)) + geom_point(size=3.5) +
  labs(y="Post-ranking beta", x="Pre-ranking beta PMI")

##############
### Retail ###
##############

beta_window <- 72
news_hmprd <- 510

spxcd <- spx0[hm_prd<=news_hmprd+windoww[2]]
spxcd <- spxcd[order(date), .SD[.N],by=date]
spxcd <- prd_map[spxcd, on = "date", nomatch=0]
# 150 obs lost...
spxcd[,lprd:=prd-1]
spxpd <- spx0[order(date), .SD[.N],by=date]
spxpd <- prd_map[spxpd, on = "date", nomatch=0]
spx <- spxpd[,.(lprd=prd, lspx=Close)][spxcd[,.(date, prd, lprd, ymprd, Time, spx=Close, year, hm_prd)], on=.(lprd), nomatch=0]
spx[,spx_ret:=100*(log(spx)-log(lspx))]

unr <- bgnews[Event %in% c("Retail Sales Advance MoM", "Advance Retail Sales")]
unr[,surprise := (Actual - Survey)]
setnames(unr, "Date", "date")
unr <- prd_map[unr, on = "date", nomatch=0]
unr <- unr[!((date==ymd("1997-01-14"))&(period == "Jan"))]

# copypaste from yr4_i7_CS:
taq <- fread("taq5m_retl_9719.csv")
taq[,date:=ymd(date)]
taqd <- fread("TAQ_daily_9719.csv")
taqd[,date:=ymd(date)]

taq <- prd_map[,.(date, prd)][taq, on="date",nomatch=0]
taq[,lprd:=prd-1]
taqd <- prd_map[,.(date, prd)][taqd[itdprd==77], on="date", nomatch=0]

taq <- taq[hm_prd<=news_hmprd+windoww[2]]
lastprd <- taq[order(hm_prd),.SD[.N],by=c("PERMNO", "prd")]
lastprd <- lastprd[hm_prd>=(windoww[2]-30)+news_hmprd]

# join taq with taqd to get pre-news price
taq <- taqd[,.(PERMNO, lprd=prd, lPRICE=PRICE)][lastprd, on=.(PERMNO, lprd), nomatch=0]
# 1% sample lost...
taq[,ret:=100*(PRICE-lPRICE)/lPRICE]
taq <- taq[abs(ret)<40]
taq[,ret:=NULL]
# this is supposed to omit splits and consolidations. currently I do not have a better way to do it, since
# I can not find taq div files after 2014. need to ask shuaiyu about them.

divs <- fread("crsp_divs_19.csv")
divs <- divs[,.(PERMNO, EXDT, date=ymd(EXDT), DISTCD, DIVAMT)]
divs[(PERMNO==89533)&(date == ymd("2014-11-07"))&(DISTCD==1272),DIVAMT:=-888]
divs <- divs[(DIVAMT!=-888)|is.na(DIVAMT)]

taq <- divs[taq, on = .(PERMNO, date)]
taq[is.na(DIVAMT), DIVAMT:=0]
taq[,lPRICE:=lPRICE-DIVAMT]
taq[,DIVAMT:=NULL]
taq[,ret:=100*(PRICE-lPRICE)/lPRICE]

unr <- unr[spx[,.(prd, spx_ret)], on="prd", nomatch=0]
unr_rets <- unr[,.(prd, Event, period, Time, surprise, spx_ret)][taq[,.(PERMNO, date, prd, hm_prd, ymprd, ret)], on="prd", nomatch=0]
setorder(unr_rets, PERMNO, prd)
unr_rets <- unr_rets[!is.na(surprise)]

if (mkt_controls == TRUE){
  beta_news <- make_beta_param_controls(copy(unr_rets[,.(PERMNO, surprise, ret, prd=ymprd, spx_ret)]), variable="surprise",
                                        retvar = "ret", window=beta_window, clean_window=beta_window, max_window=beta_window*1.2, controls = c("spx_ret"))
}

if (mkt_controls == FALSE){
  beta_news <- make_beta_param(copy(unr_rets[,.(PERMNO, surprise, ret, prd=ymprd, spx_ret)]), variable="surprise", retvar = "ret",
                               window=beta_window, clean_window=beta_window, max_window=beta_window*1.2)
}

# using spx controls, I will have the opposite sign. but i am not sure how to interpret controls for spx...
# beta wrt spx only during those days does not work.
setnames(beta_news, "beta", "beta_news")
beta_news[,beta_news:=winsor(beta_news, trim=0.01)]
beta_news[,c("ret", "spx_ret", "surprise"):=list(NULL)]
summary(beta_news[,beta_news])
setorder(beta_news, PERMNO, prd)

### ###

crsp <- fread("mCRSP_2619_p,rnm.csv")
setorder(crsp, PERMNO, prd)
crsp[, c("lme", "lprd"):=list(shift(me,1), shift(prd,1)), by=PERMNO]
crsp[(prd-lprd)>2,lme:=NA]
crsp <- crsp[!is.na(lme)]
crsp <- beta_news[crsp, on=.(PERMNO, prd), nomatch=0]

crsp[,lPRC:=shift(PRC,1), by="PERMNO"]
crsp <- crsp[!is.na(lme)][!is.na(beta_news)]
crsp[,c("size"):=list(log(lme))]
crsp <- crsp[year>=1998]

dcrsp <- fread("dTAQ_beta_sp500.csv", select=c("PERMNO", "ymprd", "beta"))
dcrsp <- dcrsp[PERMNO %fin% unique(crsp[,PERMNO])][ymprd>=476]
setnames(dcrsp, "ymprd", "prd")
crsp <- dcrsp[crsp, on=.(PERMNO, prd),nomatch=0]

temp <- crsp[,.(coef=coefficients(lm(beta_news~beta))[2], r2=summary(lm(beta_news~beta))$r.squared), by="prd"]
summary(temp)

crsp <- crsp[EXCHCD %in% c(1:3,31)]
nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_news, beta, RET)]

breaks_5 <- make_bpoints_d(copy(crsp), "beta_news", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_news", breaks_5)
breaks_5 <- make_bpoints_d(copy(crsp), "beta", 5)
crsp <- unisorter_var_d(copy(crsp), "beta", breaks_5)

crsp[order(beta_news_nt_5),.(beta_news=mean(beta_news), beta_mkt=mean(beta,na.rm=TRUE), size=mean(size), RET=mean(RET)), by=beta_news_nt_5]
crsp[order(beta_nt_5),.(beta_news=mean(beta_news), beta_mkt=mean(beta,na.rm=TRUE), size=mean(size), RET=mean(RET)), by=beta_nt_5]

# this is extremely crude way to eyeball something like noisy ew characteristics.

### beta_news ###

ptfs_vw <- characteristic_portfolios_d(copy(crsp), "beta_news_nt_5", 'RET', "value", "lme")
ptfs_vw[,LS:=beta_news_5 - beta_news_1]
ptfs_ew <- characteristic_portfolios_d(copy(crsp), "beta_news_nt_5", 'RET', "equal", "lme")
ptfs_ew[,LS:=beta_news_5 - beta_news_1]
invisible(gc())

ar_vw <- average_returns_calculator(data.frame(ptfs_vw[,-1]))
colnames(ar_vw) <- colnames(ptfs_vw[,-1])
ar_ew <- average_returns_calculator(data.frame(ptfs_ew[,-1]))
colnames(ar_ew) <- colnames(ptfs_ew[,-1])
table9_retl <- rbind(ar_ew, ar_vw)
table9_retl <- data.frame(table9_retl)
rownames(table9_retl) <- c("Mean ew", "T-stat ew", "Mean vw", "T-stat vw")
names(table9_retl) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "LS")

table32_7 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table32_5 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table32_4_ <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, STR, prd)], TRUE, 2)
table32_4 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table32_3 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table32_1 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())

table10_retl <- cbind(table32_1[11:12,1:3], table32_3[11:12,3], table32_4[11:12,3], table32_5[11:12,3], table32_7[11:12,3])
colnames(table10_retl) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")


### postranking betas ###

taqp <- crsp[,.(PERMNO, ymprd=prd, beta_news_nt_5, lme)][taq, on=.(PERMNO, ymprd),nomatch=0]
taqp[,prd:=NULL]
setnames(taqp, "ymprd", "prd")
ptfs_vw_ <- characteristic_portfolios_d(copy(taqp), "beta_news_nt_5", 'ret', "value", "lme")
ptfs_vw_[,LS:=beta_news_5 - beta_news_1]

ptfs_taq <- unr[,.(prd=ymprd, date, surprise, spx_ret)][ptfs_vw_, on="prd", nomatch=0]
ptfs_taq[,coefficients(lm(beta_news_1~surprise))[2]]
ptfs_taq <- melt(ptfs_taq, c("prd", 'surprise', "spx_ret", "date"))
setnames(ptfs_taq, c("variable", "value"), c("ptf", "ret"))

if (mkt_controls == TRUE){
  table5 <- ptfs_taq[,coefficients(lm(ret~surprise+spx_ret))[2], by=ptf]
}
if (mkt_controls == FALSE){
  table5 <- ptfs_taq[,coefficients(lm(ret~surprise))[2], by=ptf]
}

table9_retl
table10_retl


ptfs_vw_beta_news <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_news, lme, beta_news_nt_5)]), "beta_news_nt_5", 'beta_news', "value", "lme")
ptfs_vw_beta_news[,LS:=beta_news_5 - beta_news_1]
invisible(gc())

temptable <- data.frame(matrix(-88, 4, 6))
colnames(temptable) <- c(colnames(table9_retl))
rownames(temptable) <- c("RET", "RET_tstat", "prebeta", "postbeta")

temptable[1:2,] <- table9_retl[3:4,]
temptable[3,] <- colMeans(ptfs_vw_beta_news[,-1], na.rm=TRUE)
temptable[4,] <- table5$V1
temp <- t(data.table(temptable[3:4,]))
colnames(temp) <- c("Prebeta", "Postbeta")
temp <- temp[1:6,]
ggplot(data.frame(temp), aes(x=Prebeta, y=Postbeta)) + geom_point(size=3.5) +
  labs(y="Post-ranking beta", x="Pre-ranking beta Retail")


###########
### CPI ###
###########

beta_window <- 72
news_hmprd <- 510
mkt_controls <- TRUE

spxcd <- spx0[hm_prd<=news_hmprd+windoww[2]]
spxcd <- spxcd[order(date), .SD[.N],by=date]
spxcd <- prd_map[spxcd, on = "date", nomatch=0]
# 150 obs lost...
spxcd[,lprd:=prd-1]
spxpd <- spx0[order(date), .SD[.N],by=date]
spxpd <- prd_map[spxpd, on = "date", nomatch=0]
spx <- spxpd[,.(lprd=prd, lspx=Close)][spxcd[,.(date, prd, lprd, ymprd, Time, spx=Close, year, hm_prd)], on=.(lprd), nomatch=0]
spx[,spx_ret:=100*(log(spx)-log(lspx))]

unr <- bgnews[Event %in% c("CPI MoM")]
unr[,surprise := (Actual - Survey)]
setnames(unr, "Date", "date")
unr <- prd_map[unr, on = "date", nomatch=0]
unr <- unr[!((date==ymd("1997-01-14"))&(period == "Jan"))]

# copypaste from yr4_i7_CS:
taq <- fread("taq5m_cpi_9719.csv")
taq[,date:=ymd(date)]
taqd <- fread("TAQ_daily_9719.csv")
taqd[,date:=ymd(date)]

taq <- prd_map[,.(date, prd)][taq, on="date",nomatch=0]
taq[,lprd:=prd-1]
taqd <- prd_map[,.(date, prd)][taqd[itdprd==77], on="date", nomatch=0]

taq <- taq[hm_prd<=news_hmprd+windoww[2]]
lastprd <- taq[order(hm_prd),.SD[.N],by=c("PERMNO", "prd")]
lastprd <- lastprd[hm_prd>=(windoww[2]-30)+news_hmprd]

# join taq with taqd to get pre-news price
taq <- taqd[,.(PERMNO, lprd=prd, lPRICE=PRICE)][lastprd, on=.(PERMNO, lprd), nomatch=0]
# 1% sample lost...
taq[,ret:=100*(PRICE-lPRICE)/lPRICE]
taq <- taq[abs(ret)<40]
taq[,ret:=NULL]
# this is supposed to omit splits and consolidations. currently I do not have a better way to do it, since
# I can not find taq div files after 2014. need to ask shuaiyu about them.

divs <- fread("crsp_divs_19.csv")
divs <- divs[,.(PERMNO, EXDT, date=ymd(EXDT), DISTCD, DIVAMT)]
divs[(PERMNO==89533)&(date == ymd("2014-11-07"))&(DISTCD==1272),DIVAMT:=-888]
divs <- divs[(DIVAMT!=-888)|is.na(DIVAMT)]

taq <- divs[taq, on = .(PERMNO, date)]
taq[is.na(DIVAMT), DIVAMT:=0]
taq[,lPRICE:=lPRICE-DIVAMT]
taq[,DIVAMT:=NULL]
taq[,ret:=100*(PRICE-lPRICE)/lPRICE]

unr <- unr[spx[,.(prd, spx_ret)], on="prd", nomatch=0]
unr <- unique(unr)
unr_rets <- unr[,.(prd, Event, period, Time, surprise, spx_ret)][taq[,.(PERMNO, date, prd, hm_prd, ymprd, ret)], on="prd", nomatch=0]
setorder(unr_rets, PERMNO, prd)
unr_rets <- unr_rets[!is.na(surprise)]
unr_rets <- unr_rets[Time=="08:30"]

if (mkt_controls == TRUE){
  beta_news <- make_beta_param_controls(copy(unr_rets[,.(PERMNO, surprise, ret, prd=ymprd, spx_ret)]), variable="surprise",
                                        retvar = "ret", window=beta_window, clean_window=beta_window, max_window=beta_window*1.2, controls = c("spx_ret"))
}

if (mkt_controls == FALSE){
  beta_news <- make_beta_param(copy(unr_rets[,.(PERMNO, surprise, ret, prd=ymprd, spx_ret)]), variable="surprise", retvar = "ret",
                               window=beta_window, clean_window=beta_window, max_window=beta_window*1.2)
}

# using spx controls, I will have the opposite sign. but i am not sure how to interpret controls for spx...
# beta wrt spx only during those days does not work.
setnames(beta_news, "beta", "beta_news")
beta_news[,beta_news:=winsor(beta_news, trim=0.01)]
beta_news[,c("ret", "spx_ret", "surprise"):=list(NULL)]
summary(beta_news[,beta_news])
setorder(beta_news, PERMNO, prd)

### ###

crsp <- fread("mCRSP_2619_p,rnm.csv")
setorder(crsp, PERMNO, prd)
crsp[, c("lme", "lprd"):=list(shift(me,1), shift(prd,1)), by=PERMNO]
crsp[(prd-lprd)>2,lme:=NA]
crsp <- crsp[!is.na(lme)]
crsp <- beta_news[crsp, on=.(PERMNO, prd), nomatch=0]

crsp[,lPRC:=shift(PRC,1), by="PERMNO"]
crsp <- crsp[!is.na(lme)][!is.na(beta_news)]
crsp[,c("size"):=list(log(lme))]
crsp <- crsp[year>=1998]

dcrsp <- fread("dTAQ_beta_sp500.csv", select=c("PERMNO", "ymprd", "beta"))
dcrsp <- dcrsp[PERMNO %fin% unique(crsp[,PERMNO])][ymprd>=476]
setnames(dcrsp, "ymprd", "prd")
crsp <- dcrsp[crsp, on=.(PERMNO, prd),nomatch=0]

temp <- crsp[,.(coef=coefficients(lm(beta_news~beta))[2], r2=summary(lm(beta_news~beta))$r.squared), by="prd"]
summary(temp)

crsp <- crsp[EXCHCD %in% c(1:3,31)]
nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_news, beta, RET)]

breaks_5 <- make_bpoints_d(copy(crsp), "beta_news", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_news", breaks_5)
breaks_5 <- make_bpoints_d(copy(crsp), "beta", 5)
crsp <- unisorter_var_d(copy(crsp), "beta", breaks_5)

crsp[order(beta_news_nt_5),.(beta_news=mean(beta_news), beta_mkt=mean(beta,na.rm=TRUE), size=mean(size), RET=mean(RET)), by=beta_news_nt_5]
crsp[order(beta_nt_5),.(beta_news=mean(beta_news), beta_mkt=mean(beta,na.rm=TRUE), size=mean(size), RET=mean(RET)), by=beta_nt_5]

# this is extremely crude way to eyeball something like noisy ew characteristics.

### beta_news ###

ptfs_vw <- characteristic_portfolios_d(copy(crsp), "beta_news_nt_5", 'RET', "value", "lme")
ptfs_vw[,LS:=beta_news_5 - beta_news_1]
ptfs_ew <- characteristic_portfolios_d(copy(crsp), "beta_news_nt_5", 'RET', "equal", "lme")
ptfs_ew[,LS:=beta_news_5 - beta_news_1]
invisible(gc())

ar_vw <- average_returns_calculator(data.frame(ptfs_vw[,-1]))
colnames(ar_vw) <- colnames(ptfs_vw[,-1])
ar_ew <- average_returns_calculator(data.frame(ptfs_ew[,-1]))
colnames(ar_ew) <- colnames(ptfs_ew[,-1])
table9_cpi <- rbind(ar_ew, ar_vw)
table9_cpi <- data.frame(table9_cpi)
rownames(table9_cpi) <- c("Mean ew", "T-stat ew", "Mean vw", "T-stat vw")
names(table9_cpi) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "LS")

table32_7 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table32_5 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table32_4_ <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, STR, prd)], TRUE, 2)
table32_4 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table32_3 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table32_1 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())

table10_cpi <- cbind(table32_1[11:12,1:3], table32_3[11:12,3], table32_4[11:12,3], table32_5[11:12,3], table32_7[11:12,3])
colnames(table10_cpi) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")


### postranking betas ###

taqp <- crsp[,.(PERMNO, ymprd=prd, beta_news_nt_5, lme)][taq, on=.(PERMNO, ymprd),nomatch=0]
taqp[,prd:=NULL]
setnames(taqp, "ymprd", "prd")
ptfs_vw_ <- characteristic_portfolios_d(copy(taqp), "beta_news_nt_5", 'ret', "value", "lme")
ptfs_vw_[,LS:=beta_news_5 - beta_news_1]

ptfs_taq <- unr[,.(prd=ymprd, date, surprise, spx_ret)][ptfs_vw_, on="prd", nomatch=0]
ptfs_taq[,coefficients(lm(beta_news_1~surprise))[2]]
ptfs_taq <- melt(ptfs_taq, c("prd", 'surprise', "spx_ret", "date"))
setnames(ptfs_taq, c("variable", "value"), c("ptf", "ret"))

if (mkt_controls == TRUE){
  table5 <- ptfs_taq[,coefficients(lm(ret~surprise+spx_ret))[2], by=ptf]
}
if (mkt_controls == FALSE){
  table5 <- ptfs_taq[,coefficients(lm(ret~surprise))[2], by=ptf]
}

table9_cpi
table10_cpi


ptfs_vw_beta_news <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_news, lme, beta_news_nt_5)]), "beta_news_nt_5", 'beta_news', "value", "lme")
ptfs_vw_beta_news[,LS:=beta_news_5 - beta_news_1]
invisible(gc())

temptable <- data.frame(matrix(-88, 4, 6))
colnames(temptable) <- c(colnames(table9_cpi))
rownames(temptable) <- c("RET", "RET_tstat", "prebeta", "postbeta")

temptable[1:2,] <- table9_cpi[3:4,]
temptable[3,] <- colMeans(ptfs_vw_beta_news[,-1], na.rm=TRUE)
temptable[4,] <- table5$V1
temp <- t(data.table(temptable[3:4,]))
colnames(temp) <- c("Prebeta", "Postbeta")
temp <- temp[1:6,]
ggplot(data.frame(temp), aes(x=Prebeta, y=Postbeta)) + geom_point(size=3.5) +
  labs(y="Post-ranking beta", x="Pre-ranking beta CPI")



###########
### NFP ###
###########

# copypaste from yr4_i7_CS_nfp

spx <- fread("itd_spx_8320.csv")
spx[,c("date","hm_prd"):=list(ymd(date),hour*60+minute)]
spxcd <- spx[hm_prd<=news_hmprd+windoww[2]]
spxcd <- spxcd[order(date), .SD[.N],by=date]
spxcd <- prd_map[spxcd, on = "date", nomatch=0]
# 150 obs lost...
spxcd[,lprd:=prd-1]
spxpd <- spx[order(date), .SD[.N],by=date]
spxpd <- prd_map[spxpd, on = "date", nomatch=0]
spx <- spxpd[,.(lprd=prd, lspx=Close)][spxcd[,.(date, prd, lprd, ymprd, Time, spx=Close, year, hm_prd)], on=.(lprd), nomatch=0]
spx[,spx_ret:=100*(log(spx)-log(lspx))]

### do it separately for both announcement types ###

unr <- bgnews[Event == "Change in Nonfarm Payrolls"]
unr[,ymprd:=year(Date)*12+month(Date)-23500]
nfp <- fread("PAYEMS.csv")
nfp[,date := ymd(DATE)]
nfp[,ymprd:=year(date)*12+month(date)-23500+1]
# +1 is to match it with the data, announced in the next month
unr <- nfp[,.(PAYEMS, ymprd)][unr, on="ymprd", nomatch=0]
unr[,surprise := 100*(((Actual - Survey)/1000)/PAYEMS)]

setnames(unr, "Date", "date")
unr <- prd_map[unr, on = "date", nomatch=0]
# lost 5 observations. weird...

# get taq with empl dates #

taq <- fread("taq5m_empl_9719.csv")
taq[,date:=ymd(date)]
taqd <- fread("TAQ_daily_9719.csv")
taqd[,date:=ymd(date)]

taq <- prd_map[,.(date, prd)][taq, on="date",nomatch=0]
taq[,lprd:=prd-1]
taqd <- prd_map[,.(date, prd)][taqd[itdprd==77], on="date", nomatch=0]

taq <- taq[hm_prd<=news_hmprd+windoww[2]]
lastprd <- taq[order(hm_prd),.SD[.N],by=c("PERMNO", "prd")]
lastprd <- lastprd[hm_prd>=(windoww[2])+news_hmprd]

# join taq with taqd to get pre-news price
taq <- taqd[,.(PERMNO, lprd=prd, lPRICE=PRICE)][lastprd, on=.(PERMNO, lprd), nomatch=0]
# 1% sample lost...
taq[,ret:=100*(PRICE-lPRICE)/lPRICE]
taq <- taq[abs(ret)<40]
taq[,ret:=NULL]
# this is supposed to omit splits and consolidations. currently I do not have a better way to do it, since
# I can not find taq div files after 2014. need to ask shuaiyu about them.

divs <- fread("crsp_divs_19.csv")
divs <- divs[,.(PERMNO, EXDT, date=ymd(EXDT), DISTCD, DIVAMT)]
divs[(PERMNO==89533)&(date == ymd("2014-11-07"))&(DISTCD==1272),DIVAMT:=-888]
divs <- divs[(DIVAMT!=-888)|is.na(DIVAMT)]

taq <- divs[taq, on = .(PERMNO, date)]
taq[is.na(DIVAMT), DIVAMT:=0]
taq[,lPRICE:=lPRICE-DIVAMT]
taq[,DIVAMT:=NULL]
taq[,ret:=100*(PRICE-lPRICE)/lPRICE]

unr <- unr[spx[,.(prd, spx_ret)], on="prd", nomatch=0]

unr_rets <- unr[,.(prd, Event, period, Time, surprise, spx_ret)][taq[,.(PERMNO, date, prd, lprd, hm_prd, ymprd, ret)], on=.(prd), nomatch=0]
setorder(unr_rets, prd, PERMNO)
setorder(unr_rets, PERMNO, prd)
unr_rets <- unr_rets[!is.na(surprise)]

#beta1 <- make_beta_param(copy(unr_rets[,.(PERMNO, surprise, ret, prd=ymprd, spx_ret)]), variable="surprise", retvar = "ret",
#                         window=beta_window, clean_window=beta_window, max_window=beta_window*1.2)
beta1 <- make_beta_param_controls(copy(unr_rets[,.(PERMNO, surprise, ret, prd=ymprd, spx_ret)]), variable="surprise",
                                  retvar = "ret", window=beta_window, clean_window=beta_window, max_window=beta_window*1.2, controls = c("spx_ret"))
# using spx controls, I will have the opposite sign. but i am not sure how to interpret controls for spx...
# beta wrt spx only during those days does not work.
setnames(beta1, "beta", "beta1")
beta1[,beta1:=winsor(beta1, trim=0.01)]
beta1[,c("ret", "spx_ret", "surprise"):=list(NULL)]
summary(beta1[,beta1])
setorder(beta1, PERMNO, prd)


#########################################################################################################################

prd_map <- fread("prd_map_19.csv")
prd_map[,date:=ymd(date)]
monthlybetas <- copy(beta1)

crsp <- fread("mCRSP_2619_p,rnm.csv")
invisible(gc())
crsp[, c("lme", "lprd"):=list(shift(me,1), shift(prd,1)), by=PERMNO]
crsp[(prd-lprd)>2,lme:=NA]
crsp <- crsp[!is.na(lme)]
crsp[,length(unique(PERMNO))]
crsp <- monthlybetas[crsp, on=.(PERMNO, prd), nomatch=0]
crsp[,length(unique(PERMNO))]
dim(crsp)

crsp[,lPRC:=shift(PRC,1), by="PERMNO"]
crsp <- crsp[!is.na(lme)][!is.na(beta1)]
crsp[,c("size"):=list(log(lme))]
crsp <- crsp[year>=1998]
dim(crsp)

dcrsp <- fread("dTAQ_beta_sp500.csv", select=c("PERMNO", "ymprd", "beta"))
dcrsp <- dcrsp[PERMNO %fin% unique(crsp[,PERMNO])][ymprd>=476]
setnames(dcrsp, "ymprd", "prd")
crsp <- dcrsp[crsp, on=.(PERMNO, prd),nomatch=0]

temp <- crsp[,.(coef=coefficients(lm(beta1~beta))[2], r2=summary(lm(beta1~beta))$r.squared), by="prd"]
summary(temp)


# 9 lines below is my attempt to get rid of 10 most volatile stocks (in ex ante sense) for each year.
# i suspect they may be driving results.
#cpcrsp <- fread("mCPCRSP_6218_unr.csv")
#cpcrsp <- cpcrsp[year>=1998]
#cpcrsp <- crsp[,.(PERMNO, prd)][cpcrsp, on=.(PERMNO, prd),nomatch=0]
#quantile(cpcrsp[,vol12m], probs=seq(0,1,0.05))
#cpcprsp <- cpcrsp[month==1]
#temp <- cpcrsp[,.(vol12m=mean(vol12m)),by=c("PERMNO", "year")]
#setorder(temp, year, PERMNO)
#styrdrop <- temp[order(vol12m),.SD[(.N-49):.N],by="year"]
#crsp <- crsp[!styrdrop[,.(PERMNO, year)], on=.(PERMNO, year)]

crsp <- crsp[EXCHCD %in% 1:3]
crsp[,length(unique(PERMNO))]
dim(crsp)
nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta1, beta, RET)]

breaks_5 <- make_bpoints_d(copy(crsp), "beta1", 5)
crsp <- unisorter_var_d(copy(crsp), "beta1", breaks_5)
breaks_5 <- make_bpoints_d(copy(crsp), "beta", 5)
crsp <- unisorter_var_d(copy(crsp), "beta", breaks_5)
#breaks_10 <- make_bpoints_d(copy(crsp), "beta1", 10)
#crsp <- unisorter_var_d(copy(crsp), "beta1", breaks_10)

crsp[order(beta1_nt_5),.(beta1=mean(beta1), beta_mkt=mean(beta,na.rm=TRUE), size=mean(size), RET=mean(RET)), by=beta1_nt_5]
crsp[order(beta_nt_5),.(beta1=mean(beta1), beta_mkt=mean(beta,na.rm=TRUE), size=mean(size), RET=mean(RET)), by=beta_nt_5]

# this is extremely crude way to eyeboll something like noisy ew characteristics.

### Beta1 ###

ptfs_vw <- characteristic_portfolios_d(copy(crsp), "beta1_nt_5", 'RET', "value", "lme")
ptfs_vw[,LS:=beta1_5 - beta1_1]
ptfs_ew <- characteristic_portfolios_d(copy(crsp), "beta1_nt_5", 'RET', "equal", "lme")
ptfs_ew[,LS:=beta1_5 - beta1_1]
invisible(gc())

ar_vw <- average_returns_calculator(data.frame(ptfs_vw[,-1]))
colnames(ar_vw) <- colnames(ptfs_vw[,-1])
ar_ew <- average_returns_calculator(data.frame(ptfs_ew[,-1]))
colnames(ar_ew) <- colnames(ptfs_ew[,-1])
table9_nfp <- rbind(ar_ew, ar_vw)
table9_nfp <- data.frame(table9_nfp)
rownames(table9_nfp) <- c("Mean ew", "T-stat ew", "Mean vw", "T-stat vw")
names(table9_nfp) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "LS")

table32_7 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table32_5 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table32_4_ <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, STR, prd)], TRUE, 2)
table32_4 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table32_3 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table32_1 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())

table10_nfp <- cbind(table32_1[11:12,1:3], table32_3[11:12,3], table32_4[11:12,3], table32_5[11:12,3], table32_7[11:12,3])
colnames(table10_nfp) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table11 <- table32_7
colnames(table5)[1] <- "Quintile"

# 


### postranking betas ###

taqp <- crsp[,.(PERMNO, ymprd=prd, beta1_nt_5, lme)][taq, on=.(PERMNO, ymprd),nomatch=0]
taqp[,prd:=NULL]
setnames(taqp, "ymprd", "prd")
ptfs_vw_ <- characteristic_portfolios_d(copy(taqp), "beta1_nt_5", 'ret', "value", "lme")
ptfs_vw_[,LS:=beta1_5 - beta1_1]

ptfs_taq <- unr[,.(prd=ymprd, date, surprise, spx_ret)][ptfs_vw_, on="prd", nomatch=0]
ptfs_taq[,coefficients(lm(beta1_1~surprise))[2]]
ptfs_taq <- melt(ptfs_taq, c("prd", 'surprise', "spx_ret", "date"))
setnames(ptfs_taq, c("variable", "value"), c("ptf", "ret"))
# table5 <- ptfs_taq[,coefficients(lm(ret~surprise))[2], by=ptf]
table5 <- ptfs_taq[,coefficients(lm(ret~surprise+spx_ret))[2], by=ptf]
table5

# stargazer(table9_nfp, summary=FALSE)
# stargazer(table10_nfp, summary=FALSE, rownames = NULL)
# stargazer(table5, summary=FALSE, rownames = NULL)


#########################################################
### try FM to control for market beta and other stuff ###
#########################################################

### Other portfolio characteristics ###

#crsp <- fread("cpcrsp62.csv")
crsp <- fread("mCPCRSP_6218.csv")
setorder(crsp, PERMNO, prd)
crsp <- crsp[!is.na(lme)]
invisible(gc())
crsp <- unique(crsp[,.(prd, PERMNO, year, month, lme, bm, op, inv, amhd, BAspr,  vol1m, vol12m, ivol_capm, ivol_ff5, MAX, 
                       beta_b, beta_bw, beta_bsw, EXCHCD, PRC, VOL, RET, SHROUT, mom11, mom122)])
crsp[,length(unique(PERMNO))]
crsp <- beta1[crsp, on=.(PERMNO, prd), nomatch=0]
crsp[,length(unique(PERMNO))]
dim(crsp)

crsp[,c("size"):=list(log(lme))]
crsp[,bm:=log(bm)]
setnames(crsp, "beta1", "beta_nfp")

breaks_5 <- make_bpoints_d(copy(crsp), "beta_nfp", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_nfp", breaks_5)

### These characteristics are from current monthly cpcrsp ###

ptfs_vw_beta_nfp <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_nfp, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'beta_nfp', "value", "lme")
ptfs_vw_beta_nfp[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_beta_nfp <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_nfp, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'beta_nfp', "equal", "lme")
ptfs_ew_beta_nfp[,LS:=beta_nfp_5 - beta_nfp_1]

ptfs_vw_size <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, size, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'size', "value", "lme")
ptfs_vw_size[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_size <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, size, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'size', "equal", "lme")
ptfs_ew_size[,LS:=beta_nfp_5 - beta_nfp_1]

ptfs_vw_bm <- characteristic_portfolios_d(copy(crsp[is.finite(bm),.(PERMNO, prd, bm, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'bm', "value", "lme")
ptfs_vw_bm[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_bm <- characteristic_portfolios_d(copy(crsp[is.finite(bm),.(PERMNO, prd, bm, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'bm', "equal", "lme")
ptfs_ew_bm[,LS:=beta_nfp_5 - beta_nfp_1]

ptfs_vw_op <- characteristic_portfolios_d(copy(crsp[is.finite(op),.(PERMNO, prd, op, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'op', "value", "lme")
ptfs_vw_op[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_op <- characteristic_portfolios_d(copy(crsp[is.finite(op),.(PERMNO, prd, op, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'op', "equal", "lme")
ptfs_ew_op[,LS:=beta_nfp_5 - beta_nfp_1]

ptfs_vw_inv <- characteristic_portfolios_d(copy(crsp[is.finite(inv),.(PERMNO, prd, inv, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'inv', "value", "lme")
ptfs_vw_inv[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_inv <- characteristic_portfolios_d(copy(crsp[is.finite(inv),.(PERMNO, prd, inv, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'inv', "equal", "lme")
ptfs_ew_inv[,LS:=beta_nfp_5 - beta_nfp_1]

ptfs_vw_mom122 <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, mom122, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'mom122', "value", "lme")
ptfs_vw_mom122[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_mom122 <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, mom122, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'mom122', "equal", "lme")
ptfs_ew_mom122[,LS:=beta_nfp_5 - beta_nfp_1]
invisible(gc())

ptfs_vw_beta <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_bw, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'beta_bw', "value", "lme")
ptfs_vw_beta[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_beta <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_bw, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'beta_bw', "equal", "lme")
ptfs_ew_beta[,LS:=beta_nfp_5 - beta_nfp_1]

ptfs_vw_vol1m <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, vol1m, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'vol1m', "value", "lme")
ptfs_vw_vol1m[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_vol1m <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, vol1m, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'vol1m', "equal", "lme")
ptfs_ew_vol1m[,LS:=beta_nfp_5 - beta_nfp_1]

ptfs_vw_vol12m <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, vol12m, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'vol12m', "value", "lme")
ptfs_vw_vol12m[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_vol12m <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, vol12m, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'vol12m', "equal", "lme")
ptfs_ew_vol12m[,LS:=beta_nfp_5 - beta_nfp_1]

ptfs_vw_BAspr <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, BAspr, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'BAspr', "value", "lme")
ptfs_vw_BAspr[,LS:=beta_nfp_5 - beta_nfp_1]
ptfs_ew_BAspr <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, BAspr, lme, beta_nfp_nt_5)]), "beta_nfp_nt_5", 'BAspr', "equal", "lme")
ptfs_ew_BAspr[,LS:=beta_nfp_5 - beta_nfp_1]
invisible(gc())

table12 <- data.frame(matrix(-88, 13, 6))
colnames(table12) <- c(colnames(table9_nfp))
rownames(table12) <- c("RET", "RET_tstat", "prebeta", "postbeta", "size", "bm", "op", "inv", "beta", "BAspr", "mom122", "vol1m", "vol12m")

table12[1:2,] <- table9_nfp[3:4,]
table12[3,] <- colMeans(ptfs_vw_beta_nfp[,-1], na.rm=TRUE)
table12[4,] <- table5$V1
table12[5,] <- colMeans(ptfs_vw_size[,-1], na.rm=TRUE)
table12[6,] <- colMeans(ptfs_vw_bm[,-1], na.rm=TRUE)
table12[7,] <- colMeans(ptfs_vw_op[,-1], na.rm=TRUE)
table12[8,] <- colMeans(ptfs_vw_inv[,-1], na.rm=TRUE)
table12[9,] <- colMeans(ptfs_vw_beta[,-1], na.rm=TRUE)
table12[10,] <- colMeans(ptfs_vw_BAspr[,-1], na.rm=TRUE)
table12[11,] <- colMeans(ptfs_vw_mom122[,-1], na.rm=TRUE)
table12[12,] <- colMeans(ptfs_vw_vol1m[,-1], na.rm=TRUE)
table12[13,] <- colMeans(ptfs_vw_vol12m[,-1], na.rm=TRUE)

table12

crsp0 <- copy(crsp)

temp <- t(data.table(table12[3:4,]))
colnames(temp) <- c("Prebeta", "Postbeta")
temp <- temp[1:6,]
ggplot(data.frame(temp), aes(x=Prebeta, y=Postbeta)) + geom_point(size=3.5) +
  labs(y="Post-ranking beta", x="Pre-ranking beta NFP")
# this is Figure 2

###########################################################################################################################
### Fama-Mac ###

# need to winsorize all the regressors first

crsp <- copy(crsp0)
crsp[,length(unique(PERMNO))]
# there must be smth wrong with code.

#tcrsp <- fread("cpcrsp62.csv")
#prd_map[,year:=year(date)]
#tcrsp <- unique(prd_map[,.(prd=ymprd, year)])[tcrsp, on="prd"]
#crsp <- tcrsp[,.(PERMNO, prd, bm=bm/1000, mom11, mom122, op, inv, fme, beta_bsw, beta_b, beta_bw, laggedme, year)][crsp, on=.(PERMNO, prd)]
#crsp[,size:=log(fme)]
#crsp <- crsp[,.(PERMNO, prd, RET, beta, size, beta_nfp, bm, mom11, mom122, op, inv, ivol_capm, MAX, beta_bsw, beta_bw, beta_b, year)]
crsp <- crsp[!is.na(beta_nfp)]
setnames(crsp, "beta_bw", "beta")

crsp[,beta_nfp:=winsor(beta_nfp, trim=0.01, na.rm = TRUE)]
crsp[,size:=winsor(size, trim=0.1, na.rm = TRUE)]
crsp[,bm:=winsor(bm, trim=0.1, na.rm = TRUE)]
crsp[,beta:=winsor(beta, trim=0.1, na.rm = TRUE)]
crsp[,mom122:=winsor(mom122, trim=0.1, na.rm = TRUE)]
crsp[,mom11:=winsor(mom11, trim=0.1, na.rm = TRUE)]
crsp[,inv:=winsor(inv, trim=0.1, na.rm = TRUE)]
crsp[,op:=winsor(op, trim=0.1, na.rm = TRUE)]
crsp[,summary(lm(RET~size))]

crsp[,length(unique(PERMNO))]
table7fm1 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_nfp)]), c("beta_nfp"), "RET")
table7fm2 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_nfp, beta)]), c("beta_nfp", "beta"), "RET")
crsp <- crsp[!is.na(bm)]
table7fm3 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_nfp, beta, size)]), c("beta_nfp", "beta", "size"), "RET")
table7fm4 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_nfp, beta, size, bm)]), c("beta_nfp", "beta", "size", "bm"), "RET")
crsp <- crsp[!is.na(inv)][is.finite(inv)][!is.na(op)]
table7fm6 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_nfp, beta, size, bm, op, inv)]), c("beta_nfp", "beta", "size", "bm", "op", "inv"), "RET")
crsp <- crsp[!is.na(mom122)]
crsp[,length(unique(PERMNO))]
table7fm7 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_nfp, beta, size, bm, mom122, op, inv, mom11)]), c("beta_nfp", "beta", "size", "bm", "op", "inv", "mom122", "mom11"), "RET")
invisible(gc())

colnames(table7fm1)[1] <- "(Intercept)"
colnames(table7fm2)[1] <- "(Intercept)"
colnames(table7fm3)[1] <- "(Intercept)"
colnames(table7fm4)[1] <- "(Intercept)"
colnames(table7fm5)[1] <- "(Intercept)"
colnames(table7fm6)[1] <- "(Intercept)"
colnames(table7fm7)[1] <- "(Intercept)"
# this step is needed to stargaze the results later. otherwise intecept will not be reported.

tableFM <- cbind(table7fm1, table7fm2, table7fm4, table7fm6, table7fm7)

table13 <- tableFM
crsp[,.N,by=prd]

#crsp[,summary(lm(RET~size))]
#crsp[,summary(lm(RET~beta_nfp))]
#crsp[,summary(lm(RET~beta_nfp+beta))]
#crsp[,summary(lm(RET~beta_nfp+beta+size+bm))]
#crsp[,summary(lm(RET~beta_nfp+beta+size+bm+inv+mom122+mom11))]

crsp[,.(sd(beta_nfp), sd(beta), sd(size), sd(bm), sd(op), sd(inv), sd(mom122), sd(mom11))]

table9_pmi
table9_retl
table9_cpi
table9_nfp
table10_pmi
table10_retl
table10_cpi
table10_nfp
table11
table12
table13

time2 <- Sys.time()
time2 - time1

stargazer(make_stars_19(table9_nfp), summary=FALSE, digits=2)
stargazer(make_stars_19(table9_pmi), summary=FALSE, digits=2)
stargazer(make_stars_19(table9_retl), summary=FALSE, digits=2)
stargazer(make_stars_19(table9_cpi), summary=FALSE, digits=2)

stargazer(make_stars_19(table10_nfp, 1, TRUE), summary=FALSE, digits=2, rownames=FALSE)
stargazer(make_stars_19(table10_pmi, 1, TRUE), summary=FALSE, digits=2, rownames=FALSE)
stargazer(make_stars_19(table10_retl, 1, TRUE), summary=FALSE, digits=2, rownames=FALSE)
stargazer(make_stars_19(table10_cpi, 1, TRUE), summary=FALSE, digits=2, rownames=FALSE)

stargazer(table11, summary=FALSE, digits=2, rownames=FALSE)
stargazer(table12, summary=FALSE, digits=2)
