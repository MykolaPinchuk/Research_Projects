### SP19, monthly returns ###
### This is SP19_rm_35_504_updplots (the latest script before 2yr paper presentation), corrected in January ###
### created on 05/03/2020 ###
### I will use monthly betas here ###

rm(list = names(sapply(ls(),function(x){object.size(get(x))})[sapply(ls(),function(x){object.size(get(x))})>1000000]))
gc()

library("data.table")
library("lubridate")
library("dplyr")
library("stargazer")
library("zoo")
library("ggplot2")
library("reshape2")
library("tidyr")
library("sandwich")
library("rollRegres")
library("psych")
library("Rcpp")
library("RcppRoll")
library("DescTools")
options("scipen"=100, "digits"=4)

#setwd("D:/SSD_19/19_res/data")
setwd("G:/Dropbox/SSD_20/data")

time1 <- Sys.time()

prd_map <- fread("daily_prd_map.csv")
prd_map[,date:=mdy(date)]
factors7 <- fread("mFF7_19.csv")
setnames(factors7, "ymprd", "prd")
ltr <- fread("LTR.csv")
ltr[,prd:=year(ymd(paste0(date, "01")))*12 + month(ymd(paste0(date, "01"))) - 23500]
factors7 <- ltr[,.(prd, LT_Rev)][factors7, on="prd"]

#######################################################################################################################################################################################################

###################
### time series ###
###################

#monthlybetas <- fread("crsp_monthlycimadbetas63_60_full_125.csv")
#monthlybetas <- fread("SP19_mCIDbeta_24_preff3.csv")
#monthlybetas <- fread("SP19_mCIDbeta_60_postff3.csv")
#monthlybetas <- fread("SP19_mCIDbeta_24_v4_vol.csv")
#monthlybetas <- fread("SP19_mSBMbeta_24_v4.csv")
monthlybetas <- fread("SP19_mCIDbeta_24_v4.csv")

crsp <- fread("mCRSP_2619_p,rnm.csv")
invisible(gc())
dim(crsp)

crsp[, c("lme", "lprd"):=list(c(NA, me[-.N]), c(NA, prd[-.N])), by=PERMNO]
crsp[(prd-lprd)>2,lme:=NA]
crsp <- crsp[!is.na(lme)]
dim(crsp)

crsp <- restrict_crsp_monthly_20(crsp, "lme", 50, TRUE)
dim(crsp)
crsp <- monthlybetas[crsp, on=.(PERMNO, prd), nomatch=0]
dim(crsp)

crsp[,lPRC:=c(NA, PRC[-.N]), by="PERMNO"]
crsp <- restrict_crsp_monthly_20(crsp, "lPRC", 5)
dim(crsp)

# previously it had selection bias:
# crsp <- crsp[abs(PRC)>10]

crsp <- crsp[!is.na(lme)][!is.na(beta_cimad)]
crsp[,c("size"):=list(log(lme))]
dim(crsp)
crsp <- crsp[prd>=63]
crsp <- crsp[prd<=728]
# not all datasetes were updated to 2019. crsp has been, but the other things have not been yet.
crsp <- crsp[EXCHCD %in% 1:3]
dim(crsp)
nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_cimad, RET)]

breaks_5 <- make_bpoints_d(copy(crsp), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5)
#breaks_10 <- make_bpoints_d(copy(crsp), "beta_cimad", 10)
#crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_10)

ptfs_vw <- characteristic_portfolios_d(copy(crsp), "beta_cimad_nt_5", 'RET', "value", "lme")
ptfs_vw[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew <- characteristic_portfolios_d(copy(crsp), "beta_cimad_nt_5", 'RET', "equal", "lme")
ptfs_ew[,LS:=beta_cimad_5 - beta_cimad_1]
#ptfs_vw <- characteristic_portfolios_d(copy(crsp), "beta_cimad_nt_10", 'RET', "value", "lme")
#ptfs_vw[,LS:=beta_cimad_10 - beta_cimad_1]
#ptfs_ew <- characteristic_portfolios_d(copy(crsp), "beta_cimad_nt_10", 'RET', "equal", "lme")
#ptfs_ew[,LS:=beta_cimad_10 - beta_cimad_1]

invisible(gc())

ar_vw <- average_returns_calculator(data.frame(ptfs_vw[,-1]))
colnames(ar_vw) <- colnames(ptfs_vw[,-1])
ar_ew <- average_returns_calculator(data.frame(ptfs_ew[,-1]))
colnames(ar_ew) <- colnames(ptfs_ew[,-1])
table3 <- rbind(ar_ew, ar_vw)
table3 <- data.frame(table3)
rownames(table3) <- c("Mean ew", "T-stat ew", "Mean vw", "T-stat vw")
names(table3) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "LS")

# stargazer(make_stars_19(table3), summary=FALSE, digits=2)

table32_8 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, LT_Rev, prd)], TRUE, 2)
table32_7 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table32_5 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table32_4_ <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, STR, prd)], TRUE, 2)
table32_4 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table32_3 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table32_1 <- BJS_regs(ptfs_vw, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())

table4 <- cbind(table32_1[11:12,1:3], table32_3[11:12,3], table32_4[11:12,3], table32_5[11:12,3], table32_7[11:12,3])
colnames(table4) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table5 <- table32_7
colnames(table5)[1] <- "Decile"

time2 <- Sys.time()
time2-time1

# stargazer(make_stars_19(table4, 1, TRUE), summary=FALSE, digits=2, rownames=FALSE)
# stargazer(table5, summary=FALSE, digits=2, rownames=FALSE)

table3
table4
table5
table32_8

crsp01 <- copy(crsp)

# fwrite(ptfs_vw, "dailycimadbetaLS.csv")

### Post-ranking betas ###

cwd <- fread("cwd.csv")
#cwd <- fread("SP19_CID_rawdifferences.csv", select = c("prd", "cimad_vwretd"))

uncertainties <- fread("monthly_uncertainty_diff.csv")
setnames(uncertainties, "ymprd", "prd")
cwd <- uncertainties[,.(prd, roll_sd24)][cwd, on="prd"]
setnames(cwd, "cimad", "cimad_vwretd")
cwd <- cwd[!is.na(roll_sd24)]
ptfs_cid5 <- melt(ptfs_vw, c("prd"))
names(ptfs_cid5) <- c("prd", "PERMNO", "RET")
ptfs_cid5 <- cwd[,.(prd, cimad_vwretd, roll_sd24)][ptfs_cid5, on="prd"]
ptfs_cid5 <- factors7[,.(prd, EMKT, SMB, HML, RMW, CMA, MOM, STR)][ptfs_cid5, on="prd"]
setorder(ptfs_cid5, PERMNO, prd)

ptfs_vw_beta_cimad <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_cimad, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'beta_cimad', "value", "lme")
ptfs_vw_beta_cimad[,LS:=beta_cimad_5 - beta_cimad_1]
prebetas <- melt(ptfs_vw_beta_cimad, c("prd"))
names(prebetas) <- c("prd", "PERMNO", "prebeta")
prebetas <- prebetas[,.(prebeta=mean(prebeta)), by="PERMNO"]

ptfs_cid5[!is.na(roll_sd24),lm(RET~cimad_vwretd+roll_sd24)$coefficients[2], by="PERMNO"]
postbetas <- ptfs_cid5[,.(RET=mean(RET), postbeta=lm(RET~cimad_vwretd)$coefficients[2], se=coef(summary(lm(RET~cimad_vwretd)))[2,2]), by="PERMNO"]
postbetas <- prebetas[postbetas, on="PERMNO"]
postbetas[,c("lower", "upper"):=list(postbeta+2*se, postbeta-2*se)]

postbetas <- postbetas[1:5]
ggplot(data.frame(postbetas), aes(x=prebeta, y=postbeta)) + geom_line(size=1, color="black") + labs(y="Post-ranking beta", x="Pre-ranking beta") + geom_point(size=2) +
  geom_abline(aes(intercept=0, slope=0)) + geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2)


#########################
### Time-series tests ###
#########################
# wrong! I should use levels, not differences!

uncertainties <- fread("monthly_uncertainty_diff.csv")
market <- factors7[,.(date, prd, vwretd, EMKT)]
market <- uncertainties[,.(prd=ymprd, vix, fu_12, mu_12, roll_sd12, roll_sd24, roll_sd21f, civ)][market, on="prd"]
cwd <- fread("cid_level.csv")
market <- cwd[,.(prd, cisd, cimad, cimad_vwretd)][market, on="prd"]
market <- market[!is.na(EMKT)]
market[, vwretd_f12 :=  100*((Reduce(`*`, shift(vwretd/100+1, 1:12, type = "lead")))-1)]
market[, vwretd_f24 :=  100*((Reduce(`*`, shift(vwretd/100+1, 1:24, type = "lead")))-1)]
market[, vwretd_f36 :=  100*((Reduce(`*`, shift(vwretd/100+1, 1:36, type = "lead")))-1)]
market[, vwretd_f60 :=  100*((Reduce(`*`, shift(vwretd/100+1, 1:60, type = "lead")))-1)]

(lm(vwretd_f12 ~ cimad_vwretd, market[prd>=-63]))$coefficients
(lm(vwretd_f36 ~ cimad_vwretd, market[prd>=-63]))$coefficients
(lm(vwretd_f12 ~ cimad_vwretd + roll_sd21f, market[prd>=-63]))$coefficients
(lm(vwretd_f12 ~ cimad_vwretd + vwretd + roll_sd24 + civ + mu_12, market[prd>=-63]))$coefficients

summary(lm(vwretd_f12 ~ cimad_vwretd, market[prd>=-63]))
summary(lm(vwretd_f36 ~ cimad_vwretd, market[(prd>=-63)]))
summary(lm(vwretd_f36 ~ cimad_vwretd, market[(prd>=260)&(prd<=476)]))




# notice that I have not used nw for overlapping periods. So to be significant, the results above need t-stat at least 4.
# so the coefficient is not significantly different from 0. maybe marginally negative.
# anyway, it does not support my story.



#######################################
### Other portfolio characteristics ###
#######################################
# here I will need cpcrsp file with bunch of variables. do not forget pre and postformation betas. 

crsp <- fread("cpcrsp62.csv")
invisible(gc())
dim(crsp)
crsp <- unique(prd_map[,.(prd=ymprd, year=year(date))])[crsp, on="prd"]
crsp[,laggedme := NULL]

crsp[, c("lme", "lprd"):=list(c(NA, me[-.N]), c(NA, prd[-.N])), by=PERMNO]
crsp[(prd-lprd)>2,lme:=NA]
crsp <- crsp[!is.na(lme)]

crsp <- restrict_crsp_monthly_20(crsp, "lme", 50, TRUE)
dim(crsp)
crsp <- monthlybetas[crsp, on=.(PERMNO, prd), nomatch=0]
dim(crsp)

crsp[,lPRC:=c(NA, PRC[-.N]), by="PERMNO"]
crsp <- restrict_crsp_monthly_20(crsp, "lPRC", 5)
dim(crsp)

crsp[,c("size"):=list(log(lme))]
dim(crsp)
invisible(gc())
crsp[,c("RET"):=list(100*RET)]
crsp[,bm:=log(bm)]




breaks_5 <- make_bpoints_d(copy(crsp), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5)
invisible(gc())

### These characteristics are from current monthly cpcrsp ###

ptfs_vw_beta_cimad <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_cimad, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'beta_cimad', "value", "lme")
ptfs_vw_beta_cimad[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_beta_cimad <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta_cimad, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'beta_cimad', "equal", "lme")
ptfs_ew_beta_cimad[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

ptfs_vw_size <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, size, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'size', "value", "lme")
ptfs_vw_size[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_size <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, size, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'size', "equal", "lme")
ptfs_ew_size[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

ptfs_vw_bm <- characteristic_portfolios_d(copy(crsp[is.finite(bm),.(PERMNO, prd, bm, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'bm', "value", "lme")
ptfs_vw_bm[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_bm <- characteristic_portfolios_d(copy(crsp[is.finite(bm),.(PERMNO, prd, bm, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'bm', "equal", "lme")
ptfs_ew_bm[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

ptfs_vw_op <- characteristic_portfolios_d(copy(crsp[is.finite(op),.(PERMNO, prd, op, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'op', "value", "lme")
ptfs_vw_op[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_op <- characteristic_portfolios_d(copy(crsp[is.finite(op),.(PERMNO, prd, op, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'op', "equal", "lme")
ptfs_ew_op[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

ptfs_vw_inv <- characteristic_portfolios_d(copy(crsp[is.finite(inv),.(PERMNO, prd, inv, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'inv', "value", "lme")
ptfs_vw_inv[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_inv <- characteristic_portfolios_d(copy(crsp[is.finite(inv),.(PERMNO, prd, inv, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'inv', "equal", "lme")
ptfs_ew_inv[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

ptfs_vw_mom122 <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, mom122, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'mom122', "value", "lme")
ptfs_vw_mom122[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_mom122 <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, mom122, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'mom122', "equal", "lme")
ptfs_ew_mom122[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())



### these characteristics will be taken from daily cpcrsp, used for SP19_rd ###
# there is minor problem - that file is restricted by me10 (i think). in the future should create these variables in a cocnsistent way using the path, leading to "cpcrspm_0M.csv".

tcrsp <- fread("crsp_dailyvars.csv", select=c("PERMNO", "prd", "ivol_capm", "MAX", "beta_bw", "vol12m", "vol1m"))
# dailyvars does not seem to contain major selection biases, but I am not 100% sure.
dim(tcrsp)
liqvar <- fread("crsp_liquidity.csv")
# use this data carefully. it is restricted by PRC>5 expost, so it contains selection bias. at some point have to update it properly.
liqvar <- liqvar[,.(amhd252=mean(amhd252, na.rm=TRUE), amhd63=mean(amhd63, na.rm=TRUE), BAspread=mean(BAspread, na.rm=TRUE)*100), by=c("PERMNO", "ymprd")]
setnames(liqvar, "ymprd", "prd")
tcrsp <- liqvar[tcrsp, on=.(PERMNO, prd)]
crsp <- tcrsp[crsp[,.(PERMNO, prd, RET, lme, beta_cimad_nt_5, beta_cimad)], on=.(PERMNO, prd)]
setnames(crsp, c("beta_bw"), c("beta"))


ptfs_vw_beta <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'beta', "value", "lme")
ptfs_vw_beta[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_beta <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, beta, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'beta', "equal", "lme")
ptfs_ew_beta[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

ptfs_vw_vol1m <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, vol1m, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'vol1m', "value", "lme")
ptfs_vw_vol1m[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_vol1m <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, vol1m, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'vol1m', "equal", "lme")
ptfs_ew_vol1m[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

ptfs_vw_vol12m <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, vol12m, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'vol12m', "value", "lme")
ptfs_vw_vol12m[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_vol12m <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, vol12m, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'vol12m', "equal", "lme")
ptfs_ew_vol12m[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

ptfs_vw_BAspread <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, BAspread, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'BAspread', "value", "lme")
ptfs_vw_BAspread[,LS:=beta_cimad_5 - beta_cimad_1]
ptfs_ew_BAspread <- characteristic_portfolios_d(copy(crsp[,.(PERMNO, prd, BAspread, lme, beta_cimad_nt_5)]), "beta_cimad_nt_5", 'BAspread', "equal", "lme")
ptfs_ew_BAspread[,LS:=beta_cimad_5 - beta_cimad_1]
invisible(gc())

### calculate post-ranking betas ###
# monthly-frequency postranking betas really suck (see v14), use daily postarnking betas from SP19_rd_8.

table2 <- data.frame(matrix(-88, 13, 6))
colnames(table2) <- c(colnames(table3))
rownames(table2) <- c("RET", "RET_tstat", "prebeta", "postbeta", "size", "bm", "op", "inv", "beta", "BAspr", "mom122", "vol1m", "vol12m")

table2[1:2,] <- table3[3:4,]
table2[3,] <- colMeans(ptfs_vw_beta_cimad[,-1], na.rm=TRUE)
table2[4,] <- postbetas$postbeta
table2[5,] <- colMeans(ptfs_vw_size[,-1], na.rm=TRUE)
table2[6,] <- colMeans(ptfs_vw_bm[,-1], na.rm=TRUE)
table2[7,] <- colMeans(ptfs_vw_op[,-1], na.rm=TRUE)
table2[8,] <- colMeans(ptfs_vw_inv[,-1], na.rm=TRUE)
table2[9,] <- colMeans(ptfs_vw_beta[,-1], na.rm=TRUE)
table2[10,] <- colMeans(ptfs_vw_BAspread[,-1], na.rm=TRUE)
table2[11,] <- colMeans(ptfs_vw_mom122[,-1], na.rm=TRUE)
table2[12,] <- colMeans(ptfs_vw_vol1m[,-1], na.rm=TRUE)
table2[13,] <- colMeans(ptfs_vw_vol12m[,-1], na.rm=TRUE)

table2

# stargazer(table2, summary=FALSE, digits=2)

time2 <- Sys.time()
time2-time1


crsp0 <- copy(crsp)

temp <- t(data.table(table2[3:4]))
colnames(temp) <- c("Prebeta", "Postbeta")
temp <- temp[1:6,]

ggplot(data.frame(temp), aes(x=Prebeta, y=Postbeta)) + geom_point(size=3.5) + labs(y="Post-ranking beta", x="Pre-ranking beta")
# this is Figure 2

###########################################################################################################################

### Fama-Mac ###

# need to winsorize all the regressors first

crsp <- copy(crsp0)

#tcrsp <- fread("cpcrspm_0M.csv")
tcrsp <- fread("cpcrsp62.csv")
prd_map[,year:=year(date)]
tcrsp <- unique(prd_map[,.(prd=ymprd, year)])[tcrsp, on="prd"]
crsp <- tcrsp[,.(PERMNO, prd, bm=bm/1000, mom11, mom122, op, inv, fme, beta_bsw, beta_b, beta_bw, laggedme, year)][crsp, on=.(PERMNO, prd)]
crsp[,size:=log(fme)]
crsp <- crsp[,.(PERMNO, prd, RET, beta, size, beta_cimad, bm, mom11, mom122, op, inv, ivol_capm, MAX, beta_bsw, beta_bw, beta_b, year)]
crsp <- crsp[!is.na(beta)]

crsp[,beta_cimad:=winsor(beta_cimad, trim=0.1, na.rm = TRUE)]
crsp[,size:=winsor(size, trim=0.10, na.rm = TRUE)]
crsp[,bm:=winsor(bm, trim=0.10, na.rm = TRUE)]
crsp[,beta:=winsor(beta, trim=0.10, na.rm = TRUE)]
crsp[,mom122:=winsor(mom122, trim=0.10, na.rm = TRUE)]
crsp[,mom11:=winsor(mom11, trim=0.10, na.rm = TRUE)]
crsp[,inv:=winsor(inv, trim=0.10, na.rm = TRUE)]
crsp[,op:=winsor(op, trim=0.10, na.rm = TRUE)]
crsp[,mom11:=winsor(mom11, trim=0.10, na.rm = TRUE)]

table7fm1 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_cimad)]), c("beta_cimad"), "RET")
table7fm2 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_cimad, beta)]), c("beta_cimad", "beta"), "RET")
crsp <- crsp[!is.na(bm)][bm>0]
crsp[,logbm:=log(bm)]
table7fm3 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_cimad, beta, size)]), c("beta_cimad", "beta", "size"), "RET")
table7fm4 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_cimad, beta, size, logbm)]), c("beta_cimad", "beta", "size", "logbm"), "RET")
crsp <- crsp[!is.na(mom122)]
table7fm5 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_cimad, beta, size, logbm, mom122)]), c("beta_cimad", "beta", "size", "logbm", "mom122"), "RET")
crsp <- crsp[!is.na(inv)][is.finite(inv)]
table7fm6 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_cimad, beta, size, logbm, mom122, inv)]), c("beta_cimad", "beta", "size", "logbm", "mom122", "inv"), "RET")
crsp <- crsp[!is.na(MAX)]
table7fm7 <- fama_macbeth_simple_se_19_d_r2(copy(crsp[,.(PERMNO, prd, RET, beta_cimad, beta, size, logbm, mom122, inv, MAX)]), c("beta_cimad", "beta", "size", "logbm", "mom122", "inv", "MAX"), "RET")
invisible(gc())

colnames(table7fm1)[1] <- "(Intercept)"
colnames(table7fm2)[1] <- "(Intercept)"
colnames(table7fm3)[1] <- "(Intercept)"
colnames(table7fm4)[1] <- "(Intercept)"
colnames(table7fm5)[1] <- "(Intercept)"
colnames(table7fm6)[1] <- "(Intercept)"
colnames(table7fm7)[1] <- "(Intercept)"
# this step is needed to stargaze the results later. otherwise intecept will not be reported.

tableFM <- cbind(table7fm1, table7fm2, table7fm3, table7fm4,
                 table7fm5, table7fm6, table7fm7)

table7 <- tableFM
table7

crsp[,.(sd(beta_cimad))]

# the code below prepares the ols template for stargazing #
crsp <- copy(crsp0)

tcrsp <- fread("cpcrsp62.csv")
prd_map[,year:=year(date)]
tcrsp <- unique(prd_map[,.(prd=ymprd, year)])[tcrsp, on="prd"]
crsp <- tcrsp[,.(PERMNO, prd, bm, mom11, mom122, op, inv, fme, beta_bsw, beta_b, beta_bw, laggedme, year)][crsp, on=.(PERMNO, prd)]
crsp[,logbm:=log(bm)]
crsp[,size:=log(fme)]
crsp <- crsp[,.(PERMNO, prd, RET, beta, size, beta_cimad, logbm, bm, mom11, mom122, op, inv, ivol_capm, MAX, beta_bsw, beta_bw, beta_b, year)]
crsp <- crsp[!is.na(beta)]

m1 <- lm(RET~beta_cimad, crsp)
m2 <- lm(RET~beta_cimad+beta, crsp)
m3 <- lm(RET~beta_cimad+beta+size, crsp)
crsp <- crsp[!is.na(bm)][bm>0]
m4 <- lm(RET~beta_cimad+beta+size+logbm, crsp)
crsp <- crsp[!is.na(mom122)]
m5 <- lm(RET~beta_cimad+beta+size+logbm+mom122, crsp)
crsp <- crsp[!is.na(inv)][is.finite(inv)]
m6 <- lm(RET~beta_cimad+beta+size+logbm+mom122+inv, crsp)
crsp <- crsp[!is.na(MAX)]
m7 <- lm(RET~beta_cimad+beta+size+logbm+mom122+inv+MAX, crsp)

stargazer(m1, m2, m3, m4, m5, m6, m7,  
          coef = list(table7fm1[1,], table7fm2[1,], table7fm3[1,], table7fm4[1,], table7fm5[1,], table7fm6[1,], table7fm7[1,]),
          se = list(table7fm1[2,], table7fm2[2,], table7fm3[2,], table7fm4[2,], table7fm5[2,], table7fm6[2,], table7fm7[2,]),
          dep.var.labels.include = FALSE, report=('vc*t'), digits = 2)

time3 <- Sys.time()
time3-time1


###################
### Doublesorts ###
###################
# here data is not updated to 2019 yet.

crsp <- copy(crsp01)

#unibetas <- fread("crsp_dailycimadbetas63_504_full_15.csv")
betas <- fread("SP19_mCIDbeta_24_v4.csv")
uncbetas <- fread("monthly_unc_betas24.csv")
crsp <- uncbetas[crsp, on=.(PERMNO, prd)]

### csd vs cid ###

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_csmad, beta_cimad)]

breaks_5cs <- make_bpoints_d(copy(crsp), "beta_csmad", 5)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, beta_csmad, beta_cimad, lme)]), "beta_csmad", breaks_5cs)
invisible(gc())
breaks_5ci <- make_bpoints_d(copy(crsp), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "beta_csmad", "beta_cimad", "RET", c(5,5))
portfolios_csci[,c("LScs", "LSci"):=list(0.2*(beta_csmad1beta_cimad1RET+beta_csmad1beta_cimad2RET+beta_csmad1beta_cimad3RET+beta_csmad1beta_cimad4RET+beta_csmad1beta_cimad5RET)-
                                           0.2*(beta_csmad5beta_cimad1RET+beta_csmad5beta_cimad2RET+beta_csmad5beta_cimad3RET+beta_csmad5beta_cimad4RET+beta_csmad5beta_cimad5RET),
                                         0.2*(beta_csmad1beta_cimad1RET+beta_csmad2beta_cimad1RET+beta_csmad3beta_cimad1RET+beta_csmad4beta_cimad1RET+beta_csmad5beta_cimad1RET)-
                                           0.2*(beta_csmad1beta_cimad5RET+beta_csmad2beta_cimad5RET+beta_csmad3beta_cimad5RET+beta_csmad4beta_cimad5RET+beta_csmad5beta_cimad5RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table9 <- cbind(table31_1[51:54,1:3], table31_3[51:54,3], table31_4[51:54,3], table31_5[51:54,3], table31_7[51:54,3])
table9[,1] <- c("L/S CSD", "T-stat", "L/S CID", "T-stat")
colnames(table9) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table9

# stargazer(make_stars_BJS(table9), summary=FALSE, digits=2, rownames = FALSE)

crsp <- copy(crsp01)

### wid vs cid ###

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_wimad, beta_cimad)]

breaks_5cs <- make_bpoints_d(copy(crsp), "beta_wimad", 5)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, beta_wimad, beta_cimad, lme)]), "beta_wimad", breaks_5cs)
invisible(gc())
breaks_5ci <- make_bpoints_d(copy(crsp), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "beta_wimad", "beta_cimad", "RET", c(5,5))
portfolios_csci[,c("LScs", "LSci"):=list(0.2*(beta_wimad1beta_cimad1RET+beta_wimad1beta_cimad2RET+beta_wimad1beta_cimad3RET+beta_wimad1beta_cimad4RET+beta_wimad1beta_cimad5RET)-
                                           0.2*(beta_wimad5beta_cimad1RET+beta_wimad5beta_cimad2RET+beta_wimad5beta_cimad3RET+beta_wimad5beta_cimad4RET+beta_wimad5beta_cimad5RET),
                                         0.2*(beta_wimad1beta_cimad1RET+beta_wimad2beta_cimad1RET+beta_wimad3beta_cimad1RET+beta_wimad4beta_cimad1RET+beta_wimad5beta_cimad1RET)-
                                           0.2*(beta_wimad1beta_cimad5RET+beta_wimad2beta_cimad5RET+beta_wimad3beta_cimad5RET+beta_wimad4beta_cimad5RET+beta_wimad5beta_cimad5RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table8 <- cbind(table31_1[51:54,1:3], table31_3[51:54,1:3], table31_4[51:54,1:3], table31_5[51:54,1:3], table31_7[51:54,1:3])
table8[,1] <- c("L/S WID", "T-stat", "L/S CID", "T-stat")
table8 <- table8[,c(1:3,6,9,12,15)]
colnames(table8) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table8

# stargazer(make_stars_BJS(table8), summary=FALSE, digits=2, rownames = FALSE)

### nvix vs cid ###

crsp <- copy(crsp01)
uncbetas <- fread("monthly_unc_betas24_nvix.csv")
crsp <- uncbetas[crsp, on=.(PERMNO, prd)]

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_nvix, beta_cimad)]

breaks_5cs <- make_bpoints_d(copy(crsp), "beta_nvix", 5)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, beta_nvix, beta_cimad, lme)]), "beta_nvix", breaks_5cs)
invisible(gc())
breaks_5ci <- make_bpoints_d(copy(crsp), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "beta_nvix", "beta_cimad", "RET", c(5,5))
portfolios_csci[,c("LScs", "LSci"):=list(0.2*(beta_nvix1beta_cimad1RET+beta_nvix1beta_cimad2RET+beta_nvix1beta_cimad3RET+beta_nvix1beta_cimad4RET+beta_nvix1beta_cimad5RET)-
                                           0.2*(beta_nvix5beta_cimad1RET+beta_nvix5beta_cimad2RET+beta_nvix5beta_cimad3RET+beta_nvix5beta_cimad4RET+beta_nvix5beta_cimad5RET),
                                         0.2*(beta_nvix1beta_cimad1RET+beta_nvix2beta_cimad1RET+beta_nvix3beta_cimad1RET+beta_nvix4beta_cimad1RET+beta_nvix5beta_cimad1RET)-
                                           0.2*(beta_nvix1beta_cimad5RET+beta_nvix2beta_cimad5RET+beta_nvix3beta_cimad5RET+beta_nvix4beta_cimad5RET+beta_nvix5beta_cimad5RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table10a <- cbind(table31_1[51:54,1:3], table31_3[51:54,1:3], table31_4[51:54,1:3], table31_5[51:54,1:3], table31_7[51:54,1:3])
table10a[,1] <- c("L/S nvix", "T-stat", "L/S CID", "T-stat")
table10a <- table10a[,c(1:3,6,9,12,15)]
colnames(table10a) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table10a

# stargazer(make_stars_BJS(table10a), summary=FALSE, digits=2, rownames = FALSE)


### civ vs cid ###

crsp <- copy(crsp01)
uncbetas <- fread("monthly_unc_betas24.csv")
crsp <- uncbetas[crsp, on=.(PERMNO, prd)]

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_civ, beta_cimad)]

breaks_5cs <- make_bpoints_d(copy(nyse), "beta_civ", 5)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, beta_civ, beta_cimad, lme)]), "beta_civ", breaks_5cs)
invisible(gc())
breaks_5ci <- make_bpoints_d(copy(nyse), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "beta_civ", "beta_cimad", "RET", c(5,5))
portfolios_csci[,c("LScs", "LSci"):=list(0.2*(beta_civ1beta_cimad1RET+beta_civ1beta_cimad2RET+beta_civ1beta_cimad3RET+beta_civ1beta_cimad4RET+beta_civ1beta_cimad5RET)-
                                           0.2*(beta_civ5beta_cimad1RET+beta_civ5beta_cimad2RET+beta_civ5beta_cimad3RET+beta_civ5beta_cimad4RET+beta_civ5beta_cimad5RET),
                                         0.2*(beta_civ1beta_cimad1RET+beta_civ2beta_cimad1RET+beta_civ3beta_cimad1RET+beta_civ4beta_cimad1RET+beta_civ5beta_cimad1RET)-
                                           0.2*(beta_civ1beta_cimad5RET+beta_civ2beta_cimad5RET+beta_civ3beta_cimad5RET+beta_civ4beta_cimad5RET+beta_civ5beta_cimad5RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table10b <- cbind(table31_1[51:54,1:3], table31_3[51:54,1:3], table31_4[51:54,1:3], table31_5[51:54,1:3], table31_7[51:54,1:3])
table10b[,1] <- c("L/S CIV", "T-stat", "L/S CID", "T-stat")
table10b <- table10b[,c(1:3,6,9,12,15)]
colnames(table10b) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table10b
table11 <- table10b

# stargazer(make_stars_BJS(table10b), summary=FALSE, digits=2, rownames = FALSE)



### mu_12 vs cid ###

crsp <- copy(crsp01)
uncbetas <- fread("monthly_unc_betas24.csv")
crsp <- uncbetas[crsp, on=.(PERMNO, prd)]

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_mu_12, beta_cimad)]

breaks_5cs <- make_bpoints_d(copy(nyse), "beta_mu_12", 5)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, beta_mu_12, beta_cimad, lme)]), "beta_mu_12", breaks_5cs)
invisible(gc())
breaks_5ci <- make_bpoints_d(copy(nyse), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "beta_mu_12", "beta_cimad", "RET", c(5,5))
portfolios_csci[,c("LScs", "LSci"):=list(0.2*(beta_mu_121beta_cimad1RET+beta_mu_121beta_cimad2RET+beta_mu_121beta_cimad3RET+beta_mu_121beta_cimad4RET+beta_mu_121beta_cimad5RET)-
                                           0.2*(beta_mu_125beta_cimad1RET+beta_mu_125beta_cimad2RET+beta_mu_125beta_cimad3RET+beta_mu_125beta_cimad4RET+beta_mu_125beta_cimad5RET),
                                         0.2*(beta_mu_121beta_cimad1RET+beta_mu_122beta_cimad1RET+beta_mu_123beta_cimad1RET+beta_mu_124beta_cimad1RET+beta_mu_125beta_cimad1RET)-
                                           0.2*(beta_mu_121beta_cimad5RET+beta_mu_122beta_cimad5RET+beta_mu_123beta_cimad5RET+beta_mu_124beta_cimad5RET+beta_mu_125beta_cimad5RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table10c <- cbind(table31_1[51:54,1:3], table31_3[51:54,1:3], table31_4[51:54,1:3], table31_5[51:54,1:3], table31_7[51:54,1:3])
table10c[,1] <- c("L/S MU", "T-stat", "L/S CID", "T-stat")
table10c <- table10c[,c(1:3,6,9,12,15)]
colnames(table10c) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table10c

# stargazer(make_stars_BJS(table10c), summary=FALSE, digits=2, rownames = FALSE)


### fu_12 vs cid ###

crsp <- copy(crsp01)
uncbetas <- fread("monthly_unc_betas24.csv")
crsp <- uncbetas[crsp, on=.(PERMNO, prd)]

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_fu_12, beta_cimad)]

breaks_5cs <- make_bpoints_d(copy(nyse), "beta_fu_12", 5)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, beta_fu_12, beta_cimad, lme)]), "beta_fu_12", breaks_5cs)
invisible(gc())
breaks_5ci <- make_bpoints_d(copy(nyse), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "beta_fu_12", "beta_cimad", "RET", c(5,5))
portfolios_csci[,c("LScs", "LSci"):=list(0.2*(beta_fu_121beta_cimad1RET+beta_fu_121beta_cimad2RET+beta_fu_121beta_cimad3RET+beta_fu_121beta_cimad4RET+beta_fu_121beta_cimad5RET)-
                                           0.2*(beta_fu_125beta_cimad1RET+beta_fu_125beta_cimad2RET+beta_fu_125beta_cimad3RET+beta_fu_125beta_cimad4RET+beta_fu_125beta_cimad5RET),
                                         0.2*(beta_fu_121beta_cimad1RET+beta_fu_122beta_cimad1RET+beta_fu_123beta_cimad1RET+beta_fu_124beta_cimad1RET+beta_fu_125beta_cimad1RET)-
                                           0.2*(beta_fu_121beta_cimad5RET+beta_fu_122beta_cimad5RET+beta_fu_123beta_cimad5RET+beta_fu_124beta_cimad5RET+beta_fu_125beta_cimad5RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table10d <- cbind(table31_1[51:54,1:3], table31_3[51:54,1:3], table31_4[51:54,1:3], table31_5[51:54,1:3], table31_7[51:54,1:3])
table10d[,1] <- c("L/S FU", "T-stat", "L/S CID", "T-stat")
table10d <- table10d[,c(1:3,6,9,12,15)]
colnames(table10d) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table10d

# stargazer(make_stars_BJS(table10d), summary=FALSE, digits=2, rownames = FALSE)



### roll_sd24 vs cid ###

crsp <- copy(crsp01)
uncbetas <- fread("monthly_unc_betas24.csv")
crsp <- uncbetas[crsp, on=.(PERMNO, prd)]

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, beta_roll_sd24, beta_cimad)]

breaks_5cs <- make_bpoints_d(copy(nyse), "beta_roll_sd24", 5)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, beta_roll_sd24, beta_cimad, lme)]), "beta_roll_sd24", breaks_5cs)
invisible(gc())
breaks_5ci <- make_bpoints_d(copy(nyse), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "beta_roll_sd24", "beta_cimad", "RET", c(5,5))
portfolios_csci[,c("LScs", "LSci"):=list(0.2*(beta_roll_sd241beta_cimad1RET+beta_roll_sd241beta_cimad2RET+beta_roll_sd241beta_cimad3RET+beta_roll_sd241beta_cimad4RET+beta_roll_sd241beta_cimad5RET)-
                                           0.2*(beta_roll_sd245beta_cimad1RET+beta_roll_sd245beta_cimad2RET+beta_roll_sd245beta_cimad3RET+beta_roll_sd245beta_cimad4RET+beta_roll_sd245beta_cimad5RET),
                                         0.2*(beta_roll_sd241beta_cimad1RET+beta_roll_sd242beta_cimad1RET+beta_roll_sd243beta_cimad1RET+beta_roll_sd244beta_cimad1RET+beta_roll_sd245beta_cimad1RET)-
                                           0.2*(beta_roll_sd241beta_cimad5RET+beta_roll_sd242beta_cimad5RET+beta_roll_sd243beta_cimad5RET+beta_roll_sd244beta_cimad5RET+beta_roll_sd245beta_cimad5RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table10e <- cbind(table31_1[51:54,1:3], table31_3[51:54,1:3], table31_4[51:54,1:3], table31_5[51:54,1:3], table31_7[51:54,1:3])
table10e[,1] <- c("L/S VOL", "T-stat", "L/S CID", "T-stat")
table10e <- table10e[,c(1:3,6,9,12,15)]
colnames(table10e) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table10e

# stargazer(make_stars_BJS(table10e), summary=FALSE, digits=2, rownames = FALSE)



##############################
### Create CID-factor, 2x5 ###
##############################

crsp <- copy(crsp01)

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, size, beta_cimad)]

breaks_2cs <- make_bpoints_d(copy(nyse), "size", 2)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, size, beta_cimad, lme)]), "size", breaks_2cs)
invisible(gc())
breaks_5ci <- make_bpoints_d(copy(nyse), "beta_cimad", 5)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_5ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "size", "beta_cimad", "RET", c(2,5))
portfolios_csci[,c("LScs", "LSci"):=list(0.2*(size1beta_cimad1RET+size1beta_cimad2RET+size1beta_cimad3RET+size1beta_cimad4RET+size1beta_cimad5RET)-
                                           0.2*(size2beta_cimad1RET+size2beta_cimad2RET+size2beta_cimad3RET+size2beta_cimad4RET+size2beta_cimad5RET),
                                         0.5*(size1beta_cimad1RET+size2beta_cimad1RET)-
                                           0.5*(size1beta_cimad5RET+size2beta_cimad5RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table6b <- cbind(table31_1[21:24,1:3], table31_3[21:24,3], table31_4[21:24,3], table31_5[21:24,3], table31_7[21:24,3])
table6b[,1] <- c("L/S Size", "T-stat", "L/S CID", "T-stat")
colnames(table6b) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table6b

# stargazer(make_stars_BJS(table6b), summary=FALSE, digits=2, rownames = FALSE)



##############################
### Create CID-factor, 2x3 ###
##############################

crsp <- copy(crsp01)

nyse <- crsp[EXCHCD %in% c(1,31), .(PERMNO, prd, size, beta_cimad)]

breaks_2cs <- make_bpoints_d(copy(nyse), "size", 2)
crsp <- unisorter_var_d(copy(crsp[,.(PERMNO, prd, RET, size, beta_cimad, lme)]), "size", breaks_2cs)
invisible(gc())
breaks_3ci <- make_bpoints_d(copy(nyse), "beta_cimad", 3)
crsp <- unisorter_var_d(copy(crsp), "beta_cimad", breaks_3ci)
invisible(gc())

portfolios_csci <- doublesorted_portfolios_dt(copy(crsp), "size", "beta_cimad", "RET", c(2,3))
portfolios_csci[,c("LScs", "LSci"):=list(0.33*(size1beta_cimad1RET+size1beta_cimad2RET+size1beta_cimad3RET)-
                                           0.33*(size2beta_cimad1RET+size2beta_cimad2RET+size2beta_cimad3RET),
                                         0.5*(size1beta_cimad1RET+size2beta_cimad1RET)-
                                           0.5*(size1beta_cimad3RET+size2beta_cimad3RET))]
ar <- average_returns_calculator(data.frame(portfolios_csci[,-1]))
colnames(ar) <- colnames(portfolios_csci[,-1])
invisible(gc())

table31_7 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, MOM, STR, prd)], TRUE, 2)
table31_5 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, RMW, CMA, prd)], TRUE, 2)
table31_4 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, MOM, prd)], TRUE, 2)
table31_3 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, HML, SMB, prd)], TRUE, 2)
table31_1 <- BJS_regs(portfolios_csci, factors7[,.(EMKT, prd)], TRUE, 2)
invisible(gc())
table6a <- cbind(table31_1[13:16,1:3], table31_3[13:16,3], table31_4[13:16,3], table31_5[13:16,3], table31_7[13:16,3])
table6a[,1] <- c("L/S Size", "T-stat", "L/S CID", "T-stat")
colnames(table6a) <- c("Statistic", "Ret", "Alpha,CAPM", "Alpha,FF3", "Alpha,Carhart", "Alpha,FF5", "Alpha,FF5+UMD+STR")
table6a

# stargazer(make_stars_BJS(table6a), summary=FALSE, digits=2, rownames = FALSE)


ptfs_vw0 <- copy(ptfs_vw)

ptfs_vw <- unique(prd_map[,.(prd=ymprd, date)], by="prd")[ptfs_vw, on="prd"]
ptfs_vw[,LS:=1+(-LS)/100]
setorder(ptfs_vw, prd)
ptfs_vw[,cumLS:=cumprod(LS)]

plot(ptfs_vw$date, ptfs_vw$cumLS, type="l", xlab="Year", ylab="$", lwd = 1.75, log="y")
#plot(ptfs_vw$date, log(ptfs_vw$cumLS), type="l", main="Value of $1 in L/S portfolio, log scale", xlab="Year", ylab="$", lwd = 1.75)

ggplot(data.frame(ptfs_vw), aes(x=date, y=cumLS)) + geom_line(size=0.5) + labs(y="$", x="Year") + scale_y_continuous(trans='log10')


#table2
#table3
#table4
#table5
#table6a
#table7
#table8
#table9
#table10a
#table10c
#table10d
#table10e
#table11



stargazer(table2, summary=FALSE, digits=2)
stargazer(make_stars_BJS(format(round(table3,2),2)), summary=FALSE, digits=2)
stargazer(make_stars_BJS(table4,1), summary=FALSE, digits=2, rownames=FALSE)
stargazer(table5, summary=FALSE, digits=2, rownames=FALSE)
stargazer(make_stars_BJS(table6a), summary=FALSE, digits=2, rownames = FALSE)

stargazer(make_stars_BJS(table8), summary=FALSE, digits=2, rownames = FALSE)
stargazer(make_stars_BJS(table9), summary=FALSE, digits=2, rownames = FALSE)
stargazer(make_stars_BJS(table10a), summary=FALSE, digits=2, rownames = FALSE)
stargazer(make_stars_BJS(table10c), summary=FALSE, digits=2, rownames = FALSE)
stargazer(make_stars_BJS(table10d), summary=FALSE, digits=2, rownames = FALSE)
stargazer(make_stars_BJS(table10e), summary=FALSE, digits=2, rownames = FALSE)
stargazer(make_stars_BJS(table11), summary=FALSE, digits=2, rownames = FALSE)

time4 <- Sys.time()
time4-time1



