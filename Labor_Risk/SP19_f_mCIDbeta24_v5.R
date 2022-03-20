### this script estimates monthly betas of individual stocks for SP19_20 ###
# it is based upon sp19_dailycimad_betas_14_monthly_5 #
# if want to run the full script, then must have at least 20gb free ram.
# this script is a successor of sp19_dailycimad_betas_9_252_fromfullcrsp_, most recently of _betas_10upd.

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

indices <- fread("mFF7_19.csv")
setnames(indices, "ymprd", "prd")
factors7 <- indices

### lets explore industry portfolios a bit ###

nfirms <- fread("49ff_nfirms_20.csv")
nfirms[,date:=ymd(paste0(date, "01"))]
nfirms[,c("year", "month"):=list(year(date), month(date))]
nfirms[,prd:=year*12+month-23500]
nfirms[,c("year", "month", "date") :=NULL]
nfirms <- data.frame(nfirms)
nfirms[nfirms == -99.99] <- NA
nfirms[nfirms == -999] <- NA
setDT(nfirms)
nfirms <- melt(nfirms, id.vars=c("prd"))
setnames(nfirms, c("variable", "value"), c("ind", "n"))
setorder(nfirms, prd, ind)
#nfirms <- nfirms[prd<729]
n_industries_5 <- nfirms[n>5]
n_industries_10 <- nfirms[n>10]


# this code is needed only to create csd and wid
crsp <- fread("mCRSP_2619_p,rnm.csv", select = c("prd", "PERMNO", "PRC", "RET", "SHROUT", "ind"))
invisible(gc())
crsp[,RET:=as.numeric(RET)]
crsp <- crsp[!is.na(RET)]
crsp <- indices[crsp, on="prd", nomatch=0]


csd <- crsp[,.(cssd=sd(RET, na.rm = TRUE), csmad=sum(abs(RET-mean(RET, na.rm=TRUE)))/.N, csmad_vwretd=sum(abs(RET-(vwretd-RF)))/.N, csmad_ewretd=sum(abs(RET-(ewretd-RF)))/.N), by=prd]
csdplot <- copy(csd)
# remember that RET is excess, yet vwretd is not excess here
invisible(gc())
# the code below will take differences of these TS, the residualize them
csd[,c("lcssd", "lcsmad", "lcsmad_vwretd", "lcsmad_ewretd"):=list(c(NA, cssd[-.N]), c(NA, csmad[-.N]), c(NA, csmad_vwretd[-.N]), c(NA, csmad_ewretd[-.N]))]
csd[,c("dcssd", "dcsmad", "dcsmad_vwretd", "dcsmad_ewretd"):=list(cssd-lcssd, csmad-lcsmad, csmad_vwretd-lcsmad_vwretd, csmad_ewretd-lcsmad_ewretd)]

csd[,c("ldcssd", "ldcsmad", "ldcsmad_vwretd", "ldcsmad_ewretd"):=list(c(NA, dcssd[-.N]), c(NA, dcsmad[-.N]), c(NA, dcsmad_vwretd[-.N]), c(NA, dcsmad_ewretd[-.N]))]
csd <- csd[!is.na(ldcssd)]
csd[,c("cssd", "csmad", "csmad_vwretd", "csmad_ewretd"):=list(lm(dcssd~ldcssd+lcssd)$residuals, lm(dcsmad~ldcsmad+lcsmad)$residuals, 
                                                              lm(dcsmad_vwretd~ldcsmad_vwretd+lcsmad_vwretd)$residuals, lm(dcsmad_ewretd~ldcsmad_ewretd+lcsmad_ewretd)$residuals)]
csd[,c("dcssd", "dcsmad", "dcsmad_vwretd", "dcsmad_ewretd", "ldcssd", "ldcsmad", "ldcsmad_vwretd", "ldcsmad_ewretd", "lcssd", "lcsmad", "lcsmad_vwretd", "lcsmad_ewretd"):= list(NULL)]




# now I estimate CID:

inds <- fread("49ff_vw_20.csv")
inds[,date:=ymd(paste0(date, "01"))]
inds[,c("prd"):=list(year(date)*12+month(date)-23500)]
inds <- data.frame(inds)
inds[inds == -99.99] <- NA
inds[inds == -999] <- NA
setDT(inds)
dates <- inds[,.(prd, date)]
inds[,date := NULL]

inds <- melt(inds, id.vars=c("prd"))
setnames(inds, c("variable", "value"), c("ind", "RET"))
setorder(inds, prd, ind)
inds <- indices[inds, on="prd"]
inds <- unique(inds)
#inds <- inds[prd<729]
inds <- inds[!is.na(RET)]
inds <- n_industries_10[,.(prd, ind)][inds, on=.(prd, ind), nomatch=0]

# I may want to use abnormal returns here [skipped for now]

# here i compute cid from raw returns #
cid <- inds[,.(cisd=sd(RET, na.rm = TRUE), cimad=sum(abs(RET-mean(RET, na.rm=TRUE)))/.N, cimad_vwretd=sum(abs(RET-vwretd))/.N, cimad_ewretd=sum(abs(RET-ewretd))/.N), by=prd]
cidplot <- copy(cid)
# fwrite(cid, "cid_level.csv")
# the code below will take differences of these TS, the residualize them wrt differences and levels (analogous to ps03)
setorder(cid, prd)
cid[,c("lcisd", "lcimad", "lcimad_vwretd", "lcimad_ewretd"):=list(c(NA, cisd[-.N]), c(NA, cimad[-.N]), c(NA, cimad_vwretd[-.N]), c(NA, cimad_ewretd[-.N]))]
cid[,c("dcisd", "dcimad", "dcimad_vwretd", "dcimad_ewretd"):=list(cisd-lcisd, cimad-lcimad, cimad_vwretd-lcimad_vwretd, cimad_ewretd-lcimad_ewretd)]

cid[,c("ldcisd", "ldcimad", "ldcimad_vwretd", "ldcimad_ewretd"):=list(c(NA, dcisd[-.N]), c(NA, dcimad[-.N]), c(NA, dcimad_vwretd[-.N]), c(NA, dcimad_ewretd[-.N]))]
cid <- cid[!is.na(ldcisd)][!is.na(dcimad_vwretd)]
cid[,c("cisd", "cimad", "cimad_vwretd", "cimad_ewretd"):=list(lm(dcisd~ldcisd+lcisd)$residuals, lm(dcimad~ldcimad+lcimad)$residuals, 
                    lm(dcimad_vwretd~ldcimad_vwretd+lcimad_vwretd)$residuals, lm(dcimad_ewretd~ldcimad_ewretd+lcimad_ewretd)$residuals)]

# fwrite(cid, "SP19_mCIDbeta_24_v2_rawdifferences.csv")
cid[,c("dcisd", "dcimad", "dcimad_vwretd", "dcimad_ewretd", "ldcisd", "ldcimad", "ldcimad_vwretd", "ldcimad_ewretd", "lcisd", "lcimad", "lcimad_vwretd", "lcimad_ewretd"):= list(NULL)]

plot(cidplot[prd>=45, prd], cidplot[prd>=45, cimad], type = "l")

cor(cid[,cisd], cid[,c(NA, cisd[-.N])], use="complete.obs")
cor(cid[,cimad], cid[,c(NA, cimad[-.N])], use="complete.obs")
cor(cid[,cimad_vwretd], cid[,c(NA, cimad_vwretd[-.N])], use="complete.obs")
cor(cid[,cimad_ewretd], cid[,c(NA, cimad_ewretd[-.N])], use="complete.obs")

### lets check correlations with the other uncertainty measures ###
uncertainties <- fread("monthly_uncertainty_diff.csv")
setnames(uncertainties, "ymprd", "prd")
cid_unc <- uncertainties[cid, on="prd", nomatch=0]
setorder(cid_unc, prd)
# correlations are surprisingly low

table1 <- cor(cid_unc[,c(18,5,8,10,15,2)], use="complete.obs")
table1t <- cor(cid_unc[,c(18,5,8,10,15)], use="complete.obs")
table1[1:5,1:5] <- table1t
colnames(table1) <- c("CID", "FU", "MU", "VOL", "CIV", "VIX")
rownames(table1) <- c("CID", "FU", "MU", "VOL", "CIV", "VIX")
# stargazer(table1, summary=FALSE, digits=2)
# I will use this as a Table1


# calculate within-industry dispersion (WID)
# get ind into crsp, join on ind integer, get industry-adjusted return, get its average across all firms or [within each industry, then across industries]. 

# need to create mapping from ind name to its integer code. 
ind_joiner <- data.table(matrix(NA,49,2))
colnames(ind_joiner) <- c("name", "ind")
ind_joiner$name <- c("Agric", "Food", "Soda", "Beer", "Smoke", "Toys", "Fun", "Books", "Hshld", "Clths", "Hlth", "MedEq", "Drugs", "Chems", "Rubbr", "Txtls", "BldMt", "Cnstr", "Steel", "FabPr",
                     "Mach", "ElcEq", "Autos", "Aero", "Ships", "Guns", "Gold", "Mines", "Coal", "Oil", "Util", "Telcm", "PerSv", "BusSv", "Hardw", "Softw", "Chips",
                     "LabEq", "Paper", "Boxes", "Trans", "Whlsl", "Rtail", "Meals", "Banks", "Insur", "RlEst", "Fin", "Other")
ind_joiner$ind <- 1:49

inds <- ind_joiner[inds, on=c(name="ind")]
indrets <- unique(inds[,.(ind, prd, iRET=RET-RF)])
invisible(gc())

crsp <- crsp[!is.na(ind)]
crsp <- indrets[crsp, on=.(prd, ind)]
crsp[,iaRET:=RET-iRET]
wid <- crsp[,.(wisd=sd(iaRET, na.rm = TRUE), wimad=sum(abs(iaRET-mean(iaRET, na.rm=TRUE)), na.rm = TRUE)/.N, wimad_0=sum(abs(iaRET-0), na.rm = TRUE)/.N), by=prd]
widplot <- copy(wid)
wid[,c("lwisd", "lwimad", "lwimad_0"):=list(c(NA, wisd[-.N]), c(NA, wimad[-.N]), c(NA, wimad_0[-.N]))]
wid[,c("dwisd", "dwimad", "dwimad_0"):=list(wisd-lwisd, wimad-lwimad, wimad_0-lwimad_0)]
wid[,c("ldwisd", "ldwimad", "ldwimad_0"):=list(c(NA, dwisd[-.N]), c(NA, dwimad[-.N]), c(NA, dwimad_0[-.N]))]
wid <- wid[!is.na(ldwisd)][!is.na(wisd)]
wid[,c("wisd", "wimad", "wimad_0"):=list(lm(dwisd~ldwisd+lwisd)$residuals, lm(dwimad~ldwimad+lwimad)$residuals, lm(dwimad_0~ldwimad_0+lwimad_0)$residuals)]
wid[,c("dwisd", "dwimad", "dwimad_0", "ldwisd", "ldwimad", "ldwimad_0", "lwisd", "lwimad", "lwimad_0"):= list(NULL)]
wid <- wid[wimad!=0]
csd <- wid[csd, on="prd", nomatch=0]
setorder(csd, prd)
# hissing data appears due to my industry sorter, which starts at 1947.

cwd <- csd[cid, on="prd"]
# fwrite(cwd, "cwd.csv")

### Now, use CRSP to estimate betas ###

# to avoid losing observations, use precleaned crsp as opposed to crsp, containing market betas

#crsp <- fread("dCRSP14_18_prcl_excess_1.csv", select=c("prd", "PERMNO", "PRC", "RET", "SHROUT"))
#oldcrsp <- fread("dCRSP2662_prcl_excess.csv", select=c("prd", "PERMNO", "PRC", "RET", "SHROUT"))

crsp <- fread("mCRSP_2619_p,rnm.csv", select = c("prd", "PERMNO", "PRC", "RET", "SHROUT"))
setorder(crsp, PERMNO, prd)
crsp <- crsp[!is.na(RET)]
crsp[,me:=abs(PRC)*SHROUT]
crsp[, lme:=c(NA, me[-.N]), by=PERMNO]
crsp[, size:=log(lme)]
crsp[, me:=NULL]
crsp <- crsp[!is.na(lme)]
setkey(crsp, PERMNO, prd)
crsp <- unique(crsp, by=c("PERMNO", "prd"))
crsp <- indices[,.(prd, vwretd)][crsp, on="prd"]
crsp1 <- copy(crsp)
crsp[,c("lme", "size"):=list(NULL)]
invisible(gc())

setorder(crsp, PERMNO, prd)
crsp[,c("PRC", "vwretd", "SHROUT"):=list(NULL)]
dim(crsp)

crsp <- indices[crsp, on="prd", nomatch=0]
crsp <- cid[crsp, on="prd"]
crsp <- unique(crsp, by=c("PERMNO", "prd"))
#crsp <- csd[crsp, on="prd"]
crsp[,c("ewretd", "vwretd"):=list((ewretd-RF), (vwretd-RF))]
crsp <- crsp[!is.na(cisd)][!is.na(cimad_vwretd)]
crsp[,RF:=NULL]
invisible(gc())


########################
### Estimating betas ###
########################

crsp <- csd[,.(prd, csmad_ewretd, wimad, wimad_0)][crsp, on=.(prd)]

crsp[,c("cimad", "cimad_ewretd"):=list(NULL)]
setnames(crsp, c("cimad_vwretd"), c("cimad"))
crsp <- factors7[prd>-300,.(prd, EMKT, SMB, HML)][crsp, on="prd", nomatch=0]


crsp2 <- make_beta_param(copy(crsp[,.(PERMNO, prd, cimad, RET)]), "cimad", "RET", 24, 24, 24*1.25)
#crsp2 <- make_beta_param_controls(copy(crsp[,.(PERMNO, prd, cimad, RET, EMKT, SMB, HML)]), variable="cimad", window=60, clean_window=60, max_window=60*1.25, controls=c("EMKT", "SMB", "HML"))
invisible(gc())
setnames(crsp2, "beta", "beta_cimad")
crsp2[,beta_cimad:=winsor(beta_cimad, trim=0.01)]
summary(crsp2[,beta_cimad])
dim(crsp2)
crsp2 <- crsp2[!is.na(beta_cimad)]
crsp2 <- crsp2[,.(PERMNO, prd, beta_cimad)]
invisible(gc())




crsp3 <- make_beta_param(copy(crsp[!is.na(csmad_ewretd),.(PERMNO, prd, csmad_ewretd, RET)]), "csmad_ewretd", "RET", 24, 24, 24*1.5)
setnames(crsp3, "beta", "beta_csmad")
crsp3[,beta_csmad:=winsor(beta_csmad, trim=0.01)]
summary(crsp3[,beta_csmad])
dim(crsp3)
crsp3 <- crsp3[!is.na(beta_csmad)]
crsp3 <- crsp3[,.(PERMNO, prd, beta_csmad)]
invisible(gc())

crsp4 <- make_beta_param(copy(crsp[!is.na(wimad),.(PERMNO, prd, wimad, RET)]), "wimad", "RET", 24, 24, 24*1.5)
setnames(crsp4, "beta", "beta_wimad")
crsp4[,beta_wimad:=winsor(beta_wimad, trim=0.01)]
summary(crsp4[,beta_wimad])
dim(crsp4)
crsp4 <- crsp4[!is.na(beta_wimad)]
crsp4 <- crsp4[,.(PERMNO, prd, beta_wimad)]
invisible(gc())

setkey(crsp, PERMNO, prd)
setkey(crsp2, PERMNO, prd)
setkey(crsp3, PERMNO, prd)
setkey(crsp4, PERMNO, prd)

crsp1 <- crsp3[crsp2, on=.(PERMNO, prd)]
crsp1 <- crsp4[crsp1, on=.(PERMNO, prd)]

rm(crsp2, crsp3, crsp4, inds)
invisible(gc())

crsp1 <- crsp1[prd >= 45]
dim(crsp1)

time2 <- Sys.time()
time2-time1


# fwrite(crsp1, "SP19_mCIDbeta_24_v4.csv")






cidplot <- widplot[cidplot, on="prd"]
indd <- dates[cidplot, on="prd"]
indd <- indd[!is.na(date)][prd>=33]
plot(indd$date, indd$cimad, type="l", col="blue", ylab = "Dispersion measures", xlab = "Year", ylim = c(0,5))
lines(indd$date, indd$wimad, type="l", col="red")
legend(1,4.4, legend=c("WID", "CID"),
       col=c("red", "blue"), lty=rep(1,3), cex=0.8)
# for some reason here wid behaves differently since 2005-2010 compared to script_8. I guess it is because script_8 uses raw returns, probably not excess.

g <- ggplot(indd, aes(x=date, y=cimad)) + geom_line(color="steelblue", size=0.5)
g + labs(y="CID and WID", x="Year")

temp <- rbind(indd[,.(date, mad = cimad, type = "CID")], indd[,.(date, mad = wimad, type = "WID")])
ggplot(temp, aes(x=date, y=mad, group=type, col=type)) + geom_line(size=0.5) + labs(y="CID and WID", x="Year") + 
  scale_color_manual(breaks = c("CID", "WID"), values=c("darkblue", "red")) + theme(legend.title = element_blank()) + theme(legend.position = c(0.12, 0.9))





