### ### ### ### ### ### ### ### ### ### ### ### # 
### First Part of i10.2: Time series results ####
### ### ### ### ### ### ### ### ### ### ### ### #
# v6: added ffr.
# v10: merged part 6 with part 5. so now we have essentially 5 parts.
# v16: modified reported tables with interactions.
# v17: omit cpi/ppi, work more on retail.
# v30: big update of the project in oct21.

### This script will look at large set of macronews, explore which ones matter
### Then it will look at market reaction to them, explore asymmetries in the reaction and will perform decomposition.

###  Structure:
# 1. Load the mna data and organize one dt with aset prices at 1-min frequency.
# 2. Explore volatility reaction to these 18 MNs, identify those which matter. Produce a table.
# 3. Look at the market reaction to all relevant MNs. Show on the example of PMI how shorter window helps. 
#    Produce tables.
# 4. Compare NFP results to BHJ05, explain why they got weird results.
# 5. Explore market response over different environments. Report R^F asymmetry.
#    Produce bunch of tables.
# 6. Describe basic framework (in th wp) and report the results from regressions, estimating parameters.

### This is the version, producing the working paper for Spring 2021 AEC510, 4/13/21.

### The script collect the results from several earlier scripts, written over 10/20 - 02/21:
### y4_i2,1_7 - for 1.-2.
### yr4_i7_t2_3 - for 3.-6.

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
time1 <- Sys.time()
setwd("H:/Dropbox/SSD_20/data")

windoww_rate_510 <- c(10, 930)
windoww_rate_600 <- c(10, 840)

prices <- fread("itd_ap9719.csv")
dl_rates <- fread("d_rates_21.csv")
dl_rates[,c("date", "DATE"):=list(ymd(DATE), NULL)]
setorder(dl_rates, date)
dl_rates[,c("DFF", "DGS2", "DGS5", "DGS10"):=list(as.numeric(DFF), as.numeric(DGS2), as.numeric(DGS5), as.numeric(DGS10))]
dl_rates[,DGS2:=DGS2[1], .(cumsum(!is.na(DGS2)))]
dl_rates[,DGS5:=DGS5[1], .(cumsum(!is.na(DGS5)))]
dl_rates[,DGS10:=DGS10[1], .(cumsum(!is.na(DGS10)))]
dl_rates[,c("DFF", "DGS2", "DGS5", "DGS10"):=list(shift(DFF,2), shift(DGS2,2), shift(DGS5,2), shift(DGS10,2))]

qprd <- data.table(ymprd=465:740)
qprd[,qprd:=floor(ymprd/3)-154]
qprd[,saprd:=floor(qprd/2)+1]


### ### ### ### ### ### ### ### ### 
### Parts 1.-2. All 21 news ####
### ### ### ### ### ### ### ### ### 

# objects to report:  table1 - description of MNAs
#                     table2 - volatility moves in response to MNAs
#                     table3 - summstat of surprises for the MNAs, which matter

rf <- fread("mFF_RF_20.csv", select=c("date", "ymprd", "RF"))
rf[,ymprd:=ymprd+1]
prd_map <- fread("prd_map_19.csv")
prd_map[,date:=ymd(date)]
nber <- fread("USREC.csv")
nber[,ymprd:=12*year(ymd(DATE))+month(ymd(DATE))-23500]
cfnai <- fread("mBC_CFNAI.csv", select = c("ymprd", "CFNAI"))
rec <- cfnai[nber, on="ymprd"]

###  Look at all news, identify significant ones ###
### this is just copypaste from y4_i2_p2_9 ###

bgnews <- fread("bg_macronews_0108.csv")
bgnews[,c("Date", "surprise", "hm_prd"):=list(ymd(Date), Actual-Survey_m, hour(as.ITime(time))*60+minute(as.ITime(time)))]
bgnews[,Date:=ymd(format(Date, format="%Y-%m-%d"))]

bgnews[Event=="U. of Mich. Sentiment", Event:=paste(Event, substr(Period,5,5), sep=" ")]
bgnews0 <- copy(bgnews)

absret_controls <- list.load('y4_i2_retcontrols30m.rdata') 

bgnews[Event %in% c("GDP Annualized QoQ", "Personal Consumption"), rel_type:=str_sub(Period,-1,-1)]
bgnews[rel_type=="A", rel_class:=1]
bgnews[rel_type%in%c("P", "S"), rel_class:=2]
bgnews[rel_type%in%c("F", "T"), rel_class:=3]

bgnews[Event=="Construction Spending",Event:="Construction Spending MoM"]
bgnews[Event=="Advance Retail Sales",Event:="Retail Sales Advance MoM"]
bgnews[Event=="PPI Final Demand MoM",Event:="PPI MoM"]
bgnews[(Event=="Durable Goods Orders")&(hm_prd==600),hm_prd:=NA]
bgnews[(Event=="ISM Non-Manufacturing"),hm_prd:=NA]

bgnews <- bgnews[!is.na(hm_prd)]

time2 <- Sys.time()
##Time to run parts 1-2 is
time2-time1

### ### ### ### ### ### ### 
### Part 3. Response ####
### ### ### ### ### ### ### 

# restrict the attention to 5-6 news, then to 3 news.
# objects to report:  table3 - summstat of surprises and returns for the MNAs, which matter.
#                     table4 - simple regression for all5-6 news.
#                     table5 - regressions for pmi with varying windows.

# use bgnews. add es data. ideally, find a way to do it for all MNAs in 1 datatable.
# compute response, run reg, create table4.
# run regs for pmi.

# table3 will have 3+2*n rows and 11 columns.
# 11 columns are ss columns from summstat().
# the first 3 rows are CFNAI, USREC and FFR for the first(or last) day of the month, kind of the same across MNAs.
# then for each MNAs, ss of its surprise and spx_ret.

### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### first load all necessary data and clean them ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### 

bgnews <- bgnews[year(Date)<2020]
bgnews0 <- copy(bgnews)
bgnews <- unique(bgnews0[Event %in% c("Change in Nonfarm Payrolls", "Unemployment Rate", "ISM Manufacturing", "GDP Annualized QoQ",
                              "Construction Spending MoM", "Retail Sales Advance MoM", "Personal Consumption",
                              "CPI MoM", "PPI MoM", "Conf. Board Consumer Confidence", "Capacity Utilization",
                              "Initial Jobless Claims")])
bgnews <- prd_map[,.(Date=date, prd, ymprd)][bgnews, on = "Date", nomatch=0]

nfp <- fread("PAYEMS.csv")
nfp[,date := ymd(DATE)]
nfp[,ymprd:=year(date)*12+month(date)-23500+1]
# +1 is to match it with the data, announced in the next month
bgnews <- nfp[,.(PAYEMS, ymprd)][bgnews, on="ymprd", nomatch=0]
bgnews[Event=="Change in Nonfarm Payrolls",surprise := 100*(((Actual - Survey_m)/1000)/PAYEMS)]
sd_s <- bgnews[,.(sd_s=sd(surprise, na.rm = TRUE)), by=Event]
bgnews <- sd_s[bgnews, on="Event", nomatch=0]
bgnews[,surprise:=surprise/sd_s]
setnames(bgnews, "Date", "date")

ffr <- fread("dFFR3m_8819.csv", select = c("prd", "date", "tffr"))
setorder(ffr, prd)
ffr[,ltffr:=shift(tffr,1)]
bgnews <- ffr[,.(prd, ltffr)][bgnews, on="prd", nomatch=0]
bgnews <- rec[bgnews, on="ymprd", nomatch=0]
nber <- fread("USREC.csv")
nber[,ymprd:=12*year(ymd(DATE))+month(ymd(DATE))-23500]
cfnai <- fread("mBC_CFNAI.csv", select = c("ymprd", "CFNAI"))
rec <- cfnai[nber, on="ymprd"]
nrow(bgnews[Event=="ISM Manufacturing"][!is.na(surprise)][year(date)>1996])
bgnews[,c("PAYEMS", "sd_s", "DATE"):=list(NULL)]

bgnews[Event %in% c("Unemployment Rate", "Initial Jobless Claims"),surprise:=-1*surprise]

bgnews0 <- copy(bgnews)
bgnews00 <- copy(bgnews[,.(ymprd,prd,ltffr,Event,date,hm_prd,surprise)])

### ### ### ### ### ### ###  ### 
### Join prices with bgnews ####
### ### ### ### ### ### ###  ### 

windoww <- c(10, 20)
window_end <- 120

prices0 <- copy(prices)
prices <- copy(prices0)
bgnews <- copy(bgnews0)

bgnews[,c("hmprd_bef", "hmprd_aft"):=list(hm_prd-windoww[1], hm_prd+windoww[2])]

bgnews_pr <- prices[,.(date, hmprd_bef=hm_prd, tn10price_bef=tn10price, tn5price_bef=tn5price, tn2price_bef=tn2price,
                       rate1m_bef=rate1m, rate3m_bef=rate3m, srate3m_bef=srate3m, es_bef=Close)][bgnews, on=.(date,hmprd_bef)]
bgnews_pr <- prices[,.(date, hmprd_aft=hm_prd, tn10price_aft=tn10price, tn5price_aft=tn5price, tn2price_aft=tn2price,
                       rate1m_aft=rate1m, rate3m_aft=rate3m, srate3m_aft=srate3m, es_aft=Close)][bgnews_pr, on=.(date,hmprd_aft)]

bgnews_pr[,c("es_ret", "drate_ff1", "drate_ff3", "drate_sff3", "drate_t2", "drate_t5", "drate_t10"):=
      list(100*(log(es_aft)-log(es_bef)),100*(log(rate1m_aft)-log(rate1m_bef)),100*(log(rate3m_aft)-log(rate3m_bef)),100*(log(srate3m_aft)-log(srate3m_bef)),
           -100*1/1.75*(tn2price_aft-tn2price_bef),-100*1/4*(tn5price_aft-tn5price_bef),-100*1/8*(tn10price_aft-tn10price_bef))]
# i assume duration of 1.75, 4 and 8 years for 2,5 and 10-year treasuries.

# before _v33 here were 100-200 lines of code, calculating es_ret and drate.

news_ret <- copy(bgnews_pr)

# delete few duplicates
news_ret[(prd==23262)&(Event=="Construction Spending MoM"),Event:=NA]
news_ret[prd==19013,Event:=NA]
news_ret <- news_ret[!is.na(Event)]

nfp_ss <- summstat(news_ret[Event=="Change in Nonfarm Payrolls",.(es_ret, surprise)],2)
pmi_ss <- summstat(news_ret[Event=="ISM Manufacturing",.(es_ret, surprise)],2)
retl_ss <- summstat(news_ret[Event=="Retail Sales Advance MoM",.(es_ret, surprise)],2)
cons_ss <- summstat(news_ret[Event=="Construction Spending MoM",.(es_ret, surprise)],2)
unr_ss <- summstat(news_ret[Event=="Unemployment Rate",.(es_ret, surprise)],2)
gdp_ss <- summstat(news_ret[Event=="GDP Annualized QoQ",.(es_ret, surprise)],2)
pc_ss <- summstat(news_ret[Event=="Personal Consumption",.(es_ret, surprise)],2)
cpi_ss <- summstat(news_ret[Event=="CPI MoM",.(es_ret, surprise)],2)
ppi_ss <- summstat(news_ret[Event=="PPI MoM",.(es_ret, surprise)],2)


table3 <- do.call("rbind", list(nfp_ss, pmi_ss, retl_ss, cons_ss, unr_ss, gdp_ss, pc_ss, cpi_ss, ppi_ss))
table3$Variable <- rownames(table3)
setDT(table3)
table3[,News:=c("NFP", "NFP", "PMI", "PMI", "Retail", "Retail", "Construction", "Construction", 
                "Unemployment", "Unemployment", "GDP", "GDP", "PC", "PC", "CPI", "CPI", "PPI", "PPI")]
setcolorder(table3, colnames(table3)[c(13, 12, 1:11)])
table3

table4 <- news_ret[,.(b = summary(lm(es_ret~surprise))$coefficients[2,1],
            t_b = summary(lm(es_ret~surprise))$coefficients[2,3],
            r2 = summary(lm(es_ret~surprise))$adj.r.squared), by=Event]
table4

table4a <- news_ret[,.(b = summary(lm(drate_ff1~surprise))$coefficients[2,1],
                      t_b = summary(lm(drate_ff1~surprise))$coefficients[2,3],
                      r2 = summary(lm(drate_ff1~surprise))$adj.r.squared), by=Event]
#table4a

table4a <- news_ret[,.(b = summary(lm(drate_ff3~surprise))$coefficients[2,1],
                       t_b = summary(lm(drate_ff3~surprise))$coefficients[2,3],
                       r2 = summary(lm(drate_ff3~surprise))$adj.r.squared), by=Event]
#table4a

table4a <- news_ret[,.(b = summary(lm(drate_sff3~surprise))$coefficients[2,1],
                       t_b = summary(lm(drate_sff3~surprise))$coefficients[2,3],
                       r2 = summary(lm(drate_sff3~surprise))$adj.r.squared), by=Event]
#table4a

table4b <- news_ret[year(date)<2010,.(b = summary(lm(drate_ff1~surprise))$coefficients[2,1],
                       t_b = summary(lm(drate_ff1~surprise))$coefficients[2,3],
                       r2 = summary(lm(drate_ff1~surprise))$adj.r.squared), by=Event]
#table4b

table4b <- news_ret[year(date)<2010,.(b = summary(lm(drate_ff3~surprise))$coefficients[2,1],
                       t_b = summary(lm(drate_ff3~surprise))$coefficients[2,3],
                       r2 = summary(lm(drate_ff3~surprise))$adj.r.squared), by=Event]
#table4b

table4b <- news_ret[year(date)<2010,.(b = summary(lm(drate_sff3~surprise))$coefficients[2,1],
                       t_b = summary(lm(drate_sff3~surprise))$coefficients[2,3],
                       r2 = summary(lm(drate_sff3~surprise))$adj.r.squared), by=Event]
#table4b

table4c <- news_ret[,.(b = summary(lm(drate_t2~surprise))$coefficients[2,1],
                       t_b = summary(lm(drate_t2~surprise))$coefficients[2,3],
                       r2 = summary(lm(drate_t2~surprise))$adj.r.squared), by=Event]
#table4c

table4c <- news_ret[,.(b = summary(lm(drate_t5~surprise))$coefficients[2,1],
                       t_b = summary(lm(drate_t5~surprise))$coefficients[2,3],
                       r2 = summary(lm(drate_t5~surprise))$adj.r.squared), by=Event]
#table4c

table4c <- news_ret[,.(b = summary(lm(drate_t10~surprise))$coefficients[2,1],
                       t_b = summary(lm(drate_t10~surprise))$coefficients[2,3],
                       r2 = summary(lm(drate_t10~surprise))$adj.r.squared), by=Event]
#table4c

time3 <- Sys.time()
##Time to run part 3 is 
time3-time2






table4 <- news_ret[Event %in% c("ISM Manufacturing", "Retail Sales Advance MoM", "Change in Nonfarm Payrolls",
                      "Conf. Board Consumer Confidence", "Construction Spending MoM", "CPI MoM", "Unemployment Rate", "PPI MoM"),
         .(b = summary(lm(es_ret~surprise))$coefficients[2,1],
            t_b = summary(lm(es_ret~surprise))$coefficients[2,3],
            r2 = summary(lm(es_ret~surprise))$adj.r.squared), by=Event]
table4 <- table4[order(-abs(b))]

table4

#stargazer(table4, summary=FALSE, digits=2, rownames=FALSE)



### ### ### ### ### ### ### 
### Part 5. Asymmetry ####
### ### ### ### ### ### ### 

# in these two parts I  will report 3 panels for each regression (PMI, NFP, Retail)

impvols <- fread("d_sw_impvol_9721.csv")
vix <- fread("dVIX_9721.csv")
impvols <- vix[impvols,on="date"]
impvols[,c("imv2", "imv4", "imv6", "imv8", "imv11", "vix"):=list(shift(imv2,2),shift(imv4,2),shift(imv6,2),shift(imv8,2),shift(imv11,2),as.numeric(shift(vix,2)))]
news_ret0 <- copy(news_ret)
news_ret <- dl_rates[news_ret, on="date"]
news_ret <- impvols[news_ret, on="date"]

# convert implied volatilities from percentages of rate level to rate vol:
news_ret[,c("imv2", "imv4", "imv5", "imv6", "imv8", "imv10", "imv11"):=list(DGS2*imv2/100, NULL, DGS5*imv6/100, NULL, NULL, DGS10*imv11/100, NULL)]

#news_ret <- news_ret[(Event!="GDP Annualized QoQ")|((Event=="GDP Annualized QoQ")&(rel_class==3))]
# if want to add gdp, then choose the last announcement. otoh, third announcement leads to the weakest response...

Events <- c("Change in Nonfarm Payrolls")
#Events <- c("ISM Manufacturing", "Retail Sales Advance MoM", "Change in Nonfarm Payrolls")
#Events <- c("ISM Manufacturing", "Retail Sales Advance MoM", "Change in Nonfarm Payrolls", "Conf. Board Consumer Confidence")

news_ret[(date%in%ymd('2002-09-06', '2002-12-06', '2008-01-04', '2008-02-01', '2008-09-05', '2008-11-07', '2008-01-09',
                      '2001-09-07', '2008-03-07', '2008-05-02'))&
         (Event=="Change in Nonfarm Payrolls"),es_ret:=NA]

# for intuitive interpretation of results with interactions, need to demean them.
#news_ret[,c("imv2", "vix", "CFNAI"):=list(imv2-mean(imv2, na.rm=TRUE), vix-mean(vix, na.rm=TRUE), CFNAI-mean(CFNAI, na.rm=TRUE))]


m1_4_all <- news_ret[(Event %in% Events),(lm(es_ret~surprise))]
m2_1_all <- news_ret[(Event %in% Events)&(USREC==1),(lm(es_ret~surprise))]
m2_2_all <- news_ret[(Event %in% Events)&(USREC==0),(lm(es_ret~surprise))]
m2_3_all <- news_ret[(Event %in% Events)&(imv2<=0.685),(lm(es_ret~surprise))]
m2_4_all <- news_ret[(Event %in% Events)&(imv2>0.685),(lm(es_ret~surprise))]

m3_1_all <- news_ret[(Event %in% Events),(lm(es_ret~surprise))]
m3_2b_all <- news_ret[Event %in% Events,(lm(es_ret~surprise+imv2+imv2*surprise))]
m3_4b_all <- news_ret[Event %in% Events,(lm(es_ret~surprise+imv2+imv2*surprise+CFNAI+CFNAI*surprise))]
m3_5_all <- news_ret[Event %in% Events,(lm(es_ret~surprise+imv2+imv2*surprise+vix+vix*surprise))]
m4_1_all <- news_ret[Event %in% Events,(lm(drate_t2~surprise))]
m4_2_all <- news_ret[Event %in% Events,(lm(drate_t2~surprise+imv2+imv2*surprise))]
m4_3_all <- news_ret[Event %in% Events,(lm(drate_t2~surprise+imv2+imv2*surprise+vix+vix*surprise))]

table7b_all <- stargazer(m2_1_all, m2_2_all, m2_3_all, m2_4_all, omit.stat = c("rsq", "ser", "f"), 
                         add.lines=list(c("Subsample", "Recession", "Expansion", "Low mon. uncert.", "High mon. uncert.")),
                         type="latex", report=('vc*t'))

# results with interaction term #

table8_all <- stargazer(m3_1_all, m3_2b_all, m3_4b_all, m4_1_all, m4_2_all, omit.stat = c("rsq", "ser", "f"), 
                        type="text", report=('vc*t'), digits=2)


summary(m3_2b_all)$coefficients[4,1]/summary(m4_2_all)$coefficients[4,1]
summary(m3_4b_all)$coefficients[5,1]/summary(m4_2_all)$coefficients[4,1]

#######
time5 <- Sys.time()
time5-time1


imptvols <- dl_rates[impvols, on='date']
imptvols[,c("imv2", "imv4", "imv5", "imv6", "imv8", "imv10", "imv11"):=list(DGS2*imv2/100, NULL, DGS5*imv6/100, NULL, NULL, DGS10*imv11/100, NULL)]
imptvols[,ymprd:=year(date)*12+month(date)-23500]
imptvols <- imptvols[order(date), .SD[.N], by=ymprd]

plot(imptvols$date, imptvols$imv2, type="l", xlab="Year", ylab="Implied Volatility")

 








### ### ### ### ### ### ### ### ### ### ### ### ### ###  ### 
### Predicting aggregate earnings (profits) by surprise ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###  ### 

news <- news_ret[,.(date, Event, ymprd, Actual, Prior, Survey_m, Survey_a, surprise, imv2, CFNAI, vix)]
news[,rep_ymprd:=ymprd-1]
# i assume that all news released in month t are about varible value in t-1. this is not true for some minor news, 
# but as long as i ignore minor new, this is okay.
news <- qprd[,.(rep_ymprd=ymprd, rep_qprd=qprd)][news,on="rep_ymprd",nomatch=0]

earnngs <- fread("CP_4620.csv")
# the date in CP corresponds to the first day of the quarter
earnngs[,c("date", "ymprd", "DATE"):=list(ymd(DATE), year(DATE)*12+month(DATE)-23500, NULL)]
earnngs <- qprd[,.(ymprd, qprd)][earnngs[year(date)<2020], on="ymprd", nomatch=0]
earnngs[,c("date", "ymprd"):=list(NULL)]
setorder(earnngs,qprd)
earnngs[,cp_g0:=100*(log(CP)-log(shift(CP,1)))]

# notice that we want to predict cumulative change, not a one-period change!
earnngs[,c("cp_g1", "cp_g2", "cp_g3", "cp_g4", "cp_g5", "cp_g6", "cp_g7", "cp_g8"):=
          list(100*(log(shift(CP,-1))-log(shift(CP,1))), 100*(log(shift(CP,-2))-log(shift(CP,1))),
               100*(log(shift(CP,-3))-log(shift(CP,1))), 100*(log(shift(CP,-4))-log(shift(CP,1))),
               100*(log(shift(CP,-5))-log(shift(CP,1))), 100*(log(shift(CP,-6))-log(shift(CP,1))),
               100*(log(shift(CP,-7))-log(shift(CP,1))), 100*(log(shift(CP,-8))-log(shift(CP,1))))]
news[, c("rep_ymprd"):=list(NULL)]
setnames(news, "rep_qprd", "qprd")
news <- earnngs[news, on="qprd"]

mnas <- c("Change in Nonfarm Payrolls", "ISM Manufacturing", "Retail Sales Advance MoM")
news[Event %in% mnas,summary(lm(cp_g0~surprise))]
news[Event %in% mnas,summary(lm(cp_g1~surprise))]
news[Event %in% mnas,summary(lm(cp_g2~surprise))]
news[Event %in% mnas,summary(lm(cp_g3~surprise))]
news[Event %in% mnas,summary(lm(cp_g4~surprise))]

# interactions:

news[Event %in% mnas,summary(lm(cp_g0~surprise+imv2+imv2*surprise+CFNAI+CFNAI*surprise+vix+vix*surprise))]
news[Event %in% mnas,summary(lm(cp_g1~surprise+imv2+imv2*surprise+CFNAI+CFNAI*surprise+vix+vix*surprise))]
news[Event %in% mnas,summary(lm(cp_g2~surprise+imv2+imv2*surprise+CFNAI+CFNAI*surprise+vix+vix*surprise))]

# mixed results












# here i will explore time series of impvols and timeseries of responses

#news_ret[,year:=year(date)]
#news_ret[(Event %in% c("ISM Manufacturing","Retail Sales Advance MoM","Change in Nonfarm Payrolls"))&(year>1996),
#         (lm(es_ret~surprise)$coefficients[2]),by="year"]

#news_ret[,year=year(date)]
news_ret <- qprd[news_ret, on="ymprd"]
temp <- news_ret[(Event %in% c("ISM Manufacturing", "Retail Sales Advance MoM", "Change in Nonfarm Payrolls", 
                               "CPI MoM", "Conf. Board Consumer Confidence", "Initial Jobless Claims"))&(qprd>3),
                 .(b = summary(lm(es_ret~surprise))$coefficients[2,1],
                   t_b = summary(lm(es_ret~surprise))$coefficients[2,3],
                   r2 = summary(lm(es_ret~surprise))$adj.r.squared),by="saprd"]
temp <- news_ret[,.(inv2=mean(imv2), inv5=mean(imv5)),by="saprd"][temp, on="saprd"]

plot(temp$saprd, temp$b, type="l", col="blue", ylim = c(-0.4,1))
lines(temp$saprd, 1-temp$inv2, type="l", col="red")

temp[,cor.test(b,inv2)]

