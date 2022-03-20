# this script will explore response of crypto to MNA, including inflation #

# _v1: ignore tables 1-2 for now, try to get responses.
# _v2: to have not that messy code, i currently deleted table 1-2 parts.
# _v3: the first script to use "sprd": specific date period. for this family of scripts (i12 family) I need sprd for crypto, which are traded ech day.
# _v8-10: add interest rates.
# _v17: replaced all ymd with as.Date, that should work much faster. But I have not tested it! If smth fails, go back to _v16.
#       if it fails, use smth like as.Date(as.character(date), "%Y%m%d").

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

crypto <- fread("crypto6_120521.csv")

coin <- "BTC"
crypto <- crypto[crypto==coin]

crypto[,c("year", "price"):=list(year(Date), Close)]
esed <- crypto
invisible(gc())

# lets try reusing the code from i102pI_v28:

#variables_to_exclude <- c("GDP Annualized QoQ", "Personal Consumption")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############################
### Parts 1.-2. All 21 news ###
###############################

rf <- fread("mFF_RF_20.csv", select=c("date", "ymprd", "RF"))
rf[,ymprd:=ymprd+1]
prd_map <- fread("prd_map_19.csv")
prd_map[,date:=as.Date(date)]
nber <- fread("USREC.csv")
nber[,ymprd:=12*year(as.Date(DATE))+month(as.Date(DATE))-23500]
cfnai <- fread("mBC_CFNAI.csv", select = c("ymprd", "CFNAI"))
rec <- cfnai[nber, on="ymprd"]

tn5 <- fread("tn5y_0620_.csv", select=c("close", "date", "hm_prd"))
tn5[,date:=as.Date(date)]
setnames(crypto, "Date", "date")
crypto[,date:=as.Date(date)]
crypto_dates <- crypto[date<as.Date("2020-08-29"),unique(date)]

# stopped here
# read https://www.cmegroup.com/trading/interest-rates/basics-of-us-treasury-futures.html and figure out what price means and how to map it to rate.
# it does not explain anything. i can not compute rates from prices w/o coupon and i do not know where to take coupon.
# cs19 suggest using duration. i can not follow their approach exactly, since i can not get their proxy for duration from Bloomberg.
# thus i wrote script "temp_tn5_1" to estimate duration myself using daily data.
# i got estimates of 3.87 using returns and 4.55 using price difference, seems reasonable.

setnames(tn5, "close", "fprice")

# lets fill holes in both tn5 and crypto for any time btw 8:00 and 16:00:
# create joiner (the same for crypto and rates data), join to both datasets, fill holes via cumsum.

timess <- 480:960
joiner <- CJ(timess, as.Date(union(tn5[,unique(date)], crypto_dates), origin = "1970-01-01"))
colnames(joiner) <- c("hm_prd", "date")

tn5 = merge(joiner, tn5, all=TRUE, by=c('date', 'hm_prd'))
setorder(tn5, date, hm_prd)
tn5[,fprice:=fprice[1], .(cumsum(!is.na(fprice)))]

crypto = merge(joiner, crypto, all=TRUE, by=c('date', 'hm_prd'))
setorder(crypto, date, hm_prd)
crypto <- crypto[date>=as.Date("2013-03-31")]
crypto[,price:=price[1], .(cumsum(!is.na(price)))]
crypto <- crypto[!is.na(price)]
crypto0 <- copy(crypto)


###  Look at all news, identify significant ones ###
### this is just copypaste from y4_i2_p2_9 #########

windoww <- c(10, 20)
windoww_rate <- c(10, 20)
window_end <- 120
# was 120 before v27

bgnews <- fread("bg_macronews_0521.csv")
bgnews[,c("Date", "surprise", "hm_prd"):=list(mdy(Date), Actual-Survey, hour(hm(Time))*60+minute(hm(Time)))]
bgnews[,Date:=as.Date(format(Date, format="%Y-%m-%d"))]

bgnews[Event=="U. of Mich. Sentiment", Event:=paste(Event, substr(ref_month,5,5), sep=" ")]
bgnews0 <- copy(bgnews)


bgnews[Event %in% c("GDP Annualized QoQ", "Personal Consumption"), rel_type:=str_sub(ref_month,-1,-1)]
bgnews[rel_type=="A", rel_class:=1]
bgnews[rel_type%in%c("P", "S"), rel_class:=2]
bgnews[rel_type%in%c("F", "T"), rel_class:=3]


#%%%%%%%%%%%%%%%%%%%%%%%
########################
### Part 3. Response ###
########################

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

####################################################
### first load all necessary data and clean them ###
####################################################

# mnas: 5old + 2 lowfreq (gdp, pce) + 2 infl
# how to deal with ppi?
# i do not remember, i can not find my old explanation as to ppi.
# the simplest way is to use PPI MoM before 01/14 and Final demand ppi after.

bgnews[Event == "PPI Final Demand MoM", Event:="PPI MoM"]

bgnews0 <- copy(bgnews)
bgnews <- bgnews0[Event %in% c("Change in Nonfarm Payrolls", "Unemployment Rate", "ISM Manufacturing", "CPI MoM", "PPI MoM", 
                               "Construction Spending MoM", "Retail Sales Advance MoM", "Personal Consumption", "GDP Annualized QoQ")]
# out of these 9 events, pmi and constr are the only ones with 10:00 news. evth else at 8:30.
# in 1997, they used to announce cpi at 13:30.
bgnews[,ymprd:=year(Date)*12+month(Date)-23500]

nfp <- fread("PAYEMS.csv")
nfp[,date := as.Date(DATE)]
nfp[,ymprd:=year(date)*12+month(date)-23500+1]
# +1 is to match it with the data, announced in the next month

bgnews <- nfp[,.(PAYEMS, ymprd)][bgnews, on="ymprd", nomatch=0]
bgnews[Event=="Change in Nonfarm Payrolls",surprise := 100*(((Actual - Survey)/1000)/PAYEMS)]
sd_s <- bgnews[,.(sd_s=sd(surprise, na.rm = TRUE)), by=Event]
bgnews <- sd_s[bgnews, on="Event", nomatch=0]
bgnews[,surprise:=surprise/sd_s]
setnames(bgnews, "Date", "date")
# later may try different ways (rolling) to compute sd.

bgnews <- rec[,.(ymprd, CFNAI, USREC)][bgnews, on="ymprd"]
bgnews0 <- copy(bgnews)
nber <- fread("USREC.csv")
nber[,ymprd:=12*year(as.Date(DATE))+month(as.Date(DATE))-23500]
cfnai <- fread("mBC_CFNAI.csv", select = c("ymprd", "CFNAI"))
rec <- cfnai[nber, on="ymprd"]
nrow(bgnews[Event=="ISM Manufacturing"][!is.na(surprise)][year(date)>1996])

################################################################
### Load and clean data for different proxies for the market ###
################################################################

### then compute market response to news ###
### do it twice for two groups of news, defined the time they are announced ###

bgnews <- bgnews[year(date)>2008]

# now all mna except pmi and constr are at 8:30.

##############################################
### returns for evth except PMI and Constr ###
##############################################

news_hmprd <- 510

es <- copy(crypto0)

es <- es[hm_prd<=(news_hmprd+windoww[2]+10)][hm_prd>=(news_hmprd-windoww[1]-10)]
nrow(es[!is.na(price)])
es_first <- es[hm_prd %in% ((news_hmprd-windoww[1]-10):(news_hmprd-windoww[1]))]
es_first <- es_first[order(hm_prd),.SD[.N],by=date][order(date)]
es_last <- es[hm_prd %in% ((news_hmprd):(news_hmprd+windoww[2]))]
es_last <- es_last[order(hm_prd),.SD[.N],by=date][order(date)]
# these two methodologies are inconsistent, but i do not think it really matters.
es <- es_last[,.(date, Price=price)][es_first[,.(date, lPrice=price)], on="date", nomatch=0]
es[,c("ret"):=list(100*(log(Price)-log(lPrice)))]
es <- es[!is.na(ret)]
es0 <- copy(es)

# add tn5 here
#es <- tn5[es, on=.(date, hm_prd), nomatch=0]
# why do i lose 30% of crypto sample? probably weekends. they should not matter much.

drate <- tn5[crypto0, on=.(date, hm_prd), nomatch=0]
drate <- drate[hm_prd<=(news_hmprd+windoww_rate[2]+10)][hm_prd>=(news_hmprd-windoww_rate[1]-10)]
drate_first <- drate[hm_prd %in% ((news_hmprd-windoww_rate[1]-10):(news_hmprd-windoww_rate[1]))]
drate_first <- drate_first[order(hm_prd),.SD[.N],by=date][order(date)]
drate_last <- drate[hm_prd %in% ((news_hmprd):(news_hmprd+windoww_rate[2]))]
drate_last <- drate_last[order(hm_prd),.SD[.N],by=date][order(date)]
drate <- drate_last[,.(date, fprice)][drate_first[,.(date, lfprice=fprice)], on="date", nomatch=0]
drate[,c("dfprice"):=list(fprice-lfprice)]
drate[,drate:=(-1/3.9)*dfprice]
nrow(drate[!is.na(drate)])

bgnews510 <- es[bgnews[hm_prd==news_hmprd], on="date", nomatch=0]
bgnews510rate <- es[bgnews[hm_prd==news_hmprd], on="date", nomatch=0]
bgnews510rate <- drate[,.(date, drate)][bgnews510rate, on="date", nomatch=0]

########################################
### returns for PMI and construction ###
########################################

news_hmprd <- 600

es <- copy(crypto0)

es <- es[hm_prd<=(news_hmprd+windoww[2]+10)][hm_prd>=(news_hmprd-windoww[1]-10)]
nrow(es[!is.na(price)])
es_first <- es[hm_prd %in% ((news_hmprd-windoww[1]-10):(news_hmprd-windoww[1]))]
es_first <- es_first[order(hm_prd),.SD[.N],by=date][order(date)]
es_last <- es[hm_prd %in% ((news_hmprd):(news_hmprd+windoww[2]))]
es_last <- es_last[order(hm_prd),.SD[.N],by=date][order(date)]
# these two methodologies are inconsistent, but i do not think it really matters.
es <- es_last[,.(date, Price=price)][es_first[,.(date, lPrice=price)], on="date", nomatch=0]
es[,c("ret"):=list(100*(log(Price)-log(lPrice)))]
es <- es[!is.na(ret)]
es0 <- copy(es)

drate <- tn5[crypto0, on=.(date, hm_prd), nomatch=0]
drate <- drate[hm_prd<=(news_hmprd+windoww_rate[2]+10)][hm_prd>=(news_hmprd-windoww_rate[1]-10)]
drate_first <- drate[hm_prd %in% ((news_hmprd-windoww_rate[1]-10):(news_hmprd-windoww_rate[1]))]
drate_first <- drate_first[order(hm_prd),.SD[.N],by=date][order(date)]
drate_last <- drate[hm_prd %in% ((news_hmprd):(news_hmprd+windoww_rate[2]))]
drate_last <- drate_last[order(hm_prd),.SD[.N],by=date][order(date)]
drate <- drate_last[,.(date, fprice)][drate_first[,.(date, lfprice=fprice)], on="date", nomatch=0]
drate[,c("dfprice"):=list(fprice-lfprice)]
drate[,drate:=(-1/3.9)*dfprice]
nrow(drate[!is.na(drate)])

bgnews600 <- es[bgnews[hm_prd==news_hmprd], on="date", nomatch=0]
bgnews600rate <- es[bgnews[hm_prd==news_hmprd], on="date", nomatch=0]
bgnews600rate <- drate[,.(date, drate)][bgnews600rate, on="date", nomatch=0]

#############################################
### Combine all news, produce the results ###
#############################################

news_ret <- rbind(bgnews600, bgnews510)
news_ret_drate <- rbind(bgnews600rate, bgnews510rate)


# delete few duplicates
news_ret[(date==as.Date("2013-12-02"))&(Event=="Construction Spending MoM")&(is.na(Prior)),Event:=NA]
news_ret <- news_ret[!is.na(Event)]
news_ret_drate[(date==as.Date("2013-12-02"))&(Event=="Construction Spending MoM")&(is.na(Prior)),Event:=NA]
news_ret_drate <- news_ret_drate[!is.na(Event)]

macro_ss <- summstat(news_ret[Event=="ISM Manufacturing",.(CFNAI, USREC)],2)
nfp_ss <- summstat(news_ret[Event=="Change in Nonfarm Payrolls",.(ret, surprise)],2)
pmi_ss <- summstat(news_ret[Event=="ISM Manufacturing",.(ret, surprise)],2)
retl_ss <- summstat(news_ret[Event=="Retail Sales Advance MoM",.(ret, surprise)],2)
cons_ss <- summstat(news_ret[Event=="Construction Spending MoM",.(ret, surprise)],2)
unr_ss <- summstat(news_ret[Event=="Unemployment Rate",.(ret, surprise)],2)

cpi_ss <- summstat(news_ret[Event=="CPI MoM",.(ret, surprise)],2)
ppi_ss <- summstat(news_ret[Event=="PPI MoM",.(ret, surprise)],2)
gdp_ss <- summstat(news_ret[Event=="GDP Annualized QoQ",.(ret, surprise)],2)
pce_ss <- summstat(news_ret[Event=="Personal Consumption",.(ret, surprise)],2)


table3 <- do.call("rbind", list(macro_ss, nfp_ss, pmi_ss, retl_ss, cons_ss, unr_ss, cpi_ss, ppi_ss, gdp_ss, pce_ss))
table3$Variable <- rownames(table3)
setDT(table3)
table3[,News:=c("All", "All", "NFP", "NFP", "PMI", "PMI", "Retail", "Retail", "Construction", "Construction", "Unemployment", "Unemployment",
                "CPI", "CPI", "PPI", "PPI", "GDP", "GDP", "PCE", "PCE")]
setcolorder(table3, colnames(table3)[c(13, 12, 1:11)])
table3

news_ret[,ntype := 'NA']
news_ret[Event %in% c("CPI MoM", "PPI MoM"), ntype:="Inflation"]
news_ret[!(Event %in% c("CPI MoM", "PPI MoM")), ntype:="Growth"]
news_ret[Event=="Unemployment Rate", surprise:=-1*surprise]


table4 <- news_ret[,.(b = summary(lm(ret~surprise))$coefficients[2,1],
                      t_b = summary(lm(ret~surprise))$coefficients[2,3],
                      r2 = summary(lm(ret~surprise))$adj.r.squared), by=Event]

table4

table5 <- news_ret[,.(b = summary(lm(ret~surprise))$coefficients[2,1],
                      t_b = summary(lm(ret~surprise))$coefficients[2,3],
                      r2 = summary(lm(ret~surprise))$adj.r.squared), by=ntype]

table5

# try testing hypothesis that the response id due to intrerest rates. use TN5 data:

news_ret_drate[,ntype := 'NA']
news_ret_drate[Event %in% c("CPI MoM", "PPI MoM"), ntype:="Inflation"]
news_ret_drate[!(Event %in% c("CPI MoM", "PPI MoM")), ntype:="Growth"]
news_ret_drate[Event=="Unemployment Rate", surprise:=-1*surprise]

table6 <- news_ret_drate[,.(b = summary(lm(drate~surprise))$coefficients[2,1],
                      t_b = summary(lm(drate~surprise))$coefficients[2,3],
                      r2 = summary(lm(drate~surprise))$adj.r.squared), by=Event]

table6

table7 <- news_ret_drate[,.(b = summary(lm(drate~surprise))$coefficients[2,1],
                      t_b = summary(lm(drate~surprise))$coefficients[2,3],
                      r2 = summary(lm(drate~surprise))$adj.r.squared), by=ntype]

table7

### and now lets see comovement btw rates and crypto during different mna. later will compute comovement during any random period.

table8 <- news_ret_drate[,.(b = summary(lm(ret~drate))$coefficients[2,1],
                            t_b = summary(lm(ret~drate))$coefficients[2,3],
                            r2 = summary(lm(ret~drate))$adj.r.squared), by=Event]

table8

table9 <- news_ret_drate[,.(b = summary(lm(ret~drate))$coefficients[2,1],
                            t_b = summary(lm(ret~drate))$coefficients[2,3],
                            r2 = summary(lm(ret~drate))$adj.r.squared), by=ntype]

table9

summary(lm(ret~surprise, news_ret[Event == "Personal Consumption"][rel_type=="A"]))
# btc goes down when we learn that ppl consumed more. due to autocorrelation of consumption, it means more expected consumption, less saving/consumption ratio,
# so less demand for btc.

# btc is exposed only to 2 macro variables:
# - consumption growth: negative
# - inflation: negative

# one way to frame it is as a test of 2 hypotheses:
# h1: fed model / rfy model: btc is hit by rates.
# h2: consumption-savings decision.
# there are dimensions of cons-savings decision, which are kind of exogeneous (i.e., not driven by risk aversion, rd, lrr etc).
# think winters or covid.
# could try to argue that pce is a proxy for cons/saving ratio.
# idea is that by the time of pce news we have better idea about total income that its allocation across cons-savings.
# so large part of shock to pce should be about change of cons/savings ratio.
















