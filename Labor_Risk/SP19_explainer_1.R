# this script explains what I do when looking at information and daily returns, hopefully culminating in 2nd year paper
# it is renamed "information_explainer_1" from 6pm 08/25/19

# to start working with daily crsp, use dCRSP_19_HW6_5. notice that the returns are not excess yet, so we need to subtract rfr

# "macro_releases" is the first attempt
# dCRSP_19_8 produces "dCRSP14_18_prcl.csv", precleaned dCRSD as well as its small version "...small", which will reliably fit into lab`s RAM
# precleaned_crsp26 produces similar thing for crsp 26-62, together with cleaning from nontraded permnos and size restrcition, similarly to dcrsp_restricter_new.

# 05/25/19: "macro_releases_8" tries to replicate SW13, for now without FOMC
# FOMC dates are in "FOMC_dates_4". ideally, I should add nonscheduled fomc announcements there later.

# "macro_releases_9" created "macro_infsup58.csv", all macro news supply data.

# SW_13_1 replicates main findings of Savor, Wilson 2013. The pattern is exactly the same, though the magnitude is somewhat smaller
# interesting: huge IS effect in Sw13, zero OOS effect. so starting from sw13_03 I look at effects for every subsample and news type

# for SW_14 and anomalies need all the standard analysis for daily CRSP
# tools: make_momentum_19 id very good, "daily_beta_calc_2" contains beta calculator, which probably works, but I am not sure in its correctness, need to check

# dcrsp_19_9 now contains excess returns too

# daily_beta_calc_11 is the script to produce fast and reliable beta calclulator

#SW14_7 contains the basic approach to construct relatively fast unisorter. then i can do minor upgrades to daily breakpoint generator too.
#SW14_8_15 develops all the main functions for TS analysis and contains new BJS_regs.

#512_HW4_17_dailytest_... tests new high-performance calculators vs old. TS functions are correct. FM reg is correct.

#SW14_8_31 contains good starting point of research on this topic, bunch of ideas at lines 335+. 

# "beta_r2_2" was an attempt on different idea (r2 from market model enhancing performance of beta), it seems we have a negative result. 
# to be sure, we need to do TS regressions with double/triple sorts for which i do not have the code yet.

# "macrodays_" should have the results of my attempts to answer questions at the end of SW14_8_36
# "macrodays_11_" contains the results for macro vs nonmacro betas - neagtive.
# macrodays_13 is the same thing fro difference in rolling sd

# can try different ways, windows to compute beta and sd. can look at subsamples with the same number of announcements (i.e., 24 32) to get more precise windows.
# for beta already tried evth, for sd: try fomc only (script 13_..._78_fomc), try 96 periods window and then 160 periods window.

# macroinf_autocor will check autocorrelation story"
# macrosinf_autocorr_4_agg is the story for aggregate market, negative result unless i screwed up somewhere

# dCRSP_restricter will generate different ME-trimmed sunsamples to use at RAM-constrained lab PC. It generates "dCRSP14_18_prcl_excess_ __M.csv" family of csv files, restricted to 20M, 100M, 200M, 500M, 1B.

# macroinf_CS_autocorr tests whether CS_autocorrelation during fomc announcement is priced. need to do relative autocorrs (ac_fomc - ac_nonfomc) too. do fomc, since 
# for non-equally spaced announcement it is much harder.

# infsup58fixed fixes one typo in fomc dates. infsup58fixedrec adds recession dummy.


# testing aleternative explanation of mechanically-working beta due to rightshifted returns: SW14_ae1_...
# it seems that AE1 holds, defeating the claims in SW14.

# SW14 subsamples is a detailed analysis of fomc over years.
# SW14_subsample_18_4 tries cum2 returns for 2day fomc and the results somewhat rehabilitate sw13 findings. oos the results are still 50% weaker, but effect is present and probably significant.

# to test hypothesis 2, I use "fomc_hypo_2_1". since I need to control for momentum over 2-5 years, I need new momentum calculator without hole-filling.
# it will be developed in "make_long_momentum".

# BJS_daily_3 checks whether daily BJS make sense. the results seem consistent with monthly BJS, it seems there is no mechanically good capm performance.
# BJS_daily_4__ suggests updated doublesorter. that script contains calculator of the number of firms in portfolios (maybe should update it to be consistent with new doublesorter).
# doublesorter should be updated to allow for equal weights. I stopped on 06/12/19 at basic_functions_87 and BJS_daily_4__

# beta_adj is the script, where I explore mechanical beta effect.
# I apply beta adjustment in SW14_ae1_1_beta_correction_allmacro

# I will try to formally replicate SW14 in replSW14. for now, my results kill sw14.
# replSW14_4 provides reasonable results.

# When dealing with dCRSP, always use the sample, restricted to at least 10M_2018 MCap stocks. otherwise things like in "capm_subperiods_2_" will happen.
# using individually-estimated betas in FM_reg does not make much sense due to noise.
# either use beta-portfolios or individual stocks with betas taken from portfolio betas (ff92 approach).

# 06/18: I will calculate full sample betas (events betas are hard to estimate and do not seem to matter much).
# {beta_b, beta_bw, beta_bsw} are beta with Blume(1971) correction, i.e., my usual beta w/o winsorization; the same beta, winsorized with 0.5% tails; bsw beta from Ivo(2019) with delta=3.
# will do it for 4 main restricted samples in dCRSP_betas. plus add smaller samples in dCRSP_restricter.

# anecdotal evidence from aggregate market on 06/19/19 (wsj, reuters) suggests that the largest information flow comes from the first day of fomc meeting, i.e. 06/18 rather than 06/19.
# in this particular case, on 18 market kind of learnt decision, 19 mostly conveyed information about future fed actions.
# fomc_days is used to create fomc dCRSP samples with both fomc days lumped together.

# what should I do with doubleday fomc? lets find out in replSW14_5fs_fomcdays

# 06/26: have 32gb ram, now will redo evth with 20M sample. 
# now lab PC sees my ssd as D, so it is the same as at home.

# smth weird: PRC stays the same, VOL is NA, so probably no trading and yet RET is -0.0200, -0.0210, -0.0260 ... why? it is PERMNO == 56857.
# btw, this is not a microcap. I do not know where to find any info on dealing with daily CRSP. BEM16 is only about monthly CRSP and they do not mention such a problem.
# to fix this, I updated dCRSP_restricter to ___new.

# sooner or later I will need accounting variables anyway, so lets join crsp and cpst (for now bm and op). crsp should have momentum as well.
# make_cpst_prd moves cpst to monthly frequency using old slow code. d_cpcrsp_3 does this.
# I will write "make_cpst_prd_19" using dt for any frequency. btw, there have not always been 252 trading days. before mid 1950s, there were more trading days in CRSP (saturdays?).

# controlling for bm, op and mom makes the pattern weaker. there is very strong pattern in momentum of beta-sorted portfolios. investigate it further in "beta_momentum_d".

# FM_coefficient_adjustment_0 suggests the adjustment for FM coefficients on beta with decile beta-sorted portfolios.

# 07/01: lots of cleaning of macrodates, some changes. no lag on fomc before 1994 now, some typos on stlouis fed corrected, typos in ppi dates from alfred corrected.
# plus i use onw only trading days announcements and treat two-days fomc as two days together (crspdd) or only the second day(crsp).

# from today i plan to focus on the two ideas (macrofactor from past macro returns and anomalies during macrodays).

# 07/02:
# if i want to use fomc only, then read "macro_infsup58fixed_05_sw14_trd_cor.csv", which does not omit extra macroevents on the day of the macroevent.
# aMOM was stupid, excude it by fixing dCRSP_ script. now it is called dCRSP_variables_4... . It will calculate max and ivol too.

# 07/03:
# earnings_dispersion_4 creates 4 measures of profitability (via reported earnings) dispersion.
# profitability_uncertainty tests this story.

# 07/05:
# tried profitability uncertainty factor with unconditional seasonal demeaning as well as with industry-level demeaning, sometimes get marginally significant resilts with vw, but ew really sucks.
# need to do rolling mean deseasoning, crate more variables, including measures of central tendency. then should join this with ibes to get both better eps and analyst forecasts.
# plus need to get daily betas from full crsp in the way to maximize the sample.
# then should redo evth at weekly frequency.

# 07/07:
# need ibes data. to join, can use cusip and can use ticker. in ibes, ticker is key identifier.
# after fixing announcement op script, prob uncertainty produces no results at all. there is only smth weak with mean or median over long horizons (60, 72).
# wait, industry-demeaned, 60 month is clearly  better. it still may work. ind: 60m results are uniformly stronger than 48 or 72 results.
# 

# 07/11:
# todo: ibes, weekly frequency, then try different deseasoning (x11, x12, deseasoning spread measures etc).
# there was a mistake in prof_uncert_ script. I corrected it in v21. do not use earlier versions. 
# v21, 60i_w produces interesting results, hinting on idea 4 (not only measures of spread, but measures of central tendency can matter too). 60u_21_wins is similar in this respect.
# results with ibes eps make no sense at all. smth was done wrong?

# 07/13:
# when constructing TS of some variable, worry about levels/changes. I should use changes for profitability series.
# maybe I should even use residuals in AR regression with changes (kind of approach of PS2003).

# in general, on idea 2 I have a lot of choices to make:
# - eps from cpst or ibes                                                                                                           cpst                as of 07/13
# - how to prewinsorize eps ct or disp measures                                                                                     0.1% for cpst       as of 07/13, 07/15
#   op                                                                                                                              2.5%                as of 08/08, 21i_60_wins_ty975
# - difference or even residualize wrt ar1 the time series                                                                          for mean: No, for disp: diff and res.
# - do i have controls when computing beta?                                                                                         capm or nothing     as of 07/15
# - winsorize betas. shrinkage will affect only FM, irrelevant for BJS, since nonparametric sorts are monotonically-invariant       1%                  as of 07/13
# - how many lags for beta?                                                                                                         60                  as of 07/13
# - industry-demeaned or unconditionally-demeaned                                                                                   rtbd                as of 07/15

# 7/15:
# first differences completely kill any results on sd. results on mean are getting weaker.
# first differences, then residualized produces very robust results across different dispersion measures, though weak for mean. alphas are ok. 60i_wins produce ok results for dispersion, but not mean.
# at levels, 60i wo controls are ok, with winsor and capm is ok. 60u is ok wrt capm and winsorozed.
# created qmprd_map, a function from prd to intramonth frequencies.
# stopped at prof_unc_23i_60_wins_qmprd at line 58

# 7/16:
# "intramonthly_freq" creates crsp at quarter and bimonthly frequencies.
# when I try qm and bm frequencies, the results completely disappear.

# 8/7:
# 21i nw or w, 23i w or nw produce smth wrt mean.
# what is the difference btw 21 and 23? in 23 beta is calculated wrt ff5 or ff3. 

# 8/8: trying to pick the best specification
# at monthly frequency, 2.5% winsor is the best: prof_unc_21i_60_wins_ty975  produces (42, 44) bps, (48, 58) bps alphas.
# qmprd: 240 and wins 2.5 or 1% seem ok.
# prof_unc_...qmbetas uses betas, computed at qm frequency together with monthly returns.

# 8/9:
# tried idea 3, preliminary version using ff data. i3p_2 - total failure, no results. should i even bother with the rest?
# started idea 4. first, need to prepare the data (monthly cpcrsp). monthly crsp is constructed in "CRSP_variables4". cpst will be added by monthly equivalent of d_cpcrsp_4.
# [?] appears that restrict_crsp, used to construct pretty much all crsp files is wrong [???]
# about i.2: can play more iwth it: try alternative restricter, different lags, nw se etc.

# 8/10:
# trying idea 4 and getting smth interesting: i4_8.
# for momenta: size-controlled factors seem to produce significance with both levels and changes of signal spread. moreover, the returns are more monotonic.
# but max and ivol suck when controlled for size. since they are inherently related with size, this control is important.
# next step: add liq(ps03), amihud ill and earnings surprise (SUE, CAR3). then may try accounting variables.
# did str underperform because its spread shrank? probably no, need to test it.

# 8/11:
# CRSP_variables6 adds amihud liquidity, using the construction from http://cfr.ivo-welch.info/forthcoming/amihud-2018.pdf, page 5
# "cpcrspm_20M_11v.csv" is the data with 11 variables. in the future, would be nice to add PS03 liq and SUE, CAR3. 
# btw, there exists a thing, called compustat point-in-time. it seems to be quarterly accounting data, which was actually available to investors (i.e., not back-restated).
# even for the first 4 variables (mom, max and ivol), the results across i4_10 and i4_11 are different, since they use different data.

##A##: methodologically, what is relationship of ols and ntile sorts? ntile sorts are kind of non-parametric regression.
#####  but in i4 script i often see ols with t-stat 2.5 and ntile sorts showing only noise... is ols that much non-robust?
#####  additionally, there are some monotonicity tests, e.g. Patton and timmermann 2009.

# 8/12:
# todo: 
# control for volatility (both ts and cs)
# produce 4 tables
# plot ts of cumLS vs sLS
# do ew as well
# later can add liq ps03, sue, car3, short interest (dechow01), hml_devil, ia, roe.

# vwretd_sds_1 creates "vwretd_sd.csv", a data of controls for ts regressions. later need to add cs_sd over 11-12 months. cs_sd really helps, so I must add these controls.
# in cases, where signal spread appears to work, should try managed portfolios, oos etc to check whether high t-stats on levels are real.

# notice that in most files of the type "crsp4_20M_whatever", vwretd is not an excess return. RET is always excess after the application of crsp_clean_rnm.
# crsp monthly data is created in the following way:
# "indices" -> crsp_clean_rnm_20 -> CRSP_variables_6 -> [optional] m_cpcrsp or its daily analogue.

# 8/13:
# updated "vwretd_sd_all.csv" with hopefully all controls I will need.
# i4_32 delievers preliminary results. I guess it makes more sense to regress changes of LS on changes of sLS rather than LS on dLS. 
# i4_33: wins 0.5% looks even better than non-wins.

# 8/14:
# discussed it with pingle and shuaiyu, plenty of handwritten notes.
# main thing: need to clarify economic story behind it. given heterogeneous effect on different anomalies, i should have econ explanation for at least some of them.
# need to add more anomalies, mkt controls, nw ses etc, try longer holding periods.

# for now, th. framework is as follows: ts_pred = cs-implied_pred + extra ts variation due to some econ or mechanical forces (kappa).
# can measure cs-implied part from fm regs. after running oos and managed portfolios, show that for some anomalies they help a lot and for them kappa>0.
# using controls, show some economic/mechanical explanations of kappa.

# then add more anomalies. can check the results using different construction timing/frequencies.

# 8/15:
# i4_38 controls for EMKT, nothing changes
# i4_40 uses nw se and deletes lev on ch.
# i4_46 looks at different hpr: 1,6,12,24 months. should i add R^2 as additional statistics?

# 6 months seems the best for chch. 
# for oos, use gw_08_repl_8 script.

# 8/16:
# will try to test cid (across-industry dispersion) as a state variable. idea 6.
# i6_ind10 delivers good results (54, 51 bps, 32 bps alpha). 11_me10 is a bit better.
# in monthly script for csd i forgot RF: industry returns were not excess, vwretd was not excess, while RET was.

# 8/17:
# indices_daily construct daily series of 5 factors since 1963 and indices with RF after july 1926 or 1927.
# at 11.45 RAM disaster happened. even saved files from ssd disappeared. the gap covers appx 9.30-11.45 am 8/17. e.g., script, creating dCRSP14_18_prclnt_excess_20M_withbeta_exch was lost.
# i6_daily_9 produces the furst results. they seem decent. _diff version of the script produces much weaker results (40% of magnitude), but still marginally significant.

# 8/18:
# created i6_daily_15, nice script for efficient DM.

# specifications: start from 252, nowins, levels. level-estimated betas produce much stronger results. ew or vw ff49 makes no difference.

# restricted_dcrsp_for_i6 creates a couple of more data files. prc5 hepls. 
# among diffres, 10M_prc5 is the best. among levels - do not remeber, probably 10M or 10M_prc5

# 8/19:

# try estimate betas, controlling for mkt.

# 8/20: for diffres, use 10M_prc10, cimad_vw. v19 uses only cimad_vwretd, called simple cimad. i modify restricted_dcrsp_for_i6 to include beta and cpst.
# notice that we can match the timing using ff approach and fixed lag approach. I use fixed lag first.
# when estimating beta for diffres, try max_window=2.5. for explanations use 1.25 results, they are probably more robust, since they are much more monotonic (ew). but vw is perfectly monotonic with 3.
# maybe I need to delete firms with extreme st volatility, since they can drive the results.

#check aggr predictability!
# i6_13_... for levels, diff and diffres there is nothing.

#8/21:
# 6 months window is better. in FM regressions, do not control for mom11. try different window lengths for FM, for both cimd_beta and beta, since controlling for beta cuts the coefficient by 60%.
# i6_d22 started, it will contain some more robustness checks.
# csd produces larger spread, but smaller ff5 alpha in script i6_d22_csd. can decompose csd into cid and wid and then use doublesorts to see which part is larger. 
# or can do doublesort on csd and cid, look at csd spread, controlling for cid and expect to see negligible alpha.
# d24_csd contains very interesting results: csd and cid, controlling for each other, are both significant, though small. 
# however, csd is completely explained by rmw and cma, while ff5 makes cid anomaly even stronger. 

# can try alternative beta estimation, similar to the one in vv18. i can adjust make_ivol for that.

# the first results on portfolio characteristics hint that this effect is due to st reversal and momentum

# 8/22:
# prepared the first reportable results to show to Shuaiyu. latex file "cis_prel_results", using scripts "i6_d25_6m_" and"i6_d24_csd".
# factorsd_pastrets creates "FF7_indices_daily.csv", ff5 with mom11 and mom122 factors.
# so far for both levels and differences the results are completely killed by mom11 and mom122 factors. i think it is because cid is close to crash measure, so illiquid stocks fall the most (low beta_cid).
# i6_d27_6m will use several tests to try to solve mom11 disaster. taking logs does not help.
# i can not see any way to save the results when using STR. lets use instead explicit liquidity factor (amhd or ps03).

# stopped at restricted_dcrsp_ ...._9, line 338.

# 8/23:
# added amihud variables. 
# d27_...ar1difflev uses finer residuals (specification from PS03) and achives better returns. still gets murdered bu mom11.
# starting from d28 the default version is the one with ps03 residuals and 6m.
# my cleaned crsp files do not exclude firms with zero returns over the whole month or more. 
# the example of such cleaner (i ithink i did that before somewhere) will be added in crsp_ret0_cleaner.

# 8/24:
# constructing liq factors in liq_factors_2.
# among liq decile uni factors, u126_126 sucks completely, u21_126 has 2 % rets and capm alpha, smaller ff3 alpha and no other alphas. u21_252 is slightly better.
# amhd 252 performs well especially ew
# need to take average BAspread over some number of past periods

# 8/25:
# ps03data_liq will construct liquidity factor from PS03 data.
# careful: file of the type ""crsp_precleaned_rnm_26..." contain excess ret, but not vwretd!
# ps03data_liq_ creates monthly liq betas and then constructs ps03-style liq factor at daily frequency. ds 2x3 factor fails.
# l48 uni quintile produces smth small. ps03data_liq_8_l24 creates ps03_liqfactor.
# later to check additionally liq story, should use monthly returns: str should get weaker and liq measures are more reliable.
# results are completely robust to exclusion of IT stocks. the same for gold, which is the most frequent extreme industry. 
# started updating cis_prel_res, updated page 3 using i6_d32_6m__. page 4 is left.

# 8/26:
# d32 were used to produce pages 3-4 of tex file.
# talked to shuaiyu, now have the rough plan for the next 1-2 days.
# I want results_b2 look like this:

# check mcrsp returns with daily-estimated betas, monthly factors.                                                        done
# construct/import vix, mv, macro_unc, civ.                                                                               done
# when looking at TS cid, find its correlation with vix, macro_unc, CIV.                                                  done
# decile returns, alpha, characteristics, loadings.                                                                       done
# fm results                                                                                                              -
# cid vs csd (both ds and FM).                                                                                            ds done
# cid vs macro_unc, civ, mv, vix.                                                                                         ds done
# robustness to bm/s 25, other industry measures.                                                                         -


# "FF7_indices_daily.csv" contains raw index returns!

# created "uncert_variables.csv" with levels of market volatilities, civ (kelly16), ludvigson15 measures and vix. "monthly[daily]_uncertainty_diff.csv" gives the main differenced data.
# d_33 and its monthly version (not finished) produce surprisingly low correlations with other uncertainty series (differences). 

# 8/27:
# 

# m cpcrsp files: m_cpcrsp_4 creates "cpcrspm_20M_.csv", main cpcrsp file.
# it uses "crspm4_20M" and adds accounting variables to make "cpcrspm_20M_". "crspm4_20M" (created via CRSP_variables6) uses "dCRSP14_18_prclnt_excess_20M_withbeta_mom_max_ivol.csv",
# created via dCRSP_variables4. so that file used daily crsp to compute some variables like beta, ivol and max.
# monthly results with monthly betas suck, need daily betas. "i6_d33_" creates "crsp_dailycimadbetas.csv". 
# i6_d34_monthly_6_dbeta is as close as i can get to daily crsp. results are much weaker. wdf?
# I have two ways to compare CID vs monthly-frequency uncertainty measures (ou):
# 1. use mCRSP with daily-estimated beta_cid (first beta in a month) and monthly-estimated beta_ou.
# 2. use dCRSP with daily-estimated beta_cid (fixed at the first day of the month) and monthly-estimated beta_ou.
# 1. seems more reasonable, but its results are crappy, so try 2.

# NOTICE THAT using monthly-fixed breakpoints does not mean monthly rebalancing. as long as i have daily-varying betas (unisorter_var_d(.)) portfolios are rebalanced daily.
# i6_d35 allows to fix the sorting variable within month. this script will be used to test vs other uncertainty measures.
# there is small difference btw this script and mcrsp. many mcrsp achieve better return spread, but they tend to lose alpha. dcrsp somehow gains alpha.
# so we have the problem: cid weakens a lot when we fix a signal within the month, though alpha is still decent. probably will have to argue about economic significance here.

# for 8/28:
# so run ds regressions and try to get at least some results controlling for other variables. Then email to alan and prepare the presentable version of the results.
# i6_d36 is the new baseline. it uses daily breakpoints. 

# 8/28:

# i forgot to add civ. btw, i can calculate civ at daily frequency.
# d35_monthlyfixed_sd_3 gives unisorted results. additionally, can play with lme fme.
# d35_ds_4_ tests ds. cid easily beats mu1, mu12, fu1, fu12. sd 12 performs equally well. sd24, sd36 perform even better than cid. however, cid is still significant, both unc and alphas. 
# added civ to uncbetas. cid kills civ. 

# lets start organizing the code. I want to have 1 script to produce all daily results and 1 script with monthly results for SP19_pres0. 
# I expect to have at least 3 scripts: SP19_rd with standard daily results.                     based on i6_d36.
# SP19_rdmf (to be created) with daily returns but monthly-fixed signals.                       based on i6_d35_monthfixed_ds_4__   and ..._ds_3_   now they are d37 (uni or ds)
# SP19_rm (to be created) with monthly returns.                                                 based on i6_d37_monthly_corr
# 

# 8/29:
# talked to Alan, will try to get smth on his suggestions within the next several days. I should come to him on Monday, Sep 2. 
# SP19_rd_7 will be used to run spanning tests btw CID and STR. the results imply that cid_f is completely subsumed by str, but cid_f can not explain str. 
# seems like I will give up on daily results as well as daily and monthlyfixed signal.
# monthly results are at risk from UMD unless I use long window to estimate beta_cimad. 504_2 betas produce acceptable results.
# when creating betas in sp19_dailycimad_betas, do not restrict crsp. plus, try more options of max_window.

# 8/30:
# when calculating daily betas in "sp19_dailycimad_betas", I deleted prc10 restricter. now results with 504, prc 10 are much nicer. 
# when will have time, I should recreate the input file for sd19_dailycimadbetas, i.e., "dcpcrsp10M_63_4v.csv", created with restricted_dcrsp_for_i6_8. 
# it has 10M restriction and maybe some other restrictions. need to run that script at home, since it is ram-intensive.
# results are better for crsp breakpoints. 70 bps vs 57 bps. 

# given comments from alan today and yesterday, the plan is as follows:
# today - add evth needed into SP19_m. tomorrow run this thing for different industry definitions and different frequencies. then start thinking/asking about measures of labor risk.
# on Monday must email to alan and ask for a meeting, hopefully get the meeting on Monday/Tuesday. Hopefully agree with him on the paper draft to submit on Sep 15. 

# need to recreate dcrsp to cpcrsp files withoyt size restrictions. to do this, modify dCRSP_variables4, CRSP_variables6 and dCRSP_variables4. I add upd at the end of files.
# after a couple of hours, created "cpcrspm_0M.csv". 
# still SP19_rm_9 produces the best results with "crsp_precleaned_rnm". is it because of delisting returns? 
# SP19_rd_8 created "postbetacid_3.csv", 3*252 postranking beta_{cid} of decile portfolios. 

# 8/31:
# SP19_rm_16 and sp19_dailycimad_betas_2: add csd and wid.
# sp19_dailycimadbetas_6 created cid, wid and csd betas as well as Figure 1. you do not need to run the whole script to produce Figure 1.
# interestingly, in table 8 (cid vs csd) the results are much stronger for nyse breaks. tables 7 and 8 look great with nyse breaks. 
# will update uncertainty_variables_5 to incorporate VIX, while removing it from the main script (SP19_rm_19).
# the results in table10 look really good, but I would raher not push them, since those monthly measures make it hard to get precise beta estimates. 
# SP19_rm_21 contains all the results for Step1, i.e., produces all 10 tables. 

# 09/01:
# SP19_rm_21_30 shows that ff30 perform just as well as ff49. ind10 performs well too. 
# results exist even at 5ind level, but they are less robust (i.e., decile returns are not monotonic and are driven by the lowest decile, in doublesorts ls often underperforms controls).
# in general, the results are robust to industry definitions. they start to somewhat weaker (in terms of monotonicity and doublesorts) from ff30 to ff17, but nothing discrete.
# updated make_long_mom, now it is definitely correct function. 
# lf_data_4 created "semiannual_prdmap.csv" as well as quarterly and semi-annually crsp and factors. NOTICE THAT there was timing typo in lf_data_4, using 7 months for semiannual results.
# SP19_quarterly and SP19_semiannually report results with lower frequencies.

# 09/02:
# preparing the tables in SP19_pres1.tex to show to Alan...
# btw, for storing the plots, png is better than jpeg.
# SP19_industry_partitions will produce couple of plot with presentable results on industries.
# so I prepared all the stuff to show to Alan on thursday.
# tomorrow will play with famamac.

# 09/03:
# I plan to create all the tables and figures within these two days. the current plan is to have 4 figures and 12 tables. this will correspond to sp19_rm_24.


# 09/07:
# I have most of tables and figures as well as introduction. SP19_labor_10 has some presentable results on cid predicting unemployment and employment.

# 09/08:
# added table 11 with the results of predictive regressions to the paper. 

# 09/14: 
# I am submitting these results in the first version of the paper.
# will try several more things today. calculating betas, controlling for emkt weakens results. 70 bps -> 46 bps. controlling for ff3 or ff5 destroys evth. 
# can try to do smth like hkln16 instead, i.e., take abnormal retuns of industries and not control for factors when estimating beta_cid.
# will create new generation of sp19_dailycimad_betas with cid computed from abnormal returns.
# if i compute cid from capm or ff3 abnormal returns, evth is fine. controlling for ff5 is even better.

# 09/16:
# for testing 3 out of 4 hypotheses I do not need bls oes data. just emp from cpst will suffice.
# the only value, added by oes, is occupations data.

# 09/24:
# on the construction of the data files again:
# first make "crsp_precleaned_rnm.csv". then use "CRSP_variables6upd_fixed0924" to create "crspm4_0M_unrestr.csv".
# then use "m_cpcrsp_4upd_fixed0924" to make cpcrspm_0M.csv.
# some things to fix in the paper: 
# sample above 10M, not 20. old "crspm4_0M" has 20M restriction.
# the sample should start in july 1963, not 1964. 
# b/m should be downscaled by 1000.

# 09/29:
# first, update data files. need to realign the sample with the start at least july 1963. renamed updated dCRSP_variables4... to dCRSP_makevarbls
# simplify architecture:
# dCRSP:
# use dCRSP_clean to clean raw files
# then dCRSP_makevarbls to add the main variables
# all other scripts will become obsolete after I finish this process.
# dclean: dCRSP_14_18 --> dCRSP62_5varbl

# CRSP:
# "crsp_precleaned_rnm_26.csv" is the starting file. 
# CRSP_dv5 adds some variables from daily CRSP and creates "crsp_dailyvars"

# CRSP-CPST:
# cpcrsp62 script uses crsp_dailyvars and creates cpcrsp62.csv.

# need to update dCRSP_5varbls and use it to estimate betas in sp19_dailycimad...
# updated everything.

# rerun the analysis, now it is 63 bps instead of 70. why?

# 09/30
# I think that in the new approach I lose a part of the sample, since first I estimate beta_mkt, losing the first 12 months and then estimate beta_cid, losing 24 months more.
# created new beta files, which exactly replicate old results.
# now SP19_rm_31 is a new baseline. it produces 67 bps, 47 bps alpha. in sample from 65, it generates exactly the same results as the old script (70, 50 bps).

# ind_empl computes number of employess and employment growth rates across industries.
# _32 will run the tests using weighted cid
# in dailycimad, when estimating betas, I compute cimad using weights.

# it is not clear how to construct weights. across different variables, this process seems very arbitrary. so i suggest to use rank weights.
# dailycimad_betas are now called dbetas and have version 10 for weighted stuff.

# 10/01:
# problem with all code: when dealing with monthly data, adding "01" or any day to date to parse it, is incompatible with prd_map. prd_map contains only trading days! need cal_map.

# 10/02:
# got highSkill data from Belo`s website. can get BLS OES data 88-18 for industries, the merge them to get employment within highSkill industries.
# similarly, can compute highSkill-weighted LS returns. 
# can devote one section/subsection of the paper to explaining civ/ivol/etc by cid.

# additional labor evidence: should take the data from Guvenen 2014, at his website it is table a8. then check correlation btw annual cid and that thing, similarly to kelly 16 fig.6.

# IMOPORTANT: Esther Eiling has two papers, closely related to my stuff: Eiling 2013 JF, Eiling et al. 2018 wp.
# the latter paper with 3 cites over 4 years uses annual-frequency CID to predict R_m with OOS R^2 14.88% and negative sign.
# their production-based ap story is not convincing to me. 
# if I could decompose labor income risk into aggragte shock and sectoral shifts (proxied by CID) and show positive correlation between cid and R_m, controlling for aggegate shocks,
# then R_m would be a hedge against labor income risk from sectoral shifts. Together with my CS results, it would be really good paper, publishable at top journal.

# 10/04:
# cid_ts_pred_6 reports failed replication attempt of EKS18.
# I replicate summary stat of CID/CSV pretty closely, yet can not replicate predictive regression at all.
# forget about TS results before the presentation.
# just add some labor tests, kind of what hkln14 and eks18 used and then finalize the paper.

# 10/05:
# the plan (before tuesday morning [failed miserably]): 
# add the section on CIV, show in doublesort, spanning tests and maybe famamac that cid explains civ.
# add more tests of labor story:
# sector returns predict sector employment (cpst data).
# use HS data from Belo, show that cid^HS predicts evth better data from Belo and BLS OES annual.
# take the data from guvenen, song and show that cid is related to csd of employment growth.                                                      failure
# on predictive regressions for unemployment: report both levels on levels and changes on changes.                                                this is useless, decided to omit this.
# additionally, report these results for long-term unemployment.                                                                                  done in SP19_labor_21
# plus, i can use either differences in variables or changes in them. 
# can try change in civ instead of difference. seems that change in empl/unempl is better.                                                        done in SP19_labor_21

# 10/8-9:
# unempl_dur creates lt_unempl.
# SP19_labor_15 will contain new tests.
# SP19_labor_16, _17 has the results for less than 5 week unempl vs more than 5 weeks. 
# need to think about quarterly frequency and unemployment duration to make sure that there is nothing mechanical here.
# labor_21 provides the final code on lt labor story.
# csv of income_growth fails as documented in income_growth_csv_1

# industry code: cpst sich -> cpst sic -> crsp
# naics_sic_1 creates a map from naic to sic.

# HS_unempl_1 is a beginning of HS tests

# 10/10:
# I am going to update the construction of "crsp_precleaned_rnm" to be more in line with FF. now it is "crsp_precleaned_rnm_oct".
# I construct "rff49lme.csv" to replicate ff49 inds. as SP19_labor_22_rff49 shows, unemployment predictability is almost the same as the one using ff49 industries.
# next I will check CS results using replicated daily returns of ff49 portfolios. Then I will look at HighSkill stuff and unemployment predictability.
# v33 checks CS results using replicated f49 portfolios, losing 3 bps.
# I update raw input dCRSP files by adding there industry codes. hopefully, I have not omitted anything from there. whenever open them, always specify "select" parameter.
# stopped at line 205 of script HS_unempl_6_

# 10/11:
# in crsp naics is hissing btw 01/2002 and 07/2004. fixed that in HS_unempl_6__, lines 158-172.
# first, test labor predictability. v_23 uses comparable sample period.
# test of CS results: sp19_dailycimad_betas_10_skill_2_andCS. this script estimates betas and runs CS tests.
# CS tests seem to fail
# SP19_labor_23_HS__good provides nice HS vs LS results.

# now I plan to write down all labor results and prepare the next version of the paper, with updated intro and conclusion.

# 10/12:
# started block 2 v3 version of the paper. slightly modifier intro, conclusion, added 3 references. 
# abstract, conclusion and references seem ok. Intro will suffice for now, but I do not like its strcuture and its flow of thought. in particular, need to decrease the emphasis on uncertainty.
# plan to add the tables on income risk story, then rewrite this section.
# need to work on the code to report the predictive regressions. there seems to be the mismatch in sample lenth btw the first two kinds of analysis. plus have to decide whether to pick diff of ch.

# 10/14:
# emailed to alan and yixin, will schedule the meetings this week.
# to do: 
# update all tables/figures in the paper
# can use dailycimad_betas_10_upd for F1.
# updated f1, t3-5.
# updated crsp_dailyvars to be used in sp19_rm_34

# 10/15:
# work on postranking betas in _34 script.

# 10/16 - 10/17:
# updated all sequence of data files (_5varbls, dailyvats, cpcrsp62) to include correct momentum
# as of 3pm: updated all the tables except 1 and 12-14.


# 10/20:
# table 5 shows the loss of alpha monotonicity. It occurs after controlling for mom122 and str in the two highest portfolios with negative loadings on mom122 and str.
# important thing is to try to redo evth using cid from abnormal industry returns and check whether I have alpha monotonicity issue.
# from now, v_35 and beta_11 are the main scripts.
# I can not replicate table 14 from the earlier version of the paper.
# wait, I can if i use old betas.
# work further on _35 script to see whether cid from abnormal returns will help in doublesorts, 

# 10/21:
# played for a while with cid from abnormal returns. it seems that _ff5_t1__ produces decent results. now need to see whether it works with wid/cid.
# the tradeoff is alpha vs monotonicity. if i use abnormal returns to compute cid, i lose monotonicity of unconditional returns, but get larger alphas and better doublesorts.
# sp19_dailycimad_betas_12__ estimates cid, wid, csd using abnormal returns.
# when using abnormal returns to get cid, doublesorted results are significantly better. the problem is that wid/cid results are reversed: now wid is more priced.
# so i will stick with cid, wid from unconditional returns. use 

# todo today: update t1, t12, f3, f4. 

# then create sic 3(4) industry classifications.

# 10/22:
# created new betas: "crsp_dailycimadbetas63_630_full_12.csv" uses 2.5 years to estimate betas. it works somewhat better for evth except wid vs cid. 
# but it still shows that cid outperforms wid. I guess for now I will stick with 504 betas, but in the future I may wish to redo my stuff using 630 betas.
# btw, shuaiyu thinks that pastor-stambaugh residualizing specification is not very standard. he says it is not a good idea to take differences when ts is not persistent.
# he sugegsts ot take a look at the paper by maio, santa-clara 2012. that paper is about icapm generating both cs and ts predictions.
# ok, I do not have enough time. just focus on 504.
# dailycimad_betas_14 produces table1. 
# sp19_dailycimad_betas_7_17_35 is the template script to produce betas, needed for figure 4.
# _35_504_inds creates Figure 4.

# 10/23:
# table1 looks weird. need to check it.

# 10/26:
# industries_sic_34_1 produces data files to compute sic3 cid. sic3 has 560 inds, but obly 120-220 of them has more than 5 firms. they produce larger spread than ff49.
# those scripts produce the data for _betas_14_sic scripts. then I use sp19_rm_35_504_inds_sic_ to generate Figure 4. 
# now want to look at cid/wid decomposition with 5-10 industries. v35_ _5inds returns that table. it is Table 15 for now. 

# 10/29:
# bls oes have monthly-frequency industry-specific employment data, but I am not able to find the way to download these data in a consistent way.
# in the future, I will want to use those data. here is kind of bls naics-based sector classification: https://www.bls.gov/web/empsit/ceseeb1a.htm.
# to get the data, go here: https://www.bls.gov/ces/#tables > CES National databases > One-screen data search. I think it is possible to organize those data in 2 days.
# for now, the best data i can use is annual-level data: https://www.bls.gov/oes/tables.htm

# updated industry classification stuff - wrote down the results on FF5ind cid/wid. added sic2 classification, wrote down interpretation in 5.1.
# now i will reproduce the main results at full monthly frequency (i.e., using monthly data to estimate betas).
# the beta script will be sp19_...14_monthly. I take table 1 from there too. the main script is sp19_rm_35_60.
# big problem: returns of LS portfolio, formed on daily betas are uncorrelated with the returns of LS portfolio, formed from monthly betas. even loadings are very different.
# to answer this criticism, i must show that daily preranking betas predict monthly(quarterly) postranking betas.
# now it is complete disaster:
# daily prebetas are uncorrelated with post monthly betas.
# monthly prebetas strongly negatively predict post monthly betas. so actually any risk story(labor, uncertainty, pute icapm) has the wrong sign.
# this is a big problem. the only way to convince ppl (and myself) that cid does capture labor risk is by unemployment predictability. 
# SP19_labor_24_LT_d_dailycid contains unemployment predictability, analogous to table9, but using dailycimad as an input.
# the results are similar even given that I use dailycimad in a wrong way (I should average raw cid and then maybe difference/res it).
# so daily cid generates large abnormal return spread and predicts unemployment. however, it is intuitively unclear why this daily movement should worry employees.
# in SP19_35_postmonbetas_predbetad_lowf I am trying to argue that daily prebetas useful, since they can somewhat predict low-freq postbetas.
# monthly prebetas negatively predict themeselves, so , paradoxically as it may seem, we may want to use daily prebetas to predict them. but they can not (LS has exactly 0 beta).
# daily prebetas somewhat predict quarterly postbetas (0.37 realized beta of LS).
# sp19_postmonbetas_predbetas_lowf and _..._attempts record my attempts. preditability of postranking quarterly betas seems to be statistical fluke: 
# it sucks at any other frequency. but it is quarterly frequency cid, which matter the most for unemployment... I think this result was dm-ed, so forget about it.
# my paper is still pretty good 2yr paper, so having this hole is not a disaster. i guess i can afford to be direct about it if asked.

# 10/30:
# first, I will try placebo portfolios. I have 4 types of doublesorted placebo portfolios.
# now it looks like paper-ruining disaster. bms and ops portfolios produce the same spread with exactly the same loadings and even larger alphas.
# so i think that this is the same strategy, which reflects some kind of uncertainty or maybe idiosyncratic risk. 
# maybe cid still exists and is captured by monthly-freq strategy? but preranking betas of that strategy can not predict postranking betas...
# but cid still clearly predicts unemployment. yukun`s wp argues that product market competitors only 20% coincide with labor market competitors.
# so it is possible that cid premium does capture labor risk. the problem is that product-based ff49 classification is so noisy proxy for labor market competitors that
# even characteristic-based classification does slightly better.
# to provide definite answer to this question, I need occupation-specific industry classification.
# positive results of placebo portfolios are only the beginning of extensive analysis, including cid/wid analysis and culminating in occupation-specific industry classification.
# i do not have time to do that, so lets not include the placebo checks before the presentation.
# added betas of industry portfolios, file sp19_35...inportfs.
# in ts tests cid destroys civ. however, in FM civ outperforms civ, though cid remains statistically significant.
# cid_civ_FM_5 contains FM regs across decile portfolios. in both cases, cid/civ is weakened by beta_vol and completely destroyed by each other.

# aside: can csd/cid pick up liquidity effect? large daily cid/csd means larger extreme returns, so are likely related to low liquidity.
# tested this, PS liq factor explains only 5bps.

# 11/05:
# I passed, now lets get monthly industry-level employment data. btw, alan wants me to take more classes, especially from econ dept.
# https://www.bls.gov/ces/#tables > CES National databases > Multi-screen data search seems to work.

# 11/09:
# need to create good naics6 - sic4 map to use it for employment data. i actually can download employment data with naics codes.
# census.gov has naics- sic maps here: https://www.census.gov/eos/www/naics/concordances/concordances.html
# i need to figure out which version of naics bls uses. I am not completely sure, but i guess it is naics 2017. (https://www.bls.gov/bls/naics.htm)
# so i intend to: naics17 -> naics12 -> naics07 -> naics02 -> sic87. ff49 was created for sic87 as far as i understand.
# the problem: bls code is not naics code. so i can not precisely map empl to ff49. the best thing i can do is to use discretion to construct empl of inds, resembling ff49.
# i guess i need 4-7 hours for that.
# sps i do that and find no return spread, then what? or i find some return spread, uncorrelated with the one of daily-fr cid strategy?
# then i will just repeat loungani et al (90) and say that returns provide the measure of shocks to that state variable, while employment data is stale and thus is not priced in CS of returns.
# i do not believe that firms make decisions to fire workers at monthly frequency anyway. (as 2020 illustrates, that belief was wrong)
# it seems to me that empl cid will measure realized sectoral reallocation over the recent period, longer than month. so i fear it is too stale to be reflected in CS of returns.
# bls data is waste of time. when i tried to pick industries intuitively, i was able to select 43 industries, maybe 70% close to ff49.
# only 12 of them have employment data before 1990. before code 41, only 2 groups have data (out of 25).
# so i guess that`s it, i can not do more with the data I have.

# 11/10:
# need to do 2 important comprehensive rob. checks today-tomorrow:
# 1. frequencies: daily, weekly (i.e., quartermonthly), monthly, quarterly, semiannually.
#    for any frequency, see returns and preranking-postranking betas.
#    try controlling for factors when estimating betas, try using cid from abnormal returns.
#    see whether any betas can predict lowfrequency postranking betas.
# 2. placebo portfolios: unisorts, doublesorts, triplesorts on size, b/m, op, inv, mom, str, beta. 
#    for any placebo cid repeat all the analysis above.

# for rob.check1 - I will have at least 3x4+1 scripts.
# for any frequency, I will compute cid and beta_cid without controls first (robcheck1_n), then with constrols(_pre, _post). 
# _pre means using abnormal industry returns, _post means controls when estimating beta_cid. can try both too.
# I will try 4 frequencies: daily, weekly, monthly, quarterly.
# and the last script will report the correlations of LS trading startegies, created by previous at least 3x4 scripts.
# the first script is SP19_robcheck1dn_4. daily, without controls.

# 12/2/19:
# so far have done nothing, reading some other eap stuff.
# start script "yr3"

# 01/10/20:
# lets reproduce the results using correct sample restictors. need to see the results for cwd (cross-whatever dispersion) vs cid. then cwd vs csd.
# then look at literature. stievers and sun (2010), maio (2016) use cpd (cross-portfolio dispersion) from 25 or 100 size/bm doublesorts. they both seem to explore only TS predictability. 

# created SP19_20 to replicate the results from 2nd yr paper. _v1 and _v2 scripts show that half of unconditional returns and most of alphas wrt Carhart or FF7 were driven by PRC mistake.
# I guess it means that all 2nd yr paper was a mistake.






# how would I explain cwd?
# labor risk (limited labor mobility btw firms, different ib value/momentum/size ? does not sound plausible).
# trading of institutions, induced by large positions in style portfolios. high cwd means fire sales of some style portfolios, so is likely related to liquidity shocks.
# just some abstract uncertainty. too vague to be an interesting explanation.





















# add holding periods
# try to construct the factor
# add tests of cid vs civ
# try to build industry classification from bls oes data.
# bls oes annual data: for each naics industry many occupations and employment within each one. that is, for every industry, I have a vector of occupation frequencies.
# how to do industry classification? k-means clustering or smth else?

















# other things to think about/ to do:
# david suggested looking at msa-level labor mobility. additionally, i can try using philips and hoberg industry classifications.
# I forgot to compute sharpe ratios. 
# need the horse race cid vs civ. in particular, spanning tests.
# famamac across portfolios will hopefully deliver stronger results.
# cid == downward cid == upward cid

# tests of labor story:
# the simpler way would be to forecast unemployment by cid in high-skill industries.

# mention somewhere that cid does not predict market returns
# have ready code for monthly-frequency cid and CS results. 





