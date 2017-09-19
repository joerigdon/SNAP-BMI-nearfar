#Load necessary packages
#install.packages("nearfar")
#install.packages("Gmisc")
#install.packages("fmsb")
#install.packages("Hmisc")
#install.packages("optmatch")
#install.packages("AER")
#install.packages("MatchIt")
#install.packages("rms")
#install.packages("ivpack")
#install.packages("miceadds")
library(nearfar)
library(Gmisc)
library(fmsb)
library(optmatch)
library(AER)
library(MatchIt)
library(ivpack)
library(Hmisc)
library(rms)
library(MASS)
library(sandwich)
library(miceadds)

#Load necessary functions to help with table/figure creation
source("K:/Research/Research_Stanford_Basu/FoodAPS/Code/Tables.R")
source("K:/Research/Research_Stanford_Basu/Figures.R")
source("K:/Research/Research_Stanford_Basu/Functions.R")

#Load data and reshape as necessary
j = read.csv("K:/Research/Research_Stanford_Basu/FoodAPS/Data/nearfar_all.csv",header=T)
j1 = as.data.frame(apply(j,2,function(x) as.numeric(as.character(x))))
dim(j1)
length(table(j1$hhnum))
names(j1)

#Keep only variables we need for this analysis
j2a = j1[,names(j1)%in%c("bmi","snapnowhh","age","sex","raceblack","hispanic","educ","pctpovguidehh",
         "marital","workstatus","tobacco","hhsize","primstoredist_s","rural","adltfscat","wichh",
         "d_super","d_nonsuper","d_fullresto","d_limresto","povrate_p","hhdinc","bbce","hhnum",
         "bbce_asset","call","cap","compdq","ebtissuance","faceini","facerec","fingerprint",
         "noncitadultfull","oapp","outreach","reportsimple","vehexclall","hhwgt","famsize","stfips")]
dim(j2a)
names(j2a)

#Order data set
j2 = j2a[,c(which(names(j2a)=="bmi"),which(names(j2a)=="snapnowhh"),which(names(j2a)=="age"),
      which(names(j2a)=="sex"),which(names(j2a)=="raceblack"),which(names(j2a)=="hispanic"),
      which(names(j2a)=="educ"),which(names(j2a)=="pctpovguidehh"),which(names(j2a)=="marital"),
      which(names(j2a)=="workstatus"),which(names(j2a)=="tobacco"),which(names(j2a)=="hhsize"),
     	which(names(j2a)=="famsize"),
      which(names(j2a)=="primstoredist_s"),which(names(j2a)=="rural"),which(names(j2a)=="adltfscat"),
      which(names(j2a)=="wichh"),which(names(j2a)=="d_super"),which(names(j2a)=="d_nonsuper"),
      which(names(j2a)=="d_fullresto"),which(names(j2a)=="d_limresto"),which(names(j2a)=="povrate_p"),
      which(names(j2a)=="hhdinc"),which(names(j2a)=="bbce"),which(names(j2a)=="bbce_asset"),
      which(names(j2a)=="call"),which(names(j2a)=="cap"),which(names(j2a)=="compdq"),
      which(names(j2a)=="ebtissuance"),which(names(j2a)=="faceini"),which(names(j2a)=="facerec"),
      which(names(j2a)=="fingerprint"),which(names(j2a)=="noncitadultfull"),which(names(j2a)=="oapp"),
      which(names(j2a)=="outreach"),which(names(j2a)=="reportsimple"),which(names(j2a)=="vehexclall"),
      which(names(j2a)=="hhwgt"), which(names(j2a)=="stfips"), which(names(j2a)=="hhnum"))]

names(j2)

#Cut data set to only those 16 or older
j2 = j2[j2$age>=16, ]
dim(j2) #now 10k; was 14k
summary(j2$age)
table(j2$educ, exclude=NULL)

#Save analysis data set with only the variables we need
#write.csv(j2, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/Data_2017-06-13.csv",
           row.names=FALSE)

#Create unique row name
j2$ID = rownames(j2)

#Categorize education
summary(j2$educ)
j2$educ.cat2 = as.factor(cut2(j2$educ , cuts=c(14,18)))
table(j2$educ.cat2)

#Quickly look at marital variable (1=married, 2=widowed, 3=divorced, 4=separated, 5=never married)
table(j2$marital, exclude=NULL)
j2$marital2 = ifelse(j2$marital==1, 1, 0) #married yes/no variable
table(j2$marital2, j2$marital)

#How many households
dim(j2)
length(table(j2$hhnum))
nh = by(j2$hhnum, j2$hhnum, length)
table(nh) #What to do here?

##Household data set (for later merging)
hh = j2[, names(j2) %in% c("ID", "hhnum")]

#########
#TABLE 1#
#########

#Look at some descriptives of data
tab1 = mktab(data=j2,var.names=c("age","sex","raceblack","hispanic","educ.cat2","marital",
        "hhsize","pctpovguidehh","d_super","rural","povrate_p"),
	  ind.cat=c(0,1,1,1,1,1,0,0,0,1,0),group.name="snapnowhh",
        cfn=describeMean,miss="always",pval=TRUE,tot="last",digit=1)

##########
#FIGURE 1#
##########

#Re-code instrumental variables so that higher is "more encouraged into SNAP"
table(j2$bbce,exclude = NULL) #no need to recode

table(j2$bbce_asset,exclude = NULL)
j2$bbce_asset2 = NA
j2$bbce_asset2[j2$bbce_asset==-9] = 0
j2$bbce_asset2[j2$bbce_asset==0] = 1/2
j2$bbce_asset2[j2$bbce_asset==1] = 1
table(j2$bbce_asset,j2$bbce_asset2,exclude=NULL)

table(j2$call,exclude=NULL)
j2$call2 = NA
j2$call2[j2$call==0] = 0
j2$call2[j2$call==2] = 1/2
j2$call2[j2$call==1] = 1
table(j2$call,j2$call2,exclude=NULL)

table(j2$cap,exclude=NULL) #no need to change

table(j2$compdq,exclude=NULL)
j2$compdq2 = NA
j2$compdq2[j2$compdq==1] = 0
j2$compdq2[j2$compdq==0] = 1
table(j2$compdq,j2$compdq2,exclude=NULL)

table(j2$ebtissuance,exclude=NULL) #all 1; useless

table(j2$faceini,exclude=NULL)
j2$faceini2 = NA
j2$faceini2[j2$faceini==0] = 0
j2$faceini2[j2$faceini==2] = 1/2
j2$faceini2[j2$faceini==1] = 1
table(j2$faceini,j2$faceini2,exclude=NULL)

table(j2$facerec,exclude=NULL)
j2$facerec2 = NA
j2$facerec2[j2$facerec==0] = 0
j2$facerec2[j2$facerec==2] = 1/2
j2$facerec2[j2$facerec==1] = 1
table(j2$facerec,j2$facerec2,exclude=NULL)

table(j2$fingerprint,exclude=NULL)
j2$fingerprint2 = NA
j2$fingerprint2[j2$fingerprint==1] = 0
j2$fingerprint2[j2$fingerprint==2] = 1/2
j2$fingerprint2[j2$fingerprint==0] = 1
table(j2$fingerprint,j2$fingerprint2,exclude=NULL)

table(j2$noncitadultfull,exclude=NULL)

table(j2$oapp,exclude=NULL)
j2$oapp2 = NA
j2$oapp2[j2$oapp==0] = 0
j2$oapp2[j2$oapp==2] = 1/2
j2$oapp2[j2$oapp==1] = 1
table(j2$oapp,j2$oapp2,exclude=NULL)

summary(as.numeric(j2$outreach))
j2$outreach2 = as.numeric(j2$outreach)/max(as.numeric(j2$outreach), na.rm=TRUE)
summary(j2$outreach2)

table(j2$reportsimple,exclude=NULL)

table(j2$vehexclall,exclude=NULL)

#Calculate combined IV
table(j2$snapnowhh,exclude=NULL) #29 missing

#Function to get deviance from unadjusted model
get.dev = function(var.name,fact=FALSE) {
j2$IV = j2[,which(names(j2)==var.name)]
if (fact==FALSE) {
m3 = glm(snapnowhh~IV,data=j2,family=binomial)
j = anova(m3,test="Chisq")
d = j[which(rownames(j)=="IV"),which(colnames(j)=="Deviance")]
}
else if (fact==TRUE) {
m3 = glm(snapnowhh~factor(IV),data=j2,family=binomial)
j = anova(m3,test="Chisq")
d = j[which(rownames(j)=="factor(IV)"),which(colnames(j)=="Deviance")]
}
d
}

w1 = get.dev("bbce",fact=TRUE)
w2 = get.dev("bbce_asset2",fact=TRUE)
w3 = get.dev("call2",fact=TRUE)
w4 = get.dev("cap",fact=TRUE)
w5 = get.dev("compdq2",fact=TRUE)
#ebtissuance
w6 = get.dev("faceini2",fact=TRUE)
w7 = get.dev("facerec2",fact=TRUE)
w8 = get.dev("fingerprint2",fact=TRUE)
w9 = get.dev("noncitadultfull",fact=TRUE)
w10 = get.dev("oapp2",fact=TRUE)
w11 = get.dev("outreach2",fact=FALSE)
w12 = get.dev("reportsimple",fact=TRUE)
w13 = get.dev("vehexclall",fact=TRUE)
wsum = w1+w2+w3+w4+w5+w6+w7+w8+w9+w10+w11+w12+w13
wsum2 = w1+w2+w3+w4+w5+w6+w7+w8+w9+w10 #only keep IVs with sufficient variation

j2$IVcomb = (w1*j2$bbce+w2*j2$bbce_asset2+w3*j2$call2+w4*j2$cap+w5*j2$compdq2
            +w6*j2$faceini2+w7*j2$facerec2+w8*j2$fingerprint2+w9*j2$noncitadultfull
            +w10*j2$oapp2+w11*j2$outreach2+w12*j2$reportsimple+w13*j2$vehexclall)/wsum
summary(j2$IVcomb)
table(j2$stfips,j2$IVcomb)

#Define second IV as composite of IVs with sufficient variation (SD>0.4)
IVnames = c("bbce","bbce_asset2","call2","cap","compdq","faceini","facerec","fingerprint2",
            "noncitadultfull","oapp2","outreach2","reportsimple","vehexclall")
apply(j2[, names(j2) %in% IVnames], 2, function(x) sd(x[!is.na(x)]))

#remove reportsimple, vehexclall, call2, outreach2
j2$IVcomb2 = (w1*j2$bbce+w2*j2$bbce_asset2+w4*j2$cap+w5*j2$compdq2
            +w6*j2$faceini2+w7*j2$facerec2+w8*j2$fingerprint2+w9*j2$noncitadultfull
            +w10*j2$oapp2)/wsum2

summary(j2$IVcomb2)

#Appendix Figure 2: Correlation heatmap of IVs
cors = j2[, names(j2) %in% c("bbce", "bbce_asset2", "call2", "cap", "compdq", "faceini",
           "facerec", "fingerprint2", "noncitadultfull", "oapp2", "outreach2", "reportsimple",
           "vehexclall", "IVcomb", "IVcomb2")]
cc = cor(cors, use="pairwise.complete.obs")
grid0 = expand.grid(rows=rownames(cc), cols=colnames(cc))
grid0$Z = as.numeric(cc)

pdf("K:/Research/Research_Stanford_Basu/FoodAPS/Figures/AppFig2_2017-07-02.pdf",
 width=8,height=8)
hm(form="Z~cols*rows",dta=grid0, col1="grey", col2="lightcoral",
 addnum=FALSE, xtitle="Instrument", ytitle="", rotx=45)
dev.off()

#Figure 2: Heatmaps of IVs by state
fig1 = data.frame(
bbce = as.numeric(by(j2$bbce,j2$stfips,function(x) mean(x,na.rm=TRUE))),
bbce_asset = as.numeric(by(j2$bbce_asset2,j2$stfips,function(x) mean(x,na.rm=TRUE))),
call = as.numeric(by(j2$call2,j2$stfips,function(x) mean(x,na.rm=TRUE))),
cap = as.numeric(by(j2$cap,j2$stfips,function(x) mean(x,na.rm=TRUE))),
compdq = as.numeric(by(j2$compdq2,j2$stfips,function(x) mean(x,na.rm=TRUE))),
faceini = as.numeric(by(j2$faceini2,j2$stfips,function(x) mean(x,na.rm=TRUE))),
facerec = as.numeric(by(j2$facerec2,j2$stfips,function(x) mean(x,na.rm=TRUE))),
fingerprint = as.numeric(by(j2$fingerprint2,j2$stfips,function(x) mean(x,na.rm=TRUE))),
noncitadultfull = as.numeric(by(j2$noncitadultfull,j2$stfips,function(x) mean(x,na.rm=TRUE))),
oapp = as.numeric(by(j2$oapp2,j2$stfips,function(x) mean(x,na.rm=TRUE))),
outreach = as.numeric(by(j2$outreach2,j2$stfips,function(x) mean(x,na.rm=TRUE))),
reportsimple = as.numeric(by(j2$reportsimple,j2$stfips,function(x) mean(x,na.rm=TRUE))),
vehexclall = as.numeric(by(j2$vehexclall,j2$stfips,function(x) mean(x,na.rm=TRUE))),
IVcomb = as.numeric(by(j2$IVcomb,j2$stfips,function(x) mean(x,na.rm=TRUE))),
IVcomb2 = as.numeric(by(j2$IVcomb2,j2$stfips,function(x) mean(x,na.rm=TRUE)))
)
rownames(fig1) = unlist(lapply(names(by(j2$IVcomb2,j2$stfips,
                  function(x) mean(x,na.rm=TRUE))),st.fips))

#Number per state
nstate = data.frame(state=rownames(fig1),
         n=as.numeric(by(j2$stfips,j2$stfips,length)))
sum(nstate$n)

fig2 = fig1[order(fig1$IVcomb2),] #order by second IV
rownames(fig2) = c("ST27","ST26","ST25","ST24","ST23","ST22","ST21","ST20","ST19",
                   "ST18","ST17","ST16","ST15","ST14","ST13","ST12","ST11","ST10",
                   "ST09","ST08","ST07","ST06","ST05","ST04","ST03","ST02","ST01")
#anonymize row names

#mix up columns and name "policy 1", etc.
fig3 = fig2[, c(sample(1:13,13),14,15)]
names(fig3) = c("Policy 1", "Policy 2", "Policy 3", "Policy 4", "Policy 5", "Policy 6",
                "Policy 7", "Policy 8", "Policy 9", "Policy 10", "Policy 11", "Policy 12",
                "Policy 13", "IVcomb", "IVcomb2")

grid = mk.grid(fig3)

pdf("K:/Research/Research_Stanford_Basu/FoodAPS/Figures/Fig1_2017-06-22.pdf",
 width=8,height=8)
hm(form="Z~cols*rows",dta=grid,col1="grey",col2="lightcoral",
 addnum=FALSE,xtitle="Instrument",ytitle="",rotx=45)
dev.off()


########################################################################
#STATISTICAL MODELING - Results in Table 2 plus Appendix Tables 5 and 6#
########################################################################

##OLS
#Center continuous variables for interpretation
j2$age.C = j2$age-median(j2$age,na.rm=TRUE)
j2$educ.C = j2$educ-median(j2$educ,na.rm=TRUE)
j2$pctpovguidehh.C = j2$pctpovguidehh-median(j2$pctpovguidehh,na.rm=TRUE)
j2$hhsize.C = j2$hhsize-median(j2$hhsize, na.rm=TRUE)
summary(j2$hhsize.C)
table(j2$marital, exclude=NULL)

#First OLS (ind/HH only)
ols1 = lm.cluster(bmi~(snapnowhh==1)+age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+pctpovguidehh.C+hhsize.C+as.factor(marital), cluster="hhnum", data=j2)

summary(ols1)
confint(ols1)
anova(ols1)


#Second OLS (ind/HH + neighborhood)
j2$primstoredist_s.C = j2$primstoredist_s-median(j2$primstoredist_s,na.rm=TRUE)
j2$povrate_p.C = j2$povrate_p-median(j2$povrate_p,na.rm=TRUE)

ols2 = lm.cluster(bmi~(snapnowhh==1)+age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+pctpovguidehh.C+hhsize.C+as.factor(marital)+
         primstoredist_s.C+(rural==1)+povrate_p.C, cluster="hhnum", data=j2)

summary(ols2)
confint(ols2)
anova(ols2)

#################################
#Instrumental variables analysis#
#################################

#Complete case variable lists for near-far match + IV regression
names1 = c("age", "sex", "raceblack", "hispanic", "educ", "pctpovguidehh", "marital", "hhsize")
names2 = c("age", "sex", "raceblack", "hispanic", "educ", "pctpovguidehh", "marital", "hhsize",
           "primstoredist_s", "rural", "povrate_p")

#Complete case data sets for models to follow
d1 = j2[complete.cases(j2[, names(j2) %in% c(names1, "bmi", "snapnowhh", "IVcomb")]), ] #ind/hh (9961)
d2 = j2[complete.cases(j2[, names(j2) %in% c(names2, "bmi", "snapnowhh", "IVcomb")]), ] #ind/hh/county (9328)

#IV1 (ind/HH)
#Analogous 2SLS models with IVcomb
iv1 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+pctpovguidehh.C + hhsize.C+as.factor(marital) + (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C+
         pctpovguidehh.C+hhsize.C+as.factor(marital), data=d1)

summary(iv1, diagnostics=TRUE)
confint(iv1)
cc = cluster.robust.se(iv1, d1$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se

#IV2 (ind/HH)
#Analogous 2SLS models with IVcomb2
iv1.2 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+pctpovguidehh.C + hhsize.C+as.factor(marital) + (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C+
         pctpovguidehh.C+hhsize.C+as.factor(marital), data=d1)

summary(iv1.2,diagnostics=TRUE)
confint(iv1.2)
cc = cluster.robust.se(iv1.2, d1$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se


#IV1 (ind/HH/neighborhood)
iv2 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+pctpovguidehh.C+ hhsize.C+as.factor(marital)+
          primstoredist_s.C+(rural==1)+povrate_p.C
          + (snapnowhh==1) | IVcomb +age.C+(sex==2)+
         (raceblack==1)+(hispanic==1)+educ.C+pctpovguidehh.C+
          hhsize.C+as.factor(marital)
         +primstoredist_s.C+(rural==1)+povrate_p.C,data=d2)

summary(iv2,diagnostics=TRUE) #now association has disappeared
confint(iv2)
cc = cluster.robust.se(iv2, d2$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se

#IV2 (ind/HH/neighborhood; IVcomb2)
iv2.2 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+pctpovguidehh.C+ hhsize.C+as.factor(marital)+
          primstoredist_s.C+(rural==1)+povrate_p.C
          + (snapnowhh==1) | IVcomb2 +age.C+(sex==2)+
         (raceblack==1)+(hispanic==1)+educ.C+pctpovguidehh.C+
          hhsize.C+as.factor(marital)
         +primstoredist_s.C+(rural==1)+povrate_p.C,data=d2)

summary(iv2.2,diagnostics=TRUE) #now association has disappeared
confint(iv2.2)
cc = cluster.robust.se(iv2.2, d2$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se


##################
#Near-far matches#
##################

######################################################
#Ind/hh covariates only - Results in Appendix Table 4#
######################################################

#25% sinks, SD=1
nf.1.25.1 = matches(dta=d1, covs=names1, iv="IVcomb", imp.var = NA, tol.var = NA,
         sinks = 0.25, cutpoint = sd(d1$IVcomb))
#save.image(file="K:/Research/Research_Stanford_Basu/FoodAPS/Data/save.RData")
#saveRDS(nf.1.25.1, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-25-1.rds")
#fnn = readRDS("K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-25-1.rds")

iv.1.25.1 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +  (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C, data=d1[nf.1.25.1,])

summary(iv.1.25.1,diagnostics=TRUE)
confint(iv.1.25.1)
cc = cluster.robust.se(iv.1.25.1, d1[nf.1.25.1,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d1, match=nf.1.25.1, outc="bmi", trt="snapnowhh", alpha=0.05)

#50% sinks, SD=1
nf.1.50.1 = matches(dta=d1, covs=names1, iv="IVcomb", imp.var = NA, tol.var = NA,
         sinks = 0.50, cutpoint = sd(d1$IVcomb)) #report for main table
#saveRDS(nf.1.50.1, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-50-1.rds")

iv.1.50.1 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +  (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C, data=d1[nf.1.50.1,])

summary(iv.1.50.1,diagnostics=TRUE)
confint(iv.1.50.1)
cc = cluster.robust.se(iv.1.50.1, d1[nf.1.50.1,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d1, match=nf.1.50.1, outc="bmi", trt="snapnowhh", alpha=0.05)

#25% sinks, SD=2
nf.1.25.2 = matches(dta=d1, covs=names1, iv="IVcomb", imp.var = NA, tol.var = NA,
         sinks = 0.25, cutpoint = 2*sd(d1$IVcomb))
#summ_matches(dta=d1, iv="IVcomb", covs=names1, match=nf.1.25.2)
#saveRDS(nf.1.25.2, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-25-2.rds")

iv.1.25.2 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +  (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C, data=d1[nf.1.25.2,])

summary(iv.1.25.2,diagnostics=TRUE)
confint(iv.1.25.2)
cc = cluster.robust.se(iv.1.25.2, d1[nf.1.25.2,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d1, match=nf.1.25.2, outc="bmi", trt="snapnowhh", alpha=0.05)

#50% sinks, SD=2
nf.1.50.2 = matches(dta=d1, covs=names1, iv="IVcomb", imp.var = NA, tol.var = NA,
         sinks = 0.50, cutpoint = 2*sd(d1$IVcomb))
#saveRDS(nf.1.50.2, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-50-2.rds")

iv.1.50.2 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +  (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C, data=d1[nf.1.50.2,])

summary(iv.1.50.2, diagnostics=TRUE)
confint(iv.1.50.2)
cc = cluster.robust.se(iv.1.50.2, d1[nf.1.50.2,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d1, match=nf.1.50.2, outc="bmi", trt="snapnowhh", alpha=0.05)

##Repeat analyses for second instrument##

#25% sinks, SD=1
nf.2.25.1 = matches(dta=d1, covs=names1, iv="IVcomb2", imp.var = NA, tol.var = NA,
         sinks = 0.25, cutpoint = sd(d1$IVcomb2))
#saveRDS(nf.2.25.1, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf2-25-1.rds")

iv.2.25.1 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +  (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C, data=d1[nf.2.25.1,])

summary(iv.2.25.1,diagnostics=TRUE)
confint(iv.2.25.1)
cc = cluster.robust.se(iv.2.25.1, d1[nf.2.25.1,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d1, match=nf.2.25.1, outc="bmi", trt="snapnowhh", alpha=0.05)

#50% sinks, SD=1
nf.2.50.1 = matches(dta=d1, covs=names1, iv="IVcomb2", imp.var = NA, tol.var = NA,
         sinks = 0.50, cutpoint = sd(d1$IVcomb2))
#saveRDS(nf.2.50.1, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf2-50-1.rds")

iv.2.50.1 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +  (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C, data=d1[nf.2.50.1,])

summary(iv.2.50.1,diagnostics=TRUE)
confint(iv.2.50.1)
cc = cluster.robust.se(iv.2.50.1, d1[nf.2.50.1,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d1, match=nf.2.50.1, outc="bmi", trt="snapnowhh", alpha=0.05)

#25% sinks, SD=2
nf.2.25.2 = matches(dta=d1, covs=names1, iv="IVcomb2", imp.var = NA, tol.var = NA,
         sinks = 0.25, cutpoint = 2*sd(d1$IVcomb2))
#saveRDS(nf.2.25.2, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf2-25-2.rds")

#summ_matches(dta=d1, iv="IVcomb", covs=names1, match=nf.1.25.2)

iv.2.25.2 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +  (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C, data=d1[nf.2.25.2,])

summary(iv.2.25.2,diagnostics=TRUE)
confint(iv.2.25.2)
cc = cluster.robust.se(iv.2.25.2, d1[nf.2.25.2,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d1, match=nf.2.25.2, outc="bmi", trt="snapnowhh", alpha=0.05)

#50% sinks, SD=2
nf.2.50.2 = matches(dta=d1, covs=names1, iv="IVcomb2", imp.var = NA, tol.var = NA,
         sinks = 0.50, cutpoint = 2*sd(d1$IVcomb2))
#saveRDS(nf.2.50.2, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf2-50-2.rds")

iv.2.50.2 = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +  (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C, data=d1[nf.2.50.2,])

summary(iv.2.50.2,diagnostics=TRUE)
confint(iv.2.50.2)
cc = cluster.robust.se(iv.2.50.2, d1[nf.2.50.2,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d1, match=nf.2.50.2, outc="bmi", trt="snapnowhh", alpha=0.05)

######################################################################
#Setup 2 - add county level confounders - Results in Appendix Table 5#
######################################################################

#25% sinks; SD=1
nf.1.25.1a = matches(dta=d2, covs=names2, iv="IVcomb", imp.var = NA, tol.var = NA,
         sinks = 0.25, cutpoint = sd(d2$IVcomb))
#saveRDS(nf.1.25.1a, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-25-1a.rds")

iv.1.25.1a = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +
         +primstoredist_s.C+(rural==1)+povrate_p.C + (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C
         +primstoredist_s.C+(rural==1)+povrate_p.C, data=d2[nf.1.25.1a,])

summary(iv.1.25.1a,diagnostics=TRUE)
confint(iv.1.25.1a)
cc = cluster.robust.se(iv.1.25.1a, d2[nf.1.25.1a,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d2, match=nf.1.25.1a, outc="bmi", trt="snapnowhh", alpha=0.05)

#50% sinks; SD=1
nf.1.50.1a = matches(dta=d2, covs=names2, iv="IVcomb", imp.var = NA, tol.var = NA,
         sinks = 0.50, cutpoint = sd(d2$IVcomb))
#saveRDS(nf.1.50.1a, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-50-1a.rds")
iv.1.50.1a = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +
         +primstoredist_s.C+(rural==1)+povrate_p.C + (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C
         +primstoredist_s.C+(rural==1)+povrate_p.C, data=d2[nf.1.50.1a,])

summary(iv.1.50.1a,diagnostics=TRUE)
confint(iv.1.50.1a)
cc = cluster.robust.se(iv.1.50.1a, d2[nf.1.50.1a,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d2, match=nf.1.50.1a, outc="bmi", trt="snapnowhh", alpha=0.05)

#25% sinks; SD=2
nf.1.25.2a = matches(dta=d2, covs=names2, iv="IVcomb", imp.var = NA, tol.var = NA,
         sinks = 0.25, cutpoint = 2*sd(d2$IVcomb))
#saveRDS(nf.1.25.2a, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-25-2a.rds")

iv.1.25.2a = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +
         +primstoredist_s.C+(rural==1)+povrate_p.C + (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C
         +primstoredist_s.C+(rural==1)+povrate_p.C, data=d2[nf.1.25.2a,])

summary(iv.1.25.2a,diagnostics=TRUE)
confint(iv.1.25.2a)
cc = cluster.robust.se(iv.1.25.2a, d2[nf.1.25.2a,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d2, match=nf.1.25.2a, outc="bmi", trt="snapnowhh", alpha=0.05)

#50% sinks; SD=2
nf.1.50.2a = matches(dta=d2, covs=names2, iv="IVcomb", imp.var = NA, tol.var = NA,
         sinks = 0.50, cutpoint = 2*sd(d2$IVcomb))
#saveRDS(nf.1.50.2a, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-50-2a.rds")
iv.1.50.2a = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +
         +primstoredist_s.C+(rural==1)+povrate_p.C + (snapnowhh==1) |
         IVcomb +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C
         +primstoredist_s.C+(rural==1)+povrate_p.C, data=d2[nf.1.50.2a,])

summary(iv.1.50.2a,diagnostics=TRUE)
confint(iv.1.50.2a)
cc = cluster.robust.se(iv.1.50.2a, d2[nf.1.50.2a,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d2, match=nf.1.50.2a, outc="bmi", trt="snapnowhh", alpha=0.05)


##Repeat analyses for second instrument##

#25% sinks; SD=1
nf.2.25.1a = matches(dta=d2, covs=names2, iv="IVcomb2", imp.var = NA, tol.var = NA,
         sinks = 0.25, cutpoint = sd(d2$IVcomb2))
#saveRDS(nf.2.25.1a, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf2-25-1a.rds")

iv.2.25.1a = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +
         +primstoredist_s.C+(rural==1)+povrate_p.C + (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C
         +primstoredist_s.C+(rural==1)+povrate_p.C, data=d2[nf.2.25.1a,])

summary(iv.2.25.1a,diagnostics=TRUE)
confint(iv.2.25.1a)
cc = cluster.robust.se(iv.2.25.1a, d2[nf.2.25.1a,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d2, match=nf.2.25.1a, outc="bmi", trt="snapnowhh", alpha=0.05)

#50% sinks; SD=1
nf.2.50.1a = matches(dta=d2, covs=names2, iv="IVcomb2", imp.var = NA, tol.var = NA,
         sinks = 0.50, cutpoint = sd(d2$IVcomb2))
#saveRDS(nf.2.50.1a, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf2-50-1a.rds")

iv.2.50.1a = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +
         +primstoredist_s.C+(rural==1)+povrate_p.C + (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C
         +primstoredist_s.C+(rural==1)+povrate_p.C, data=d2[nf.2.50.1a,])

summary(iv.2.50.1a,diagnostics=TRUE)
confint(iv.2.50.1a)
cc = cluster.robust.se(iv.2.50.1a, d2[nf.2.50.1a,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d2, match=nf.2.50.1a, outc="bmi", trt="snapnowhh", alpha=0.05)

#25% sinks; SD=2
nf.2.25.2a = matches(dta=d2, covs=names2, iv="IVcomb2", imp.var = NA, tol.var = NA,
         sinks = 0.25, cutpoint = 2*sd(d2$IVcomb2))
#saveRDS(nf.2.25.2a, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf2-25-2a.rds")

iv.2.25.2a = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +
         +primstoredist_s.C+(rural==1)+povrate_p.C + (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C
         +primstoredist_s.C+(rural==1)+povrate_p.C, data=d2[nf.2.25.2a,])

summary(iv.2.25.2a,diagnostics=TRUE)
confint(iv.2.25.2a)
cc = cluster.robust.se(iv.2.25.2a, d2[nf.2.25.2a,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d2, match=nf.2.25.2a, outc="bmi", trt="snapnowhh", alpha=0.05)

#50% sinks; SD=2
nf.2.50.2a = matches(dta=d2, covs=names2, iv="IVcomb2", imp.var = NA, tol.var = NA,
         sinks = 0.50, cutpoint = 2*sd(d2$IVcomb))
#saveRDS(nf.2.50.2a, "K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf2-50-2a.rds")
iv.2.50.2a = ivreg(bmi~age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+ as.factor(marital) + hhsize.C +pctpovguidehh.C +
         +primstoredist_s.C+(rural==1)+povrate_p.C + (snapnowhh==1) |
         IVcomb2 +age.C+(sex==2)+ (raceblack==1)+(hispanic==1)+educ.C +
         as.factor(marital) + hhsize.C +pctpovguidehh.C
         +primstoredist_s.C+(rural==1)+povrate_p.C, data=d2[nf.2.50.2a,])

summary(iv.2.50.2a,diagnostics=TRUE)
confint(iv.2.50.2a)
cc = cluster.robust.se(iv.2.50.2a, d2[nf.2.50.2a,]$hhnum)
est = cc[rownames(cc)=="snapnowhh == 1TRUE", 1]
se = cc[rownames(cc)=="snapnowhh == 1TRUE", 2]
est
est-qnorm(0.975)*se
est+qnorm(0.975)*se
eff_ratio(dta=d2, match=nf.2.50.2a, outc="bmi", trt="snapnowhh", alpha=0.05)

###############################################
#Propensity score matches - Results in Table 2#
###############################################

##Ind/HH only
psm = glm(snapnowhh ~ age.C + sex + raceblack + hispanic + educ.C + as.factor(marital) +
           hhsize.C, family=binomial, data=d1)
length(predict(psm))
boxplot(psm)

#Trim bottom 90 and top 10
ps = predict(psm, type="response")
dd = data.frame(ID=names(ps), ps=ps)
dd$dec = as.numeric(cut2(dd$ps, g=10))
d1a = merge(d1, dd, by="ID")
d1b = d1a[!d1a$dec %in% c(1,10), ]

#Execute match
m1 = matchit(snapnowhh ~ age.C+sex+raceblack+hispanic+educ.C+
      hhsize.C+factor(marital), data=d1b[, !names(d1b) %in%
      c("primstoredist_s", "wichh", "primstoredist_s.C", "tobacco", "workstatus")],
      method="nearest")

d1.m1 = match.data(m1)

#Fit model
ols1.m1 = lm.cluster(bmi~(snapnowhh==1)+age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+pctpovguidehh.C+hhsize.C+as.factor(marital), cluster="hhnum", data=d1.m1)

summary(ols1.m1)
confint(ols1.m1)
anova(ols1.m1)

##Bigger PSM as requested by reviewer
p1 = j1[rownames(j1) %in% rownames(d1), ]

#Find 50 or so non-missing variables
apply(p1, 2, function(x) mean(is.na(x)))

psm1 = glm(snapnowhh ~ age + sex + raceblack + hispanic + educ + as.factor(marital) +
           hhsize + nutritioneduc + anydieting + huntfish + gardenown + foodpantry +
           foodsufficient + foodsecureq2 + wiccategelig + farmworkerhh, family=binomial, data=p1)

ps1 = predict(psm1, type="response")
dd1 = data.frame(ID=names(ps1), ps=ps1)
dd1$dec = as.numeric(cut2(dd1$ps, g=10))
d1c = merge(d1, dd1, by="ID")
d1d = d1c[!d1c$dec %in% c(1,10), ]
p1a = p1[rownames(p1) %in% d1d$ID, ]
m1a = matchit(snapnowhh ~ age + sex + raceblack + hispanic + educ + as.factor(marital) +
           hhsize + nutritioneduc + anydieting + huntfish + gardenown + foodpantry +
           foodsufficient +
           foodsecureq2 + wiccategelig + farmworkerhh, data=p1a[, names(p1a) %in%
           c("snapnowhh", "age", "sex", "raceblack", "hispanic", "educ", "marital",
           "hhsize", "nutritioneduc", "anydieting", "huntfish", "gardenown", "foodpantry",
           "altstoresource", "primstoretravelmode", "primstoresource", "foodsufficient",
           "foodsecureq2", "wiccategelig", "farmworkerhh", "bmi", "pctpovguidehh", "hhnum")],
           method="nearest")

d1.m1a = match.data(m1a)
ols1.m1a = lm.cluster(bmi~(snapnowhh==1)+ age + sex + raceblack + hispanic + educ + as.factor(marital) +
           hhsize + pctpovguidehh + nutritioneduc + anydieting + huntfish + gardenown + foodpantry +
           foodsufficient + foodsecureq2 + wiccategelig + farmworkerhh, cluster="hhnum", data=d1.m1a)
           #0.97 (0.57, 1.37)
summary(ols1.m1a)
confint(ols1.m1a)

##Ind/HH/county
psm2 = glm(snapnowhh ~ age.C + sex + raceblack + hispanic + educ.C + as.factor(marital) +
           hhsize.C+primstoredist_s.C+rural+povrate_p.C,
           family=binomial, data=d2)
length(predict(psm2))
boxplot(psm2)

#Trim bottom 90 and top 10
ps2 = predict(psm2, type="response")
dd2 = data.frame(ID=names(ps2), ps=ps2)
dd2$dec = as.numeric(cut2(dd2$ps, g=10))
d2a = merge(d2, dd2, by="ID")
d2b = d2a[!d2a$dec %in% c(1,10), ]

#Execute match
m2 = matchit(snapnowhh ~ age.C+sex+raceblack+hispanic+educ.C+as.factor(marital)+hhsize.C+
      +primstoredist_s.C+rural+povrate_p.C, data=d2b[, !names(d2b) %in%
      c("wichh", "tobacco", "workstatus")], method="nearest")

d2.m2 = match.data(m2)

#Fit model
ols2.m2 = lm.cluster(bmi~(snapnowhh==1)+age.C+(sex==2)+(raceblack==1)+(hispanic==1)+
         educ.C+pctpovguidehh.C+hhsize.C+as.factor(marital)+
         primstoredist_s.C+(rural==1)+povrate_p.C, cluster="hhnum", data=d2.m2)

summary(ols2.m2)
confint(ols2.m2)
anova(ols2.m2)

##Bigger PSM as requested by reviewer
p2 = j1[rownames(j1) %in% rownames(d2), ]

#Find 50 or so non-missing variables
apply(p2, 2, function(x) mean(is.na(x)))

psm2 = glm(snapnowhh ~ age + sex + raceblack + hispanic + educ + as.factor(marital) +
           hhsize + nutritioneduc + anydieting + huntfish + gardenown + foodpantry +
           altstoresource + primstoretravelmode + primstoresource + foodsufficient +
           foodsecureq2 + wiccategelig + farmworkerhh + primstoredist_s + rural +
           povrate_p + primstoredist_d + primstoretime_d + primstoretype,
           family=binomial, data=p2)

ps2 = predict(psm2, type="response")
dd2 = data.frame(ID=names(ps2), ps=ps2)
dd2$dec = as.numeric(cut2(dd2$ps, g=10))
d2c = merge(d2, dd2, by="ID")
d2d = d2c[!d2c$dec %in% c(1,10), ]
p2a = p2[rownames(p2) %in% d2d$ID, ]
m2a = matchit(snapnowhh ~ age + sex + raceblack + hispanic + educ + as.factor(marital) +
           hhsize + nutritioneduc + anydieting + huntfish + gardenown + foodpantry +
           altstoresource + primstoretravelmode + primstoresource + foodsufficient +
           foodsecureq2 + wiccategelig + farmworkerhh + primstoredist_s + rural +
           povrate_p + primstoredist_d + primstoretime_d + primstoretype,
           data=p2a[, names(p2a) %in%
           c("snapnowhh", "age", "sex", "raceblack", "hispanic", "educ", "marital",
           "hhsize", "nutritioneduc", "anydieting", "huntfish", "gardenown", "foodpantry",
           "altstoresource", "primstoretravelmode", "primstoresource", "foodsufficient",
           "foodsecureq2", "wiccategelig", "farmworkerhh", "bmi", "primstoredist_s", "rural",
           "povrate_p", "primstoredist_d", "primstoretime_d", "primstoretype", "primstoresource",
           "pctpovguidehh", "hhnum")], method="nearest")

d2.m2a = match.data(m2a)
ols2.m2a = lm.cluster(bmi~(snapnowhh==1)+ age + sex + raceblack + hispanic + educ + as.factor(marital) +
           hhsize + pctpovguidehh + nutritioneduc + anydieting + huntfish + gardenown + foodpantry +
           altstoresource + primstoretravelmode + primstoresource + foodsufficient +
           foodsecureq2 + wiccategelig + farmworkerhh + primstoredist_s + rural +
           povrate_p + primstoredist_d + primstoretime_d + primstoretype, cluster="hhnum",
           data=d2.m2a) #0.74 (0.33, 1.15)
summary(ols2.m2a)
confint(ols2.m2a)


###################
#Appendix Figure 2#
###################

#Overlap of PS pre- and post-10/90 cut
pdf("K:/Research/Research_Stanford_Basu/FoodAPS/Figures/PSoverlap_2017-08-21.pdf",
 width=8,height=8)
par(mfrow=c(2,2))
boxplot(d1a$ps ~ d1a$snapnowhh, xaxt="n", xlab="", main="a", ylab="Propensity score", ylim=c(0,1))
axis(1, at=c(1,2), c("SNAP no", "SNAP yes"))
boxplot(d1b$ps ~ d1b$snapnowhh, xaxt="n", xlab="", main="b", ylab="Propensity score", ylim=c(0,1))
axis(1, at=c(1,2), c("SNAP no", "SNAP yes"))

boxplot(d2a$ps ~ d2a$snapnowhh, xaxt="n", xlab="", main="c", ylab="Propensity score", ylim=c(0,1))
axis(1, at=c(1,2), c("SNAP no", "SNAP yes"))
boxplot(d2b$ps ~ d2b$snapnowhh, xaxt="n", xlab="", main="d", ylab="Propensity score", ylim=c(0,1))
axis(1, at=c(1,2), c("SNAP no", "SNAP yes"))
dev.off()

###################
#Appendix Table 2a#
###################
#Summarize data pre- and post-match
#PS-match, ind/hh
summ_matches(dta=d1, iv=NA, covs=c(names1, "marital2"), match=cbind(which(d1$snapnowhh==0),
             c(which(d1$snapnowhh==1), rep(NA,6573-3388)) ))
summ_matches(dta=d1.m1, iv=NA, covs=c(names1, "marital2"), match=cbind(which(d1.m1$snapnowhh==0),
             c(which(d1.m1$snapnowhh==1)) ))
table(d1$snapnowhh)


###################
#Appendix Table 2b#
###################
#PS-match, ind/hh/county
summ_matches(dta=d2, iv=NA, covs=c(names2, "marital2"), match=cbind(which(d2$snapnowhh==0),
             c(which(d2$snapnowhh==1), rep(NA,6180-3148)) ))
summ_matches(dta=d2.m2, iv=NA, covs=c(names2, "marital2"), match=cbind(which(d2.m2$snapnowhh==0),
             c(which(d2.m2$snapnowhh==1)) ))
table(d2$snapnowhh)

#summ.matches(d2.m2, cbind(which(d2.m2$snapnowhh==0), which(d2.m2$snapnowhh==1)))

###################
#Appendix Table 3a#
###################
#NF-match, ind/hh (nf.1.50.1)
d1$med = ifelse(d1$IVcomb<median(d1$IVcomb), 0, 1)
table(d1$med)
summ_matches(dta=d1, iv="IVcomb", covs=c(names1, "marital2"), match=cbind(c(which(d1$med==0),
             rep(NA, 5016-4945)), which(d1$med==1)))
summ_matches(dta=d1, iv="IVcomb", covs=c(names1, "marital2"), match=nf.1.50.1)

###################
#Appendix Table 3b#
###################
#NF-match, ind/hh/county (nf.1.50.1a)
d2$med = ifelse(d2$IVcomb<median(d2$IVcomb), 0, 1)
table(d2$med)
summ_matches(dta=d2, iv="IVcomb", covs=c(names2, "marital2"), match=cbind(c(which(d2$med==0),
             rep(NA, 12)), which(d2$med==1)))
summ_matches(dta=d2, iv="IVcomb", covs=c(names2, "marital2"), match=nf.1.50.1a)

################################################
#Appendix Table 6: Demographics in/out of match#
################################################
names(d2)
nf.1.50.1 = readRDS("K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-50-1.rds")
nf.1.50.1a = readRDS("K:/Research/Research_Stanford_Basu/FoodAPS/Data/nf1-50-1a.rds")
d2aa = d2[nf.1.50.1a, ]
d2$inmatch = ifelse(rownames(d2) %in% rownames(d2aa), 1, 0)
table(d2$inmatch)

app6 = mktab(data=d2, var.names=c("age","sex","raceblack","hispanic","educ.cat2","marital",
        "hhsize","pctpovguidehh","d_super","rural","povrate_p"),
	  ind.cat=c(0,1,1,1,1,1,0,0,0,1,0),group.name="inmatch",
        cfn=describeMean, miss="always", pval=TRUE, tot="last", digit=1)

#Citations for paper
citation("optmatch")
citation("nearfar")
citation()
R.Version()
