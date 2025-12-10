# NMA ANALYSIS -----

library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)
library(knitr)
library(ggplot2)
library(netmeta)
library(robvis)
library(purrr)
library(readr)

# NOTES 
# the suffix _subX (where X is a number) denotes subgroups 
# the suffix _subnX denotes a subnetwork 
# n is used to denote nonclinical source e.g. nma_anxn = nonclinical anxiety nma object 
# c is used to denote clinical source data e.g. dfc_sad = clinical social anxiety data 
# code to write results to txt or save graphs has been supressed with a # 

### source cleaning script ----
source("util/pwma_analysis.R")

# **CLINICAL SOURCE** ----

# SOCIAL ANXIETY ----

# define results location 
#if (#dir.exists('result/nma/clinical') == FALSE){
#dir.create('result/nma/clinical')
#}

#if (#dir.exists('result/nma/clinical/social_anxiety') == FALSE){
#dir.create('result/nma/clinical/social_anxiety')
#}


# define treatment 
dfc_sad$trt <- dfc_sad$node

##### create subgroup dfs ----
dfc_sad_sub1 <- subset(dfc_sad, dfc_sad$target_bias == "attention")
dfc_sad_sub2 <- subset(dfc_sad, dfc_sad$session == "multi")
dfc_sad_sub3 <- subset(dfc_sad, dfc_sad$target_bias == "interpretation")

##### create pairwise data ----
pw_sa <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfc_sad,
  sm = "SMD",
)

##### identify subnetworks ----
netconn <- netconnection(pw_sa)

##### add subnetwork into netconn 
pw_sa$subnet <- netconn$subnet
table(pw_sa$subnet)

#### identify disconnected studies ----
pw_sa[pw_sa$subnet == 1, c("studlab", "treat1", "treat2", "TE", "seTE")]

#### exclude subnetwork  ----
pw_sa_connected <- subset(pw_sa, subnet == 2)

### clean pairwise df ----
pw_sa_connected <- pw_sa_connected %>%
  select(-studlab.orig, -baseline_n.x2, -baseline_n.x1, -baseline_mean1, -baseline_mean2,
         -baseline_sd1, -baseline_sd2, -baseline_n.y1, -endpoint_mean1,   
         -endpoint_mean2, -endpoint_sd1, -endpoint_sd2, -baseline_n.y1,    
         -baseline_n.y2, -.seTE)


#### create pw sensitivity data----

# social anx df excluding studies with a high risk of bias on RoB2 
pw_rob <- pw_sa_connected %>%
  dplyr::filter(!grepl("H", pw_sa_connected$rob))

# social anx df excluding studies that did not use diagnostic interviews to screen 
pw_diag <- pw_sa_connected %>%
  dplyr::filter(grepl("TRUE", pw_sa_connected$diagnosis))

#### create pw subgroup data ----
# attentional bias
pw_sad_sub1 <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfc_sad_sub1,
  sm = "SMD"
)
# identify and select relevant subnetwork 
netconn <- netconnection(pw_sad_sub1)
pw_sad_sub1$subnet <- netconn$subnet
pw_sad_sub1_connected <- subset(pw_sad_sub1, subnet == 1)

# interpretation bias 
pw_sad_sub3 <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfc_sad_sub3,
  sm = "SMD"
)
# identify and select relecant subnetwork
netconn <- netconnection(pw_sad_sub3)
pw_sad_sub3$subnet <- netconn$subnet
pw_sad_sub3_connected <- subset(pw_sad_sub3, subnet == 2)

#### run nma ----
# primary
nma_sa <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_sa_connected,
  sm = "SMD",
  reference.group = "Control CBM-A",
)

# attentional bias
nma_sa_sub1 <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_sad_sub1_connected,
  sm = "SMD",
  reference.group = "Control CBM-A"
)

# interpretation bias
nma_sa_sub3 <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_sad_sub3_connected,
  sm = "SMD",
  reference.group = "Control CBM-I"
)

# rob sensitivity
nma_rob_sens <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_rob,
  sm = "SMD",
  reference.group = "Control CBM-A"
)

#diagnosis sensitivity
nma_diag_sens <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_diag,
  sm = "SMD",
  reference.group = "Control CBM-A"
)


#### create netgraph ---- 
#png("result/nma/clinical/social_anxiety/social_anx_netgraph.png", width = 1000, height = 750, res = 150)
#netgraph(nma_sa, plastic=F, number.of.studies=T, cex.points=nma_sa$k.trts/2)
#dev.off()

#png("result/nma/clinical/social_anxiety/sad_rob_sens_netgraph.png", width = 800, height = 600)
#netgraph(nma_rob_sens, plastic=F, number.of.studies=T, cex.points=nma_rob_sens$k.trts/2)
#dev.off()

#png("result/nma/clinical/social_anxiety/sad_diag_sens_netgraph.png", width = 800, height = 600)
#netgraph(nma_diag_sens, plastic=F, number.of.studies=T, cex.points=nma_diag_sens$k.trts/2)
#dev.off()

#png("result/nma/clinical/social_anxiety/social_anx_netgraph_attn.png", width = 800, height = 600)
#netgraph(nma_sa_sub1, plastic=F, number.of.studies=T, cex.points=nma_sa_sub1$k.trts/2)
#dev.off()

#png("result/nma/clinical/social_anxiety/social_anx_netgraph_int.png", width = 800, height = 600)
#netgraph(nma_sa_sub3, plastic=F, number.of.studies=T, cex.points=nma_sa_sub3$k.trts/2)
#dev.off()


#### create forest plot ----
#png("result/nma/clinical/social_anxiety/social_anx_forest.png", width = 1000, height = 750, res = 150)
#forest(nma_sa,ref = "Control CBM-A", sortvar = -Pscore, label.left = "Favours intervention", label.right = "Favours control CBM-A")
#dev.off()


#png("result/nma/clinical/social_anxiety/sad_rob_sens_forest.png", width = 800, height = 600)
#forest(nma_rob_sens, ref = "Control CBM-A", sortvar = -Pscore)
#dev.off()

#png("result/nma/clinical/social_anxiety/sad_diag_sens_forest.png", width = 800, height = 600)
#forest(nma_diag_sens, ref = "Control CBM-A", sortvar = -Pscore)
#dev.off()

#png("result/nma/clinical/social_anxiety/social_anx_forest_attn.png", width = 800, height = 600)
#forest(nma_sa_sub1,ref = "Control CBM-A", sortvar = -Pscore)
#dev.off()

#png("result/nma/clinical/social_anxiety/social_anx_forest_int.png", width = 800, height = 600)
#forest(nma_sa_sub3,ref = "Control CBM-I", sortvar = -Pscore)
#dev.off()

### netsplit test ----

net_split_sa <- netsplit(nma_sa)
net_split_sa_sub1 <- netsplit(nma_sa_sub1)
net_split_sa_sub3 <- netsplit(nma_sa_sub3, show = "all")
net_split_rob <- netsplit(nma_rob_sens, show = "all")
net_split_diag <- netsplit(nma_diag_sens, show = "all")

### design by trt test ----
design_test_diag <- decomp.design(nma_diag_sens)
design_test_sa <- decomp.design(nma_sa)
design_test_sa_sub1 <- decomp.design(nma_sa_sub1)
design_test_sa_sub3 <- decomp.design(nma_sa_sub3)
design_test_rob <- decomp.design(nma_rob_sens)

#### SUCRA ----
sucrac_sa <- netrank(nma_sa, small.values = "good", method = "SUCRA")
sucrac_sa_sub1 <- netrank(nma_sa_sub1, small.values = "good", method = "SUCRA")
sucrac_sa_sub3 <- netrank(nma_sa_sub3, small.values = "good", method = "SUCRA")
sucrac_rob <- netrank(nma_rob_sens, small.values = "good", method = "SUCRA")
sucrac_diag <- netrank(nma_diag_sens, small.values = "good", method = "SUCRA")

####funnel plot ----
treatment_order <- c("Control CBM-A", "TAU", "Waitlist", "Control CBM-I",
                     "CBM-A", "CBM-I","Opposite CBM-A","Multicomponent")

#png("result/nma/clinical/social_anxiety/social_anx_funnel.png", width = 800, height = 600)
funnel(nma_sa,
       type = "contour",
       order = treatment_order,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()

treatment_order_s1 <- c("Control CBM-A", "CBM-A", "Opposite CBM-A", "Waitlist")

#png("result/nma/clinical/social_anxiety/social_anx_funnel_attn.png", width = 800, height = 600)
funnel(nma_sa_sub1,
       type = "contour",
       order = treatment_order_s1,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
##dev.off()

treatment_order_s3 <- c("Control CBM-I", "CBM-I", "TAU", "Waitlist")

#png("result/nma/clinical/social_anxiety/social_anx_funnel_int.png", width = 800, height = 600)
funnel(nma_sa_sub3,
       type = "contour",
       order = treatment_order_s3,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()

treatment_order_rob <- c("Control CBM-A", "CBM-I","Combined control", "CBM-A", "Control CBM-I", "Multicomponent",   
                         "Opposite CBM-A",   "Opposite CBM-I", "Waitlist")

#png("result/nma/clinical/social_anxiety/sad_rob_sens_funnel.png", width = 800, height = 600)
funnel(nma_rob_sens,
       type = "contour",
       order = treatment_order_rob,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()

treatment_order_diag <- c("Control CBM-A", "CBM-I", "Combined control", "CBM-A", "Control CBM-I", "Multicomponent",
                          "Opposite CBM-A",   "Waitlist")

#png("result/nma/clinical/social_anxiety/sad_diag_sens_funnel.png", width = 800, height = 600)
funnel(nma_diag_sens,
       type = "contour",
       order = treatment_order_diag,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()


#### league table ----
leaguetable_sad = netleague(nma_sa, seq = netrank(nma_sa, method = "SUCRA"), digits = 2)
ltable_sad <- leaguetable_sad$random 


# attention subgroup 
leaguetable_sad_sub1 = netleague(nma_sa_sub1,digits = 2)
ltable_sad_sub1 <- leaguetable_sad_sub1$random 
colnames(ltable_sad_sub1) <- leaguetable_sad_sub1$seq
rownames(ltable_sad_sub1) <- leaguetable_sad_sub1$seq

# interpretation subgroup 
leaguetable_sad_sub3 = netleague(nma_sa_sub3,digits = 2)
ltable_sad_sub3 <- leaguetable_sad_sub3$random 
colnames(ltable_sad_sub3) <- leaguetable_sad_sub3$seq
rownames(ltable_sad_sub3) <- leaguetable_sad_sub3$seq

# risk of bias sensitivity
leaguetable_rob = netleague(nma_rob_sens,digits = 2)
ltable_rob <- leaguetable_rob$random 
colnames(ltable_rob) <- leaguetable_rob$seq
rownames(ltable_rob) <- leaguetable_rob$seq

# diagnosis sensitivity
leaguetable_diag = netleague(nma_diag_sens,digits = 2)
ltable_diag <- leaguetable_diag$random 
colnames(ltable_diag) <- leaguetable_diag$seq
rownames(ltable_diag) <- leaguetable_diag$seq


#### save results ----
# save league table as csv 
#write.csv(ltable_sad, "result/nma/clinical/social_anxiety/ltable_social_anxiety.csv")
#write.csv(ltable_sad_sub1, "result/nma/clinical/social_anxiety/ltable_social_anxiety_attn.csv")
#write.csv(ltable_sad_sub3, "result/nma/clinical/social_anxiety/ltable_social_anxiety_int.csv")
#write.csv(ltable_rob, "result/nma/clinical/social_anxiety/ltable_social_anxiety_rob.csv")
#write.csv(ltable_diag, "result/nma/clinical/social_anxiety/ltable_social_anxiety_diag.csv")

# save summary as txt 
#sink('result/nma/clinical/social_anxiety/social_anx.txt')
#summary(nma_sa, digits =2)
#cat(paste0(''), sep = '\n')
#kable(ltable_sad, caption = "Random-Effects NMA League Table", align = "c")
#cat(paste0(''), sep = '\n')
#netsplit(nma_sa)
#cat(paste0(''), sep = '\n')
#decomp.design(nma_sa)
#cat(paste0(''), sep = '\n')
#sucrac_sa
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

# save summary as txt 
#sink('result/nma/clinical/social_anxiety/sad_rob_sens.txt')
#summary(nma_rob_sens, digits =2)
#cat(paste0(''), sep = '\n')
#kable(ltable_rob, caption = "Random-Effects NMA League Table", align = "c")
#cat(paste0(''), sep = '\n')
#netsplit(nma_rob_sens)
#cat(paste0(''), sep = '\n')
#decomp.design(nma_rob_sens)
#cat(paste0(''), sep = '\n')
#sucrac_rob
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

# save summary as txt 
#sink('result/nma/clinical/social_anxiety/sad_diag_sens.txt')
#summary(nma_diag_sens, digits =2)
#cat(paste0(''), sep = '\n')
#kable(ltable_rob, caption = "Random-Effects NMA League Table", align = "c")
#netsplit(nma_diag_sens)
#cat(paste0(''), sep = '\n')
#decomp.design(nma_diag_sens)
#cat(paste0(''), sep = '\n')
#sucrac_diag
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

# save summary as txt 
#sink('result/nma/clinical/social_anxiety/social_anx_subgroup_attn.txt')
#summary(nma_sa_sub1, digits =2)
#cat(paste0(''), sep = '\n')
#kable(ltable_sad_sub1, caption = "Random-Effects NMA League Table", align = "c")
#cat(paste0(''), sep = '\n')
#netsplit(nma_sa_sub1)
#cat(paste0(''), sep = '\n')
#decomp.design(nma_sa_sub1)
#cat(paste0(''), sep = '\n')
#sucrac_sa_sub1
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

# save summary as txt 
#sink('result/nma/clinical/social_anxiety/social_anx_subgroup_int.txt')
#summary(nma_sa_sub3, digits =2)
#cat(paste0(''), sep = '\n')
#kable(ltable_sad_sub3, caption = "Random-Effects NMA League Table", align = "c")
#cat(paste0(''), sep = '\n')
#netsplit(nma_sa_sub3, show = "all")
#cat(paste0(''), sep = '\n')
#decomp.design(nma_sa_sub3)
#cat(paste0(''), sep = '\n')
#sucrac_sa_sub3
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()



# DEPRESSION ----

#if (#dir.exists('result/nma/clinical') == FALSE){
#dir.create('result/nma/clinical')
#}

#if (#dir.exists('result/nma/clinical/depression') == FALSE){
#dir.create('result/nma/clinical/depression')
#}

# define treatment 
dfc_dep$trt <- dfc_dep$node

#### create pairwise data ----
pw_dep <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfc_dep,
  sm = "SMD"
)

#### identify subnetworks ----
netconn <- netconnection(pw_dep)

#### add subnetwork into netconn 
pw_dep$subnet <- netconn$subnet
table(pw_dep$subnet)

#### identify disconnected studies ----
pw_dep[pw_dep$subnet == 1, c("studlab", "treat1", "treat2", "TE", "seTE")]

#### exclude subnetwork----
pw_dep_connected <- subset(pw_dep, subnet == 2)

#### run nma----
nma_dep <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_dep_connected,
  sm = "SMD",
  reference.group = "Control CBM-A"
)

#### create netgraph ---- 
#png("result/nma/clinical/depression/depression_netgraph.png", width = 800, height = 600)
#netgraph(nma_dep, plastic=F, number.of.studies=T, cex.points=nma_dep$k.trts/2)
#dev.off()

#### forest plot ----
#png("result/nma/clinical/depression/depressionforest.png", width = 800, height = 600)
#forest(nma_dep,ref = "Control CBM-A", sortvar = -Pscore)
#dev.off()

#### netsplit test ----
net_split_dep <- netsplit(nma_dep)

### design by trt test ----
design_test_dep <- decomp.design(nma_dep)

#### SUCRA ----
sucrac_dep <- netrank(nma_dep, small.values = "good", method = "SUCRA")

#### funnel plot ----
treatment_order <- c("CBM-A", "CBM-I", "Control CBM-A", "Control CBM-I", "Multicomponent", "Opposite CBM-A", "TAU", "Waitlist")

png("result/nma/clinical/depression/depression_funnel.png", width = 800, height = 600)
funnel(nma_dep,
       type = "contour",
       order = treatment_order,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
dev.off()

#### league table ----
leaguetable_dep = netleague(nma_dep, seq = netrank(nma_dep, method = "SUCRA"), digits = 2)
ltable_dep <- leaguetable_dep$random 


#### save results ----
# save league table as csv 
#write.csv(ltable_dep, "result/nma/clinical/depression/ltable_depression.csv")

# save summary as txt 
#sink('result/nma/clinical/depression/depression.txt')
#summary(nma_dep, digits =2)
#kable(ltable_dep, caption = "Random-Effects NMA League Table", align = "c")
#netsplit(nma_dep)
#decomp.design(nma_dep)
#sucrac_dep
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

# GENERAL ANXIETY ----

#if (#dir.exists('result/nma/clinical/') == FALSE){
#dir.create('result/nma/clinical/')
#}

#if (#dir.exists('result/nma/clinical/anxiety') == FALSE){
#dir.create('result/nma/clinical/anxiety')
#}

dfc_anx$trt <- dfc_anx$node

#### create pairwise data ----
pw_anx <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfc_anx,
  sm = "SMD"
)

#### identify subnetworks ----
netconn <- netconnection(pw_anx)
print(netconn)

#### add subnetwork into netconn 
pw_anx$subnet <- netconn$subnet
table(pw_anx$subnet)

#### identify disconnected studies ----
pw_anx[pw_anx$subnet == 1, c("studlab", "treat1", "treat2", "TE", "seTE")]
pw_anx[pw_anx$subnet == 2, c("studlab", "treat1", "treat2", "TE", "seTE")]
pw_anx[pw_anx$subnet == 3, c("studlab", "treat1", "treat2", "TE", "seTE")]

#### exclude subnetwork ----
pw_anx_connected1 <- subset(pw_anx, subnet == 1)
pw_anx_connected3 <- subset(pw_anx, subnet == 3)

#### run nma ----
nma_anx_subn1 <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_anx_connected1,
  sm = "SMD",
  reference.group = "Control CBM-A"
)

nma_anx_subn3 <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_anx_connected3,
  sm = "SMD",
  reference.group = "Waitlist"
)

#### create netgraph ---- 
#png("result/nma/clinical/anxiety/anxiety_netgraph_subnet1.png", width = 800, height = 600)
#netgraph(nma_anx_subn1, plastic=F, number.of.studies=T, cex.points=nma_anx_subn1$k.trts/2)
#dev.off()

#png("result/nma/clinical/anxiety/anxiety_netgraph_subnet3.png", width = 800, height = 600)
#netgraph(nma_anx_subn3, plastic=F, number.of.studies=T, cex.points=nma_anx_subn3$k.trts/2)
#dev.off()

#### forest plot ----
#png("result/nma/clinical/anxiety/anxiety_forest_subnet1.png", width = 800, height = 600)
#forest(nma_anx_subn1,ref = "Control CBM-A", sortvar = -Pscore)
#dev.off()

#png("result/nma/clinical/anxiety/anxiety_forest_subnet3.png", width = 800, height = 600)
#forest(nma_anx_subn3,ref = "Waitlist", sortvar = -Pscore)
#dev.off()

#### netsplit test ----
net_split_anx_subn1 <- netsplit(nma_anx_subn1)
net_split_anx_subn3 <- netsplit(nma_anx_subn3)

### design by trt test ----
design_test_anx_subn1 <- decomp.design(nma_anx_subn1)
design_test_anx_subn3 <- decomp.design(nma_anx_subn3)

#### funnel plot ----

treatment_order_s1 <- c("CBM-A","Control CBM-A","Multicomponent", "Opposite CBM-A")
#png("result/nma/clinical/anxiety/anxiety_funnel_subnet1.png", width = 800, height = 600)
funnel(nma_anx_subn1,
       type = "contour",
       order = treatment_order_s1,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()

treatment_order_s3 <- c("CBM-I", "Control CBM-I", "Waitlist")

#png("result/nma/clinical/anxiety/anxiety_funnel_subnet3.png", width = 800, height = 600)
funnel(nma_anx_subn3,
       type = "contour",
       order = treatment_order_s3,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()

#### SUCRA ----
sucrac_anx_subn1 <- netrank(nma_anx_subn1, small.values = "good", method = "SUCRA")
sucrac_anx_subn3 <- netrank(nma_anx_subn3, small.values = "good", method = "SUCRA")


#### league table ----
leaguetable_anx_s1 = netleague(nma_anx_subn1, seq = netrank(nma_anx_subn1, method = "SUCRA"), digits = 2)
ltable_anx_s1 <- leaguetable_anx_s1$random 

leaguetable_anx_s3 = netleague(nma_anx_subn3, seq = netrank(nma_anx_subn3, method = "SUCRA"), digits = 2)
ltable_anx_s3 <- leaguetable_anx_s3$random 


#### save results ----
# save league table as csv 
#write.csv(ltable_anx_s1, "result/nma/clinical/anxiety/ltable_anxiety_subnet1.csv")
#write.csv(ltable_anx_s3, "result/nma/clinical/anxiety/ltable_anxiety_subnet3.csv")

#save summary as txt 
#sink('result/nma/clinical/anxiety/anxiety_subnet1.txt')
#summary(nma_anx_subn1, digits =2)
#kable(ltable_anx_s1, caption = "Random-Effects NMA League Table", align = "c")
#netsplit(nma_anx_subn1)
#decomp.design(nma_anx_subn1)
#sucrac_anx_subn1
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

#sink('result/nma/clinical/anxiety/anxiety_subnet3.txt')
#summary(nma_anx_subn3, digits =2)
#kable(ltable_anx_s3, caption = "Random-Effects NMA League Table", align = "c")
#netsplit(nma_anx_subn3)
#decomp.design(nma_anx_subn3)
#sucrac_anx_subn3
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()



# CHANGE IN BIAS ----

#if (#dir.exists('result/nma/clinical') == FALSE){
#dir.create('result/nma/clinical')
#}

#if (#dir.exists('result/nma/clinical/bias') == FALSE){
#dir.create('result/nma/clinical/bias')


dfc_bias$trt <- dfc_bias$node

# Koc needs to be removed 
indices_to_modify <- which(grepl("Koc \\(2021\\)", dfc_bias$studlab))
print(indices_to_modify)
dfc_bias <- dfc_bias[-indices_to_modify, ]

# ensure no missing data
dfc_bias <- dfc_bias %>%
  drop_na()

#### create pairwise data ----
pw_bias <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfc_bias,
  sm = "SMD"
)

pw_bias_connected <- pw_bias

#### run nma ----
nma_bias <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_bias_connected,
  sm = "SMD",
  reference.group = "Control CBM-A"
)

#### create netgraph ---- 
#png("result/nma/clinical/bias/bias_netgraph.png", width = 2000, height = 2000, res = 250)
#netgraph(nma_bias, plastic=F, number.of.studies=T, cex.points=nma_bias$k.trts/2)
#dev.off()

#### create forest plot ----
#png("result/nma/clinical/bias/bias_forest.png", width = 1000, height = 1000)
#forest(nma_bias,ref = "Control CBM-A", sortvar = -Pscore)
#dev.off()


#### netsplit test ----
net_split_bias <- netsplit(nma_bias)

#### design by trt test ----
design_test_bias <- decomp.design(nma_bias)

#### SUCRA ----
sucrac_bias<- netrank(nma_bias, small.values = "good", method = "SUCRA")

#### funnel plot ----
treatment_order_s1 <- c("CBM-A", "Control CBM-A", "CBM-I", "Control CBM-I", "Multicomponent",
                        "Combined control", "Opposite CBM-A", "TAU", "Waitlist")

#png("result/nma/clinical/bias/bias_funnel.png", width = 800, height = 600)
funnel(nma_bias,
       type = "contour",
       order = treatment_order_s1,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()


#### league table ----
leaguetable_bias = netleague(nma_bias, seq = netrank(nma_bias, method = "SUCRA"), digits = 2)
ltable_bias <- leaguetable_bias$random 


#### save results ----
# save league table as csv 
#write.csv(ltable_bias, "result/nma/clinical/bias/ltable_bias.csv")

# save summary as txt 
#sink('result/nma/clinical/bias/bias.txt')
#summary(nma_bias, digits =2)
#kable(ltable_bias, caption = "Random-Effects NMA League Table", align = "c")
#netsplit(nma_bias)
#decomp.design(nma_bias)
#sucrac_bias
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

# QUALITY OF LIFE ----

#if (#dir.exists('result/nma/clinical') == FALSE){
#dir.create('result/nma/clinical')
#}

#if (#dir.exists('result/nma/clinical/quality_of_life') == FALSE){
#dir.create('result/nma/clinical/quality_of_life')
#}

# remove outcomes in Bomyea (2023) that were not prioritised 
indices_to_modify <- which(grepl("^Bomyea \\(2023\\)", dfc_qol$studlab) & grepl("Interpersonal Outcomes: Relationship Satisfaction", dfc_qol$outcome))
dfc_qol <- dfc_qol[-indices_to_modify, ]

indices_to_modify <- which(grepl("^Bomyea \\(2023\\)", dfc_qol$studlab) & grepl("Interpersonal Outcomes: Social Approach", dfc_qol$outcome))
dfc_qol <- dfc_qol[-indices_to_modify, ]

dfc_qol$trt <- dfc_qol$node

# subgroup analysis not conducted as only one study targeted interpretation bias and the attentional subgroup is made up of more subnetworks.

#### create pairwise data ----
pw_qol <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfc_qol,
  sm = "SMD"
)

#### identify subnetworks ----
netconn <- netconnection(pw_qol)
print(netconn)

#### add subnetwork into netconn 
pw_qol$subnet <- netconn$subnet
table(pw_qol$subnet)

#### identify disconnected study 
pw_qol[pw_qol$subnet == 1, c("studlab", "treat1", "treat2", "TE", "seTE")]

#### exclude subnetwork ----
pw_qol_connected <- subset(pw_qol, subnet == 2)

#### run nma----
nma_qol <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_qol_connected,
  sm = "SMD",
  reference.group = "Control CBM-I"
)

#### create netgraph ---- 
#png("result/nma/clinical/quality_of_life/quality_of_life_netgraph.png", width = 2000, height = 2000, res = 250)
#netgraph(nma_qol, plastic=F, number.of.studies=T, cex.points=nma_qol$k.trts/2)
#dev.off()

#### create forest plot ----
#png("result/nma/clinical/quality_of_life/qol_forest.png", width = 800, height = 600)
#forest(nma_qol,ref = "Control CBM-I", sortvar = -Pscore)
#dev.off()

#### design by trt test ----
design_test_qol <- decomp.design(nma_qol)

#### netsplit test ----
net_split_qol <- netsplit(nma_qol)

#### SUCRA ----
sucrac_qol <- netrank(nma_qol, small.values = "good", method = "SUCRA")

#### funnel plot ----
treatment_order_qol <- c("CBM-A", "CBM-I", "Combined control", "Control CBM-A", "Control CBM-I", "Multicomponent", "Opposite CBM-A")

#png("result/nma/clinical/quality_of_life/qol_funnel.png", width = 800, height = 600)
funnel(nma_qol,
       type = "contour",
       order = treatment_order_qol,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()

#### league tables ----
leaguetable_qol = netleague(nma_qol, seq = netrank(nma_qol, method = "SUCRA"), digits = 2)
ltable_qol <- leaguetable_qol$random 


#### save results ----
# save league table as csv 
#write.csv(ltable_qol, "result/nma/clinical/quality_of_life/ltable_qol.csv")

# save summary as txt 
#sink('result/nma/clinical/quality_of_life/quality_of_life.txt')
#summary(nma_qol, digits =2)
#kable(ltable_qol, caption = "Random-Effects NMA League Table", align = "c")
#netsplit(nma_qol)
#decomp.design(nma_qol)
#sucrac_qol
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()


# ACCEPTABILITY ----

#if (#dir.exists('result/nma/clinical/acceptability') == FALSE){
#dir.create('result/nma/clinical/acceptability')
#}

dfc_acc$trt <- dfc_acc$node

#### create pairwise data ----
pw_acc <- pairwise(
  treat = trt,
  event = endpoint_dropouts,
  n = baseline_n, # n randomised 
  studlab = studlab,
  data = dfc_acc,
  sm = "OR"
)

pw_acc <- pw_acc %>%
  drop_na()

#### identify subnetworks ----
netconn <- netconnection(pw_acc)

#### add subnetwork into netconn 
pw_acc$subnet <- netconn$subnet

#### exclude subnetwork ----
pw_acc_connected <- subset(pw_acc, subnet == 2)

#### run nma ----
nma_acc <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pw_acc_connected,
  sm = "OR",
  reference.group = "Control CBM-A",
)

#### create netgraph ---- 
#png("result/nma/clinical/acceptability/acceptability_netgraph.png", width = 2000, height = 2000, res = 250)
#netgraph(nma_acc, plastic=F, number.of.studies=T, cex.points=nma_acc$k.trts/2)
#dev.off()

#### create forest plot ----
#png("result/nma/clinical/acceptability/acceptability_forest.png", width = 800, height = 600)
#forest(nma_acc,ref = "Control CBM-A", sortvar = -Pscore)
#dev.off()

#### design by trt test ----
design_test_acc <- decomp.design(nma_acc)

### netsplit test ----
net_split_acc <- netsplit(nma_acc)

#### SUCRA ----
sucrac_acc <- netrank(nma_acc, small.values = "good", method = "SUCRA")

#### funnel plot ----
treatment_order_acc <- c("CBM-A", "CBM-I", "Control CBM-A", "Control CBM-I", "Multicomponent", "Opposite CBM-A", "TAU", "Waitlist")

#png("result/nma/clinical/acceptability/acceptability_funnel.png", width = 800, height = 600)
funnel(nma_acc,
       type = "contour",
       order = treatment_order_acc,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()


#### league table ----
leaguetable_acc = netleague(nma_acc, seq = netrank(nma_acc, method = "SUCRA"), digits = 2)
ltable_acc <- leaguetable_acc$random 


#### save results ----
# save league table as csv 
#write.csv(ltable_acc, "result/nma/clinical/acceptability/ltable_acceptability.csv")

#save summary as txt 
#sink('result/nma/clinical/acceptability/acceptability.txt')
#summary(nma_acc, digits =2)
#kable(ltable_acc, caption = "Random-Effects NMA League Table", align = "c")
#netsplit(nma_acc)
#decomp.design(nma_acc)
#sucrac_acc
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()


# **NONCLINICAL SOURCE** ----


# SOCIAL ANXIETY ----

# define results location 
#if (#dir.exists('result/nma/nonclinical') == FALSE){
#dir.create('result/nma/nonclinical')
#}

#if (#dir.exists('result/nma/nonclinical/social_anxiety') == FALSE){
#dir.create('result/nma/nonclinical/social_anxiety')
#}

# only one study contributing so NMA not conducted 


# DEPRESSION ----

#if (#dir.exists('result/nma/nonclinical') == FALSE){
#dir.create('result/nma/nonclinical')
#}

#if (#dir.exists('result/nma/nonclinical/depression') == FALSE){
#dir.create('result/nma/nonclinical/depression')
#}

# only two studies so NMA not conducted

# GENERAL ANXIETY ----

#if (#dir.exists('result/nma/nonclinical') == FALSE){
#dir.create('result/nma/clinical')
#}

#if (#dir.exists('result/nma/nonclinical/anxiety') == FALSE){
#dir.create('result/nma/nonclinical/anxiety')
#}

dfn_anx$trt <- dfn_anx$node

# remove ineligible study
indices_to_modify <- which(grepl("^Ferrari \\(2018\\)", dfn_anx$studlab))
dfn_anx <- dfn_anx[-indices_to_modify, ]

#### create pairwise data ----
pwn_anx <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfn_anx,
  sm = "SMD"
)

#### identify subnetworks ----
netconn <- netconnection(pwn_anx)
print(netconn)

#### add subnetwork into netconn 
pwn_anx$subnet <- netconn$subnet
table(pwn_anx$subnet)

#### identify disconnected studies ----
pwn_anx[pwn_anx$subnet == 1, c("studlab", "treat1", "treat2", "TE", "seTE")]
pwn_anx[pwn_anx$subnet == 2, c("studlab", "treat1", "treat2", "TE", "seTE")]
# subnetwork 1) CBM-A/Control CBM-A/waitlist, 1 study
# subnetwork 3) CBM-I/Control CBM-I/opposite CBM-I, 3 studies


#### exclude subnetwork ----
pwn_anx_connected <- subset(pwn_anx, subnet == 2)

#### run nma ----
nma_anxn <- netmeta(
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab,
  data = pwn_anx_connected,
  sm = "SMD",
  reference.group = "Control CBM-I"
)

#### create netgraph ---- 
#png("result/nma/nonclinical/anxiety/anxiety_netgraph.png", width = 800, height = 600)
#netgraph(nma_anxn)
#dev.off()

#### create forest plot ----
#png("result/nma/nonclinical/anxiety/anxiety_forest.png", width = 800, height = 600)
#forest(nma_anxn,ref = "Control CBM-I")
#dev.off()

#### design by trt test----
design_test_anxn <- decomp.design(nma_anxn)

#### netsplit test ----
net_split_anxn <- netsplit(nma_anxn)

#### funnel plot ----
treatment_order <- c("CBM-I", "Control CBM-I", "Opposite CBM-I")
#png("result/nma/nonclinical/anxiety/anxiety_funnel.#png", width = 800, height = 600)
funnel(nma_anxn,
       type = "contour",
       order = treatment_order,
       method.bias = "Egger",
       pos.tests = "bottomright",
       contour.levels = c(0.9, 0.95, 0.99))
#dev.off()

#### SUCRA ----
sucran_anx <- netrank(nma_anxn, small.values = "good")

#### league table ----
leaguetable_anx_nonclin = netleague(nma_anxn, seq = netrank(nma_anxn, method = "SUCRA"), digits = 2)
ltable_anx_nonclin <- leaguetable_anx_nonclin$random 


#### save results ----
# save league table as csv 
#write.csv(ltable_anx_nonclin, "result/nma/nonclinical/anxiety/ltable_anxiety.csv")

#save summary as txt 
##sink('result/nma/nonclinical/anxiety/anxiety.txt')
#summary(nma_anxn, digits =2)
#kable(ltable_anx_nonclin, caption = "Random-Effects NMA League Table", align = "c")
#netsplit(nma_anxn, show = "all")
#decomp.design(nma_anxn)
#sucran_anx
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
##sink()


# CHANGE IN BIAS ----

#if (#dir.exists('result/nma/nonclinical') == FALSE){
#dir.create('result/nma/nonclinical')
#}

#if (#dir.exists('result/nma/nonclinical/bias') == FALSE){
#dir.create('result/nma/nonclinical/bias')
#}

# remove ineligible study (AAT)
indices_to_modify <- which(grepl("^Ferrari \\(2018\\)", dfn_bias$studlab))
dfn_bias <- dfn_bias[-indices_to_modify, ]

dfn_bias$trt <- dfn_bias$node

#### create pairwise data ----
pw_biasn <- pairwise(
  treat = trt,
  mean = endpoint_mean,
  sd = endpoint_sd,
  n = endpoint_n,
  studlab = studlab,
  data = dfn_bias,
  sm = "SMD"
)

### identify subnetworks ----
netconn <- netconnection(pw_biasn)
print(netconn)

#### add subnetwork into netconn 
pw_biasn$subnet <- netconn$subnet
table(pw_biasn$subnet)

# this gives 2 subnetworks: 3 studies CBM-A vs control CBM-A and 2 studies CBM-I/control CBM-I/opposite CBM-I
# due to lack of studies, NMA not conducted 

# QUALITY OF LIFE ----

# no eligible studies so NMA not conducted 

# ACCEPTABILITY ----

dfn_acc <- dfn_acc %>%
  drop_na()

dfn_acc$trt <- dfn_acc$node

#### create pairwise data ----
pw_accn <- pairwise(
  treat = trt,
  event = endpoint_dropouts,
  n = baseline_n, # n randomised
  studlab = studlab,
  data = dfn_acc,
  sm = "OR"
)

pw_accn <- pw_accn %>%
  drop_na()

#### identify subnetworks ---- 
netconn <- netconnection(pw_accn)

#### add subnetwork into netconn 
pw_accn$subnet <- netconn$subnet
table(pw_accn$subnet)

#### identify disconnected studies ----
pw_accn[pw_accn$subnet == 1, c("studlab", "treat1", "treat2", "TE", "seTE")]
pw_accn[pw_accn$subnet == 2, c("studlab", "treat1", "treat2", "TE", "seTE")]

# subnetwork 1) CBM-A/Control CBM-A/Waitlist, 3 studies
# subnetwork 2) CBM-I/Control CBM-I/, 1 study

# only three studies in subnetwork 1 so NMA not conducted


# **DATA PREP FOR RMD** ----

# NOTE:
# this function creates a tibble that contains relevant values for inline coding 
# it sources these values directly from netmeta:: objects 

#### extract results values ----
extract_nma_stats <- function(nma_obj, design_test_obj, net_split_obj, nma_name = NA, comparator = "Control CBM-A") {
  te_df       <- as.data.frame(nma_obj[["TE.random"]])
  upper_df    <- as.data.frame(nma_obj[["upper.random"]])
  lower_df    <- as.data.frame(nma_obj[["lower.random"]])
  upper_pi_df <- as.data.frame(nma_obj[["upper.predict"]])
  lower_pi_df <- as.data.frame(nma_obj[["lower.predict"]])
  
  if (!comparator %in% colnames(te_df)) {
    stop(paste0("Comparator '", comparator, "' not found in TE.random."))
  }
  
  # find best and worst performing interventions (by TE) 
  max_index <- which.max(te_df[[comparator]])
  min_index <- which.min(te_df[[comparator]])
  
  # adjust index if the smallest or biggest TE is the comparator 
  if (te_df[[comparator]][[max_index]] == 0) {
    te_df[[comparator]][[max_index]] <- NA }
  
  if (te_df[[comparator]][[min_index]] == 0) {
    te_df[[comparator]][[min_index]] <- NA }
  
  # repeating this adjusts _index if minimum or maximum TE was 0 
  max_index <- which.max(te_df[[comparator]])
  min_index <- which.min(te_df[[comparator]])
  
  # global inconsistency (design-by-treatment)
  total_q        <- tryCatch(design_test_obj[["Q.inc.random"]][["Q"]][[1]], error = function(e) NA)
  total_q_pval   <- tryCatch(design_test_obj[["Q.inc.random"]][["pval"]], error = function(e) NA)
  within_q       <- tryCatch(design_test_obj[["Q.decomp"]][["Q"]][[2]], error = function(e) NA)
  within_q_pval  <- tryCatch(design_test_obj[["Q.decomp"]][["pval"]][[2]], error = function(e) NA)
  between_q      <- tryCatch(design_test_obj[["Q.decomp"]][["Q"]][[3]], error = function(e) NA)
  between_q_pval <- tryCatch(design_test_obj[["Q.decomp"]][["pval"]][[3]], error = function(e) NA)
  
  # SIDE loop n 
  side_p <- list(net_split_obj[["compare.random"]][["p"]])
  side_p <- as.data.frame(side_p)
  colnames(side_p)[1] <- "loops"
  side_p <- side_p %>%
    dplyr::filter(!is.na(side_p))
  n_loops <- length(side_p$loops)
  side_p$sig <- ifelse(side_p$loops <= 0.05, 1, 0)
  sig_loops <- sum(side_p$sig)
  
  # evaluate significance 
  global_sig  <- if (!is.na(total_q_pval) && total_q_pval <= 0.05) "significant" else "no significant"
  between_sig <- if (!is.na(between_q_pval) && between_q_pval <= 0.05) "significant" else "no significant"
  
  # local inconsistency 
  local_inconsistency <- tryCatch({
    if (any(net_split_obj$p.val < 0.05, na.rm = TRUE)) {
      "significant values"
    } else {
      "no significant values"
    }
  }, error = function(e) NA)
  
  # overall heterogeneity 
  i2 <- tryCatch(nma_obj[["I2"]], error = function(e) NA)
  tau2 <- tryCatch(nma_obj[["tau2"]], error = function(e) NA)
  
  # create tibble 
  tibble(
    nma = nma_name,
    comparator = comparator,

    best_tx   = rownames(te_df)[max_index],
    worst_tx  = rownames(te_df)[min_index],
    best_te   = te_df[[comparator]][max_index],
    worst_te  = te_df[[comparator]][min_index],
  
    best_upper95ci  = upper_df[[comparator]][max_index],
    best_lower95ci  = lower_df[[comparator]][max_index],
    worst_upper95ci = upper_df[[comparator]][min_index],
    worst_lower95ci = lower_df[[comparator]][min_index],
    
    best_upper95pi  = upper_pi_df[[comparator]][[max_index]],
    best_lower95pi  = lower_pi_df[[comparator]][[max_index]],
    worst_upper95pi = upper_pi_df[[comparator]][[min_index]],
    worst_lower95pi = lower_pi_df[[comparator]][[min_index]],
    
    k = nma_obj[["k"]],
    m = nma_obj[["m"]],
    n = nma_obj[["n"]],
    
    # global inconsistency
    total_q = total_q,
    total_q_pval = total_q_pval,
    within_q = within_q,
    within_q_pval = within_q_pval,
    between_q = between_q,
    between_q_pval = between_q_pval,
    global_inconsistency = global_sig,
    between_inconsistency = between_sig,
    
    n_loops = n_loops, # n loops
    sig_loops = sig_loops, # sig loops
    local_inconsistency = local_inconsistency,
    i2 = i2, # heterogeneity
    tau2 = tau2,
  )
}

# run funct across outcomes
values <- bind_rows(
  # social anxiety outcome
  extract_nma_stats(nma_sa, design_test_sa, net_split_sa, "nma_sa", "Control CBM-A"),
  extract_nma_stats(nma_sa_sub1, design_test_sa, net_split_sa, "nma_sa_sub1", "Control CBM-A"),
  extract_nma_stats(nma_sa_sub3, design_test_sa, net_split_sa, "nma_sa_sub3", "Control CBM-I"),
  extract_nma_stats(nma_rob_sens, design_test_rob, net_split_rob, "nma_rob_sens", "Control CBM-A"),
  extract_nma_stats(nma_diag_sens, design_test_diag, net_split_diag, "nma_diag_sens", "Control CBM-A"),
  # depression outcome
  extract_nma_stats(nma_dep, design_test_dep, net_split_dep, "nma_dep", "Control CBM-A"),
  # anxiety outcome
  extract_nma_stats(nma_anx_subn1, design_test_anx_subn1, net_split_anx_subn1, "nma_anx_subn1", "Control CBM-A"),
  extract_nma_stats(nma_anx_subn3, design_test_anx_subn3, net_split_anx_subn3, "nma_anx_subn3", "Control CBM-I"),
  # qol outcome
  extract_nma_stats(nma_qol, design_test_qol, net_split_qol, "nma_qol", "Control CBM-A"),
  # bias outcome
  extract_nma_stats(nma_bias, design_test_bias, net_split_bias, "nma_bias", "Control CBM-A"),
  # acceptability outcome
  extract_nma_stats(nma_acc, design_test_acc, net_split_acc, "nma_acc", "Control CBM-A"),
  # nonclinical anxiety outcome
  extract_nma_stats(nma_anxn, design_test_anxn, net_split_anxn, "nma_anxn", "Control CBM-I"),
)

# evaluate whether calculating the percentage of significant loops will give NaN  
get_sig_loops <- function(nma_object){
  if ((values$sig_loops[values$nma == nma_object] == 0) & (values$n_loops[values$nma == nma_object] == 0)) {
    return(0)
  } else {
    return((values$sig_loops[values$nma == nma_object]) / (values$n_loops[values$nma == nma_object]))
  }
}

### load RoB2 assessments ----

# this sources RoB assessments xlsx files in /data and formats them for ROBVIS 
prepare_robvis <- function(sheet_name, file_path = "data/rob2_domain_scores.xlsx") {
  rob_df <- readxl::read_excel(file_path, sheet = sheet_name)
  colnames(rob_df)[1] <- "studlab"
  
  rob_df <- rob_df %>%
    dplyr::mutate(across(`1`:`5`, ~ dplyr::recode(.x,
                                                  "L" = "Low", "SC" = "Some concerns", "H" = "High")),
                  overall = dplyr::recode(overall, "L" = "Low", "SC" = "Some concerns", "H" = "High")) %>%
    dplyr::rename(
      Study = studlab,
      `Bias arising from the randomization process` = `1`,
      `Bias due to deviations from intended interventions` = `2`,
      `Bias due to missing outcome data` = `3`,
      `Bias in measurement of the outcome` = `4`,
      `Bias in selection of the reported result` = `5`,
      Overall = overall
    ) %>%
    dplyr::select(Study, starts_with("Bias"), Overall)
  
  return(rob_df)
}

# prepare data for robvis 
rob2_sad <- prepare_robvis("SAD")
rob2_dep <- prepare_robvis("DEP")
rob2_anx <- prepare_robvis("ANX")
rob2_bias <- prepare_robvis("BIAS")
rob2_qol <- prepare_robvis("QOL")
rob2_acc <- prepare_robvis("ACC")
rob2_sadn <- prepare_robvis("SAD_N")
rob2_anxn <- prepare_robvis("ANX_N")
rob2_accn <- prepare_robvis("ACC_N")
rob2_biasn <- prepare_robvis("BIAS_N")


### retrieve n_females/mean age ----
# clinical source 
df.clin1 <- df.clin %>%
  select(studlab, timepoint, intervention, n) %>%
  group_by(studlab, intervention) %>%
  summarise(
    studlab = first(studlab),
    timepoint = any(timepoint == 0),
    intervention = first(intervention),
    n = first(n)
  )

df.clin2 <- df.clin1 %>%
  left_join(int_class, by = c("intervention" = "Intervention")) %>%
  rename(node = Node) 

df.clin3 <- df.clin2 %>%
  left_join(covar_data, by = c("studlab", "node")) 

df.clin4 <- df.clin3 %>%
  dplyr::filter(!is.na(n_female))

df.clin4$female_prop <- df.clin4$n_female / df.clin4$n
mean_age_clin <- mean(df.clin4$mean_age, na.rm = TRUE) # mean age 
female_prop_clin <- mean(df.clin4$female_prop, na.rm = TRUE) # proportion of females 

rm(df.clin1, df.clin2, df.clin3, df.clin4) # clean up 

# nonclinical source 
df.nonclin1 <- df.nonclin %>%
  select(studlab, timepoint, intervention, n) %>%
  group_by(studlab, intervention) %>%
  summarise(
    studlab = first(studlab),
    timepoint = any(timepoint == 0),
    intervention = first(intervention),
    n = first(n)
  )

df.nonclin2 <- df.nonclin1 %>%
  left_join(int_class, by = c("intervention" = "Intervention")) %>%
  rename(node = Node) 

df.nonclin3 <- df.nonclin2 %>%
  left_join(covar_data, by = c("studlab", "node")) 

df.nonclin4 <- df.nonclin3 %>%
  dplyr::filter(!is.na(n_female))

df.nonclin4$female_prop <- df.nonclin4$n_female / df.nonclin4$n
mean_age_nonclin <- mean(df.nonclin4$mean_age, na.rm = TRUE) # mean age 
female_prop_nonclin <- mean(df.nonclin4$female_prop, na.rm = TRUE) # proportion of females 

rm(df.nonclin1, df.nonclin2, df.nonclin3, df.nonclin4) # clean up 












