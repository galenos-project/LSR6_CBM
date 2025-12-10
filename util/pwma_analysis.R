# PWMA analysis  ----

# NOTES 
# the suffix _subX (where X is a number) denotes subgroups 
# the suffix _subnX denotes a subnetwork 
# n is used to denote nonclinical source e.g. nma_anxn = nonclinical anxiety nma object 
# c is used to denote clinical source data e.g. dfc_sad = clinical social anxiety data 
# code to write results to txt or save graphs has been supressed with a # 

#### source cleaning script----

source("util/combined_cleaning.R")


# CLINICAL SOURCE ----



### SOCIAL ANXIETY ----

##### define social anxiety df ----
# remove missing data

dfcp_sad <- dfcp_analysis %>%
  select(studlab, sa_outcome, cbm_sa_endpoint_n, cbm_sa_endpoint_mean, cbm_sa_endpoint_sd,
         control_sa_endpoint_n, control_sa_endpoint_mean, control_sa_endpoint_sd) %>%
  drop_na()

dfcp_sad <- dfcp_sad %>%
  dplyr::mutate(across(3:8, as.numeric))


#### run metacont ----

pwmac.social_anx <- metacont(n.e = cbm_sa_endpoint_n,
                             mean.e = cbm_sa_endpoint_mean,
                             sd.e = cbm_sa_endpoint_sd,
                             n.c = control_sa_endpoint_n,
                             mean.c = control_sa_endpoint_mean,
                             sd.c = control_sa_endpoint_sd,
                             studlab = studlab,
                             data = dfcp_sad,
                             sm = "SMD",          
                             method.smd = "Hedges",
                             common = FALSE,       # Use random-effects model
                             random = TRUE,
                             method.tau = 'REML',
                             prediction = TRUE,
                             method.random.ci = "HK"
)


##if (dir.exists('result')==F) {
# #dir.create('result/')
##dir.create('result/pairwise')
##dir.create('result/pairwise/clinical/social_anxiety/')
#}

#### social anxiety forest plot ----

#png("result/pairwise/clinical/social_anxiety/sad_clinical_forest.png", width = 4000, height = 3000, res = 300)
forest(pwmac.social_anx,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Social anxiety severity',
       prediction = TRUE,
       width = 4000,
       height = 3000
)
#dev.off() 


#### save results----

#sink('result/pairwise/clinical/social_anxiety/social_anxiety.txt')
#pwmac.social_anx
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()


### DEPRESSION ----

#### define depression df ----
# remove missing data

dfcp_dep <- dfcp_analysis %>%
  select(studlab, dep_outcome, cbm_dep_endpoint_n, cbm_dep_endpoint_mean, cbm_dep_endpoint_sd,
         control_dep_endpoint_n, control_dep_endpoint_mean, control_dep_endpoint_sd) %>%
  drop_na()

dfcp_dep <- dfcp_dep %>%
  dplyr::mutate(across(3:8, as.numeric))

#### run metacont ----
pwmac.dep <- metacont(n.e = cbm_dep_endpoint_n,
                      mean.e = cbm_dep_endpoint_mean,
                      sd.e = cbm_dep_endpoint_sd,
                      n.c = control_dep_endpoint_n,
                      mean.c = control_dep_endpoint_mean,
                      sd.c = control_dep_endpoint_sd,
                      studlab = studlab,
                      data = dfcp_dep,
                      sm = "SMD",          
                      method.smd = "Hedges",
                      common = FALSE,       # Use random-effects model
                      random = TRUE,
                      method.tau = 'REML',
                      prediction = TRUE,
                      method.random.ci = "HK"
)

##if (dir.exists('result/pairwise/clinical/depression') == FALSE) {
# #dir.create('result/pairwise/clinical/depression/')
#}

#### depression forest plot ----

#png("result/pairwise/clinical/depression/dep_clinical_forest.png", width = 3000, height = 1500, res = 300)
forest(pwmac.dep,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Depression symptom severity',
       prediction = TRUE
)
#dev.off()


#sink('result/pairwise/clinical/depression/depression.txt')
#pwmac.dep
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()


### GENERAL ANXIETY ----

#### define anxiety df ----
# remove missing data

dfcp_anx <- dfcp_analysis %>%
  select(studlab, anx_outcome, cbm_anx_endpoint_n, cbm_anx_endpoint_mean, cbm_anx_endpoint_sd,
         control_anx_endpoint_n, control_anx_endpoint_mean, control_anx_endpoint_sd) %>%
  drop_na()

dfcp_anx <- dfcp_anx %>%
  dplyr::mutate(across(3:8, as.numeric))

#### run metacont ----
pwmac.anx <- metacont(n.e = cbm_anx_endpoint_n,
                      mean.e = cbm_anx_endpoint_mean,
                      sd.e = cbm_anx_endpoint_sd,
                      n.c = control_anx_endpoint_n,
                      mean.c = control_anx_endpoint_mean,
                      sd.c = control_anx_endpoint_sd,
                      studlab = studlab,
                      data = dfcp_anx,
                      sm = "SMD",          
                      method.smd = "Hedges",
                      common = FALSE,       # Use random-effects model
                      random = TRUE,
                      method.tau = 'REML',
                      prediction = TRUE,
                      method.random.ci = "HK"
)


##if (dir.exists('result/pairwise/clinical/anxiety') == FALSE) {
# #dir.create('result/pairwise/clinical/anxiety/')
#}

#### general anxiety forest plot ----
#png("result/pairwise/clinical/anxiety/anx_clinical_forest.png", width = 3000, height = 2000, res = 300)
forest(pwmac.anx,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Anxiety symptom severity',
       prediction = TRUE
)
#dev.off()

#sink('result/pairwise/clinical/anxiety/anxiety.txt')
#pwmac.anx
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()


### BIAS CHANGE----

#### define bias df ----
# remove missing data

dfcp_bias <- dfcp_analysis %>%
  select(studlab, bias_outcome, cbm_bias_endpoint_n, cbm_bias_endpoint_mean, cbm_bias_endpoint_sd,
         control_bias_endpoint_n, control_bias_endpoint_mean, control_bias_endpoint_sd) %>%
  drop_na()

dfcp_bias <- dfcp_bias %>%
  dplyr::mutate(across(3:8, as.numeric))

#### run metacont ----
pwmac.bias <- metacont(n.e = cbm_bias_endpoint_n,
                       mean.e = cbm_bias_endpoint_mean,
                       sd.e = cbm_bias_endpoint_sd,
                       n.c = control_bias_endpoint_n,
                       mean.c = control_bias_endpoint_mean,
                       sd.c = control_bias_endpoint_sd,
                       studlab = studlab,
                       data = dfcp_bias,
                       sm = "SMD",          
                       method.smd = "Hedges",
                       common = FALSE,       # random-effects model
                       random = TRUE,
                       method.tau = 'REML',
                       prediction = TRUE,
                       method.random.ci = "HK"
)

##if (dir.exists('result/pairwise/clinical/bias') == FALSE) {
#  #dir.create('result/pairwise/clinical/bias/')
#}


#### bias change forest plot ----
#png("result/pairwise/clinical/bias/bias_clinical_forest.png", width = 4000, height = 3000, res = 300)
forest(pwmac.bias,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Bias change',
       prediction = TRUE
)
#dev.off()

#sink('result/pairwise/clinical/bias/bias.txt')
#pwmac.bias
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()


### QUALITY OF LIFE ----

#### define qol df ----
#remove missing data

dfcp_qol <- dfcp_analysis %>%
  select(studlab, qol_outcome, cbm_qol_endpoint_n, cbm_qol_endpoint_mean, cbm_qol_endpoint_sd,
         control_qol_endpoint_n, control_qol_endpoint_mean, control_qol_endpoint_sd) %>%
  drop_na()

dfcp_qol <- dfcp_qol %>%
  dplyr::mutate(across(3:8, as.numeric))

#### run metacont ----
pwmac.qol <- metacont(n.e = cbm_qol_endpoint_n,
                      mean.e = cbm_qol_endpoint_mean,
                      sd.e = cbm_qol_endpoint_sd,
                      n.c = control_qol_endpoint_n,
                      mean.c = control_qol_endpoint_mean,
                      sd.c = control_qol_endpoint_sd,
                      studlab = studlab,
                      data = dfcp_qol,
                      sm = "SMD",          
                      method.smd = "Hedges",
                      common = FALSE,        # random-effects model
                      random = TRUE,
                      method.tau = 'REML',
                      prediction = TRUE,
                      method.random.ci = "HK"
)

#if (dir.exists('result/pairwise/clinical/quality_of_life') == FALSE) {
#dir.create('result/pairwise/clinical/quality_of_life/')
#}


#### QoL forest plot ----
#png("result/pairwise/clinical/quality_of_life/qol_clinical_forest.png", width = 3000, height = 1500, res = 300)
forest(pwmac.qol,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'QoL',
       prediction = TRUE
)
#dev.off()

#sink("result/pairwise/clinical/quality_of_life/quality_of_life.txt")
#pwmac.qol
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

#ACCEPTABILITY ----

#### define acc df ----
#remove missing data

dfcp_acc <- dfcp_analysis %>%
  select(studlab, cbm_acc_baseline_n, cbm_acc_endpoint_dropouts, control_acc_baseline_n,
         control_acc_endpoint_dropouts) %>%
  drop_na()


dfcp_acc <- dfcp_acc %>%
  dplyr::mutate(across(2:4, as.numeric))

#### run metabin ----
pwmac.acc <- metabin(event.e = cbm_acc_endpoint_dropouts,
                     n.e = cbm_acc_baseline_n,
                     event.c = control_acc_endpoint_dropouts,
                     n.c = control_acc_baseline_n,
                     studlab = studlab,
                     data = dfcp_acc,
                     sm = "OR",
                     method.tau = "REML",
                     prediction = TRUE)



#if (dir.exists('result/pairwise/clinical/acceptibility') == FALSE) {
#dir.create('result/pairwise/clinical/acceptibility/')
#}


#### acc forest plot ----
#png("result/pairwise/clinical/acceptibility/acc_clinical_forest.png", width = 3000, height = 3000, res = 300)
forest(pwmac.acc,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Dropouts (any reason)',
       prediction = TRUE
)
#dev.off()

#sink("result/pairwise/clinical/acceptibility/acceptibility.txt")
#pwmac.acc
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()




# NONCLINICAL SOURCE ----
### SOCIAL ANXIETY ----

##### define social anxiety df ----
# remove missing data

dfn_sad <- dfnp_analysis %>%
  select(studlab, sa_outcome, cbm_sa_endpoint_n, cbm_sa_endpoint_mean, cbm_sa_endpoint_sd,
         control_sa_endpoint_n, control_sa_endpoint_mean, control_sa_endpoint_sd) %>%
  drop_na()

dfn_sad <- dfn_sad %>%
  dplyr::mutate(across(3:8, as.numeric))


#### run metacont ----

pwman.social_anx <- metacont(n.e = cbm_sa_endpoint_n,
                             mean.e = cbm_sa_endpoint_mean,
                             sd.e = cbm_sa_endpoint_sd,
                             n.c = control_sa_endpoint_n,
                             mean.c = control_sa_endpoint_mean,
                             sd.c = control_sa_endpoint_sd,
                             studlab = studlab,
                             data = dfn_sad,
                             sm = "SMD",          
                             method.smd = "Hedges",
                             common = TRUE,       # Use random-effects model
                             random = FALSE,
                             method.tau = 'REML',
                             prediction = TRUE
)


#if (dir.exists('result')==F) {
#dir.create('result/')
#dir.create('result/pairwise')
#dir.create('result/pairwise/nonclinical')
#dir.create('result/pairwise/nonclinical/social_anxiety/')
#}

#### social anxiety forest plot ----
#png("result/pairwise/nonclinical/social_anxiety/sad_nonclinical_forest.png", width = 3000, height = 1500, res = 300)
forest(pwman.social_anx,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Social anxiety severity',
       prediction = TRUE,
       common = TRUE
)
#dev.off() 


#sink('result/pairwise/nonclinical/social_anxiety/social_anxiety.txt')
pwman.social_anx
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

### DEPRESSION ----

#### define anxiety df ----
# remove missing data

dfnp <- dfnp_analysis %>%
  select(studlab, dep_outcome, cbm_dep_endpoint_n, cbm_dep_endpoint_mean, cbm_dep_endpoint_sd,
         control_dep_endpoint_n, control_dep_endpoint_mean, control_dep_endpoint_sd) %>%
  drop_na()

dfnp <- dfnp %>%
  dplyr::mutate(across(3:8, as.numeric))

#### run metacont ----
pwman.dep <- metacont(n.e = cbm_dep_endpoint_n,
                      mean.e = cbm_dep_endpoint_mean,
                      sd.e = cbm_dep_endpoint_sd,
                      n.c = control_dep_endpoint_n,
                      mean.c = control_dep_endpoint_mean,
                      sd.c = control_dep_endpoint_sd,
                      studlab = studlab,
                      data = dfnp,
                      sm = "SMD",          
                      method.smd = "Hedges",
                      common = FALSE,       # Use random-effects model
                      random = TRUE,
                      method.tau = 'REML',
                      prediction = TRUE
)


#if (dir.exists('result/pairwise/nonclinical/depression') == FALSE) {
#dir.create('result/pairwise/nonclinical/depression/')
#}

#### depression forest plot ----
#png("result/pairwise/nonclinical/depression/dep_nonclinical_forest.png", width = 3000, height = 1500, res = 300)
forest(pwman.dep,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Depression severity',
       prediction = TRUE
)
#dev.off()

#sink('result/pairwise/nonclinical/depression/depression.txt')
pwman.dep
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()

### GENERAL ANXIETY ----

#### define anxiety df ----
# remove missing data

dfnp_anx <- dfnp_analysis %>%
  select(studlab, anx_outcome, cbm_anx_endpoint_n, cbm_anx_endpoint_mean, cbm_anx_endpoint_sd,
         control_anx_endpoint_n, control_anx_endpoint_mean, control_anx_endpoint_sd) %>%
  drop_na()

dfnp_anx <- dfnp_anx %>%
  dplyr::mutate(across(3:8, as.numeric))

#### run metacont ----
pwman.anx <- metacont(n.e = cbm_anx_endpoint_n,
                      mean.e = cbm_anx_endpoint_mean,
                      sd.e = cbm_anx_endpoint_sd,
                      n.c = control_anx_endpoint_n,
                      mean.c = control_anx_endpoint_mean,
                      sd.c = control_anx_endpoint_sd,
                      studlab = studlab,
                      data = dfnp_anx,
                      sm = "SMD",          
                      method.smd = "Hedges",
                      common = TRUE,       # Use only common-effects model because 2 studies only
                      random = TRUE,
                      method.tau = 'REML',
                      prediction = TRUE
)


#if (dir.exists('result/pairwise/nonclinical/anxiety') == FALSE) {
#dir.create('result/pairwise/nonclinical/anxiety/')
#}

#### general anxiety forest plot ----
#png("result/pairwise/nonclinical/anxiety/anx_nonclinical_forest.png", width = 3000, height = 1500, res = 300)
forest(pwman.anx,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Anxiety symptom severity'
)
#dev.off()

#sink('result/pairwise/nonclinical/anxiety/anxiety.txt')
pwman.anx
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()


### CHANGE IN BIAS----

#### define bias df ----
# remove missing data

dfnp_bias <- dfnp_analysis %>%
  select(studlab, bias_outcome, cbm_bias_endpoint_n, cbm_bias_endpoint_mean, cbm_bias_endpoint_sd,
         control_bias_endpoint_n, control_bias_endpoint_mean, control_bias_endpoint_sd) %>%
  drop_na()

dfnp_bias <- dfnp_bias %>%
  dplyr::mutate(across(3:8, as.numeric))

#### run metacont ----
pwman.bias <- metacont(n.e = cbm_bias_endpoint_n,
                       mean.e = cbm_bias_endpoint_mean,
                       sd.e = cbm_bias_endpoint_sd,
                       n.c = control_bias_endpoint_n,
                       mean.c = control_bias_endpoint_mean,
                       sd.c = control_bias_endpoint_sd,
                       studlab = studlab,
                       data = dfnp_bias,
                       sm = "SMD",          
                       method.smd = "Hedges",
                       #common = FALSE,       # Use random-effects model
                       random = TRUE,
                       method.tau = 'REML',
                       #prediction = TRUE
)


#if (dir.exists('result/pairwise/nonclinical/bias') == FALSE) {
#dir.create('result/pairwise/nonclinical/bias/')
#}

#### change in bias forest plot ----
#png("result/pairwise/nonclinical/bias/bias_nonclinical_forest.png", width = 3000, height = 1500, res = 300)
forest(pwman.bias,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Change in bias',
       prediction = TRUE
)
#dev.off()

#sink('result/pairwise/nonclinical/bias/bias.txt')
pwman.bias
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()


### QUALITY OF LIFE ----
# NOTE: NOT ENOUGH DATA - only 1 study

#### define qol df ----
# remove missing data

#dfnp_qol <- dfnp_analysis %>%
#select(studlab, qol_outcome, cbm_qol_endpoint_n, cbm_qol_endpoint_mean, cbm_qol_endpoint_sd,
#  control_qol_endpoint_n, control_qol_endpoint_mean, control_qol_endpoint_sd) %>%
#drop_na()

#### run metacont ----
#pwmac.qol <- metacont(n.e = cbm_bias_endpoint_n,
#                     mean.e = cbm_bias_endpoint_mean,
#                    sd.e = cbm_bias_endpoint_sd,
#                   n.c = control_bias_endpoint_n,
#                  mean.c = control_bias_endpoint_mean,
#                 sd.c = control_bias_endpoint_sd,
#                studlab = studlab,
#               data = dfnp_qol,
#              sm = "SMD",          
#             method.smd = "Hedges",
#            common = FALSE,       # random-effects model
#           random = TRUE,
#          method.tau = 'REML',
#         prediction = TRUE
#)

##if (dir.exists('result/pairwise/clinical/quality_of_life') == FALSE) {
# #dir.create('result/pairwise/clinical/quality_of_life/')
#}


#### bias change forest plot ----
##png("result/pairwise/clinical/quality_of_life/qol_clinical_forest.png", width = 1600, height = 1200, res = 300)
##orest(pwmac.qol,
#     sortvar = seTE,
#    print.I2.ci = TRUE,
#   label.e = 'CBM',
#  label.c = 'Control',
# label.left = 'Favours CBM',
#label.right = "Favours control",
#smlab = 'QoL',
#prediction = TRUE
#)
##dev.off()

##sink('result/pairwise/clinical/quality_of_life.txt')
#pwmac.qol
##cat(paste0(''), sep = '\n')
##cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
##sink()

#ACCEPTABILITY ----

#### define acc df ----
#remove missing data

dfnp_acc <- dfnp_analysis %>%
  select(studlab, cbm_acc_baseline_n, cbm_acc_endpoint_dropouts, control_acc_baseline_n,
         control_acc_endpoint_dropouts) %>%
  drop_na()

dfnp_acc <- dfnp_acc %>%
  dplyr::mutate(across(2:4, as.numeric))

#### run metabin ----
pwman.acc <- metabin(event.e = cbm_acc_endpoint_dropouts,
                     n.e = cbm_acc_baseline_n,
                     event.c = control_acc_endpoint_dropouts,
                     n.c = control_acc_baseline_n,
                     studlab = studlab,
                     data = dfnp_acc,
                     sm = "OR",
                     method.tau = "REML",
                     prediction = TRUE)



#if (dir.exists('result/pairwise/nonclinical/acceptibility') == FALSE) {
#dir.create('result/pairwise/nonclinical/acceptibility/')
#}


#### acc forest plot ----
#png("result/pairwise/nonclinical/acceptibility/acc_nonclinical_forest.png", width = 3000, height = 2000, res = 300)
forest(pwman.acc,
       sortvar = seTE,
       print.I2.ci = TRUE,
       label.e = 'CBM',
       label.c = 'Control',
       label.left = 'Favours CBM',
       label.right = "Favours control",
       smlab = 'Dropouts (any reason)',
       prediction = TRUE
)
#dev.off()

#sink("result/pairwise/nonclinical/acceptibility/acceptibility.txt")
pwman.acc
#cat(paste0(''), sep = '\n')
#cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']]), sep = '\n')
#sink()



