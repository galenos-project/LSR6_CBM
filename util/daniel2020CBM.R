# Data Extraction of Daniel (2020) using IPD ----

# Daniel (2020) measured but did not report social anxiety 
# This script uses publicly available IPD to calculate aggregate social anxiety scores
# data available at https://osf.io/eprwt/files/osfstorage 
# 10.1007/s10608-020-10088-2 

# load packages
library(readr)
library(dplyr)

### load df with participant ids ---- 
RT_surveys_shared <- read_csv("RT_surveys_shared.csv")

# create sid_df with unique SubjectID and each corresponding condition
sid_df <- RT_surveys_shared %>%
distinct(SubjectID, Condition, .keep_all = TRUE)

# rename SubjectID column for consistency
colnames(sid_df)[1] <- "subjectID"

# determining condition: 

# manuscript reports n=55 n ema condition, n=59 in CBM, so 1 = CBM, 2 = EMA 
filtered_ema <- subset(sid_df, Condition == 1)
filtered_cbm <- subset(sid_df, Condition == 2)

### load prescreening SIAS ----
SIAS_prescreen <- read_csv("SAMMI_SIAS_prescreen.csv")

# calculate total SIAS score at baseline
SIAS_prescreen$total <- rowSums(SIAS_prescreen[, c(3, 4, 5, 6, 8, 
                                                   9, 10, 11, 13, 14, 
                                                   16, 17, 18, 19, 20, 
                                                   21, 22, 23, 24, 25)], na.rm = TRUE)


# identify participants that were randomised (SIAS prescreen > 28) 
baseline_SIAS_rand <- subset(SIAS_prescreen, total > 28)

# assign correct participant numbers from sid_df to baseline SIAS df 
baseline_SIAS_rand <- baseline_SIAS_rand %>%
left_join(sid_df[, c("subjectID", "Condition")], by = "subjectID")

# count participant numbers in each group 
condition_count <- table(baseline_SIAS_rand$Condition)
print(condition_count)

### replace missing condition ----
# CBM group only has n=58 whereas manuscript states n=59  
# one participant in baseline_sias_rand does not have an assigned condition 

# replace NA value in baseline_sias_rand col with 2 
baseline_SIAS_rand$Condition[is.na(baseline_SIAS_rand$Condition)] <- 2

### person-mean imputation ----

# participant 6012 is missing 4 SIAS items
participant_id <- 6012

# select SIAS items 
sias_items <- grep("^SIAS", names(baseline_SIAS_rand), value = TRUE)

# compute the person-mean (excluding NAs) for participant 6012 
person_mean <- rowMeans(baseline_SIAS_rand[baseline_SIAS_rand$subjectID == participant_id, sias_items], na.rm = TRUE)

# replace missing values in SIAS 15-18 with the computed person mean 
baseline_SIAS_rand[baseline_SIAS_rand$subjectID == participant_id, c("SIAS15", "SIAS16", "SIAS17", "SIAS18")] <- 
  ifelse(is.na(baseline_SIAS_rand[baseline_SIAS_rand$subjectID == participant_id, c("SIAS15", "SIAS16", "SIAS17", "SIAS18")]),
         person_mean, 
         baseline_SIAS_rand[baseline_SIAS_rand$subjectID == participant_id, c("SIAS15", "SIAS16", "SIAS17", "SIAS18")])

### followup data ----

# load followup scores 
followup_SIAS_rand <- read_csv("SAMMI_Session2Questionnaires_shared.csv") 

# calculate total SIAS followup 
followup_SIAS_rand$total <- rowSums(followup_SIAS_rand[, c(15, 16, 17, 18, 20, 
                                                           21, 22, 23, 25, 26, 
                                                           28, 29, 30, 31, 32, 
                                                           33, 34, 35, 36)], na.rm = FALSE)

# add condition column from baseline_SIAS_rand to followup_SIAS_rand 
followup_SIAS_rand <- followup_SIAS_rand %>%
  left_join(baseline_SIAS_rand[, c("subjectID", "Condition")], by = "subjectID")

# calculate BFNE total scores 
followup_SIAS_rand$BFNEtotal <- rowSums(followup_SIAS_rand[, c(7:14)])


### create ITT df ----
# merge cols
ITT <- followup_SIAS_rand %>%
  select(subjectID, total) %>%  # keep SubjectID and endpoint score
  rename(SIAS_followup_total = total) %>% 
  left_join(baseline_SIAS_rand %>% select(subjectID, Condition, total), by = "subjectID") %>%
  rename(SIAS_baseline_total = total)

#ITT df now corresponds with n=106, 55=EMA, 51=CBM as in the manuscrupt

ITT$BFNE_followup_total <- followup_SIAS_rand$BFNEtotal

# as two participants did not complete followup SIAS, their baseline scores need to be excluded 
indices_to_modify <- which(is.na(ITT$SIAS_followup_total))
ITT$SIAS_baseline_total[indices_to_modify] <- NA

ITT_ema <- subset(ITT, Condition == 2)
ITT_cbm <- subset(ITT, Condition == 1)


### create ITT aggregate df ----
ITT_aggregate <- data.frame(
  Condition = c("CBM", "EMA"),  # row labels
  SIASbaseline_mean = NA,       # placeholder
  SIASbaseline_sd = NA,
  SIASfollowup_mean = NA,
  SIASfollowup_sd = NA,
  BNFEfollowup_mean = NA,
  BNFEfollowup_sd = NA
)

### calculate aggregate means and sd ----
ITT_aggregate[1, 2] <- mean(ITT_cbm$SIAS_baseline_total)
ITT_aggregate[2, 2] <- mean(ITT_ema$SIAS_baseline_total)
ITT_aggregate[1, 3] <- sd(ITT_cbm$SIAS_baseline_total)
ITT_aggregate[2, 3] <- sd(ITT_ema$SIAS_baseline_total)

ITT_aggregate[1, 4] <- mean(ITT_cbm$SIAS_followup_total)
ITT_aggregate[2, 4] <- mean(ITT_ema$SIAS_followup_total)
ITT_aggregate[1, 5] <- sd(ITT_cbm$SIAS_followup_total)
ITT_aggregate[2, 5] <- sd(ITT_ema$SIAS_followup_total)

ITT_aggregate[1, 6] <- mean(ITT_cbm$BFNE_followup_total)
ITT_aggregate[2, 6] <- sd(ITT_cbm$BFNE_followup_total)
ITT_aggregate[1, 7] <- mean(ITT_ema$BFNE_followup_total)
ITT_aggregate[2, 7] <- sd(ITT_ema$BFNE_followup_total)


### sink aggregate scores to txt  ----
sink("Daniel 2020 Aggregate Scores.txt")
cat("Aggregate mean and standard deviationfor ITT Population in Daniel (2020)\n")
cat("\n\n")
print(ITT_aggregate)
sink()




