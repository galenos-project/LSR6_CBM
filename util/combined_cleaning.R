# Combined cleaning Script: PWMA + NMA 

# NOTES 
# Cleaning to prepare data for pairwise and network meta-analysis 
# the suffix _subX (where X is a number) denotes subgroups 
# the suffix _subnX denotes a subnetwork 
# n is used to denote nonclinical source e.g. nma_anxn = nonclinical anxiety nma object 
# c is used to denote clinical source data e.g. dfc_sad = clinical social anxiety data 

# A: *PWMA CLEANING *----

#### packages ----
library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)
library(purrr)

#### load data ----
df_raw <- read_excel("data/eppi_export_070525edited.xlsx",
                     sheet = "Outcomes")

pwma_edits <- read_excel("data/pwma_edits.xlsx")

infobox_data <- read_excel("data/eppi_export_070525edited.xlsx", sheet = "InfoBox (arms)", col_names = FALSE)
# NOTE: this is the raw eppi export (in same folder) but for mean age and n_female grey (incomplete) data has been removed in infobox sheet
# all cols in infobox (arms) sheet but mean_age, dropouts, n female and n participants have been deleted

# risk of bias overall scores for primary outcome
rob_sad <- read_excel("data/sensitivity_data.xlsx", sheet = "SAD_ROB")

diag_sad <- read_excel("data/sensitivity_data.xlsx", sheet = "SAD_DIAG")

#### filter completed records ----
df1 <- df_raw %>%
  dplyr::filter(str_starts(df_raw$IsCompleted, "TRUE"))

#### remove unecessary cols ----
df2 <- df1[, -20:-31]

df3 <- df2 %>%
  select(-`Reviewer`, -ITEM_ID, -IsCompleted, -`I/E/D/S flag`, -`Outcome description`,
         -Comparison, -`Arm 2`, -`Data 2`, -`Data 4`)

#### rename cols ----
names <- c("studlab",
           "outcome_title",
           "timepoint",
           "outcome",
           "intervention",
           "arm",
           "outcome_type",
           "n",
           "mean",
           "sd")

colnames(df3) <- names

### convert se to sd  ----
indices_to_modify <- which(grepl("Khalili-Torghabeh \\(2014\\)", df3$studlab) & grepl("Target", df3$outcome))
for (i in indices_to_modify) {
  df3$sd[i] <- df3$sd[i] * sqrt(df3$n[i])
}

### preserve timepoint ----
df3days <- df3 %>%
  dplyr::filter(str_ends(df3$timepoint, "days"))
df3days$units <- "days"

df3weeks <- df3 %>%
  dplyr::filter(str_ends(df3$timepoint, "weeks"))
df3weeks$units <- "weeks"

df3months <- df3 %>%
  dplyr::filter(str_ends(df3$timepoint, "months"))
df3months$units <- "months"

df3hours <- df3 %>%
  dplyr::filter(str_ends(df3$timepoint, "hours"))
df3hours$units <- "hours"

#rejoin dfs
df3 <- rbind(df3hours, df3days, df3months, df3weeks) 


#### remove units from timepoint col----
df3$timepoint<- sub(" weeks", "", df3$timepoint)
df3$timepoint<- sub(" hours", "", df3$timepoint)
df3$timepoint<- sub(" days", "", df3$timepoint)
df3$timepoint<- sub(" months", "", df3$timepoint)
df3$timepoint <- as.numeric(df3$timepoint)

timepoint <- df3$timepoint

#### standardise timepoint notation ----
df4 <- df3 %>% mutate(across(everything(), ~ replace(.x, .x == 9999, 99999))) 

df4$timepoint <- timepoint

#### funct to extract infobox data from eppi export ----

# Study names are in column 2 starting from row 5 
study_names <- infobox_data[[2]][6:nrow(infobox_data)]

# General extraction function for all variables 
extract_arms_value <- function(text) {
  if (is.na(text) || !str_detect(text, "\\d")) {
    return(tibble(arm = character(), value = numeric()))
  }
  
  arms <- str_split(text, "[-]{2,}")[[1]]
  
  res <- map(arms, function(entry) {
    lines <- str_split(entry, "\\n")[[1]]
    lines <- str_trim(lines)
    lines <- lines[lines != ""]
    if (length(lines) < 2) return(NULL)
    arm_name <- lines[1]
    val <- as.numeric(str_extract(paste(lines[-1], collapse = " "), "\\d+\\.*\\d*"))
    tibble(arm = arm_name, value = val)
  })
  
  bind_rows(res)
}

# Function to extract and stack values from 6-column blocks
extract_block <- function(start_col, end_col, new_col_name) {
  block <- infobox_data[6:nrow(infobox_data), start_col:end_col]
  colnames(block) <- paste0("col_", seq_len(ncol(block)))
  block <- mutate(block, Short_Title = study_names)
  
  long_data <- block %>%
    pivot_longer(cols = starts_with("col_"), values_to = "text") %>%
    dplyr::filter(!is.na(text)) %>%
    group_by(Short_Title) %>%
    summarise(extracted = list(map_df(text, extract_arms_value)), .groups = "drop") %>%
    unnest(extracted, keep_empty = TRUE) %>%
    mutate(
      studlab_clean = str_squish(tolower(Short_Title)),
      arm_clean = if ("arm" %in% names(.)) str_squish(tolower(str_remove(arm, ":$"))) else NA_character_
    )
  
  if ("value" %in% names(long_data)) {
    long_data <- long_data %>%
      rename(!!paste0(new_col_name, "_new") := value)
  } else {
    long_data[[paste0(new_col_name, "_new")]] <- NA_real_
  }
  
  return(long_data)
}

# Prepare df4 if needed (ensure it has studlab and arm)
df4 <- df4 %>%
  mutate(
    studlab_clean = str_squish(tolower(studlab)),
    arm_clean = str_squish(tolower(arm))
  )

# extract vars from infobox_data

         
#### extract mean_age ---- 
# cols 147–152
mean_age_long <- extract_block(4, 9, "mean_age")
   df4 <- df4 %>%
      left_join(mean_age_long, by = c("studlab_clean", "arm_clean"))

          
#### extract n_female ----
# cols 153–158
female_long <- extract_block(16, 21, "female")
   df4 <- df4 %>%
     left_join(female_long, by = c("studlab_clean", "arm_clean"))

### extract dropouts----  
   
dropout_long <- extract_block(10, 15, "dropouts")
df4 <- df4 %>%
  left_join(dropout_long, by = c("studlab_clean", "arm_clean"))

#### extract n participants
n_long <- extract_block(22, 27, "n_randomised")
df4 <- df4 %>%
  left_join(n_long, by = c("studlab_clean", "arm_clean"))
  
# clean up
df5 <- df4 %>% 
   select(studlab, outcome_title, timepoint, outcome, intervention, arm.x, outcome_type, 
          n, mean, sd, units, mean_age_new, female_new, dropouts_new, n_randomised_new)

names(df5)[names(df5) == "arm.x"] <- "arm"
names(df5)[names(df5) == "mean_age_new"] <- "mean_age"
names(df5)[names(df5) == "female_new"] <- "n_female"
names(df5)[names(df5) == "dropouts_new"] <- "dropouts"
names(df5)[names(df5) == "n_randomised_new"] <- "n_randomised"

#### split into clin/on-clin sources of evidence ----
clin_source <- pwma_edits %>%
  dplyr::filter(str_starts(pwma_edits$source, "C"))

nonclin_source <- pwma_edits %>%
  dplyr::filter(str_starts(pwma_edits$source, "NC"))

df.clin <- df5 %>%
  dplyr::filter(studlab %in% clin_source$studlab)

df.nonclin <- df5 %>%
  dplyr::filter(studlab %in% nonclin_source$studlab)

indices_to_modify <- which(grepl("^Amir \\(2009\\)", df.clin$studlab) & (df.clin$mean == 12.4))
df.clin$timepoint[indices_to_modify] <- 4

indices_to_modify <- which(grepl("^Amir \\(2012\\)", df.clin$studlab) & (df.clin$mean == 22))
df.clin$n[indices_to_modify] <- 26


#### remove studies ineligible for pwma ---- 

# identify corresponding studies
studies_to_remove <- pwma_edits %>%
  dplyr::filter(operation == "RM") %>%
  dplyr::pull(studlab)

# remove 
df.clin <- df.clin %>%
  dplyr::filter(!studlab %in% studies_to_remove)

df.nonclin <- df.nonclin %>%
  dplyr::filter(!studlab %in% studies_to_remove)

#### function to combine arms ----
combine_arms <- function(df, study, arms_to_combine, new_arm_name) {
  df_to_combine <<- df %>%
    dplyr::filter(studlab == study, arm %in% arms_to_combine)
  
  # check for missing n, mean, or sd 
  missing_values <<- df_to_combine %>%
    dplyr::filter(is.na(n) | is.na(mean) | is.na(sd))
  
  if (nrow(missing_values) > 0) {
    warning(paste0(
      "Missing values detected in 'n', 'mean', or 'sd' for study '",
      study,
      "' and arms: ",
      paste(unique(missing_values$arm), collapse = ", "),
      ". Combined sd will be NA for affected groups."
    ))
  }
  
  df_combined2 <<- df_to_combine %>%
    group_by(outcome_title, timepoint) %>%
    dplyr::filter(n() == 2) %>%
    summarise(
      studlab = first(studlab),
      outcome = first(outcome),
      intervention = new_arm_name,
      arm = new_arm_name,
      outcome_type = first(outcome_type),
      n_comb = sum(n),
      mean_comb = sum(n * mean) / sum(n),
      sd_comb = if (any(is.na(n) | is.na(mean) | is.na(sd))) NA_real_ else sqrt(
        ((n[1] - 1) * sd[1]^2 +
           (n[2] - 1) * sd[2]^2 +
           (n[1] * n[2]) / (n[1] + n[2]) * (mean[1] - mean[2])^2) /
          (n[1] + n[2] - 1)
      ),
      mean_age = if (all(!is.na(mean_age))) sum(n * mean_age) / sum(n) else NA_real_,
      n_female = if (all(!is.na(n_female))) sum(n_female) else NA_integer_,
      .groups = "drop"
    ) %>%
    rename(
      n = n_comb,
      mean = mean_comb,
      sd = sd_comb
    )
  
  df_unmatched <<- df_to_combine %>%
    group_by(outcome_title, timepoint) %>%
    dplyr::filter(n() < 2) %>%
    ungroup()
  
  result <- df %>%
    dplyr::filter(!(studlab == study & arm %in% arms_to_combine)) %>%
    bind_rows(df_combined2, df_unmatched)
  
  return(result)
}

#### combine Mobini (2014) ----
df_combined <- combine_arms(
  df = df.clin,
  study = "Mobini (2014)",
  arms_to_combine = c("Standard CBM-I", "Explicit CBM-I"),
  new_arm_name = "Combined CBM-I"
)

df.clin <- df_combined

#### combine Yao (2015) ----
df_combined <- combine_arms(
  df = df.clin,
  study = "Yao (2015)",
  arms_to_combine = c("AGC", "ACC"),
  new_arm_name = "Control CBM-A"
)

df.clin <- df_combined

#### combine Yeung (2019) ----
df_combined <- combine_arms(
  df = df.clin,
  study = "Yeung (2019)",
  arms_to_combine = c("Interpretation Bias Modification (CBM-I )", "Attentional Bias Modification (CBM-A)"),
  new_arm_name = "Multicomponent CBM"
)
df.clin <- df_combined

#### combine Liang (2016) ----
df_combined <- combine_arms(
  df = df.clin,
  study = "Liang (2016)",
  arms_to_combine = c("AP-100", "AP-500"),
  new_arm_name = "Control CBM-A"
)
df.clin <- df_combined

df_combined <- combine_arms(
  df = df.clin,
  study = "Liang (2016)",
  arms_to_combine = c("ABM-100", "ABM-500"),
  new_arm_name = "CBM-A"
)
df.clin <- df_combined

# manually fix extraction error
indices_to_modify <- which(grepl("Liang \\(2016\\)", df.clin$studlab) & grepl("AP-100", df.clin$arm))

df.clin$arm[indices_to_modify] <- "Control CBM-A"

#### combine Edwards (2018) ----
dfn_combined <- combine_arms(
  df = df.nonclin,
  study = "Edwards (2018)",
  arms_to_combine = c("CBM-50", "CBM-100"),
  new_arm_name = "CBM-I"
)

df.nonclin <- dfn_combined


#### create intervention vectors ----
source("util/intervention_classification.R")

#### classify interventions as active or control ----
df.clin$int_type <- ifelse(df.clin$intervention %in% control, "control",
                           ifelse(df.clin$intervention %in% active, "cbm", NA))

df.nonclin$int_type <- ifelse(df.nonclin$intervention %in% control, "control",
                              ifelse(df.nonclin$intervention %in% active, "cbm", NA))

# CLINICAL SOURCE OF EVIDENCE----
#### create analysis dfs ----

# NOTE: from here dfc refers to clinical source of evidence

#### remove ineligible interventions 
df_clean <- df.clin %>%
  dplyr::filter(!is.na(int_type))

#### remove Heeren (2011) ineligible arms - see manuscript for more details ----
indices_to_modify <- which(grepl("Heeren \\(2011\\)", df_clean$studlab) & grepl("^ABM disengage$", df_clean$arm))
df_clean <- df_clean[-indices_to_modify, ]

indices_to_modify <- which(grepl("Heeren \\(2011\\)", df_clean$studlab) & grepl("^ABM re-engage$", df_clean$arm))
df_clean <- df_clean[-indices_to_modify, ]

# correct an error with extraction
indices_to_modify <- which(grepl("Beard \\(2008\\)", df_clean$studlab) & grepl ("WSAS", df_clean$outcome))
df_clean[indices_to_modify, "outcome"] <- "WSAP: Negative endorsement"

indices_to_modify <- which(grepl("Ollendick \\(2019\\)", df_clean$studlab) & is.na(df_clean$outcome))
df_clean$outcome[indices_to_modify] <- "SCARED Social Anxiety Subscale"

indices_to_modify <- which(grepl("Liu \\(2024\\)", df_clean$studlab))
df_clean$outcome[indices_to_modify] <- "Change in attentional bias"

indices_to_modify <- which(grepl("Klumpp \\(2010\\)", df_clean$studlab))
df_clean$intervention[indices_to_modify] <- "Opposite CBM-A"


#### rearrange cols 
df_clean$arm <- df_clean$int_type

#### sort by outcome type 
source("util/scale_categories.R")


#### create outcome dfs ----
#### dep df ----
dfc_dep <- df_clean %>%
  dplyr::filter(outcome %in% dep)

#### bias change df ----
dfc_bias <- df_clean %>%
  dplyr::filter(outcome %in% bias_change)

#### anx df ----
dfc_anx <- df_clean %>%
  dplyr::filter(outcome %in% general_anx)

#### social_anx df ----
dfc_sad <- df_clean %>%
  dplyr::filter(outcome %in% social_anx)

#### qol df ----
dfc_qol <- df_clean %>%
  dplyr::filter(outcome %in% qol)

#### acc df ----
dfc_acc <- df.clin


# save timepoint col 
sa_timepoint <- dfc_sad$timepoint
dep_timepoint <- dfc_dep$timepoint
bias_timepoint <- dfc_bias$timepoint
anx_timepoint <- dfc_anx$timepoint
qol_timepoint <- dfc_qol$timepoint
acc_timepoint <- dfc_acc$timepoint

####  change missing data to NA ----
dfc_sad <- dfc_sad %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_dep <- dfc_dep %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_anx <- dfc_anx %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_bias <- dfc_bias %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_qol <- dfc_qol %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_acc <- dfc_acc %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))

# replace timepoint col 
dfc_sad$timepoint <- sa_timepoint
dfc_dep$timepoint <- dep_timepoint
dfc_anx$timepoint <- anx_timepoint
dfc_bias$timepoint <- bias_timepoint
dfc_qol$timepoint <- qol_timepoint
dfc_acc$timepoint <- acc_timepoint

#### standardise timepoints ----
# change 99999 (post-intervention, same day) timepoint to 1 
dfc_sad$timepoint <- lapply(dfc_sad$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_dep$timepoint <- lapply(dfc_dep$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_anx$timepoint <- lapply(dfc_anx$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_bias$timepoint <- lapply(dfc_bias$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_qol$timepoint <- lapply(dfc_qol$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_acc$timepoint <- lapply(dfc_acc$timepoint, function(x) ifelse(x == 99999, 1, x))

#### calculate threat bias score from congruent/incongruent RTs ----
# for Neubauer (2013)
bias_calculate <- function(df, study, outcomes_to_combine, new_outcome) {
  # Clean and filter the relevant rows
  df_filtered <- df %>%
    mutate(outcome_title = stringr::str_trim(outcome_title)) %>%
    dplyr::filter(studlab == study, outcome %in% outcomes_to_combine) %>%
    group_by(studlab, timepoint, intervention, arm, outcome_type, outcome_title) %>%
    summarise(
      n = mean(n, na.rm = TRUE),
      mean = mean(mean, na.rm = TRUE),
      sd = mean(sd, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot wider to align outcomes side-by-side
  df_wide <- df_filtered %>%
    tidyr::pivot_wider(
      id_cols = c(studlab, timepoint, intervention, arm, outcome_type),
      names_from = outcome_title,
      values_from = c(n, mean, sd),
      names_sep = "_"
    )
  
  # Define required columns dynamically
  required_cols <- unlist(lapply(c("mean_", "sd_", "n_"), function(prefix) {
    paste0(prefix, outcomes_to_combine)
  }))
  
  # Check that all required columns are present
  if (!all(required_cols %in% colnames(df_wide))) {
    message("Missing one or more required columns for study: ", study)
    return(df)
  }
  
  # Filter for complete cases (i.e., no NAs in relevant columns)
  df_wide <- df_wide %>%
    dplyr::filter(
      !is.na(.data[[paste0("mean_", outcomes_to_combine[1])]]) &
        !is.na(.data[[paste0("mean_", outcomes_to_combine[2])]]) &
        !is.na(.data[[paste0("sd_", outcomes_to_combine[1])]]) &
        !is.na(.data[[paste0("sd_", outcomes_to_combine[2])]])
    )
  
  if (nrow(df_wide) == 0) {
    message("No valid rows with both congruent and incongruent data for: ", study)
    return(df)
  }
  
  # Compute bias score and pooled SD
  df_bias <- df_wide %>%
    mutate(
      outcome = new_outcome,
      mean = .data[[paste0("mean_", outcomes_to_combine[2])]] -
        .data[[paste0("mean_", outcomes_to_combine[1])]],
      n = coalesce(.data[[paste0("n_", outcomes_to_combine[1])]],
                   .data[[paste0("n_", outcomes_to_combine[2])]]),
      sd = sqrt(
        ((.data[[paste0("n_", outcomes_to_combine[1])]] - 1) *
           (.data[[paste0("sd_", outcomes_to_combine[1])]])^2 +
           (.data[[paste0("n_", outcomes_to_combine[2])]] - 1) *
           (.data[[paste0("sd_", outcomes_to_combine[2])]])^2 +
           (.data[[paste0("n_", outcomes_to_combine[1])]] *
              .data[[paste0("n_", outcomes_to_combine[2])]]) /
           (.data[[paste0("n_", outcomes_to_combine[1])]] +
              .data[[paste0("n_", outcomes_to_combine[2])]]) *
           (.data[[paste0("mean_", outcomes_to_combine[1])]] -
              .data[[paste0("mean_", outcomes_to_combine[2])]])^2) /
          (.data[[paste0("n_", outcomes_to_combine[1])]] +
             .data[[paste0("n_", outcomes_to_combine[2])]] - 1)
      )
    ) %>%
    select(studlab, timepoint, outcome, intervention, arm, outcome_type, n, mean, sd)
  
  # Return original data with new bias rows appended
  df %>%
    dplyr::filter(!(studlab == study & outcome %in% outcomes_to_combine)) %>%
    bind_rows(df_bias)
}

# remove non-prioritised rows so the function works properly
indices_to_modify <- which(grepl("Neubauer \\(2013\\)", dfc_bias$studlab) & grepl("Neutral reaction time", dfc_bias$outcome))
dfc_bias <- dfc_bias[-indices_to_modify, ]

dfc_bias <- bias_calculate(
  df = dfc_bias,
  study = "Neubauer (2013)",
  outcomes_to_combine = c("Congruent reaction time", "Incongruent reaction time"),
  new_outcome = "Threat bias measurement"
)


 #### apply intervention labels ----
dfc_bias$int_type <- ifelse(dfc_bias$intervention %in% control, "control",
                             ifelse(dfc_bias$intervention %in% active, "cbm", NA))


#### select prioritised outcomes ----
po_sa <- read_excel("data/prioritised_outcomes.xlsx", sheet = "SAD")
po_dep <- read_excel("data/prioritised_outcomes.xlsx", sheet = "DEP")
po_anx <- read_excel("data/prioritised_outcomes.xlsx", sheet = "ANX")
po_bias <- read_excel("data/prioritised_outcomes.xlsx", sheet = "BIAS")
po_qol <- read_excel("data/prioritised_outcomes.xlsx", sheet = "QOL")



# join outcomes by studlab 
dfc_sad1 <- dfc_sad %>%
  left_join(po_sa, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfc_dep1 <- dfc_dep %>%
  left_join(po_dep, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfc_anx1 <- dfc_anx %>%
  left_join(po_anx, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfc_bias1 <- dfc_bias %>%
  left_join(po_bias, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfc_qol1 <- dfc_qol %>%
  left_join(po_bias, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(!is.na(outcome.po)) %>%  
  select(-outcome.po) 


#### remove arms with missing data  ----
# social anxiety
dfc_sad1 <- dfc_sad1 %>%
  dplyr::filter(!is.na(mean))

dfc_sad1 <- dfc_sad1 %>%
  dplyr::filter(!is.na(sd))

dfc_sad1 <- dfc_sad1 %>%
  dplyr::filter(!is.na(n))

# depression 
dfc_dep1 <- dfc_dep1 %>%
  dplyr::filter(!is.na(mean))

dfc_dep1 <- dfc_dep1 %>%
  dplyr::filter(!is.na(sd))

dfc_dep1 <- dfc_dep1 %>%
  dplyr::filter(!is.na(n))

# general anxiety 
dfc_anx1 <- dfc_anx1 %>%
  dplyr::filter(!is.na(mean))

dfc_anx1 <- dfc_anx1 %>%
  dplyr::filter(!is.na(sd))

dfc_anx1 <- dfc_anx1 %>%
  dplyr::filter(!is.na(n))

# change in bias 
dfc_bias1 <- dfc_bias1 %>%
  dplyr::filter(!is.na(mean))

dfc_bias1 <- dfc_bias1 %>%
  dplyr::filter(!is.na(sd))

dfc_bias1 <- dfc_bias1 %>%
  dplyr::filter(!is.na(n))

# quality of life
dfc_qol1 <- dfc_qol1 %>%
  dplyr::filter(!is.na(mean))

dfc_qol1 <- dfc_qol1 %>%
  dplyr::filter(!is.na(sd))

dfc_qol1 <- dfc_qol1 %>%
  dplyr::filter(!is.na(n))

# acceptability 
dfc_acc1 <- dfc_acc %>%
  dplyr::filter(!is.na(n))

dfc_acc1 <- dfc_acc1 %>%
  dplyr::filter(!is.na(dropouts))


#### pivot data for analysis ----
# social anxiety 
dfc_sad2 <- dfc_sad1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_sad3 <- dfc_sad2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# depression
dfc_dep2 <- dfc_dep1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_dep3 <- dfc_dep2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# general anxiety
dfc_anx2 <- dfc_anx1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_anx3 <- dfc_anx2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# change in bias 
dfc_bias2 <- dfc_bias1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_bias3 <- dfc_bias2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# quality of life
dfc_qol2 <- dfc_qol1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_qol3 <- dfc_qol2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# acceptibility

dfc_acc2 <- dfc_acc1 %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  group_by(studlab) %>%
  mutate(
    baseline_time = min(timepoint, na.rm = TRUE),
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")
  ) %>%
  ungroup() %>%
  select(-baseline_time)

# pivot again
dfc_acc3 <- dfc_acc2 %>%
  group_by(studlab, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    dropouts = mean(as.numeric(dropouts), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, intervention, int_type),
    names_from = timepoint_label,
    values_from = c(n, dropouts),
    names_glue = "{timepoint_label}_{.value}"
  )


#### funct to invert scores ----
invert_scores <- function(df, studlab_value, columns_to_invert) {
  # Check if studlab column exists
  if (!"studlab" %in% names(df)) {
    stop("The data frame does not contain a 'studlab' column.")
  }
  
  # Check columns exist
  if (!all(columns_to_invert %in% names(df))) {
    missing_cols <- setdiff(columns_to_invert, names(df))
    stop(paste("These columns are not in the data frame:", paste(missing_cols, collapse = ", ")))
  }
  
  # Perform the inversion
  df[df$studlab == studlab_value, columns_to_invert] <- 
    -1 * df[df$studlab == studlab_value, columns_to_invert]
  
  return(df)
}

### invert qol scores ----
# invert qol scores where higher = better
dfc_qol3 <- invert_scores(dfc_qol3, studlab_value = "Amir (2009)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol3 <- invert_scores(dfc_qol3, studlab_value = "Amir (2012)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol3 <- invert_scores(dfc_qol3, studlab_value = "Boettcher (2013)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol3 <- invert_scores(dfc_qol3, studlab_value = "Orchard (2017)", columns_to_invert = c("baseline_mean", "endpoint_mean"))


#### join dfs ----

#### function to rename outcome-specific cols ----
rename_with_prefix <- function(df, prefix, cols = everything()) {
  df %>%
    rename_with(~ paste0(prefix, .), .cols = {{ cols }})
}

# apply prefixing to each domain-specific dataframe
dfc_social <- rename_with_prefix(
  dfc_sad3, "social_anx_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)

dfc_dep <- rename_with_prefix(
  dfc_dep3, "dep_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)

dfc_anx <- rename_with_prefix(
  dfc_anx3, "anx_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)

dfc_bias <- rename_with_prefix(
  dfc_bias3, "bias_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)

dfc_qol <- rename_with_prefix(
  dfc_qol3, "qol_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)

dfc_acc <- rename_with_prefix(
  dfc_acc3, "acc_", 
  cols = c(baseline_n, endpoint_n, baseline_dropouts, endpoint_dropouts)
)

# NOTE: this creates redundant col (baseline_dropouts) to ensure correct structure for join but is later removed 


# join
df_joined <- dfc_social %>%
  full_join(dfc_dep,  by = c("studlab", "int_type")) %>%
  full_join(dfc_anx,  by = c("studlab", "int_type")) %>%
  full_join(dfc_bias, by = c("studlab", "int_type")) %>%
  full_join(dfc_qol,  by = c("studlab", "int_type")) %>%
  full_join(dfc_acc,  by = c("studlab", "int_type"))


dfcp_analysis <- df_joined

#### rename cols ----
names <- c("studlab",
           "sa_outcome",
           "intervention",
           "int_type",
           "sa_baseline_n",
           "sa_endpoint_n",
           "sa_baseline_mean",
           "sa_endpoint_mean",
           "sa_baseline_sd",
           "sa_endpoint_sd",
           "dep_outcome",
           "intervention.y",
           "dep_baseline_n",
           "dep_endpoint_n",
           "dep_baseline_mean",
           "dep_endpoint_mean",       
           "dep_baseline_sd",
           "dep_endpoint_sd",
           "anx_outcome",
           "intervention.z",
           "anx_baseline_n",
           "anx_endpoint_n",
           "anx_baseline_mean",
           "anx_endpoint_mean",
           "anx_baseline_sd",
           "anx_endpoint_sd",
           "bias_outcome",
           "intervention.y.y",
           "bias_baseline_n",
           "bias_endpoint_n",
           "bias_baseline_mean",
           "bias_endpoint_mean",
           "bias_baseline_sd",
           "bias_endpoint_sd",
           "qol_outcome",
           "intervention.z.z",
           "qol_baseline_n",
           "qol_endpoint_n",
           "qol_baseline_mean",
           "qol_endpoint_mean",
           "qol_baseline_sd",
           "qol_endpoint_sd",
           "intervention.y.y.y",
           "acc_baseline_n",
           "acc_endpoint_n",
           "acc_baseline_dropouts",
           "acc_endpoint_dropouts")
          

colnames(dfcp_analysis) <- names

#### remove unecessary cols ----
dfcp_analysis <- dfcp_analysis %>%
 select(-intervention.y, -intervention.z, -intervention.y.y, - intervention.z.z, -intervention.y.y.y)

# remove redundant rows created in the pivot 
dfcp_analysis <- dfcp_analysis %>%
  dplyr::filter(!is.na(int_type))

# remove ineligible study for pwma
indices_to_modify <- which(grepl("Yang \\(2017\\)", dfcp_analysis$studlab))
dfcp_analysis <- dfcp_analysis[-indices_to_modify, ]

#### pivot wider----
dfcp_analysis <- dfcp_analysis %>%
  select(studlab, sa_outcome, int_type, sa_endpoint_n, sa_endpoint_mean, sa_endpoint_sd,
         dep_outcome, dep_endpoint_n, dep_endpoint_mean, dep_endpoint_sd,
         anx_outcome, anx_endpoint_n, anx_endpoint_mean, anx_endpoint_sd,
         bias_outcome, bias_endpoint_n, bias_endpoint_mean, bias_endpoint_sd,
         qol_outcome, qol_endpoint_n, qol_endpoint_mean, qol_endpoint_sd,
         acc_baseline_n, acc_endpoint_dropouts) %>%
  pivot_wider(
    names_from = int_type,
    values_from = c(sa_endpoint_n, sa_endpoint_mean, sa_endpoint_sd,
                    dep_endpoint_n, dep_endpoint_mean, dep_endpoint_sd,
                    anx_endpoint_n, anx_endpoint_mean, anx_endpoint_sd,
                    bias_endpoint_n, bias_endpoint_mean, bias_endpoint_sd,
                    qol_endpoint_n, qol_endpoint_mean, qol_endpoint_sd,
                    acc_baseline_n, acc_endpoint_dropouts),
    names_glue = "{int_type}_{.value}"
  )

### invert bias scores ----
# reverse bias scores for outcomes where an increase in scores is good
po_bias <- po_bias %>%
  mutate(invert = if_else(is.na(invert_bias_score), FALSE, invert_bias_score))

dfcp_analysis <- dfcp_analysis %>%
  left_join(po_bias %>% select(studlab, invert), by = "studlab") %>%
  mutate(cbm_bias_endpoint_mean = if_else(invert, -cbm_bias_endpoint_mean, cbm_bias_endpoint_mean)) %>%
  select(-invert)

dfcp_analysis <- dfcp_analysis %>%
  left_join(po_bias %>% select(studlab, invert), by = "studlab") %>%
  mutate(control_bias_endpoint_mean = if_else(invert, -control_bias_endpoint_mean, control_bias_endpoint_mean)) %>%
  select(-invert)


# add additional data (will be in subsequent EPPI export)
indices_to_modify <- which(grepl("Lam \\(2025\\)", dfcp_analysis$studlab))
# social anxiety
dfcp_analysis[indices_to_modify, ][["sa_outcome"]] <- "SPS"
dfcp_analysis[indices_to_modify, ][["cbm_sa_endpoint_n"]] <- 28
dfcp_analysis[indices_to_modify, ][["control_sa_endpoint_mean"]] <- 38.70
dfcp_analysis[indices_to_modify, ][["cbm_sa_endpoint_sd"]] <- 10.72
dfcp_analysis[indices_to_modify, ][["control_sa_endpoint_n"]] <- 30
dfcp_analysis[indices_to_modify, ][["cbm_sa_endpoint_mean"]] <- 34.36
dfcp_analysis[indices_to_modify, ][["control_sa_endpoint_sd"]] <- 11.81
# change in bias
dfcp_analysis[indices_to_modify, ][["bias_outcome"]] <- "SRT"
dfcp_analysis[indices_to_modify, ][["cbm_bias_endpoint_n"]] <- 28
dfcp_analysis[indices_to_modify, ][["cbm_bias_endpoint_mean"]] <- -0.390
dfcp_analysis[indices_to_modify, ][["cbm_bias_endpoint_sd"]] <- 0.780
dfcp_analysis[indices_to_modify, ][["control_bias_endpoint_n"]] <- 30
dfcp_analysis[indices_to_modify, ][["control_bias_endpoint_mean"]] <- 0.360
dfcp_analysis[indices_to_modify, ][["control_bias_endpoint_sd"]] <- 0.580


### remove studies with 0 dropouts in  both arms ----
indices_to_modify <- which((dfcp_analysis$cbm_acc_endpoint_dropouts == 0) & (dfcp_analysis$control_acc_endpoint_dropouts == 0))
dfcp_analysis$cbm_acc_endpoint_dropouts[indices_to_modify] <- NA
dfcp_analysis$control_acc_endpoint_dropouts[indices_to_modify] <- NA


# NONCLINICAL SOURCE OF EVIDENCE----

#NOTE from here dfn refers to nonclinical source

#### create analysis dfs ----
# this filters opposite CBM & waitlist interventions

#### remove ineligible interventions 
dfn_clean <- df.nonclin %>%
  dplyr::filter(!is.na(int_type))

#### rearrange cols 
dfn_clean$arm <- dfn_clean$int_type

#### sort by outcome type 
source("util/scale_categories.R")


#### create outcome dfs ----
#### dep df ----
dfn_dep <- dfn_clean %>%
  dplyr::filter(outcome %in% dep)

#### bias change df----
dfn_bias <- dfn_clean %>%
  dplyr::filter(outcome %in% bias_change)

#### anx df ----
dfn_anx <- dfn_clean %>%
  dplyr::filter(outcome %in% general_anx)

#### social_anx df ----
dfn_sad <- dfn_clean %>%
  dplyr::filter(outcome %in% social_anx)

#### qol df ----
#dfn_qol <- dfn_clean %>%
  #dplyr::filter(outcome %in% qol)

# ***NOTE*** as there are no nonclinical qol studies, from here on qol code is ignored using #. These can be removed if future updates have data for this.

#### acc df ----
dfn_acc <- df.nonclin

nsad_timepoint <- dfn_sad$timepoint
ndep_timepoint <- dfn_dep$timepoint
nbias_timepoint <- dfn_bias$timepoint
nanx_timepoint <- dfn_anx$timepoint
#nqol_timepoint <- dfn_qol$timepoint
nacc_timepoint <- dfn_acc$timepoint

####  change missing data to NA ----
dfn_sad <- dfn_sad %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_dep <- dfn_dep %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_anx <- dfn_anx %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_bias <- dfn_bias %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
#dfn_qol <- dfn_qol %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_acc <- dfn_acc %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))

# replace timepoint col 
dfn_sad$timepoint <- nsad_timepoint
dfn_dep$timepoint <- ndep_timepoint
dfn_anx$timepoint <- nanx_timepoint
dfn_bias$timepoint <- nbias_timepoint
#dfn_qol$timepoint <- nqol_timepoint
dfn_acc$timepoint <- nacc_timepoint


#### standardise timepoints ----
# change 99999 (post-intervention, same day) timepoint to 1 
dfn_sad$timepoint <- lapply(dfn_sad$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_dep$timepoint <- lapply(dfn_dep$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_anx$timepoint <- lapply(dfn_anx$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_bias$timepoint <- lapply(dfn_bias$timepoint, function(x) ifelse(x == 99999, 1, x))
#dfn_qol$timepoint <- lapply(dfn_qol$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_acc$timepoint <- lapply(dfn_acc$timepoint, function(x) ifelse(x == 99999, 1, x))


#### select prioritised outcomes ----
po_sa_nonclin <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "SAD")
po_dep_nonclin <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "DEP")
po_anx_nonclin <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "ANX")
po_bias_nonclin <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "BIAS")
#po_qol_nonclin <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "QOL")


# join outcomes by studlab 
dfn_sad1 <- dfn_sad %>%
  left_join(po_sa_nonclin, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfn_dep1 <- dfn_dep %>%
  left_join(po_dep_nonclin, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfn_anx1 <- dfn_anx %>%
  left_join(po_anx_nonclin, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

# remove additional outcomes for Dennis (2014)
dfn_bias1 <- dfn_bias %>%
  dplyr::filter(!str_starts("Vigilance", outcome_title))

dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!str_starts("Disengagement", outcome_title))

# remove additional outcome for Vassilopolous (2014)

dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!str_starts("Benign Interpretation", outcome))

# remove additional outcome for White (2011)
dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!str_ends("RT", outcome_title))

#dfn_qol1 <- dfn_qol %>%
  #left_join(po_qol_nonclin, by = "studlab", suffix = c("", ".po")) %>%
  #dplyr::filter(outcome == outcome.po) %>%  
 # select(-outcome.po) 

#### remove arms with missing data  ----
# social anxiety
dfn_sad1 <- dfn_sad1 %>%
  dplyr::filter(!is.na(mean))

dfn_sad1 <- dfn_sad1 %>%
  dplyr::filter(!is.na(sd))

dfn_sad1 <- dfn_sad1 %>%
  dplyr::filter(!is.na(n))

# depression 
dfn_dep1 <- dfn_dep1 %>%
  dplyr::filter(!is.na(mean))

dfn_dep1 <- dfn_dep1 %>%
  dplyr::filter(!is.na(sd))

dfn_dep1 <- dfn_dep1 %>%
  dplyr::filter(!is.na(n))

# general anxiety 
dfn_anx1 <- dfn_anx1 %>%
  dplyr::filter(!is.na(mean))

dfn_anx1 <- dfn_anx1 %>%
  dplyr::filter(!is.na(sd))

dfn_anx1 <- dfn_anx1 %>%
  dplyr::filter(!is.na(n))

# change in bias 
dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!is.na(mean))

dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!is.na(sd))

dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!is.na(n))

# quality of life

#dfn_qol1 <- dfn_qol1 %>%
 # dplyr::filter(!is.na(mean))

#dfn_qol1 <- dfn_qol1 %>%
 # dplyr::filter(!is.na(sd))

#dfn_qol1 <- dfn_qol1 %>%
 # dplyr::filter(!is.na(n))

# acceptibility
dfn_acc1 <- dfn_acc %>%
  dplyr::filter(!is.na(n))

dfn_acc1 <- dfn_acc1 %>%
  dplyr::filter(!is.na(dropouts))

#### pivot data for analysis ----
# social anxiety
dfn_sad2 <- dfn_sad1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_sad3 <- dfn_sad2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# depression
dfn_dep2 <- dfn_dep1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_dep3 <- dfn_dep2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# general anxiety
dfn_anx2 <- dfn_anx1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_anx3 <- dfn_anx2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# change in bias 
dfn_bias2 <- dfn_bias1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_bias3 <- dfn_bias2 %>%
  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, int_type),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )


# quality of life
#dfn_qol2 <- dfn_qol1 %>%
 # dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
 # group_by(studlab) %>%
#  dplyr::mutate(
#    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
 #   timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
 # ) %>%
 # ungroup() %>%
#  select(-baseline_time) 

# pivot again 
#dfn_qol3 <- dfn_qol2 %>%
#  group_by(studlab, outcome, intervention, int_type, timepoint_label) %>%
 # summarise(
 #   n = mean(as.numeric(n), na.rm = TRUE),
 #   mean = mean(as.numeric(mean), na.rm = TRUE),
  #  sd = mean(as.numeric(sd), na.rm = TRUE),
  #  .groups = "drop"
#  ) %>%
#  pivot_wider(
 #   id_cols = c(studlab, outcome, intervention, int_type),
 #   names_from = timepoint_label,  
 #   values_from = c(n, mean, sd),  
 #   names_glue = "{timepoint_label}_{.value}"
#  )

# acceptibility
dfn_acc2 <- dfn_acc1 %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  group_by(studlab) %>%
  mutate(
    baseline_time = min(timepoint, na.rm = TRUE),
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")
  ) %>%
  ungroup() %>%
  select(-baseline_time)

# pivot again
dfn_acc3 <- dfn_acc2 %>%
  group_by(studlab, intervention, int_type, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    dropouts = mean(as.numeric(dropouts), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, intervention, int_type),
    names_from = timepoint_label,
    values_from = c(n, dropouts),
    names_glue = "{timepoint_label}_{.value}"
  )



#### function to rename outcome-specific cols ----
rename_with_prefix <- function(df, prefix, cols = everything()) {
  df %>%
    rename_with(~ paste0(prefix, .), .cols = {{ cols }})
}

# apply prefixing to each domain-specific dataframe
dfn_sad <- rename_with_prefix(
  dfn_sad3, "social_anx_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)

dfn_dep <- rename_with_prefix(
  dfn_dep3, "dep_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)

dfn_anx <- rename_with_prefix(
  dfn_anx3, "anx_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)

dfn_bias <- rename_with_prefix(
  dfn_bias3, "bias_", 
  cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
)


# dfn_qol <- rename_with_prefix(
  #dfn_qol3, "qol_", 
  #cols = c(baseline_mean, endpoint_mean, baseline_sd, endpoint_sd)
#)

dfn_acc <- rename_with_prefix(
  dfn_acc3, "acc_", 
  cols = c(baseline_n, endpoint_n, baseline_dropouts, endpoint_dropouts)
)

# join
df_joined <- dfn_sad %>%
  full_join(dfn_dep, by = c("studlab", "int_type")) %>%
  full_join(dfn_anx, by = c("studlab", "int_type")) %>%
  full_join(dfn_bias, by =c("studlab", "int_type")) %>%
  full_join(dfn_acc,  by = c("studlab", "int_type")) #%>%
  # full_join(dfn_qol, by =c("studlab", "int_type"))
  

dfnp_analysis <- df_joined

#### rename cols ----
names <- c("studlab",
           "sa_outcome",
           "intervention",
           "int_type",
           "sa_baseline_n",
           "sa_endpoint_n",
           "sa_baseline_mean",
           "sa_endpoint_mean",
           "sa_baseline_sd",
           "sa_endpoint_sd",
           "dep_outcome",
           "intervention.y",
           "dep_baseline_n",
           "dep_endpoint_n",
           "dep_baseline_mean",
           "dep_endpoint_mean",       
           "dep_baseline_sd",
           "dep_endpoint_sd",
           "anx_outcome",
           "intervention.z",
           "anx_baseline_n",
           "anx_endpoint_n",
           "anx_baseline_mean",
           "anx_endpoint_mean",
           "anx_baseline_sd",
           "anx_endpoint_sd",
           "bias_outcome",
           "intervention.y.y",
           "bias_baseline_n",
           "bias_endpoint_n",
           "bias_baseline_mean",
           "bias_endpoint_mean",
           "bias_baseline_sd",
           "bias_endpoint_sd",
           "intervention.y.y.y",
           "acc_baseline_n",
           "acc_endpoint_n",
           "acc_baseline_dropouts",
           "acc_endpoint_dropouts"
           )
            

colnames(dfnp_analysis) <- names

#### remove unecessary cols ----
dfnp_analysis <- dfnp_analysis %>%
  select(-intervention.y, -intervention.z, -intervention.y.y, -intervention.y.y.y)
# remove study with only one timepoint

indices_to_modify <- which(grepl("Vassilopoulos \\(2014\\)", dfnp_analysis$studlab))
dfnp_analysis <- dfnp_analysis[-indices_to_modify, ]

#### pivot wider----
dfnp_analysis1 <- dfnp_analysis %>%
  dplyr::select(studlab, sa_outcome, int_type, sa_endpoint_n, sa_endpoint_mean, sa_endpoint_sd,
         dep_outcome, dep_endpoint_n, dep_endpoint_mean, dep_endpoint_sd,
         anx_outcome, anx_endpoint_n, anx_endpoint_mean, anx_endpoint_sd,
         bias_outcome, bias_endpoint_n, bias_endpoint_mean, bias_endpoint_sd,
         acc_baseline_n, acc_endpoint_dropouts) %>%
  pivot_wider(
    names_from = int_type,
    values_from = c(sa_endpoint_n, sa_endpoint_mean, sa_endpoint_sd,
                    dep_endpoint_n, dep_endpoint_mean, dep_endpoint_sd,
                    anx_endpoint_n, anx_endpoint_mean, anx_endpoint_sd,
                    bias_endpoint_n, bias_endpoint_mean, bias_endpoint_sd,
                    acc_baseline_n, acc_endpoint_dropouts),
    names_glue = "{int_type}_{.value}"
  )

dfnp_analysis <- dfnp_analysis1

#### final check for not reported data ----
dfnp_analysis <- dfnp_analysis %>% mutate(across(everything(), ~ replace(.x, .x == 999999, NA)))
dfcp_analysis <- dfcp_analysis %>% mutate(across(everything(), ~ replace(.x, .x == 999999, NA)))
dfnp_analysis <- dfnp_analysis %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfcp_analysis <- dfcp_analysis %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))

#### add female_prop variable ----
dfc_sad2$female_prop <- (dfc_sad2$n_female / dfc_sad2$n)
dfc_dep2$female_prop <- (dfc_dep2$n_female / dfc_dep2$n)
dfc_anx2$female_prop <- (dfc_anx2$n_female / dfc_anx2$n)
dfc_bias2$female_prop <- (dfc_bias2$n_female / dfc_bias2$n)
#dfc_qol2$female_prop <- (dfc_qol2$n_female / dfc_qol2$n)

dfn_sad2$female_prop <- (dfn_sad2$n_female / dfn_sad2$n)
dfn_dep2$female_prop <- (dfn_dep2$n_female / dfn_dep2$n)
dfn_anx2$female_prop <- (dfn_anx2$n_female / dfn_anx2$n)
dfn_bias2$female_prop <- (dfn_bias2$n_female / dfn_bias2$n)
#dfn_qol2$female_prop <- (dfn_qol2$n_female / dfn_qol2$n)



# B: *NMA CLEANING* ----


# LSR6 Data Cleaning for Network Meta-Analysis


#### load data ----
# manual EPPI export with intervention edited to remove general 'Sham/control' classification
# Sham/control intervention has been modifed manually 


# clinical/non-clinical designation for each study and removing any ineligible records
nma_edits <- read_excel("data/nma_edits.xlsx")


# classification of interventions
int_class <- read_excel("data/intervention_classification.xlsx")


#### script with scales categorised by outcome
source("util/scale_categories.R")

# manually fix extraction error (will be correct in next eppi export) 
indices_to_modify <- which(grepl("IBM/CBM-I", df_raw$Intervention) & grepl("^Boettcher \\(2013\\)", df_raw$`Short Title`))
df_raw$Intervention[indices_to_modify] <- "Opposite ABM/CBM-A"

#### filter completed records ----
df1 <- df_raw %>%
  dplyr::filter(str_starts(df_raw$IsCompleted, "TRUE"))

#### remove unecessary cols ----
df2 <- df1[, -20:-31]

df3 <- df2 %>%
  select(-`Reviewer`, -ITEM_ID, -IsCompleted, -`I/E/D/S flag`, -`Outcome description`,
         -Comparison, -`Arm 2`, -`Data 2`, -`Data 4`)

#### rename cols ----
names <- c("studlab",
           "outcome_title",
           "timepoint",
           "outcome",
           "intervention",
           "arm",
           "outcome_type",
           "n",
           "mean",
           "sd")

colnames(df3) <- names

### convert se to sd  ----
indices_to_modify <- which(grepl("Khalili-Torghabeh \\(2014\\)", df3$studlab) & grepl("Target", df3$outcome))
for (i in indices_to_modify) {
  df3$sd[i] <- df3$sd[i] * sqrt(df3$n[i])
}

# create col to specify whether unit is weeks or days
df3days <- df3 %>%
  dplyr::filter(str_ends(df3$timepoint, "days"))
df3days$units <- "days"

df3weeks <- df3 %>%
  dplyr::filter(str_ends(df3$timepoint, "weeks"))
df3weeks$units <- "weeks"

df3months <- df3 %>%
  dplyr::filter(str_ends(df3$timepoint, "months"))
df3months$units <- "months"

df3hours <- df3 %>%
  dplyr::filter(str_ends(df3$timepoint, "hours"))
df3hours$units <- "hours"

#rejoin dfs
df3 <- rbind(df3hours, df3days, df3months, df3weeks) 


#### remove weeks from timepoint ----
df3$timepoint<- sub(" weeks", "", df3$timepoint)
df3$timepoint<- sub(" hours", "", df3$timepoint)
df3$timepoint<- sub(" days", "", df3$timepoint)
df3$timepoint<- sub(" months", "", df3$timepoint)
df3$timepoint <- as.numeric(df3$timepoint)

timepoint <- df3$timepoint

#### standardise timepoint notation ----
df4 <- df3 %>% mutate(across(everything(), ~ replace(.x, .x == 9999, 99999))) 

df4$timepoint <- timepoint

#### funct to extract infobox data from eppi export ----

# Study names are in column 2 starting from row 5 
study_names <- infobox_data[[2]][6:nrow(infobox_data)]

# General extraction function for all variables 
extract_arms_value <- function(text) {
  if (is.na(text) || !str_detect(text, "\\d")) return(NULL)
  arms <- str_split(text, "[-]{2,}")[[1]]
  map_df(arms, function(entry) {
    lines <- str_split(entry, "\\n")[[1]]
    lines <- str_trim(lines)
    lines <- lines[lines != ""]
    if (length(lines) < 2) return(NULL)
    arm_name <- lines[1]
    val <- as.numeric(str_extract(paste(lines[-1], collapse = " "), "\\d+\\.*\\d*"))
    if (!is.na(val)) tibble(arm = arm_name, value = val) else NULL
  })
}

# Function to extract and stack values from 6-column blocks
extract_block <- function(start_col, end_col, new_col_name) {
  block <- infobox_data[6:nrow(infobox_data), start_col:end_col]
  colnames(block) <- paste0("col_", seq_len(ncol(block)))
  block <- mutate(block, Short_Title = study_names)
  
  long_data <- block %>%
    pivot_longer(cols = starts_with("col_"), values_to = "text") %>%
    dplyr::filter(!is.na(text)) %>%
    group_by(Short_Title) %>%
    dplyr::summarise(extracted = list(map_df(text, extract_arms_value)), .groups = "drop") %>%
    unnest(extracted) %>%
    mutate(
      studlab_clean = str_squish(tolower(Short_Title)),
      arm_clean = str_squish(tolower(str_remove(arm, ":$")))
    ) %>%
    rename(!!paste0(new_col_name, "_new") := value)
  
  return(long_data)
}

# Prepare df4 if needed (ensure it has studlab and arm)
df4 <- df4 %>%
  mutate(
    studlab_clean = str_squish(tolower(studlab)),
    arm_clean = str_squish(tolower(arm))
  )

# extract vars from infobox_data


#### extract dropouts ----

dropout_long <- extract_block(10, 15, "dropouts")
df4 <- df4 %>%
  left_join(dropout_long, by = c("studlab_clean", "arm_clean"))

# clean up
df5 <- df4 %>% 
  select(studlab, outcome_title, timepoint, outcome, intervention, arm.x, outcome_type, 
         n, mean, sd, units, dropouts_new)

names(df5)[names(df5) == "arm.x"] <- "arm"
names(df5)[names(df5) == "dropouts_new"] <- "dropouts"



#### split clin/nonclin sources of evidence ----

clin_source <- nma_edits %>%
  dplyr::filter(str_starts(nma_edits$source, "C"))

nonclin_source <- nma_edits %>%
  dplyr::filter(str_starts(nma_edits$source, "NC"))

df.clin <- df5 %>%
  dplyr::filter(studlab %in% clin_source$studlab)

df.nonclin <- df5 %>%
  dplyr::filter(studlab %in% nonclin_source$studlab)


#### remove ineliegible studies (temporary) ---- 

# identify corresponding studies
studies_to_remove <- nma_edits %>%
  dplyr::filter(operation == "RM") %>%
  dplyr::pull(studlab)

# remove 
df.clin <- df.clin %>%
  dplyr::filter(!studlab %in% studies_to_remove)

df.nonclin <- df.nonclin %>%
  dplyr::filter(!studlab %in% studies_to_remove)

# correct an error with extraction
indices_to_modify <- which(grepl("Beard \\(2008\\)", df.clin$studlab) & grepl ("WSAS", df.clin$outcome))
df.clin[indices_to_modify, "outcome"] <- "WSAP: Negative endorsement"

indices_to_modify <- which(grepl("^Amir \\(2009\\)", df.clin$studlab) & (df.clin$mean == 12.4))
df.clin$timepoint[indices_to_modify] <- 4

indices_to_modify <- which(grepl("^Amir \\(2012\\)", df.clin$studlab) & (df.clin$mean == 22))
df.clin$n[indices_to_modify] <- 26

indices_to_modify <- which(grepl("Liu \\(2024\\)", df_clean$studlab))
df_clean$outcome[indices_to_modify] <- "Change in attentional bias"

# avoid join issues below




#### function to combine arms ----
combine_arms <- function(df, study, arms_to_combine, new_arm_name) {
  # select rows to combine
  df_to_combine <- df %>%
    dplyr::filter(studlab == study, arm %in% arms_to_combine)
  
  # combine pairs with complete sd and mean values
  df_combined <- df_to_combine %>%
    group_by(outcome_title, timepoint) %>%
    dplyr::filter(n() == 2, all(!is.na(sd)), all(!is.na(mean))) %>%
    arrange(arm) %>%
    summarise(
      studlab = first(studlab),
      outcome = first(outcome),
      intervention = new_arm_name,
      arm = new_arm_name,
      outcome_type = first(outcome_type),
      n1 = first(n),
      n2 = last(n),
      m1 = first(mean),
      m2 = last(mean),
      sd1 = first(sd),
      sd2 = last(sd),
      drop1 = first(dropouts),
      drop2 = last(dropouts),
      dropouts = ifelse(is.na(drop1) | is.na(drop2), NA, drop1 + drop2),
      n = n1 + n2,
      mean = (n1 * m1 + n2 * m2) / (n1 + n2),
      sd = sqrt(
        ((n1 - 1) * sd1^2 +
           (n2 - 1) * sd2^2 +
           (n1 * n2) / (n1 + n2) * (m1 - m2)^2) /
          (n1 + n2 - 1)
      ),
      .groups = "drop"
    ) %>%
    select(-n1, -n2, -m1, -m2, -sd1, -sd2, -drop1, -drop2)
  
  # keep unmatched rows with only one arm present
  df_unmatched <- df_to_combine %>%
    group_by(outcome_title, timepoint) %>%
    dplyr::filter(n() < 2) %>%
    ungroup()
  
  # return updated dataframe
  df %>%
    dplyr::filter(!(studlab == study & arm %in% arms_to_combine)) %>%
    bind_rows(df_combined, df_unmatched)
}


# CLINICAL SOURCE OF EVIDENCE----

#### combine Mobini (2014) ----
df_combined <- combine_arms(
  df = df.clin,
  study = "Mobini (2014)",
  arms_to_combine = c("Standard CBM-I", "Explicit CBM-I"),
  new_arm_name = "CBM-I"
)

#### combine Carleton (2015) ----
df_combined1 <- combine_arms(
  df = df_combined,
  study = "Carleton (2015)",
  arms_to_combine = c("AMC-Lab", "AMC-Remote"),
  new_arm_name = "CBM-A"
)

df_combined1 <- combine_arms(
  df = df_combined1,
  study = "Carleton (2015)",
  arms_to_combine = c("ACC-Lab", "ACC-Remote"),
  new_arm_name = "Control CBM-A"
)
#### combine Liang (2016) ----
df_combined2 <- combine_arms(
  df = df_combined1,
  study = "Liang (2016)",
  arms_to_combine = c("ABM-100", "ABM-500"),
  new_arm_name = "CBM-A"
)

df_combined3 <- combine_arms(
  df = df_combined2,
  study = "Liang (2016)",
  arms_to_combine = c("AP-100", "AP-500"),
  new_arm_name = "Control CBM-A"
)

####combine Ma (2020) ----
df_combined4 <- combine_arms(
  df = df_combined3,
  study = "Ma (2020)",
  arms_to_combine = c("2D Disgust", "3D Disgust"),
  new_arm_name = "Opposite CBM-A"
)

df_combined5 <- combine_arms(
  df = df_combined4,
  study = "Ma (2020)",
  arms_to_combine = c("2D Neutral", "3D Neutral"),
  new_arm_name = "CBM-A"
)

#### combine Murphy (2007) ----
df_combined5 <- combine_arms(
  df = df_combined5,
  study = "Murphy (2007)",
  arms_to_combine = c("Non-Negative Interpretation Training", "Positive Interpretation Training"),
  new_arm_name = "CBM-I"
)

#### combine Yao (2015) ----
df_combined5 <- combine_arms(
  df = df_combined5,
  study = "Yao (2015)",
  arms_to_combine = c("AGC", "ACC"),
  new_arm_name = "Control CBM-A"
)



# change sham/control to arm name

#### classify interventions ----
# uses the categories decided in the adjudication meeting 17/04/25 
# clin source 
df_combined6 <- df_combined5 %>%
  left_join(int_class, by = c("intervention" = "Intervention")) %>%
  rename(node = Node)

#### remove Heeren (2011) ineligible arms - see manuscript for more details ----
indices_to_modify <- which(grepl("Heeren \\(2011\\)", df_combined6$studlab) & grepl("ABM disengage", df_combined6$arm))
df_combined6 <- df_combined6[-indices_to_modify, ]

indices_to_modify <- which(grepl("Heeren \\(2011\\)", df_combined6$studlab) & grepl("^ABM re-engage$", df_combined6$arm))
df_combined6 <- df_combined6[-indices_to_modify, ]

indices_to_modify <- which(grepl("Heeren \\(2011\\)", df_combined6$studlab) & grepl("^Control$", df_combined6$arm))
df_combined6 <- df_combined6[-indices_to_modify, ]

indices_to_modify <- which(grepl("Ollendick \\(2019\\)", df_clean$studlab) & is.na(df_clean$outcome))
df_clean$outcome[indices_to_modify] <- "SCARED Social Anxiety Subscale"

indices_to_modify <- which(grepl("Liu \\(2024\\)", df_clean$studlab))
df_clean$outcome[indices_to_modify] <- "Change in attentional bias"

indices_to_modify <- which(grepl("Klumpp \\(2010\\)", df_clean$studlab))
df_clean$intervention[indices_to_modify] <- "Opposite CBM-A"

# avoid join issues below 
indices_to_modify <- which(grepl("Beard \\(2008\\)", df_clean$studlab) & grepl("SPAI - social phobia subscale", df_clean$outcome))
df_clean$outcome[indices_to_modify] <- "SPAI"

indices_to_modify <- which((df.clin$sd == 11.17) & grepl("ACC-Lab", df.clin$arm))
df.clin$n[indices_to_modify] <- 20



# CLIN SOURCE ONLY 
#### create analysis dfs ----


# social anxiety df 
dfc_sad <- df_combined6 %>%
  dplyr::filter(outcome %in% social_anx) 
# depression df 
dfc_dep <- df_combined6 %>%
  dplyr::filter(outcome %in% dep)
# general anx df
dfc_anx <- df_combined6 %>%
  dplyr::filter(outcome %in% general_anx)
# quality of life df 
dfc_qol <- df_combined6 %>%
  dplyr::filter(outcome %in% qol)
#bias change
dfc_bias <- df_combined6 %>%
  dplyr::filter(outcome %in% bias_change)
# acceptibility
dfc_acc <- df_combined6

# save timepoint col 
sa_timepoint <- dfc_sad$timepoint
dep_timepoint <- dfc_dep$timepoint
bias_timepoint <- dfc_bias$timepoint
anx_timepoint <- dfc_anx$timepoint
qol_timepoint <- dfc_qol$timepoint
bias_timepoint <- dfc_bias$timepoint
acc_timepoint <- dfc_acc$timepoint

#  change missing data to NA 
dfc_sad <- dfc_sad %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_dep <- dfc_dep %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_anx <- dfc_anx %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_bias <- dfc_bias %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_qol <- dfc_qol%>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfc_acc <- dfc_acc %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))



# replace timepoint col 
dfc_sad$timepoint <- sa_timepoint
dfc_dep$timepoint <- dep_timepoint
dfc_anx$timepoint <- anx_timepoint
dfc_bias$timepoint <- bias_timepoint
dfc_qol$timepoint <- qol_timepoint
dfc_acc$timepoint <- acc_timepoint


# change single session study post-intervention timepoints to 1 
dfc_sad$timepoint <- lapply(dfc_sad$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_dep$timepoint <- lapply(dfc_dep$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_anx$timepoint <- lapply(dfc_anx$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_bias$timepoint <- lapply(dfc_bias$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_qol$timepoint <- lapply(dfc_qol$timepoint, function(x) ifelse(x == 99999, 1, x))
dfc_acc$timepoint <- lapply(dfc_acc$timepoint, function(x) ifelse(x == 99999, 1, x))


#### prioritise outcomes ----
# load outcomes prioritised by study (social_anx)
po <- read_excel("data/prioritised_outcomes.xlsx", sheet = "SAD")
po_dep <- read_excel("data/prioritised_outcomes.xlsx", sheet = "DEP")
po_anx <- read_excel("data/prioritised_outcomes.xlsx", sheet = "ANX")
po_bias <- read_excel("data/prioritised_outcomes.xlsx", sheet = "BIAS")
po_qol <- read_excel("data/prioritised_outcomes.xlsx", sheet = "QOL")

# funct to stardardise studlabs
standardise_studlab <- function(x) {
  x %>%
    stringr::str_squish() %>%               # remove extra internal whitespace
    iconv(to = "ASCII//TRANSLIT") %>%       # normalize encoding
    trimws()                                # remove leading/trailing whitespace
}
dfc_qol <- dfc_qol %>% mutate(studlab = standardise_studlab(studlab))
po_qol <- po_qol %>% mutate(studlab = standardise_studlab(studlab))


# join outcomes by studlab 
dfc_sad1 <- dfc_sad %>%
  left_join(po, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfc_dep1 <- dfc_dep %>%
  left_join(po_dep, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfc_anx1 <- dfc_anx %>%
  left_join(po_anx, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfc_bias1 <- dfc_bias %>%
  left_join(po_bias, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfc_qol1 <- dfc_qol %>%
  left_join(po_qol, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(!is.na(outcome.po)) %>%  
  select(-outcome.po) 


### remove arms with missing data  ----
# social anxiety
dfc_sad1 <- dfc_sad1 %>%
  dplyr::filter(!is.na(mean))

dfc_sad1 <- dfc_sad1 %>%
  dplyr::filter(!is.na(sd))

dfc_sad1 <- dfc_sad1 %>%
  dplyr::filter(!is.na(n))

# depression 
dfc_dep1 <- dfc_dep1 %>%
  dplyr::filter(!is.na(mean))

dfc_dep1 <- dfc_dep1 %>%
  dplyr::filter(!is.na(sd))

dfc_dep1 <- dfc_dep1 %>%
  dplyr::filter(!is.na(n))

# general anxiety 
dfc_anx1 <- dfc_anx1 %>%
  dplyr::filter(!is.na(mean))

dfc_anx1 <- dfc_anx1 %>%
  dplyr::filter(!is.na(sd))

dfc_anx1 <- dfc_anx1 %>%
  dplyr::filter(!is.na(n))

# change in bias 
dfc_bias1 <- dfc_bias1 %>%
  dplyr::filter(!is.na(mean))

dfc_bias1 <- dfc_bias1 %>%
  dplyr::filter(!is.na(sd))

dfc_bias1 <- dfc_bias1 %>%
  dplyr::filter(!is.na(n))

# quality of life

dfc_qol1 <- dfc_qol %>%
  dplyr::filter(!is.na(mean))

dfc_qol1 <- dfc_qol %>%
  dplyr::filter(!is.na(sd))

dfc_qol1 <- dfc_qol1 %>%
  dplyr::filter(!is.na(n))

# acceptability 
dfc_acc1 <- dfc_acc %>%
  dplyr::filter(!is.na(n))

dfc_acc1 <- dfc_acc1 %>%
  dplyr::filter(!is.na(dropouts))

# correction of manual extraction errors - will be correct in the next eppi export 
# without this Beard (2008) will be dropped in the pivot

indices_to_modify <- which(grepl("^Beard \\(2008\\)", dfc_bias1$studlab) & grepl("Control CBM-I", dfc_bias1$intervention))
dfc_bias1$arm[indices_to_modify] <- "Interpretation Control Condition (ICC)"

indices_to_modify <- which(grepl("^Beard \\(2008\\)", dfc_bias1$studlab) & grepl("IMP", dfc_bias1$intervention))
dfc_bias1$arm[indices_to_modify] <- "Interpretation Modification Program (IMP)"


#### pivot data ----
# social anxiety
dfc_sad3<- dfc_sad1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_sad4 <- dfc_sad3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# depression
dfc_dep3 <- dfc_dep1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_dep4 <- dfc_dep3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )


 # general anxiety
dfc_anx3<- dfc_anx1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_anx4 <- dfc_anx3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# change in bias

dfc_bias3 <- dfc_bias1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_bias4 <- dfc_bias3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# quality of life 
dfc_qol3 <- dfc_qol1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfc_qol4 <- dfc_qol3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# acceptibility

dfc_acc3 <- dfc_acc1 %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  group_by(studlab) %>%
  mutate(
    baseline_time = min(timepoint, na.rm = TRUE),
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")
  ) %>%
  ungroup() %>%
  select(-baseline_time)

# pivot again
dfc_acc4 <- dfc_acc3 %>%
  group_by(studlab, intervention, node, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    dropouts = mean(as.numeric(dropouts), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, intervention, node),
    names_from = timepoint_label,
    values_from = c(n, dropouts),
    names_glue = "{timepoint_label}_{.value}"
  )

#### funct to invert scores ----
invert_scores <- function(df, studlab_value, columns_to_invert) {
  # Check if studlab column exists
  if (!"studlab" %in% names(df)) {
    stop("The data frame does not contain a 'studlab' column.")
  }
  
  # Check columns exist
  if (!all(columns_to_invert %in% names(df))) {
    missing_cols <- setdiff(columns_to_invert, names(df))
    stop(paste("These columns are not in the data frame:", paste(missing_cols, collapse = ", ")))
  }
  
  # Perform the inversion
  df[df$studlab == studlab_value, columns_to_invert] <- 
    -1 * df[df$studlab == studlab_value, columns_to_invert]
  
  return(df)
}

### invert qol scores ----
# invert qol scores where higher = better
dfc_qol4 <- invert_scores(dfc_qol4, studlab_value = "Amir (2009)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol4 <- invert_scores(dfc_qol4, studlab_value = "Amir (2012)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol4 <- invert_scores(dfc_qol4, studlab_value = "Boettcher (2013)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol4 <- invert_scores(dfc_qol4, studlab_value = "Bomyea (2023)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol4 <- invert_scores(dfc_qol4, studlab_value = "Carlbring (2012)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol4 <- invert_scores(dfc_qol4, studlab_value = "Naim (2018)", columns_to_invert = c("baseline_mean", "endpoint_mean"))
dfc_qol4 <- invert_scores(dfc_qol4, studlab_value = "Orchard (2017)", columns_to_invert = c("baseline_mean", "endpoint_mean"))

### invert bias scores ----
# reverse bias scores for outcomes where an increase in scores is good
po_bias <- po_bias %>%
  mutate(invert = if_else(is.na(invert_bias_score), FALSE, invert_bias_score))

dfc_bias4 <- dfc_bias4 %>%
  left_join(po_bias %>% select(studlab, invert), by = "studlab") %>%
  mutate(baseline_mean = if_else(invert, -baseline_mean, baseline_mean)) %>%
  select(-invert)

dfc_bias4 <- dfc_bias4 %>%
  left_join(po_bias %>% select(studlab, invert), by = "studlab") %>%
  mutate(endpoint_mean = if_else(invert, -endpoint_mean, endpoint_mean)) %>%
  select(-invert)


#### rename dfs ----
dfc_sad <- dfc_sad4
dfc_dep <- dfc_dep4
dfc_anx <- dfc_anx4
dfc_bias <- dfc_bias4
dfc_qol <- dfc_qol4
dfc_acc <- dfc_acc4

# add additional data (will be in subsequent EPPI export)
# social anxiety
new_row1 <- tibble(
  studlab = "Lam (2025)",
  outcome = "SPS",      
  intervention = "CBM-I",  
  arm = "Interpretation Modification Program",           
  node =  "CBM-I",        
  baseline_n = 36,   
  endpoint_n = 28,    
  baseline_mean = 41.81, 
  endpoint_mean = 34.36, 
  baseline_sd = 13.31,   
  endpoint_sd = 10.72
)

new_row2 <- tibble(
  studlab = "Lam (2025)",
  outcome = "SPS",      
  intervention = "Control CBM-I",  
  arm = "Control CBM-I",           
  node =  "Control CBM-I",        
  baseline_n = 36,   
  endpoint_n = 30,    
  baseline_mean = 43.75, 
  endpoint_mean = 38.70, 
  baseline_sd = 12.21,   
  endpoint_sd = 11.81
)

# add rows to existing dfc_sad
dfc_sad <- bind_rows(dfc_sad, new_row1)
dfc_sad <- bind_rows(dfc_sad, new_row2)

# change in bias
new_row3 <- tibble(
  studlab = "Lam (2025)",
  outcome = "SRT",      
  intervention = "CBM-I",  
  arm = "Interpretation Modification Program",           
  node =  "CBM-I",        
  baseline_n = 36,   
  endpoint_n = 28,    
  baseline_mean = 0.26, 
  endpoint_mean = -0.390, 
  baseline_sd = 0.440,   
  endpoint_sd = 0.780
)

new_row4 <- tibble(
  studlab = "Lam (2025)",
  outcome = "SRT",      
  intervention = "Control CBM-I",  
  arm = "Control CBM-I",           
  node =  "Control CBM-I",        
  baseline_n = 36,   
  endpoint_n = 30,    
  baseline_mean = 0.380, 
  endpoint_mean = 0.360, 
  baseline_sd = 0.680,   
  endpoint_sd = 0.580
)

# add rows to existing dfc_bias
dfc_bias <- bind_rows(dfc_bias, new_row3)
dfc_bias <- bind_rows(dfc_bias, new_row4)


#### label subgroups ----
# pull bias group studlabs 
attention <- nma_edits %>%
  dplyr::filter(tgt_bias == "attentional") %>%
  dplyr::pull(studlab)

interpretation <- nma_edits %>%
  dplyr::filter(tgt_bias == "interpretation") %>%
  dplyr::pull(studlab)

approach <- nma_edits %>%
  dplyr::filter(tgt_bias == "approach_bias") %>%
  dplyr::pull(studlab)

memory <- nma_edits %>%
  dplyr::filter(tgt_bias == "memory") %>%
  dplyr::pull(studlab)

multiple <- nma_edits %>%
  dplyr::filter(tgt_bias == "multiple") %>%
  dplyr::pull(studlab)

# assign to studlabs 
dfc_sad$target_bias <- ifelse(dfc_sad$studlab %in% attention, "attention",
                              ifelse(dfc_sad$studlab %in% interpretation, "interpretation",
                                ifelse(dfc_sad$studlab %in% approach, "approach",
                                  ifelse(dfc_sad$studlab %in% memory, "memory",
                                   ifelse(dfc_sad$studlab %in% multiple, "combination", NA)))))

dfc_dep$target_bias <- ifelse(dfc_dep$studlab %in% attention, "attention",
                              ifelse(dfc_dep$studlab %in% interpretation, "interpretation",
                                     ifelse(dfc_dep$studlab %in% approach, "approach",
                                            ifelse(dfc_dep$studlab %in% memory, "memory",
                                                   ifelse(dfc_dep$studlab %in% multiple, "combination", NA)))))


dfc_anx$target_bias <- ifelse(dfc_anx$studlab %in% attention, "attention",
                              ifelse(dfc_anx$studlab %in% interpretation, "interpretation",
                                     ifelse(dfc_anx$studlab %in% approach, "approach",
                                            ifelse(dfc_anx$studlab %in% memory, "memory",
                                                   ifelse(dfc_anx$studlab %in% multiple, "combination", NA)))))

dfc_bias$target_bias <- ifelse(dfc_bias$studlab %in% attention, "attention",
                              ifelse(dfc_bias$studlab %in% interpretation, "interpretation",
                                     ifelse(dfc_bias$studlab %in% approach, "approach",
                                            ifelse(dfc_bias$studlab %in% memory, "memory",
                                                   ifelse(dfc_bias$studlab %in% multiple, "combination", NA)))))

dfc_qol$target_bias <- ifelse(dfc_qol$studlab %in% attention, "attention",
                               ifelse(dfc_qol$studlab %in% interpretation, "interpretation",
                                      ifelse(dfc_qol$studlab %in% approach, "approach",
                                             ifelse(dfc_qol$studlab %in% memory, "memory",
                                                    ifelse(dfc_qol$studlab %in% multiple, "combination", NA)))))

dfc_acc$target_bias <- ifelse(dfc_acc$studlab %in% attention, "attention",
                              ifelse(dfc_acc$studlab %in% interpretation, "interpretation",
                                     ifelse(dfc_acc$studlab %in% approach, "approach",
                                            ifelse(dfc_acc$studlab %in% memory, "memory",
                                                   ifelse(dfc_acc$studlab %in% multiple, "combination", NA)))))


# number of sessions
single <- nma_edits %>%
  dplyr::filter(session == "single") %>%
  dplyr::pull(studlab)

multi <- nma_edits %>%
  dplyr::filter(session == "multi") %>%
  dplyr::pull(studlab)

dfc_sad$session <- ifelse(dfc_sad$studlab %in% single, "single",
                               ifelse(dfc_sad$studlab %in% multi, "multi", NA))

dfc_dep$session <- ifelse(dfc_dep$studlab %in% single, "single",
                          ifelse(dfc_dep$studlab %in% multi, "multi", NA))

dfc_anx$session <- ifelse(dfc_anx$studlab %in% single, "single",
                          ifelse(dfc_anx$studlab %in% multi, "multi", NA))

dfc_bias$session <- ifelse(dfc_bias$studlab %in% single, "single",
                          ifelse(dfc_bias$studlab %in% multi, "multi", NA))

dfc_qol$session <- ifelse(dfc_qol$studlab %in% single, "single",
                          ifelse(dfc_qol$studlab %in% multi, "multi", NA))

dfc_acc$session <- ifelse(dfc_acc$studlab %in% single, "single",
                          ifelse(dfc_acc$studlab %in% multi, "multi", NA))

# mode of delivery 
computer <- nma_edits %>%
  dplyr::filter(mode_of_delivery == "Computer") %>%
  dplyr::pull(studlab)

smartphone <- nma_edits %>%
  dplyr::filter(mode_of_delivery == "Smartphone") %>%
  dplyr::pull(studlab)

vr <- nma_edits %>%
  dplyr::filter(mode_of_delivery == "Virtual reality headset") %>%
  dplyr::pull(studlab)

read_aloud <- nma_edits %>%
  dplyr::filter(mode_of_delivery == "Situations read aloud") %>%
  dplyr::pull(studlab)

any <- nma_edits %>%
  dplyr::filter(mode_of_delivery == "Any device with internet access") %>%
  dplyr::pull(studlab)


dfc_sad$mode_of_delivery <- ifelse(dfc_sad$studlab %in% computer, "computer",
                                ifelse(dfc_sad$studlab %in% smartphone, "smartphone",
                                     ifelse(dfc_sad$studlab %in% vr, "virtual reality headset",
                                            ifelse(dfc_sad$studlab %in% read_aloud, "situations read aloud",
                                                   ifelse(dfc_sad$studlab %in% any, "any device with internet access", NA)))))

dfc_dep$mode_of_delivery <- ifelse(dfc_dep$studlab %in% computer, "computer",
                                   ifelse(dfc_dep$studlab %in% smartphone, "smartphone",
                                          ifelse(dfc_dep$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfc_dep$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfc_dep$studlab %in% any, "any device with internet access", NA)))))

dfc_bias$mode_of_delivery <- ifelse(dfc_bias$studlab %in% computer, "computer",
                                   ifelse(dfc_bias$studlab %in% smartphone, "smartphone",
                                          ifelse(dfc_bias$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfc_bias$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfc_bias$studlab %in% any, "any device with internet access", NA)))))

dfc_anx$mode_of_delivery <- ifelse(dfc_anx$studlab %in% computer, "computer",
                                   ifelse(dfc_anx$studlab %in% smartphone, "smartphone",
                                          ifelse(dfc_anx$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfc_anx$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfc_anx$studlab %in% any, "any device with internet access", NA)))))

dfc_qol$mode_of_delivery <- ifelse(dfc_qol$studlab %in% computer, "computer",
                                   ifelse(dfc_qol$studlab %in% smartphone, "smartphone",
                                          ifelse(dfc_qol$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfc_qol$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfc_qol$studlab %in% any, "any device with internet access", NA)))))

dfc_acc$mode_of_delivery <- ifelse(dfc_acc$studlab %in% computer, "computer",
                                   ifelse(dfc_acc$studlab %in% smartphone, "smartphone",
                                          ifelse(dfc_acc$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfc_acc$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfc_acc$studlab %in% any, "any device with internet access", NA)))))

# allegiance bias
present <- nma_edits %>%
  dplyr::filter(allegiance_bias == "present") %>%
  dplyr::pull(studlab)

none <- nma_edits %>%
  dplyr::filter(allegiance_bias == "none") %>%
  dplyr::pull(studlab)

dfc_sad$allegiance_bias <- ifelse(dfc_sad$studlab %in% present, "present",
                            ifelse(dfc_sad$studlab %in% none, "none", NA))

dfc_dep$allegiance_bias <- ifelse(dfc_dep$studlab %in% present, "present",
                                  ifelse(dfc_dep$studlab %in% none, "none", NA))

dfc_anx$allegiance_bias <- ifelse(dfc_anx$studlab %in% present, "present",
                                  ifelse(dfc_anx$studlab %in% none, "none", NA))

dfc_bias$allegiance_bias <- ifelse(dfc_bias$studlab %in% present, "present",
                                  ifelse(dfc_bias$studlab %in% none, "none", NA))

dfc_qol$allegiance_bias <- ifelse(dfc_qol$studlab %in% present, "present",
                                  ifelse(dfc_qol$studlab %in% none, "none", NA))

dfc_acc$allegiance_bias <- ifelse(dfc_acc$studlab %in% present, "present",
                                  ifelse(dfc_acc$studlab %in% none, "none", NA))



# correct node for Naim (2018)
indices_to_modify <- which(grepl("^Naim \\(2018\\)", dfc_qol$studlab) & is.na(dfc_qol$node))
print(indices_to_modify)
dfc_qol$node[indices_to_modify] <- "Combined control"

# add target bias to Naim (2020)
indices_to_modify <- which(grepl("^Naim \\(2018\\)", dfc_qol$studlab) & grepl("CBM-A", dfc_qol$node))
dfc_qol$target_bias[indices_to_modify] <- "attention"

indices_to_modify <- which(grepl("^Naim \\(2018\\)", dfc_qol$studlab) & grepl("Combined control", dfc_qol$node))
dfc_qol$target_bias[indices_to_modify] <- "multiple"

indices_to_modify <- which(grepl("^Naim \\(2018\\)", dfc_qol$studlab) & grepl("Multicomponent", dfc_qol$node))
dfc_qol$target_bias[indices_to_modify] <- "multiple"

indices_to_modify <- which(grepl("^Naim \\(2018\\)", dfc_qol$studlab) & grepl("CBM-I", dfc_qol$node))
dfc_qol$target_bias[indices_to_modify] <- "interpretation"

### add RoB2 scores ----

# normalise to avoid join issues
normalize_studlab <- function(x) {
  x %>%
    str_to_lower() %>%          # lowercase
    str_squish() %>%            # remove excess internal whitespace
    str_replace_all("[^[:alnum:]()\\s]", "")  # remove special chars except () and whitespace
}

# Apply normalization to both sources
dfc_sad <- dfc_sad %>%
  mutate(studlab_clean = normalize_studlab(studlab))

rob_sad <- rob_sad %>%
  mutate(studlab_clean = normalize_studlab(studlab)) %>%
  select(studlab_clean, score)

# join by normalised studlab
dfc_sad <- dfc_sad %>%
  left_join(rob_sad, by = "studlab_clean") %>%
  rename(rob = score) %>%
  select(-studlab_clean)

### add diagnosis details ----
dfc_sad <- dfc_sad %>%
  mutate(studlab_clean = normalize_studlab(studlab))

diag_sad <- diag_sad %>%
  mutate(studlab_clean = normalize_studlab(studlab)) %>%
  select(studlab_clean, diagnosis)

dfc_sad <- dfc_sad %>%
  left_join(diag_sad, by = "studlab_clean") %>%
  rename(diagnosis = diagnosis) %>%
  select(-studlab_clean)

### add covariate data ----

# load and select relevant cols 
covar_data <- read_excel("data/table_of_characteristics.xlsx")
covar_data <- covar_data[, 1:6]

names <- c("studlab", "country", "node", "baseline_n", "n_female", "mean_age")
colnames(covar_data) <- names

covar_data <- covar_data %>%
  select(studlab, node, baseline_n, n_female, mean_age)

# harmonise value formats 
covar_data <- covar_data %>% mutate(across(everything(), ~ replace(.x, .x == "NR", NA)))
covar_data <- covar_data %>% mutate(across(everything(), ~ replace(.x, .x == "CBM-A + CBM-I", "Multicomponent")))
covar_data <- covar_data %>% mutate(across(everything(), ~ replace(.x, .x == "Combined Control", "Combined control")))

# harmonise covariate df treatments with final nodes 
covar_data[129, 2] <- "Control CBM-A"

covar_data[117, 2] <- "Control CBM-I"

covar_data[41, 2] <- "TAU"

covar_data[177, 2] <- "CBM-I"

covar_data[147, 2] <- "Opposite CBM-I"

covar_data[101, 2] <- "Opposite CBM-A"

# ensure cols in correct format 
covar_data$baseline_n <- as.numeric(covar_data$baseline_n)
covar_data$n_female <- as.numeric(covar_data$n_female)
covar_data$mean_age <- as.numeric(covar_data$mean_age)

### remove rows to avoid left join issues ----
indices_to_modify <- which(grepl("^Carleton \\(2015\\)", covar_data$studlab))
covar_data <- covar_data[-indices_to_modify, ]


### function to combine covar rows (so dfs match) ----

combine_arms_in_study <- function(
    df,
    study,                      
    arms,                       
    new_name,                   
    studlab_col = studlab,
    node_col    = node,
    count_cols  = c("baseline_n", "n_female"),
    mean_cols   = c("mean_age"),
    weight_col  = "baseline_n"
) {
  stopifnot(requireNamespace("dplyr", quietly = TRUE),
            requireNamespace("rlang", quietly = TRUE),
            requireNamespace("tibble", quietly = TRUE))
  
  studlab_col <- rlang::enquo(studlab_col)
  node_col    <- rlang::enquo(node_col)
  
  # check 
  req_cols <- unique(c(rlang::as_name(studlab_col), rlang::as_name(node_col),
                       count_cols, mean_cols, weight_col))
  miss <- setdiff(req_cols, names(df))
  if (length(miss)) stop("Missing required column(s): ", paste(miss, collapse = ", "))
  
  # subset to the target study + target arms
  in_study <- df %>%
    dplyr::filter(!!studlab_col == study)
  
  to_combine <- in_study %>%
    dplyr::filter(!!node_col %in% arms)
  
  if (nrow(to_combine) < 2L) {
    stop("Within study '", study, "' I found ", nrow(to_combine),
         " matching arm(s). Need ≥2 to combine. Arms sought: ",
         paste(arms, collapse = ", "))
  }
  
  # coerce declared numeric columns if they arrived as character
  to_num <- function(x) suppressWarnings(as.numeric(x))
  for (cc in intersect(count_cols, names(to_combine))) to_combine[[cc]] <- to_num(to_combine[[cc]])
  for (mc in intersect(mean_cols,  names(to_combine))) to_combine[[mc]] <- to_num(to_combine[[mc]])
  if (!is.numeric(to_combine[[weight_col]])) to_combine[[weight_col]] <- to_num(to_combine[[weight_col]])
  
  # columns not explicitly aggregated (we keep identical values; else NA)
  protected <- unique(c(rlang::as_name(studlab_col), rlang::as_name(node_col),
                        count_cols, mean_cols, weight_col))
  other_cols <- setdiff(names(df), protected)
  
  # build the combined row
  combined_row <- to_combine %>%
    dplyr::summarise(
      "{rlang::as_name(studlab_col)}" := study,
      "{rlang::as_name(node_col)}"    := new_name,
      # --- compute weighted means FIRST (weight column still row-wise here)
      dplyr::across(
        dplyr::all_of(mean_cols),
        ~ {
          w <- .data[[weight_col]]
          if (all(is.na(.x)) || all(is.na(w))) NA_real_
          else stats::weighted.mean(.x, w = w, na.rm = TRUE)
        }
      ),
      # collapse 
      dplyr::across(dplyr::all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
      
      dplyr::across(
        dplyr::all_of(other_cols),
        ~ {
          vals <- .x[!is.na(.x)]
          if (!length(vals)) return(NA)
          u <- unique(vals)
          if (length(u) == 1L) u[[1]] else NA
        }
      ),
      .groups = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(names(df))) %>%
    tibble::as_tibble()
  
  # remove original arm
  out <- df %>%
    dplyr::filter(!( (!!studlab_col == study) & (!!node_col %in% arms) )) %>%
    dplyr::bind_rows(combined_row) %>%
    dplyr::arrange(!!studlab_col, !!node_col)
  
  tibble::as_tibble(out)
}

### combine arms to ensure dfs match ----

covar_data <- combine_arms_in_study(
  covar_data,
  study = "Mobini (2014)",
  arms  = c("CBM-I", "CBM-I"),
  new_name = "CBM-I",
  studlab_col = studlab,
  node_col    = node,
  count_cols  = c("baseline_n", "n_female"),
  mean_cols   = c("mean_age"),
  weight_col  = "baseline_n"
)

covar_data <- combine_arms_in_study(
  covar_data,
  study = "Ma (2020)",
  arms  = c("Control CBM-A", "Control CBM-A"),
  new_name = "Control CBM-A",
  studlab_col = studlab,
  node_col    = node,
  count_cols  = c("baseline_n", "n_female"),
  mean_cols   = c("mean_age"),
  weight_col  = "baseline_n"
)

covar_data <- combine_arms_in_study(
  covar_data,
  study = "Ma (2020)",
  arms  = c("CBM-A", "CBM-A"),
  new_name = "CBM-A",
  studlab_col = studlab,
  node_col    = node,
  count_cols  = c("baseline_n", "n_female"),
  mean_cols   = c("mean_age"),
  weight_col  = "baseline_n"
)

covar_data <- combine_arms_in_study(
  covar_data,
  study = "Liang (2016)",
  arms  = c("CBM-A", "CBM-A"),
  new_name = "CBM-A",
  studlab_col = studlab,
  node_col    = node,
  count_cols  = c("baseline_n", "n_female"),
  mean_cols   = c("mean_age"),
  weight_col  = "baseline_n"
)

covar_data <- combine_arms_in_study(
  covar_data,
  study = "Liang (2016)",
  arms  = c("Control CBM-A", "Control CBM-A"),
  new_name = "Control CBM-A",
  studlab_col = studlab,
  node_col    = node,
  count_cols  = c("baseline_n", "n_female"),
  mean_cols   = c("mean_age"),
  weight_col  = "baseline_n"
)

### join covar data ----
dfc_sad <- dfc_sad %>%
  left_join(covar_data, by = c("studlab", "node"))

### calculate proportion of females ----

dfc_sad$female_prop <- (dfc_sad$n_female / dfc_sad$endpoint_n)


# NONCLINICAL SOURCE OF EVIDENCE----

# manually fix extraction error
indices_to_modify <- which(grepl("Benign - Spoken", df.nonclin$arm) & (df.nonclin$mean == 2.85))


df.nonclin$timepoint[indices_to_modify] <- 1

#### combine Vassilopoulos (2014) ----
df.nonclin <- combine_arms(
  df = df.nonclin,
  study = "Vassilopoulos (2014)",
  arms_to_combine = c("Benign - Spoken", "Benign - Written"),
  new_arm_name = "CBM-I"
)

df.nonclin <- combine_arms(
  df = df.nonclin,
  study = "Vassilopoulos (2014)",
  arms_to_combine = c("Negative - Spoken", "Negative - Written"),
  new_arm_name = "Opposite CBM-I"
)

#### combine Julian (2012) ----
df.nonclin <- combine_arms(
  df = df.nonclin,
  study = "Julian (2012)",
  arms_to_combine = c("Exercise + Attention Modification (EX + AMP)", "Rest + Attention Modification (REST + AMP)"),
  new_arm_name = "CBM-A"
)

df.nonclin <- combine_arms(
  df = df.nonclin,
  study = "Julian (2012)",
  arms_to_combine = c("Rest + Attention Control (REST + ACC)", "Exercise + Attention Control (EX + ACC)"),
  new_arm_name = "Control CBM-A"
)

#### combine Dennis (2014) ----
df.nonclin <- combine_arms(
  df = df.nonclin,
  study = "Dennis (2014)",
  arms_to_combine = c("Short Placebo Training", "Long Placebo Training"),
  new_arm_name = "Control CBM-A"
)

df.nonclin <- combine_arms(
  df = df.nonclin,
  study = "Dennis (2014)",
  arms_to_combine = c("Long Attention Bias Modification (ABM)", "Short Attention Bias Modification (ABM)"),
  new_arm_name = "CBM-A"
)

#### classify interventions ----
# uses the categories decided in the adjudication meeting 17/04/25 

# nonclin source 
df.nonclin <- df.nonclin %>%
  left_join(int_class, by = c("intervention" = "Intervention")) %>%
  rename(node = Node)

#### remove Ferrari (2018) ineligible arms - see manuscript for more details ----
indices_to_modify <- which(grepl("Ferrari \\(2018\\)", df.nonclin$studlab) & grepl("^Avoid-Negative \\(AvN\\) Training$", df.nonclin$arm))
df.nonclin <- df.nonclin[-indices_to_modify, ]

indices_to_modify <- which(grepl("Ferrari \\(2018\\)", df.nonclin$studlab) & grepl("^Approach-Positive \\(ApP\\) Training$", df.nonclin$arm))
df.nonclin <- df.nonclin[-indices_to_modify, ]


#### create analysis dfs ----


# social anxiety df 
dfn_sad <- df.nonclin %>%
  dplyr::filter(outcome %in% social_anx) 
# depression df 
dfn_dep <- df.nonclin %>%
  dplyr::filter(outcome %in% dep)
# general anx df
dfn_anx <- df.nonclin %>%
  dplyr::filter(outcome %in% general_anx)
# quality of life df 
dfn_qol <- df.nonclin %>%
  dplyr::filter(outcome %in% qol)
#bias change
dfn_bias <- df.nonclin %>%
  dplyr::filter(outcome %in% bias_change)
# acceptibility 
dfn_acc <- df.nonclin



# save timepoint col 
san_timepoint <- dfn_sad$timepoint
depn_timepoint <- dfn_dep$timepoint
biasn_timepoint <- dfn_bias$timepoint
anxn_timepoint <- dfn_anx$timepoint
qoln_timepoint <- dfn_qol$timepoint
biasn_timepoint <- dfn_bias $timepoint
nacc_timepoint <- dfn_acc$timepoint

#  change missing data to NA 
dfn_sad <- dfn_sad %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_dep <- dfn_dep %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_anx <- dfn_anx %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_bias <- dfn_bias %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_qol <- dfn_qol%>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))
dfn_acc <- dfn_acc %>% mutate(across(everything(), ~ replace(.x, .x == 99999, NA)))


# replace timepoint col 
dfn_sad$timepoint <- san_timepoint
dfn_dep$timepoint <- depn_timepoint
dfn_anx$timepoint <- anxn_timepoint
dfn_bias$timepoint <- biasn_timepoint
dfn_qol$timepoint <- qoln_timepoint
dfn_acc$timepoint <- nacc_timepoint


# change single session study post-intervention timepoints to 1 
dfn_sad$timepoint <- lapply(dfn_sad$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_dep$timepoint <- lapply(dfn_dep$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_anx$timepoint <- lapply(dfn_anx$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_bias$timepoint <- lapply(dfn_bias$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_qol$timepoint <- lapply(dfn_qol$timepoint, function(x) ifelse(x == 99999, 1, x))
dfn_acc$timepoint <- lapply(dfn_acc$timepoint, function(x) ifelse(x == 99999, 1, x))


#### prioritise outcomes ----
# load outcomes prioritised by study
pon <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "SAD")
pon_dep <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "DEP")
pon_anx <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "ANX")
pon_bias <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "BIAS")
pon_qol <- read_excel("data/prioritised_outcomes_nonclin.xlsx", sheet = "QOL")


# not necessary for qol as no studied used multiple measures for it 

# join outcomes by studlab 
dfn_sad1 <- dfn_sad %>%
  left_join(pon, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfn_dep1 <- dfn_dep %>%
  left_join(pon_dep, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfn_anx1 <- dfn_anx %>%
  left_join(pon_anx, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

# funct to stardardise strings to ensure correct filtering
standardise_str<- function(x) {
x %>%
stringr::str_squish() %>%               # remove extra internal whitespace
iconv(to = "ASCII//TRANSLIT") %>%       # normalize encoding
trimws()                                # remove leading/trailing whitespace
}
dfn_bias <- dfn_bias %>% mutate(studlab = standardise_str(studlab))
pon_bias <- pon_bias %>% mutate(studlab = standardise_str(studlab))
pon_bias <- pon_bias %>% mutate(outcome = standardise_str(outcome))
dfn_bias <- dfn_bias %>% mutate(outcome = standardise_str(outcome))

dfn_bias1 <- dfn_bias %>%
  left_join(pon_bias, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(outcome == outcome.po) %>%  
  select(-outcome.po) 

dfn_qol1 <- dfn_qol %>%
  left_join(po_qol, by = "studlab", suffix = c("", ".po")) %>%
  dplyr::filter(!is.na(outcome.po)) %>%  
  select(-outcome.po) 


### remove arms with missing data  ----
# social anxiety
dfn_sad1 <- dfn_sad1 %>%
  dplyr::filter(!is.na(mean))

dfn_sad1 <- dfn_sad1 %>%
  dplyr::filter(!is.na(sd))

dfn_sad1 <- dfn_sad1 %>%
  dplyr::filter(!is.na(n))

# depression 
dfn_dep1 <- dfn_dep1 %>%
  dplyr::filter(!is.na(mean))

dfn_dep1 <- dfn_dep1 %>%
  dplyr::filter(!is.na(sd))

dfn_dep1 <- dfn_dep1 %>%
  dplyr::filter(!is.na(n))

# general anxiety 
dfn_anx1 <- dfn_anx1 %>%
  dplyr::filter(!is.na(mean))

dfn_anx1 <- dfn_anx1 %>%
  dplyr::filter(!is.na(sd))

dfn_anx1 <- dfn_anx1 %>%
  dplyr::filter(!is.na(n))

# change in bias 
dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!is.na(mean))

dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!is.na(sd))

dfn_bias1 <- dfn_bias1 %>%
  dplyr::filter(!is.na(n))

# quality of life
dfn_qol1 <- dfn_qol %>%
  dplyr::filter(!is.na(mean))

dfn_qol1 <- dfn_qol %>%
  dplyr::filter(!is.na(sd))

dfn_qol1 <- dfn_qol1 %>%
  dplyr::filter(!is.na(n))

# acceptibility
dfn_acc1 <- dfn_acc %>%
  dplyr::filter(!is.na(n))

dfn_acc1 <- dfn_acc1 %>%
  dplyr::filter(!is.na(dropouts))

### pivot data ----

dfn_sad3 <- dfn_sad1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_sad4 <- dfn_sad3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# depression
dfn_dep3 <- dfn_dep1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_dep4 <- dfn_dep3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )


# general anxiety
dfn_anx3 <- dfn_anx1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_anx4 <- dfn_anx3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# change in bias

dfn_bias3 <- dfn_bias1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_bias4 <- dfn_bias3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# quality of life 
dfn_qol3 <- dfn_qol1 %>%
  dplyr::mutate(timepoint = as.numeric(timepoint)) %>%  # convert timepoint to numeric
  group_by(studlab) %>%
  dplyr::mutate(
    baseline_time = min(timepoint, na.rm = TRUE),  # find the lowest timepoint per study
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")  
  ) %>%
  ungroup() %>%
  select(-baseline_time) 

# pivot again 
dfn_qol4 <- dfn_qol3 %>%
  group_by(studlab, outcome, intervention, arm, timepoint_label, node) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    mean = mean(as.numeric(mean), na.rm = TRUE),
    sd = mean(as.numeric(sd), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, outcome, intervention, arm, node),
    names_from = timepoint_label,  
    values_from = c(n, mean, sd),  
    names_glue = "{timepoint_label}_{.value}"
  )

# acceptibility 
dfn_acc3 <- dfn_acc1 %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  group_by(studlab) %>%
  mutate(
    baseline_time = min(timepoint, na.rm = TRUE),
    timepoint_label = ifelse(timepoint == baseline_time, "baseline", "endpoint")
  ) %>%
  ungroup() %>%
  select(-baseline_time)

# pivot again
dfn_acc4 <- dfn_acc3 %>%
  group_by(studlab, intervention, node, timepoint_label) %>%
  summarise(
    n = mean(as.numeric(n), na.rm = TRUE),
    dropouts = mean(as.numeric(dropouts), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(studlab, intervention, node),
    names_from = timepoint_label,
    values_from = c(n, dropouts),
    names_glue = "{timepoint_label}_{.value}"
  )


dfn_sad <- dfn_sad4
dfn_dep <- dfn_dep4
dfn_anx <- dfn_anx4
dfn_bias <- dfn_bias4
dfn_qol <- dfn_qol4
dfn_acc <- dfn_acc4


### label subgroups ----
# bias type 
dfn_sad$target_bias <- ifelse(dfn_sad$studlab %in% attention, "attention",
                              ifelse(dfn_sad$studlab %in% interpretation, "interpretation",
                                     ifelse(dfn_sad$studlab %in% approach, "approach",
                                            ifelse(dfn_sad$studlab %in% memory, "memory",
                                                   ifelse(dfn_sad$studlab %in% multiple, "combination", NA)))))

dfn_dep$target_bias <- ifelse(dfn_dep$studlab %in% attention, "attention",
                              ifelse(dfn_dep$studlab %in% interpretation, "interpretation",
                                     ifelse(dfn_dep$studlab %in% approach, "approach",
                                            ifelse(dfn_dep$studlab %in% memory, "memory",
                                                   ifelse(dfn_dep$studlab %in% multiple, "combination", NA)))))

dfn_anx$target_bias <- ifelse(dfn_anx$studlab %in% attention, "attention",
                              ifelse(dfn_anx$studlab %in% interpretation, "interpretation",
                                     ifelse(dfn_anx$studlab %in% approach, "approach",
                                            ifelse(dfn_anx$studlab %in% memory, "memory",
                                                   ifelse(dfn_anx$studlab %in% multiple, "combination", NA)))))

dfn_bias$target_bias <- ifelse(dfn_bias$studlab %in% attention, "attention",
                               ifelse(dfn_bias$studlab %in% interpretation, "interpretation",
                                      ifelse(dfn_bias$studlab %in% approach, "approach",
                                             ifelse(dfn_bias$studlab %in% memory, "memory",
                                                    ifelse(dfn_bias$studlab %in% multiple, "combination", NA)))))

dfn_qol$target_bias <- ifelse(dfn_qol$studlab %in% attention, "attention",
                              ifelse(dfn_qol$studlab %in% interpretation, "interpretation",
                                     ifelse(dfn_qol$studlab %in% approach, "approach",
                                            ifelse(dfn_qol$studlab %in% memory, "memory",
                                                   ifelse(dfn_qol$studlab %in% multiple, "combination", NA)))))

dfn_acc$target_bias <- ifelse(dfn_acc$studlab %in% attention, "attention",
                              ifelse(dfn_acc$studlab %in% interpretation, "interpretation",
                                     ifelse(dfn_acc$studlab %in% approach, "approach",
                                            ifelse(dfn_acc$studlab %in% memory, "memory",
                                                   ifelse(dfn_acc$studlab %in% multiple, "combination", NA)))))

# number of sessions
dfn_sad$session <- ifelse(dfn_sad$studlab %in% single, "single",
                          ifelse(dfn_sad$studlab %in% multi, "multi", NA))

dfn_dep$session <- ifelse(dfn_dep$studlab %in% single, "single",
                          ifelse(dfn_dep$studlab %in% multi, "multi", NA))

dfn_anx$session <- ifelse(dfn_anx$studlab %in% single, "single",
                          ifelse(dfn_anx$studlab %in% multi, "multi", NA))

dfn_bias$session <- ifelse(dfn_bias$studlab %in% single, "single",
                           ifelse(dfn_bias$studlab %in% multi, "multi", NA))

dfn_qol$session <- ifelse(dfn_qol$studlab %in% single, "single",
                          ifelse(dfn_qol$studlab %in% multi, "multi", NA))

dfn_acc$session <- ifelse(dfn_acc$studlab %in% single, "single",
                          ifelse(dfn_acc$studlab %in% multi, "multi", NA))

# mode of delivery
dfn_sad$mode_of_delivery <- ifelse(dfn_sad$studlab %in% computer, "computer",
                                   ifelse(dfn_sad$studlab %in% smartphone, "smartphone",
                                          ifelse(dfn_sad$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfn_sad$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfn_sad$studlab %in% any, "any device with internet access", NA)))))

dfn_dep$mode_of_delivery <- ifelse(dfn_dep$studlab %in% computer, "computer",
                                   ifelse(dfn_dep$studlab %in% smartphone, "smartphone",
                                          ifelse(dfn_dep$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfn_dep$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfn_dep$studlab %in% any, "any device with internet access", NA)))))

dfn_bias$mode_of_delivery <- ifelse(dfn_bias$studlab %in% computer, "computer",
                                    ifelse(dfn_bias$studlab %in% smartphone, "smartphone",
                                           ifelse(dfn_bias$studlab %in% vr, "virtual reality headset",
                                                  ifelse(dfn_bias$studlab %in% read_aloud, "situations read aloud",
                                                         ifelse(dfn_bias$studlab %in% any, "any device with internet access", NA)))))

dfn_anx$mode_of_delivery <- ifelse(dfn_anx$studlab %in% computer, "computer",
                                   ifelse(dfn_anx$studlab %in% smartphone, "smartphone",
                                          ifelse(dfn_anx$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfn_anx$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfn_anx$studlab %in% any, "any device with internet access", NA)))))

dfn_qol$mode_of_delivery <- ifelse(dfn_qol$studlab %in% computer, "computer",
                                   ifelse(dfn_qol$studlab %in% smartphone, "smartphone",
                                          ifelse(dfn_qol$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfn_qol$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfn_qol$studlab %in% any, "any device with internet access", NA)))))

dfn_acc$mode_of_delivery <- ifelse(dfn_acc$studlab %in% computer, "computer",
                                   ifelse(dfn_acc$studlab %in% smartphone, "smartphone",
                                          ifelse(dfn_acc$studlab %in% vr, "virtual reality headset",
                                                 ifelse(dfn_acc$studlab %in% read_aloud, "situations read aloud",
                                                        ifelse(dfn_acc$studlab %in% any, "any device with internet access", NA)))))

# allegiance bias

dfn_sad$allegiance_bias <- ifelse(dfn_sad$studlab %in% present, "present",
                                 ifelse(dfn_sad$studlab %in% none, "none", NA))

dfn_dep$allegiance_bias <- ifelse(dfn_dep$studlab %in% present, "present",
                                  ifelse(dfn_dep$studlab %in% none, "none", NA))

dfn_anx$allegiance_bias <- ifelse(dfn_anx$studlab %in% present, "present",
                                  ifelse(dfn_anx$studlab %in% none, "none", NA))

dfn_bias$allegiance_bias <- ifelse(dfn_bias$studlab %in% present, "present",
                                   ifelse(dfn_bias$studlab %in% none, "none", NA))

dfn_qol$allegiance_bias <- ifelse(dfn_qol$studlab %in% present, "present",
                                  ifelse(dfn_qol$studlab %in% none, "none", NA))

dfn_acc$allegiance_bias <- ifelse(dfn_acc$studlab %in% present, "present",
                                  ifelse(dfn_acc$studlab %in% none, "none", NA))
