# 2020-10-08 ------------------------------
## preliminaries/functions ------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, options(knitr.kable.NA = '', dev = 'svg'), knitr.graphics.error = FALSE,
                      warning = FALSE, message = FALSE, fig.align = "left", comment = NA)
library(kableExtra)
library(janitor)
library(countrycode)
library(Cairo)
library(tidyverse)
library(naniar)
library(formattable)
suppressMessages(library(meta))
settings.meta(CIbracket = "(", CIseparator = ", ")

# functions for all
round_0 <- function(x) {formatC( round(x, 0), format = 'f', digits = 0)}
round_1 <- function(x) {formatC( round(x, 1), format = 'f', digits = 1)}
round_1d <- function(x) {format(x, digits = 1)}
round_0d <- function(x) {format(x, digits = 0)}
round_2 <- function(x) {formatC( round(x, 2), format = 'f', digits = 2)}
sd_conf_t <- function(low, up, n){(abs(up - low)/2)/abs(qt(0.025, n))}
sd_confint <- function(low, up){(abs(up - low)/2)/1.96}
conf_tl <- function(samp_mean, samp_sd, n, low_up){
  error <- qt(0.975, df = n - 1) * samp_sd/(sqrt(n))
  (ci_low <- samp_mean - error)
}

# variable coded as TF will give "n (XX.X)" percent, can specify digits
n_per_tf <- function(var_name, n_dig = 1){
  paste0(sum(var_name == TRUE), " (", format(round(100*(mean(var_name)), n_dig), nsmall = 1), ")")
}

conf_tu <- function(samp_mean, samp_sd, n, low_up){
  error <- qt(0.975, df = n - 1) * samp_sd/(sqrt(n))
  (ci_up <- samp_mean + error)
}

conf_q <- function(samp_mean, samp_sd, n){
  error <- qnorm(0.975) * samp_sd/(sqrt(n))
  ci_up <- samp_mean + error
  ci_low <- samp_mean - error
  c(ci_low, ci_up)
}

combine_contin <- function(n_1, n_2, x_1, x_2, sd_1, sd_2){
  n_comb <- n_1 + n_2
  x_comb <-  (n_1 * x_1 + n_2 * x_2)/(n_comb)
  sd_comb <- sqrt(((n_1 - 1) * sd_1^2 + (n_2 - 1) * sd_2^2 + (n_1 * n_2)/(n_comb) * (x_1 - x_2)^2) / (n_comb - 1))
  return(c(n_comb, x_comb, sd_comb))}

n_percent <- function(a, b){
  str_c(a," (", round_0(b), ")")
}

bwgrp_pval <- function(m1, m2, n1, n2, pVal) {
  sd2 <- abs((m2 - m1) / qt(pVal / 2, n1 + n2 - 2)) * sqrt(n2 * n1 / (n2 + n1))
  sd1 <- sd2
  print(c(n1, m1, sd1, n2, m2, sd2))
}

# return single value
sd_bwgrp <- function(m1, m2, n1, n2, pVal) {
  abs((m2 - m1) / qt(pVal / 2, n1 + n2 - 2)) * sqrt(n2 * n1 / (n2 + n1))
}

pack_sub <- function(kable_input, name, a, b) {
  pack_rows(kable_input, name,
    start_row = a, end_row = b,
    label_row_css = "border-bottom: 0px solid;", color = "black", background = "white", bold = FALSE, indent = FALSE
  )
}

pack_top <- function(kable_input, name, a, b) {
  pack_rows(kable_input, name,
    start_row = a, end_row = b,
    label_row_css = "border-top: 1px solid;", color = "black", background = "#EBEBEB"
  )
}

result_pack <- function(kable_input, result, row, padding = 62, bold = FALSE, color = "gray", italic = TRUE){
  padding <- paste0("padding-left:" , padding, "%; ", "border-bottom: 0px solid;")
  pack_rows(kable_input, result, row, row, label_row_css = padding, underline = FALSE, bold = bold, color = color, italic = italic, indent = FALSE)
}

result_pack_line <- function(kable_input, result, row, padding = 62, bold = FALSE, color = "gray", italic = TRUE){
  padding <- paste0("padding-left:" , padding, "%; ", "border-bottom: 0.5px solid gray;")
  pack_rows(kable_input, result, row, row, label_row_css = padding, underline = FALSE, bold = bold, color = color, italic = italic, indent = FALSE)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
view_rec <- function(data_set, refid_select) {
  data_set %>%
    filter(refid == refid_select) %>%
    janitor::remove_empty(which = "cols") %>%
    t() %>%
    View()
}

or_join <- function(a_tbl, outcome) {
  left_join(a_tbl %>% filter(row_number() == 1), a_tbl %>% filter(row_number() == 2), by = "refid") %>%
    select(-study.y) %>%
    rename(
      c_tx = arm_tx.x, tx = arm_tx.y, study = study.x,
      n_c = arm_n.x, event_c = str_c(outcome, "_n.x"),
      n_e = arm_n.y, event_e = str_c(outcome, "_n.y"))
}

odds_ratio <- function(event1, n1, event2, n2, refid, digits = 2) {
  a <- meta::metabin(event1, n1, event2, n2, sm = "OR")
  a <- with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(exp(TE), digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), "-",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
  tibble(refid, a) %>%
    rename(or_ci = a)
}

# or_hunger <- with(or_join(dichot_hunger_or, "hunger"), odds_ratio(event_e, n_e, event_c, n_c, refid = refid, digits = 2))

# add a footnote to a variable and value eg, add_foot(study, "Tudor-Drobjewski 2018", "c")
add_foot <- function(.data, use_var, study_to_footnote, value){
  foot_letter <- paste0("^", value, "^")
  use_var <- enquo(use_var)
  .data %>%
    mutate(!!use_var := if_else(!!use_var == study_to_footnote, paste0(study_to_footnote, foot_letter), !!use_var) )
}

# kable defaults
opt_font <-  c("Source Sans Pro")
opt_boot <- c("striped", "hover", "condensed")
# bot_a <- "border-bottom: 0px solid;"
# bot_b <- "border-top: 1px solid;"
# col_a <- "black"
# bak_a <- "white"

# file to read most recent distiller data
# change working directory when debugging
# setwd("_protein")
# library(countrycode)
# library(Cairo)
# library(tidyverse)
# suppressMessages(library(meta))
# library(kableExtra)

path_csv <- function(name_csv){
  sheet <- name_csv
  path <- str_c("data/", sheet)
  return(path)
}

# function to calculate means, sd, and create analytical data set (w/log transform)
calc_mn_sd <- function(n_e, m_e, sd_e, md_e, q1_e, q3_e, min_e, max_e, study, tx, subgroup = NULL, refid, data = NULL, log_trans = FALSE) {
  temp.dat <- data %>%
    select(all_of(c(n_e, m_e, sd_e, md_e, q1_e, q3_e, min_e, max_e, study, tx, subgroup, refid))) %>%
    # placeholder control arm values to obtain standard deviations if missing
    mutate(n_c = 20, m_c = 2, sd_c = 1, md_c = 3, q1_c = 2, q3_c = 3, min_c = 1, max_c = 5)
  names(temp.dat)[1:11] <- c("n_e", "m_e", "sd_e", "md_e", "q1_e", "q3_e", "min_e", "max_e", "study", "tx", "subgroup")

  temp <- metacont(
    n.e = n_e,
    n.c = n_c,
    comb.fixed = TRUE,
    mean.e = m_e,
    sd.e = sd_e,
    median.e = md_e,
    q1.e = q1_e,
    q3.e = q3_e,
    min.e = min_e,
    max.e = max_e,
    mean.c = m_c,
    sd.c = sd_c,
    median.c = md_c,
    q1.c = q1_c,
    q3.c = q3_c,
    min.c = min_c,
    max.c = max_c,
    sm = "SMD",
    studlab = study,
    data = temp.dat
  )

  temp <- as_tibble(temp[c("studlab", "n.e", "mean.e", "sd.e")])

  mean_raw_log <- function(raw_mean, raw_sd) {
    (log_mean <- log(raw_mean) - 0.5 * log(raw_sd^2 / raw_mean^2 + 1))
  }
  sd_raw_log <- function(raw_mean, raw_sd) {
    (log_sd <- sqrt(log(raw_sd^2 / raw_mean^2 + 1)))
  }

  if (log_trans == TRUE) {
    temp$mean_log.e <- mean_raw_log(temp$mean.e, temp$sd.e)
    temp$sd_log.e <- sd_raw_log(temp$mean.e, temp$sd.e)
  }
  temp <- cbind(temp.dat$tx, temp)
  names(temp) <- c("arm_tx", "study", "n", "mean", "sd", "mean_log", "sd_log")
  temp[, c(2, 1, 3:7)]
}

## data files ####
data_files <- as_tibble(list.files("data/"))

study_arm_file <- data_files %>%
  filter(str_detect(value, "studyArm")) %>%
  arrange(desc(value)) %>%
  slice(1)

study_char_file <- data_files %>%
  filter(str_detect(value, "studyChar")) %>%
  arrange(desc(value)) %>%
  slice(1)

cont_out_file <- data_files %>%
  filter(str_detect(value, "contOutcomes")) %>%
  arrange(desc(value)) %>%
  slice(1)

dichot_out_file <- data_files %>%
  filter(str_detect(value, "dichotOutcomes")) %>%
  arrange(desc(value)) %>%
  slice(1)

likert_out_file <- data_files %>%
  filter(str_detect(value, "likertOutcomes")) %>%
  arrange(desc(value)) %>%
  slice(1)

rob_file <- data_files %>%
  filter(str_detect(value, "rob_")) %>%
  arrange(desc(value)) %>%
  slice(1)

# display file characteristics
a <- as.character(file.mtime(paste0("data/", study_arm_file)))
b <- as.character(file.mtime(paste0("data/", study_char_file)))
c <- as.character(file.mtime(paste0("data/", cont_out_file)))
d <- as.character(file.mtime(paste0("data/", dichot_out_file)))
e <- as.character(file.mtime(paste0("data/", likert_out_file)))
f <- as.character(file.mtime(paste0("data/", rob_file)))
z <- matrix(c(
  paste(b, study_char_file),
  paste(a, study_arm_file),
  paste(c, cont_out_file),
  paste(d, dichot_out_file),
  paste(e, likert_out_file),
  paste(f, rob_file)))
z
write_delim(data.frame(z), "used_files_dates.txt", delim = "--", col_names = FALSE)
rm(a, b, c, d, e, f, z)

## get subgroups data (note old file) ####
refs.dat <- suppressWarnings(read_csv("data/incl_mg_distsr_fasting_2021-03-09-17-42-12.csv")) %>%
  janitor::clean_names() %>%
  # filter(user == "Anne_Marbella") %>%
  select(refid, age, starts_with("subgroup"))

# * (end)

## study characteristics ####
path <- path_csv(study_char_file)
study_char.dat <- read_csv(path)

# number of studies
(study_char_n <- study_char.dat %>%
  distinct(Refid) %>%
  count())

study_char.dat <- study_char.dat %>%
  select(-c(6:8)) %>%  # change to add linked
  janitor::clean_names() %>%
  rename(author_dist = author, author = author_added) %>%  # author distiller, author entered
  # fix Karamian user for accounting purposes
  mutate(user = ifelse(refid %in% c(69, 7990), "Anne_Marbella", user))

# check last variable is notes
tail(names(study_char.dat), 1) == "notes_study_char"

# filter refs
study_char.dat <- study_char.dat %>%
  filter(!(design == "rct" & user != "Anne_Marbella")) %>%
  filter(!(design == "crossover" & user != "Anne_Marbella")) %>%
  filter(!(refid == 131 & user == "Anne_Marbella")) %>%
  # filter(!(refid == 5815 & user == "Anne_Marbella")) %>%
  filter(!(refid == 1678 & user == "Anne_Marbella"))

study_char.dat <- study_char.dat %>%
  mutate(design = ifelse(design == "quasiexp", "nrsi", design),
    design = factor(design,
    levels = c(
      "rct",
      "crossover",
      "cluster",
      "nrsi",
      "prospect_coh",
      "retrospect_coh",
      "casecontrol",
      "case_series",
      "other"))) %>%
  mutate(study = paste(stringr::str_extract(author_dist, "^([^,])+"), year)) %>%
 select(refid, study, everything())

# create new author field and append a,b,c etc for same year
# list of multiple author-year publications
temp_1 <- study_char.dat %>%
  mutate(study = paste(stringr::str_extract(author_dist, "^([^,])+"), year)) %>%
  arrange(author, refid) %>%
  group_by(study) %>%
  mutate(
    n = row_number(),
    let_add = ifelse(n > 0, letters[n], "")) %>%
  ungroup() %>%
  mutate(study_r = str_c(study, let_add),
    # study = study_r,
    surg_nosurg = ifelse(is.na(no_anesth), "surgical", "non-surgical"))

# with multiple per year letters added to year n = 144 12/5/20
temp_2 <- temp_1 %>%
  group_by(study) %>%
    filter(n() > 1) %>%
  ungroup() %>%
  mutate(study = study_r) %>%
  select(-study_r)

# one publication per year n = 13 12/5/20
temp_1 <- temp_1 %>%
  group_by(study) %>%
    filter(n() == 1) %>%
  ungroup() %>%
  select(-study_r)

study_char.dat <- bind_rows(temp_1, temp_2) %>%
  # select(!c(author, author_dist, refid_ver, year_added, pub_type, n, let_add)) %>%
  select(!c(author, author_dist, user, reviewer, refid_ver, year_added, pub_type, n, let_add)) %>%
  select(refid, year, study, surg_nosurg, design, everything())

rm(temp_1, temp_2)

# add subgroup
study_char.dat <- left_join(study_char.dat, refs.dat[, c(1,2)], by = "refid") %>%
  arrange(age, desc(surg_nosurg),desc(design), year, study) %>%
  # shorten Dock-Nascimento 2011 Dock-Nascimento 2012a Dock-Nascimento 2012b
  mutate(study = ifelse(study == "Tudor-Drobjewski 2018", "TDrobjewski 2018", study),
         study = ifelse(study == "Jabbari Moghaddam 2014", "Moghaddam 2014", study),
         study = ifelse(study == "Dock-Nascimento 2011","DNascimento 2011", study),
         study = ifelse(study == "Dock-Nascimento 2012a", "DNascimento 2012a", study),
         study = ifelse(study == "Dock-Nascimento 2012b", "DNascimento 2012b", study),
         study = ifelse(study == "de Aguilar-Nascimento 2014", "deANascimento 2014", study),
         # Nascimento 2019 change to "surgical" as in labor
         surg_nosurg = ifelse(refid == "499", "surgical", surg_nosurg)) %>%
  relocate(linked_references, .after = last_col())

# verify 164 unique 2021/06/22 07:43
study_char_n
length(unique(study_char.dat$refid)) == study_char_n
rm(study_char_n)

# * (end)

## study arm ####
path <- path_csv(study_arm_file)
study_arm.dat <- read_csv(path)

# number of studies 2021/06/22 07:59, n = 164
(study_arm_n <- study_arm.dat %>%
    distinct(Refid) %>%
    count())

# delete <- length(names(study_arm.dat))
study_arm.dat <- study_arm.dat %>%
  select(-c(2, 3, 5:7)) %>%
  janitor::clean_names()

tail(names(study_arm.dat), 1) == "notes_arm"
ifelse(tail(names(study_arm.dat), 1) == "notes_arm", "final variable correct: notes_arm", "error in code")

# use updated study names for duplicate author year, appended w/letter
study_names <- study_char.dat %>% select(refid, study, year)
study_arm.dat <- left_join(study_arm.dat, study_names, by = "refid") %>%
  select(refid, study, year, design, design, everything()) %>%
  mutate(
    timeingest1 = ifelse(timeingest1 == 99, 12, timeingest1),
    timeingest2 = ifelse(timeingest2 == 99, 12, timeingest2),
    # fix Karamian user for accounting purposes
    user = ifelse(refid %in% c(69, 7990), "Anne_Marbella", user))

# add age and surg_nosurg
study_arm.dat <- left_join(study_arm.dat, study_char.dat[, c("refid", "age", "surg_nosurg")], by = "refid")

# filter refs
# rct and crossover (dual extraction)
am_rct_cross <- study_arm.dat %>%
  filter(design %in% c("rct", "crossover") & user == "Anne_Marbella")

# others single
not_rct_cross <- study_arm.dat %>%
  filter(!design %in% c("rct", "crossover")) %>%
  filter(!(refid == 131 & user == "Anne_Marbella")) %>%
  filter(!(refid == 5815 & user == "Anne_Marbella")) %>%
  filter(!(refid == 1678 & user == "Anne_Marbella"))

study_arm.dat <- bind_rows(am_rct_cross, not_rct_cross) %>%
  arrange(refid) %>%
  mutate(design = ifelse(design == "quasiexp", "nrsi", design),
    design = factor(design,
      levels = c(
        "rct",
        "crossover",
        "cluster",
        "nrsi",
        "prospect_coh",
        "retrospect_coh",
        "casecontrol",
        "case_series",
        "other"))) %>%
  group_by(refid) %>%
  mutate(arm = row_number()) %>%
  ungroup() %>%
  # fix Oyama to AM and PM
  mutate(study = ifelse(study == "Oyama 2011" & arm_n < 30, "Oyama 2011 (AM)", study),
         study = ifelse(study == "Oyama 2011" & arm_n > 30, "Oyama 2011 (PM)", study))

# check n same as study_arm_n = 164 on (2021/02/26 12:29)
length(unique(study_arm.dat$refid)) == study_arm_n

rm(am_rct_cross, not_rct_cross, study_arm_n)
# * (end)

## protein refids ####
protein_refids <- study_arm.dat %>%
  select(refid, arm_tx) %>%
  filter(arm_tx %in% c("prot_simp", "prot_comp", "prot")) %>%
  select(refid) %>%
  distinct() %>%
  pull()

# * (end)

## cho refids ####
cho_refids <- study_arm.dat %>%
  filter(arm_tx %in% c("prot_simp", "prot_comp", "cho_comp", "cho_simp")) %>%
  select(refid) %>%
  distinct() %>%
  pull()

## gum refids ####
# n = 12, but 2 studies different gum, 10 unique refids 12/5/20
gum_refids <- study_arm.dat %>%
  select(refid, arm_tx) %>%
  filter(str_detect(arm_tx, "gum")) %>%
  rename(tx = arm_tx) %>%
  distinct()

# add tx including sugar and sugar free
gum_refids <- full_join(gum_refids, gum_refids, by = "refid") %>%
  mutate(tx = ifelse(tx.x == tx.y, tx.x, "sugar_sugarfree")) %>%
  arrange(desc(tx)) %>%
  group_by(refid) %>%
  slice(1) %>%
  select(refid, tx) %>%
  ungroup()

# * (end)

## continuous outcome data ####
path <- path_csv(cont_out_file)
contin.dat <- read_csv(path) %>%
  select(-c(5:7)) %>%
  janitor::clean_names() %>%
  mutate(
    author = sapply(strsplit(author, ","), "[", 1),
    study = str_c(author, year, sep = " "),
    outcomes_c_k = str_remove(outcomes_c_k, "\\|")) %>%
  select(refid, study, year, 4:219)

(contin_n <- contin.dat %>%
    distinct(refid) %>%
    count())

tail(names(contin.dat), 1) == "notes_continuous"
ifelse(tail(names(contin.dat), 1) == "notes_continuous", "final variable correct: notes_continuous", "error in code")

contin.dat <- left_join(contin.dat, refs.dat, by = "refid") %>%
  mutate(
    design = ifelse(design == "quasiexp", "nrsi", design),
    surg_nosurg = ifelse(is.na(no_anesth), "surgical", "non-surgical"),
    design = factor(design,
      levels = c(
        "rct",
        "crossover",
        "cluster",
        "nrsi",
        "prospect_coh",
        "retrospect_coh",
        "casecontrol",
        "case_series",
        "other"))) %>%
  select(refid, study, year, user, design, surg_nosurg, age, starts_with("subgroup"), everything())

am <- contin.dat %>% filter(user %in% c("Anne_Marbella", "Mark_Grant")) %>%
  # both Anne and Mark records for Zelic 2982
  filter(!(user == "Mark_Grant" & refid == 2982))
ma <- contin.dat %>% filter(!refid %in% am$refid)
contin.dat <- bind_rows(am, ma) %>% select(!study) # replace below
rm(am, ma)

# replace correct names
contin.dat <- left_join(contin.dat[,-3], study_names[, c(1,2)], by = "refid") %>%
  group_by(refid) %>%
  mutate(arm = row_number()) %>%
  ungroup() %>%
  mutate(study = ifelse(study == "Oyama 2011" & arm_n < 30, "Oyama 2011 (AM)", study),
         study = ifelse(study == "Oyama 2011" & arm_n > 30, "Oyama 2011 (PM)", study),
         arm = ifelse(study == "Oyama 2011 (PM)" & arm_n == 33, 4, arm),
         arm = ifelse(study == "Oyama 2011 (PM)" & arm_n == 35, 3, arm)) %>%
  select(refid, study, arm, everything()) %>%
  arrange(refid, arm)

contin.dat %>% filter(arm == 1) %>% tally()
# check n same as contin_n = 164 on (2021/02/26 12:29)
length(unique(contin.dat$refid)) == contin_n
rm(contin_n)
# * (end)

## dichotomous outcome data ####
path <- path_csv(dichot_out_file)
dichot.dat <- read_csv(path) %>%
  select(-c(5:7)) %>%
  janitor::clean_names() %>%
  mutate(
    author = sapply(strsplit(author, ","), "[", 1),
    study = str_c(author, year, sep = " "),
    outcomes_d_k = str_remove(outcomes_d_k, "\\|")) %>%
  select(refid, study, year, everything()) %>%
  select(-author)

(dichot_n <- dichot.dat %>%
    distinct(refid) %>%
    count())

tail(names(dichot.dat), 1) == "summary_d_results"
ifelse(tail(names(dichot.dat), 1) == "summary_d_results", "final variable correct: summary_d_results", "error in code")

dichot.dat <- left_join(dichot.dat, refs.dat, by = "refid") %>%
  mutate(
    design = ifelse(design == "quasiexp", "nrsi", design),
    surg_nosurg = ifelse(is.na(no_anesth), "surgical", "non-surgical"),
    design = factor(design,
      levels = c(
        "rct",
        "crossover",
        "cluster",
        "nrsi",
        "prospect_coh",
        "retrospect_coh",
        "casecontrol",
        "case_series",
        "other"
      )
    )
  ) %>%
  select(refid, study, year, user, design, surg_nosurg, age, starts_with("subgroup"), everything())

am <- dichot.dat %>% filter(user %in% c("Anne_Marbella", "Mark_Grant")) %>%
  filter(!(user == "Mark_Grant" & refid == 2982))
ma <- dichot.dat %>% filter(!refid %in% am$refid)
dichot.dat <- bind_rows(am, ma) %>% select(!study) # replace below
rm(am, ma)

length(unique(dichot.dat$refid)) == 164

# replace correct names
dichot.dat <- left_join(dichot.dat[, -3], study_names[, c(1, 2)], by = "refid") %>%
  group_by(refid) %>%
  mutate(arm = row_number()) %>%
  ungroup() %>%
  mutate(
    study = ifelse(study == "Oyama 2011" & arm_n < 30, "Oyama 2011 (AM)", study),
    study = ifelse(study == "Oyama 2011" & arm_n > 30, "Oyama 2011 (PM)", study),
    arm = ifelse(study == "Oyama 2011 (PM)" & arm_n == 33, 4, arm),
    arm = ifelse(study == "Oyama 2011 (PM)" & arm_n == 35, 3, arm)
  ) %>%
  select(refid, study, arm, everything()) %>%
  arrange(refid, arm)

dichot.dat %>% filter(arm == 1) %>% tally()
# check n same as contin_n = 164 on (2021/02/26 12:29)
length(unique(dichot.dat$refid)) == dichot_n
rm(dichot_n)
# * (end)

## likert outcome data ####
# note parsing error due to empty line in file; benign
path <- path_csv(likert_out_file)

likert.dat <- read_csv(path) %>%
  select(-c(5:7)) %>%
  janitor::clean_names() %>%
  mutate(
    author = sapply(strsplit(author, ","), "[", 1),
    study = str_c(author, year, sep = " "),
    outcomes_l_k = str_remove(outcomes_l_k, "\\|")) %>%
  select(refid, study, year, everything()) %>%
  select(-author)

(likert_n <- likert.dat %>%
    distinct(refid) %>%
    count())

tail(names(likert.dat), 1) == "summary_l_result"
ifelse(tail(names(likert.dat), 1) == "summary_l_result", "final variable correct: summary_l_result", "error in code")

likert.dat <- left_join(likert.dat, refs.dat, by = "refid") %>%
  mutate(
    design = ifelse(design == "quasiexp", "nrsi", design),
    surg_nosurg = ifelse(is.na(no_anesth), "surgical", "non-surgical"),
    design = factor(design,
      levels = c(
        "rct",
        "crossover",
        "cluster",
        "nrsi",
        "prospect_coh",
        "retrospect_coh",
        "casecontrol",
        "case_series",
        "other"
      ),
      ordered = TRUE
    )
  ) %>%
  select(refid, study, year, user, design, surg_nosurg, age, starts_with("subgroup"), everything())

am <- likert.dat %>% filter(user %in% c("Anne_Marbella", "Mark_Grant")) %>%
  filter(!(user == "Mark_Grant" & refid == 2982))
ma <- likert.dat %>% filter(!refid %in% am$refid)
likert.dat <- bind_rows(am, ma) %>% select(!study) # replace below
rm(am, ma)

length(unique(likert.dat$refid)) == 164

# replace correct names
likert.dat <- left_join(likert.dat[,-3], study_names[, c(1,2)], by = "refid") %>%
  group_by(refid) %>%
  mutate(arm = row_number()) %>%
  ungroup() %>%
  mutate(study = ifelse(study == "Oyama 2011" & arm_n < 30, "Oyama 2011 (AM)", study),
         study = ifelse(study == "Oyama 2011" & arm_n > 30, "Oyama 2011 (PM)", study),
         arm = ifelse(study == "Oyama 2011 (PM)" & arm_n == 33, 4, arm),
         arm = ifelse(study == "Oyama 2011 (PM)" & arm_n == 35, 3, arm)) %>%
  select(refid, study, arm, everything()) %>%
  arrange(refid, arm)

likert.dat %>% filter(arm == 1) %>% tally()
# check n same as contin_n = 164 on (2021/02/26 12:29)
length(unique(likert.dat$refid)) == likert_n
rm(likert_n)

# * (end)

## rob data ####
path <- path_csv(rob_file)
rob.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  select(-c(5:7,13)) %>%
  select(-author, -year) %>%
  # TODO: fix Karamian user for accounting purposes from Mark to Anne user
  mutate(user = ifelse(refid %in% c(69, 7990), "Anne_Marbella", user))

(rob_n <- rob.dat %>%
    distinct(refid) %>%
    count())

tail(names(rob.dat), 1) == "overall_notes"
ifelse(tail(names(rob.dat), 1) == "overall_notes", "final variable correct: overall_notes", "error in code")

rob.dat <- left_join(rob.dat, study_names, by = "refid") %>%
  select(refid, study, year, design, everything())

# filter refs
# rct and crossover (dual extraction)
am_rct_cross <- rob.dat %>%
  filter(design %in% c("rct", "crossover") & user == "Anne_Marbella")

# others single
not_rct_cross <- rob.dat %>%
  filter(!design %in% c("rct", "crossover")) %>%
  filter(!(refid == 131 & user == "Anne_Marbella")) %>%
  filter(!(refid == 1678 & user == "Anne_Marbella"))

  # arrange(refid, user)
  # group_by(refid)

rob.dat <- bind_rows(am_rct_cross, not_rct_cross) %>%
  select(-study)

# replace correct names
rob.dat <- left_join(rob.dat, study_names[, c(1,2)], by = "refid") %>%
  select(refid, study, everything())

rm(am_rct_cross, not_rct_cross)

rob.dat %>% tally()
# check n same as contin_n = 164 on (2021/02/26 12:29)
length(unique(rob.dat$refid)) == rob_n
rm(rob_n)

# * (end)

# add timing --------------------------------------------------------------
dichot.dat  <- dichot.dat %>%
  mutate(
    # surgical studies (note none have times 0 or negative)
    # hr_2 is ≤ 2; hr_26 is (2, 6] >2 to 6; hr_gt6 is >6
    hr_2 = 0, hr_26 = 0, hr_gt6 = 0,
    hr_2 = if_else(timeingest1 <= 2 & timeingest1 > 0, amtingest1, hr_2, missing = 0),
    hr_26 = if_else(timeingest1 > 2 & timeingest1 < 6, amtingest1, hr_26, missing = 0),
    hr_gt6 = if_else(timeingest1 >= 6, amtingest1, hr_gt6, missing = 0),
    hr_2 = if_else(hr_2 == 0 & timeingest2 <= 2 & timeingest2 > 0, amtingest2, hr_2, missing = 0),
    hr_26 = if_else(hr_26 == 0 & timeingest2 > 2 & timeingest2 < 6, amtingest2, hr_26, missing = 0),
    hr_gt6 = if_else(hr_gt6 == 0 & timeingest2 >= 6, amtingest2, hr_gt6, missing = 0),
    hr_gt6 = if_else(grepl("also received 300", type_ingest_1), 500, hr_gt6), # ) %>%  # Oyama PM
    # for nonsurgical
    hr_0 = 0, hr_minus2 = 0,
    hr_0 = if_else(surg_nosurg != "surgical" & timeingest1 == 0, amtingest1, hr_0, missing = 0),
    hr_minus2 = if_else(surg_nosurg != "surgical" & timeingest1 == -2, amtingest1, hr_minus2, missing = 0),
    hr_0 = if_else(surg_nosurg != "surgical" & timeingest2 == 0 & hr_0 == 0, amtingest2, hr_0, missing = 0),
    hr_minus2 = if_else(surg_nosurg != "surgical" & timeingest2 == -2 & hr_minus2 == 0, amtingest2, hr_minus2, missing = 0)
  ) %>%
  replace_with_na_at(.vars = c("hr_gt6", "hr_26", "hr_2", "hr_minus2", "hr_0"), condition = ~ .x == 0)

contin.dat <- contin.dat %>%
  mutate(
    # surgical studies (note none have times 0 or negative)
    # hr_2 is ≤ 2; hr_26 is (2, 6] >2 to 6; hr_gt6 is >6
    hr_2 = 0, hr_26 = 0, hr_gt6 = 0,
    hr_2 = if_else(timeingest1 <= 2 & timeingest1 > 0, amtingest1, hr_2, missing = 0),
    hr_26 = if_else(timeingest1 > 2 & timeingest1 < 6, amtingest1, hr_26, missing = 0),
    hr_gt6 = if_else(timeingest1 >= 6, amtingest1, hr_gt6, missing = 0),
    hr_2 = if_else(hr_2 == 0 & timeingest2 <= 2 & timeingest2 > 0, amtingest2, hr_2, missing = 0),
    hr_26 = if_else(hr_26 == 0 & timeingest2 > 2 & timeingest2 < 6, amtingest2, hr_26, missing = 0),
    hr_gt6 = if_else(hr_gt6 == 0 & timeingest2 >= 6, amtingest2, hr_gt6, missing = 0),
    hr_gt6 = if_else(grepl("also received 300", type_ingest_1), 500, hr_gt6), # ) %>%  # Oyama PM
    # for nonsurgical
    hr_0 = 0, hr_minus2 = 0,
    hr_0 = if_else(surg_nosurg != "surgical" & timeingest1 == 0, amtingest1, hr_0, missing = 0),
    hr_minus2 = if_else(surg_nosurg != "surgical" & timeingest1 == -2, amtingest1, hr_minus2, missing = 0),
    hr_0 = if_else(surg_nosurg != "surgical" & timeingest2 == 0 & hr_0 == 0, amtingest2, hr_0, missing = 0),
    hr_minus2 = if_else(surg_nosurg != "surgical" & timeingest2 == -2 & hr_minus2 == 0, amtingest2, hr_minus2, missing = 0)
  ) %>%
  replace_with_na_at(.vars = c("hr_gt6", "hr_26", "hr_2", "hr_minus2", "hr_0"), condition = ~ .x == 0)

likert.dat  <- likert.dat %>%
  mutate(
    # surgical studies (note none have times 0 or negative)
    # hr_2 is ≤ 2; hr_26 is (2, 6] >2 to 6; hr_gt6 is >6
    hr_2 = 0, hr_26 = 0, hr_gt6 = 0,
    hr_2 = if_else(timeingest1 <= 2 & timeingest1 > 0, amtingest1, hr_2, missing = 0),
    hr_26 = if_else(timeingest1 > 2 & timeingest1 < 6, amtingest1, hr_26, missing = 0),
    hr_gt6 = if_else(timeingest1 >= 6, amtingest1, hr_gt6, missing = 0),
    hr_2 = if_else(hr_2 == 0 & timeingest2 <= 2 & timeingest2 > 0, amtingest2, hr_2, missing = 0),
    hr_26 = if_else(hr_26 == 0 & timeingest2 > 2 & timeingest2 < 6, amtingest2, hr_26, missing = 0),
    hr_gt6 = if_else(hr_gt6 == 0 & timeingest2 >= 6, amtingest2, hr_gt6, missing = 0),
    hr_gt6 = if_else(grepl("also received 300", type_ingest_1), 500, hr_gt6), # ) %>%  # Oyama PM
    # for nonsurgical
    hr_0 = 0, hr_minus2 = 0,
    hr_0 = if_else(surg_nosurg != "surgical" & timeingest1 == 0, amtingest1, hr_0, missing = 0),
    hr_minus2 = if_else(surg_nosurg != "surgical" & timeingest1 == -2, amtingest1, hr_minus2, missing = 0),
    hr_0 = if_else(surg_nosurg != "surgical" & timeingest2 == 0 & hr_0 == 0, amtingest2, hr_0, missing = 0),
    hr_minus2 = if_else(surg_nosurg != "surgical" & timeingest2 == -2 & hr_minus2 == 0, amtingest2, hr_minus2, missing = 0)
  ) %>%
  replace_with_na_at(.vars = c("hr_gt6", "hr_26", "hr_2", "hr_minus2", "hr_0"), condition = ~ .x == 0)


# verify timing 2 hours ####
two_hours <- function(refids){
  temp <- study_arm.dat %>%
    filter(refid %in% refids)
  table(temp$timeingest1)
}

# groupings for tables
groupings <- function(data) {
  data %>%
    mutate(row = row_number()) %>%
    select(row, age, surg_nosurg, design, year) %>%
    group_by(age, surg_nosurg, design) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    arrange(age, row)
}

