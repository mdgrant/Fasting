# 2020-10-08 ------------------------------
## preliminaries -----------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, options(knitr.kable.NA = '', dev = 'svg'), knitr.graphics.error = FALSE,
                      warning = FALSE, message = FALSE)
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
sd_conf_t <- function(low, up, n){(abs(up-low)/2)/abs(qt(0.025, n))}
sd_confint <- function(low, up){(abs(up - low)/2)/1.96}
conf_tl <- function(samp_mean, samp_sd, n, low_up){
  error <- qt(0.975, df = n - 1) * samp_sd/(sqrt(n))
  (ci_low <- samp_mean - error)
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
pack_sub <- function(kable_input, name, a, b) {
  pack_rows(kable_input, name,
    start_row = a, end_row = b,
    label_row_css = "border-bottom: 0px solid;", color = "black", background = "white", bold = FALSE
  )
}
pack_top <- function(kable_input, name, a, b) {
  pack_rows(kable_input, name,
    start_row = a, end_row = b,
    label_row_css = "border-top: 1px solid;", color = "black", background = "#EBEBEB"
  )
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
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), ", ",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
  tibble(refid, a) %>%
    rename(or_ci = a)
}

# or_hunger <- with(or_join(dichot_hunger_or, "hunger"), odds_ratio(event_e, n_e, event_c, n_c, refid = refid, digits = 2))

# add a footnote to a variable and value eg, add_foot(study, "Tudor-Drobjewski 2018", "c")
add_foot <- function(.data, use_var , study_to_footnote, value){
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

## get subgroups data ####
refs.dat <- read_csv("data/incl_mg_distsr_fasting_2020-11-21.csv") %>%
  janitor::clean_names() %>%
  # filter(user == "Anne_Marbella") %>%
  select(refid, age, starts_with("subgroup"))

# * (end)

## study characteristics ####
path <- path_csv(study_char_file)
study_char.dat <- read_csv(path)
# to delete last observation empty column
delete <- length(names(study_char.dat))
study_char.dat <- study_char.dat %>%
  select(-c(5:7, all_of(delete))) %>%
  janitor::clean_names() %>%
  rename(author_dist = author, author = author_added) # author distiller, author entered

rm(delete)

# check last variable is notes
tail(names(study_char.dat), 1)

# filter refs
study_char.dat <- study_char.dat %>%
  filter(!(design == "rct" & user != "Anne_Marbella")) %>%
  filter(!(design == "crossover" & user != "Anne_Marbella")) %>%
  filter(!(refid == 131 & user == "Anne_Marbella"))

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
         study = ifelse(study == "de Aguilar-Nascimento 2014", "deANascimento 2014", study))

# 157 unique

# * (end)

## study arm ####
path <- path_csv(study_arm_file)
study_arm.dat <- read_csv(path)
delete <- length(names(study_arm.dat))
study_arm.dat <- study_arm.dat %>%
  select(-c(2, 3, 5:7, all_of(delete))) %>%
  janitor::clean_names()
tail(names(study_arm.dat), 1)
study_names <- study_char.dat %>% select(refid, study, year)
study_arm.dat <- left_join(study_arm.dat, study_names, by = "refid") %>%
  select(refid, study, year, design, design, everything()) %>%
  mutate(
    timeingest1 = ifelse(timeingest1 == 99, 12, timeingest1),
    timeingest2 = ifelse(timeingest2 == 99, 12, timeingest2))

# add age and surg_nosurg
study_arm.dat <- left_join(study_arm.dat, study_char.dat[, c("refid", "age", "surg_nosurg")], by = "refid")

# filter refs
# rct and crossover (dual extraction)
am_rct_cross <- study_arm.dat %>%
  filter(design %in% c("rct", "crossover") & user == "Anne_Marbella")

# others single
not_rct_cross <- study_arm.dat %>%
  filter(!design %in% c("rct", "crossover")) %>%
  filter(!(refid == 131 & user == "Anne_Marbella"))

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

# check n same as study_char.dat n = 157 on 12/5/20
length(unique(study_arm.dat$refid)) == length(unique(study_char.dat$refid))

rm(am_rct_cross, not_rct_cross)
# * (end)

## protein refids ####
protein_refids <- study_arm.dat %>%
  select(refid, arm_tx) %>%
  filter(arm_tx == "protein") %>%
  rename(tx = arm_tx) %>%
  distinct()

# * (end)

## cho refids ####
cho_refids <- study_arm.dat %>%
  select(refid, arm_tx) %>%
  filter(arm_tx %in% c("protein", "cho")) %>%
  rename(tx = arm_tx) %>%
  # filter(refid == 2703) %>%
  distinct()

# add tx including cho and cho/protein
cho_refids_choprotein <- cho_refids %>%
  group_by(refid) %>%
  select(refid) %>%
  slice(2) %>%
  pull(refid)

cho_refids <- cho_refids %>%
  mutate(tx = ifelse(refid %in%cho_refids_choprotein, "cho/protein", tx)) %>%
  distinct()

rm(cho_refids_choprotein)

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
  select(-c(5:7, 223)) %>%
  janitor::clean_names() %>%
  mutate(
    author = sapply(strsplit(author, ","), "[", 1),
    study = str_c(author, year, sep = " "),
    outcomes_c_k = str_remove(outcomes_c_k, "\\|")) %>%
  select(refid, study, year, 4:219)

tail(names(contin.dat), 5)

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

am <- contin.dat %>% filter(user == "Anne_Marbella")
ma <- contin.dat %>% filter(!refid %in% am$refid)
contin.dat <- bind_rows(am, ma) %>% select(!study) # replace below
rm(am, ma)

length(unique(contin.dat$refid)) == 157

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

# * (end)

## dichotomous outcome data ####
path <- path_csv(dichot_out_file)
dichot.dat <- read_csv(path) %>%
  select(-c(5:7, 78)) %>%
  janitor::clean_names() %>%
  mutate(
    author = sapply(strsplit(author, ","), "[", 1),
    study = str_c(author, year, sep = " "),
    outcomes_d_k = str_remove(outcomes_d_k, "\\|")) %>%
  select(refid, study, year, everything()) %>%
  select(-author)

tail(names(dichot.dat), 1)

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
        "other"))) %>%
  select(refid, study, year, user, design, surg_nosurg, age, starts_with("subgroup"), everything())

am <- dichot.dat %>% filter(user == "Anne_Marbella")
ma <- dichot.dat %>% filter(!refid %in% am$refid)
dichot.dat <- bind_rows(am, ma) %>% select(!study) # replace below
rm(am, ma)

length(unique(dichot.dat$refid)) == 157

# replace correct names
dichot.dat <- left_join(dichot.dat[,-3], study_names[, c(1,2)], by = "refid") %>%
  group_by(refid) %>%
  mutate(arm = row_number()) %>%
  ungroup() %>%
  mutate(study = ifelse(study == "Oyama 2011" & arm_n < 30, "Oyama 2011 (AM)", study),
         study = ifelse(study == "Oyama 2011" & arm_n > 30, "Oyama 2011 (PM)", study),
         arm = ifelse(study == "Oyama 2011 (PM)" & arm_n == 33, 4, arm),
         arm = ifelse(study == "Oyama 2011 (PM)" & arm_n == 35, 3, arm)) %>%
  select(refid, study, arm, everything()) %>%
  arrange(refid, arm)

dichot.dat %>% filter(arm == 1) %>% tally()

# * (end)

## likert outcome data ####
path <- path_csv(likert_out_file)
likert.dat <- read_csv(path) %>%
  select(-c(5:7, 126)) %>%
  janitor::clean_names() %>%
  mutate(
    author = sapply(strsplit(author, ","), "[", 1),
    study = str_c(author, year, sep = " "),
    outcomes_l_k = str_remove(outcomes_l_k, "\\|")) %>%
  select(refid, study, year, everything()) %>%
  select(-author)

tail(names(likert.dat), 1)

likert.dat <- left_join(likert.dat, refs.dat, by = "refid") %>%
  mutate(design = ifelse(design == "quasiexp", "nrsi", design),
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
        "other"),
      ordered = TRUE)) %>%
  select(refid, study, year, user, design, surg_nosurg, age, starts_with("subgroup"), everything())

am <- likert.dat %>% filter(user == "Anne_Marbella")
ma <- likert.dat %>% filter(!refid %in% am$refid)
likert.dat <- bind_rows(am, ma) %>% select(!study) # replace below
rm(am, ma)

length(unique(likert.dat$refid)) == 157

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

# * (end)

## rob data ####
path <- path_csv(rob_file)
rob.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  select(-c(5:7,13,66)) %>%
  select(-author, -year)

tail(names(rob.dat), 1)

rob.dat <- left_join(rob.dat, study_names, by = "refid") %>%
  select(refid, study, year, design, everything())

# filter refs
# rct and crossover (dual extraction)
am_rct_cross <- rob.dat %>%
  filter(design %in% c("rct", "crossover") & user == "Anne_Marbella")

# others single
not_rct_cross <- rob.dat %>%
  filter(!design %in% c("rct", "crossover")) %>%
  filter(!(refid == 131 & user == "Anne_Marbella"))

rob.dat <- bind_rows(am_rct_cross, not_rct_cross) %>%
  select(-study)

# replace correct names
rob.dat <- left_join(rob.dat, study_names[, c(1,2)], by = "refid") %>%
  select(refid, study, everything())

rm(am_rct_cross, not_rct_cross)

# check n same as study_char.dat
length(unique(rob.dat$refid)) == length(unique(study_char.dat$refid))

# * (end)

