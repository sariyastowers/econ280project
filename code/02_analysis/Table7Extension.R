library(haven)
library(dplyr)
library(fixest)
library(modelsummary)
library(tibble)
library(kableExtra)
library(gt) 

setwd("/Users/sariya/Documents/documents_local/econ280project/")
data_jpal <- read_dta("data/02_analysis/ms_blel_jpal_wide.dta")
data_school <- read_dta("data/02_analysis/sc_results.dta")
latex_path <- "results/table7.tex"
excel_path <- "results/table7.xls"

set.seed(12345)

## ****************** DATA PREPARATION ****************** ##

data_school <- data_school %>%
  filter(year == "2015-16") %>%
  mutate(group = as.factor(paste(school, class))) %>%
  filter(!is.na(treatment))

standardize_scores <- function(df, score_var, group_var) {
  df %>%
    group_by({{group_var}}) %>%
    mutate(
      mean_c = mean({{score_var}}[treatment == 0], na.rm = TRUE),
      sd_c = sd({{score_var}}[treatment == 0], na.rm = TRUE),
      z_score = ({{score_var}} - mean_c) / sd_c
    ) %>%
    ungroup() %>%
    select(-mean_c, -sd_c)
}

data_school <- data_school %>%
  standardize_scores(math_term2_sa2, group) %>% rename(z_math = z_score) %>%
  standardize_scores(science_term2_sa2, group) %>% rename(z_science = z_score) %>%
  standardize_scores(social_term2_sa2, group) %>% rename(z_social = z_score) %>%
  standardize_scores(lang2_term2_sa2, group) %>% rename(z_hindi = z_score) %>%
  standardize_scores(lang1_term2_sa2, group) %>% rename(z_english = z_score) %>%
  rowwise() %>%
  mutate(z_aggregate = mean(c(z_math, z_hindi, z_science, z_social, z_english), na.rm = TRUE)) %>%
  ungroup() %>%
  select(st_id, school, class, treatment, starts_with("z_"), finalresult, group)

data <- data_school %>%
  left_join(data_jpal %>% select(st_id, strata, ms_center1, m_theta_mle1, h_theta_mle1), by = "st_id") %>%
  mutate(center_id = as.factor(ms_center1)) %>%
  filter(!is.na(strata))

## ****************** REGRESSIONS ****************** ##

reg_hindi <- feols(z_hindi ~ treatment + h_theta_mle1 + i(school) + i(center_id) | strata, data = data, vcov = ~strata)
reg_math <- feols(z_math ~ treatment + m_theta_mle1 + i(school) + i(center_id) | strata, data = data, vcov = ~strata)
reg_science <- feols(z_science ~ treatment + m_theta_mle1 + h_theta_mle1 + i(school) + i(center_id) | strata, data = data, vcov = ~strata)
reg_social <- feols(z_social ~ treatment + m_theta_mle1 + h_theta_mle1 + i(school) + i(center_id) | strata, data = data, vcov = ~strata)
reg_english <- feols(z_english ~ treatment + m_theta_mle1 + h_theta_mle1 + i(school) + i(center_id) | strata, data = data, vcov = ~strata)
reg_aggregate <- feols(z_aggregate ~ treatment + m_theta_mle1 + h_theta_mle1 + i(school) + i(center_id) | strata, data = data, vcov = ~strata)

model_list <- list(
  "Hindi (1)" = reg_hindi, "Math (2)" = reg_math, "Science (3)" = reg_science, 
  "Social Sciences (4)" = reg_social, "English (5)" = reg_english, "Aggregate (6)" = reg_aggregate
)

gof_mapping <- tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "Observations", 0,
  "r.squared", "R-squared", 2, 
  "vcov.strata", "Clustered S.E. (Strata)", 0 
)

coef_map <- c(
  "treatment" = "Treatment",
  "h_theta_mle1" = "Baseline Score (Hindi)",
  "m_theta_mle1" = "Baseline Score (Math)"
)

## ****************** EXPORT RESULTS ****************** ##

table_7 <- modelsummary(
  model_list,
  stars = FALSE,
  fmt = 3,
  gof_map = gof_mapping,
  coef_map = coef_map,
  output = "gt",
)

table_7_final <- table_7 %>%
  tab_style(
    style = cell_text(weight = "normal"),
    locations = cells_column_labels(columns = everything())
  )

print(table_7_final)

gtsave(table_7_final, filename = latex_path)