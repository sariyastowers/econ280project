library(haven)
library(dplyr)
library(fixest)
library(modelsummary)
library(tibble)
library(kableExtra)
library(gt) 


setwd("/Users/sariya/Documents/documents_local/econ280project/")
data <- read_dta("data/02_analysis/ms_blel_jpal_wide.dta")

latex_path <- "results/table2.tex"

set.seed(12345)

# ****************** REGRESSIONS ****************** #

reg_ols_m <- feols(m_theta_mle2 ~ treat + m_theta_mle1, data = data)
reg_ols_h <- feols(h_theta_mle2 ~ treat + h_theta_mle1, data = data)

reg_fe_m <- feols(
  m_theta_mle2 ~ treat + m_theta_mle1 | strata,
  data = data,
  vcov = ~strata
)

reg_fe_h <- feols(
  h_theta_mle2 ~ treat + h_theta_mle1 | strata,
  data = data,
  vcov = ~strata
)

# ****************** EXPORT RESULTS ****************** #

model_list <- list("Math (1)" = reg_ols_m, "Hindi (2)" = reg_ols_h, 
                   "Math (3)" = reg_fe_m, "Hindi (4)" = reg_fe_h)

gof_mapping <- tribble(
  ~raw,           ~clean,         ~fmt,
  "nobs",         "Observations", 0,
  "r.squared",    "R-squared",    2
)

coef_map <- c(
  "treat"         = "Treatment",
  "m_theta_mle1"  = "Baseline Score",
  "h_theta_mle1"  = "Baseline Score",
  "(Intercept)"   = "Coefficient"
)


table_2 <- modelsummary(
  model_list,
  stars = FALSE,
  fmt = 3,
  gof_map = gof_mapping,
  coef_map = coef_map,
  output = "gt",
)

table_2_final <- table_2 %>%
  tab_style(
    style = cell_text(weight = "normal"),
    locations = cells_column_labels(columns = everything())
  )

print(table_2_final)
