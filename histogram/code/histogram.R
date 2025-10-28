library(dplyr)
library(ggplot2)
library(haven)

data <- read_dta("~/Documents/econ280project/histogram/data/ms_blel_jpal_long.dta")

##  1: Filter the data for round 1
data_round1 <- data_file %>%
  filter(round == 1)

##  2: Prepare the grade variable for plotting
grade_levels <- c(4, 5, 6, 7, 8, 9)
grade_labels <- c("Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9")

# Convert 'st_grade1' to a factor to ensure correct ordering and labeling
data_round1$grade_factor <- factor(
  data_round1$st_grade1,
  levels = grade_levels,
  labels = grade_labels
)

##  3: Create the bar chart (frequency plot)

grade_plot <- ggplot(data_round1, aes(x = grade_factor)) +
  geom_bar(fill = "#0072B2") + 
  labs(
    title = "Frequency Distribution of Student Grades (Baseline)",
    x = "Student Grade",
    y = "Number of Observations (Students)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

print(grade_plot)

ggsave("grade_distribution.png", plot = grade_plot, width = 8, height = 6)

## 4: Plot math scores
math_histogram <- ggplot(data_round1, aes(x = per_math)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Proportion Correct on Math Test (Baseline)",
    x = "Proportion Correct on Math Test",
    y = "Frequency (Number of Students)"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
  theme_minimal()

print(math_histogram)

ggsave("math_distribution.png", plot = math_histogram, width = 8, height = 6)