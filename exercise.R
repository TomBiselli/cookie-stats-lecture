# ============================================================
#  Quantitative Data Analysis in HCI
#  Guest Lecture — Mobile HCI
#
#  EXERCISE — Replicate the SUS Analysis
#
#  Instructions:
#  Replace every ___ with the correct code.
#  Run each section with Ctrl+Enter (Cmd+Enter on Mac).
#  Check your results against the expected output in the comments.
#  If you get stuck → open solution.R
# ============================================================


# ── SETUP ───────────────────────────────────────────────────
# Already loaded from analysis.R — run this if starting fresh

library(tidyverse)
library(readxl)
library(rstatix)
library(dunn.test)
library(effectsize)

# Load and clean data (copy from analysis.R steps 1-2)
df_raw <- read_excel("cookie_study_data.xlsx",
                     sheet = "ppa_exp_with_varexplanations",
                     skip  = 1)

df <- df_raw |>
  select(
    experimental_group  = `experimental group`,
    privacy_knowledge   = `privacy knowledge`,
    cookies_accepted    = `number of accepted cookies`,
    sus_score           = `sus score`,
    age, gender, education,
    ati_score           = `ati score`,
    oplis_score         = `oplis score`
  ) |>
  mutate(
    cookies_accepted   = as.numeric(cookies_accepted),
    experimental_group = factor(experimental_group,
                                levels = c("PPA", "PA", "CG"))
  )


# ============================================================
# TASK 1 — Descriptive Statistics
# ============================================================
# Compute mean, median, and SD of sus_score per group.
# Which group rated usability highest?

sus_desc <- df |>
  group_by(___) |>
  summarise(
    n          = n(),
    mean_sus   = round(mean(___, na.rm = TRUE), 1),
    median_sus = median(___, na.rm = TRUE),
    sd_sus     = round(sd(___, na.rm = TRUE), 1)
  )

print(sus_desc)

# Expected output (approximately):
# PPA: mean ≈ 83.5,  median ≈ 87.5
# PA:  mean ≈ 72.8,  median ≈ 77.5
# CG:  mean ≈ 63.1,  median ≈ 67.5


# ============================================================
# TASK 2 — Boxplot
# ============================================================
# Create a boxplot of sus_score by experimental_group.
# Use the same colour scheme as the cookies boxplot.

ggplot(df, aes(x = ___,
               y = ___,
               fill = ___)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c(
    "PPA" = "#5B8A3C",
    "PA"  = "#A0714F",
    "CG"  = "#C0392B"
  )) +
  labs(
    title = ___,
    x     = "Group",
    y     = "SUS Score (0 – 100)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# What does the distribution tell you?
# Which group has the highest median? The most spread?


# ============================================================
# TASK 3 — Normality Check
# ============================================================
# Check whether sus_score is normally distributed per group.
# Use the Shapiro-Wilk test.

normality_sus <- df |>
  group_by(___) |>
  summarise(
    shapiro_p = shapiro.test(___)$p.value
  )

print(normality_sus)

# If p < .05 → non-normal → which test should you use?
# Write your conclusion as a comment:
# Conclusion: ___


# ============================================================
# TASK 4 — Statistical Test
# ============================================================
# Run the appropriate test to compare SUS scores across all 3 groups.

kw_sus <- kruskal.test(___ ~ ___,
                       data = ___)

print(kw_sus)

# Effect size
df |> kruskal_effsize(___ ~ ___)

# Fill in the blanks:
# chi-squared = ___
# df = ___
# p-value = ___
# Conclusion: ___


# ============================================================
# TASK 5 — Post-Hoc Test + Effect Size
# ============================================================
# Run Dunn's post-hoc test with BH correction.
# Add effect size r = |Z| / sqrt(N).
# Which pairs differ significantly?

dunn_sus <- df |>
  dunn_test(___ ~ ___,
            p.adjust.method = ___)

dunn_sus <- dunn_sus |>
  mutate(r_effect = abs(___) / sqrt(___ + ___))

print(dunn_sus)

# Which pairs are significant (p.adj < .05)?
# ___

# Write one result sentence using the APA template:
# "A Kruskal-Wallis test revealed ..."
# ___


# ============================================================
# BONUS — Correlation
# ============================================================
# Is there a relationship between OPLIS score (privacy knowledge)
# and SUS score? Do users with higher privacy knowledge
# rate the banner as more or less usable?

# Scatter plot
ggplot(df, aes(x = ___, y = ___)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "OPLIS Score vs. SUS Score",
       x = "OPLIS score (privacy knowledge)",
       y = "SUS score") +
  theme_minimal()

# Spearman correlation (OPLIS is ordinal)
cor.test(___, ___, method = "spearman")

# Interpret the result:
# r = ___
# p = ___
# Conclusion: ___
