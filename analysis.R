# ============================================================
#  Quantitative Data Analysis in HCI
#  Guest Lecture — Mobile HCI
#  Live Demo Script
#
#  Dataset: Biselli et al. (2024)
#           Supporting Informed Choices about Browser Cookies
#           PoPETs 2024(1)
#
#  Run this script section by section during the lecture.
#  Use Ctrl+Enter (Cmd+Enter on Mac) to run one line at a time,
#  or click the line and press Run in the toolbar.
# ============================================================


# ── 0. SETUP ────────────────────────────────────────────────
# Run this block first — loads all packages we need today

library(tidyverse)   # data wrangling + ggplot2
library(readxl)      # read Excel files
library(rstatix)     # tidy statistical tests
library(dunn.test)   # Dunn's post-hoc test
library(effectsize)  # effect sizes (Cohen's d, eta squared)


# ============================================================
# SECTION 5 — DESCRIPTIVE STATISTICS + R
# ============================================================

# ── 1. LOAD & INSPECT THE DATA ──────────────────────────────

# The dataset has two header rows (common in SoSci Survey exports)
# We skip the first technical header row and use the human-readable one

df_raw <- read_excel("cookie_study_data.xlsx",
                     sheet = "ppa_exp_with_varexplanations",
                     skip  = 1)

# Quick look at what we have
glimpse(df_raw)
head(df_raw, 5)


# ── 2. CLEAN & RENAME COLUMNS ───────────────────────────────

df <- df_raw |>
  select(
    experimental_group  = `experimental group`,
    privacy_knowledge   = `privacy knowledge`,
    cookies_accepted    = `number of accepted cookies`,
    sus_score           = `sus score`,
    age,
    gender,
    education,
    ati_score           = `ati score`,
    oplis_score         = `oplis score`
  ) |>
  mutate(
    cookies_accepted   = as.numeric(cookies_accepted),
    experimental_group = factor(experimental_group,
                                levels = c("PPA", "PA", "CG"))
  )

# Check everything looks right
glimpse(df)
summary(df)

# Any missing values?
colSums(is.na(df))


# ── 3. DESCRIPTIVE STATISTICS ───────────────────────────────

# Summary table by group
desc_stats <- df |>
  group_by(experimental_group) |>
  summarise(
    n            = n(),
    mean_cookies = round(mean(cookies_accepted, na.rm = TRUE), 1),
    median_coo   = median(cookies_accepted, na.rm = TRUE),
    sd_cookies   = round(sd(cookies_accepted, na.rm = TRUE), 1),
    mean_sus     = round(mean(sus_score, na.rm = TRUE), 1),
    median_sus   = median(sus_score, na.rm = TRUE)
  )

print(desc_stats)

# Compare with paper Table 3:
# PPA: mean = 76,  median = 65   (our data: ~73.7 / 65)
# PA:  mean = 99,  median = 65   (our data: ~99.2 / 65)
# CG:  mean = 173, median = 231  (our data: ~173.2 / 231)

# Group sizes
table(df$experimental_group)
table(df$privacy_knowledge)


# ── 4. VISUALISATIONS ───────────────────────────────────────

# --- Boxplot: cookies accepted by group ---
ggplot(df, aes(x = experimental_group,
               y = cookies_accepted,
               fill = experimental_group)) +
  geom_boxplot(alpha = 0.8,
               outlier.colour = "grey40",
               outlier.alpha  = 0.6) +
  scale_fill_manual(values = c(
    "PPA" = "#5B8A3C",
    "PA"  = "#A0714F",
    "CG"  = "#C0392B"
  )) +
  labs(
    title = "Cookies Accepted by Experimental Group",
    x     = "Group",
    y     = "Total cookies accepted (min 65 – max 259)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# What do you notice?
# - CG median is much higher (~231) than PPA and PA (both ~65)
# - PPA and PA have a floor effect: many participants chose minimum
# - CG has a wide spread — very different behaviour within the group


# --- Histogram: distribution shape ---
ggplot(df, aes(x = cookies_accepted, fill = experimental_group)) +
  geom_histogram(bins = 20, alpha = 0.8, colour = "white") +
  facet_wrap(~experimental_group) +
  scale_fill_manual(values = c(
    "PPA" = "#5B8A3C",
    "PA"  = "#A0714F",
    "CG"  = "#C0392B"
  )) +
  labs(
    title = "Distribution of Accepted Cookies per Group",
    x     = "Cookies accepted",
    y     = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# The pile at 65 (minimum possible) is very clear for PPA and PA
# CG is spread across the full range — very different pattern


# --- Bar chart with error bars (mean + SE) ---
df |>
  group_by(experimental_group) |>
  summarise(
    m  = mean(cookies_accepted, na.rm = TRUE),
    se = sd(cookies_accepted, na.rm = TRUE) / sqrt(n())
  ) |>
  ggplot(aes(x = experimental_group, y = m, fill = experimental_group)) +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = m - se, ymax = m + se), width = 0.2) +
  scale_fill_manual(values = c(
    "PPA" = "#5B8A3C",
    "PA"  = "#A0714F",
    "CG"  = "#C0392B"
  )) +
  labs(
    title = "Mean Cookies Accepted (± SE)",
    x     = "Group",
    y     = "Mean cookies accepted"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ============================================================
# SECTION 6 — INFERENCE STATISTICS + R
# ============================================================

# ── 5. CHECK NORMALITY ──────────────────────────────────────

# Shapiro-Wilk test per group
# H0: data is normally distributed
# If p < .05 → non-normal → use non-parametric test

normality_check <- df |>
  group_by(experimental_group) |>
  summarise(
    shapiro_p = shapiro.test(cookies_accepted)$p.value
  )

print(normality_check)

# All p < .05 → clearly non-normal
# → We use Kruskal-Wallis (non-parametric alternative to ANOVA)


# Q-Q plots to visualise normality
ggplot(df, aes(sample = cookies_accepted)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  facet_wrap(~experimental_group) +
  labs(title = "Q-Q Plots — Are Cookies Normally Distributed?") +
  theme_minimal()

# Points far from the red line = non-normal
# The strong deviation confirms: use non-parametric tests


# ── 6. CORRELATION — PEARSON & SPEARMAN ─────────────────────

# Question: Is there a relationship between tech affinity (ATI)
# and cookies accepted? Do more tech-savvy users accept fewer cookies?
# (Made-up use case to illustrate correlation)

# Scatter plot first — always look before testing
ggplot(df, aes(x = ati_score, y = cookies_accepted)) +
  geom_point(alpha = 0.4, colour = "#A0714F") +
  geom_smooth(method = "lm", se = TRUE, colour = "#5B8A3C") +
  labs(
    title = "ATI Score vs. Cookies Accepted",
    x     = "ATI score (technology affinity)",
    y     = "Cookies accepted"
  ) +
  theme_minimal()

# Pearson correlation (assumes normality)
cor.test(df$ati_score, df$cookies_accepted, method = "pearson")

# Spearman correlation (non-parametric, use for non-normal data)
cor.test(df$ati_score, df$cookies_accepted, method = "spearman")

# Interpret:
# r = correlation coefficient (-1 to +1)
# Negative r = higher ATI → fewer cookies (if significant)
# p < .05 = statistically significant association
# Remember: correlation ≠ causation!


# ── 7. INDEPENDENT SAMPLES T-TEST ───────────────────────────

# Question: Do male and female participants differ in SUS score?
# (Made-up use case — 2 independent groups, continuous DV)

# Filter to two gender groups only
df_gender <- df |>
  filter(gender %in% c("male", "female"))

# Check normality per group first
df_gender |>
  group_by(gender) |>
  summarise(sw_p = shapiro.test(sus_score)$p.value)

# Independent t-test (Welch — does not assume equal variances)
t.test(sus_score ~ gender,
       data      = df_gender,
       var.equal = FALSE)

# Effect size: Cohen's d
cohens_d(sus_score ~ gender, data = df_gender)

# Interpret:
# t-statistic = standardised difference between means
# p < .05 = groups differ significantly
# Cohen's d: small = 0.2, medium = 0.5, large = 0.8


# ── 8. PAIRED T-TEST ────────────────────────────────────────

# Imagine: participants rated the cookie banner BEFORE and AFTER
# a short privacy awareness video. Did their SUS scores change?
# (Simulated data for illustration — same subjects, two timepoints)

set.seed(42)
n_paired <- 50
before <- rnorm(n_paired, mean = 65, sd = 15)
after  <- before + rnorm(n_paired, mean = 8, sd = 10)
paired_df <- data.frame(before, after)

# Visualise the change
paired_df |>
  pivot_longer(everything(), names_to = "time", values_to = "sus") |>
  mutate(time = factor(time, levels = c("before", "after"))) |>
  ggplot(aes(x = time, y = sus, fill = time)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "SUS Before vs. After Privacy Video (Simulated)",
       x = "Time", y = "SUS Score") +
  theme_minimal() +
  theme(legend.position = "none")

# Paired t-test
t.test(paired_df$after, paired_df$before, paired = TRUE)

# Effect size
cohens_d(after - before ~ 1, data = paired_df)

# Interpret:
# Tests whether the DIFFERENCE (after - before) is significantly ≠ 0
# If p < .05 → the video changed usability ratings


# ── 9. ONE-WAY ANOVA ────────────────────────────────────────

# Question: Do education levels differ in ATI score?
# (Made-up use case — 3 groups, parametric)

# Recode education to 3 levels
df <- df |>
  mutate(edu3 = case_when(
    education <= 3 ~ "low",
    education <= 5 ~ "medium",
    TRUE           ~ "high"
  ) |> factor(levels = c("low", "medium", "high")))

# Check normality
df |>
  group_by(edu3) |>
  summarise(sw_p = shapiro.test(ati_score)$p.value)

# One-way ANOVA
aov_result <- aov(ati_score ~ edu3, data = df)
summary(aov_result)

# Post-hoc: Tukey HSD (which pairs differ?)
TukeyHSD(aov_result)

# Effect size: eta-squared
eta_squared(aov_result)

# Interpret:
# F-statistic = ratio of between-group to within-group variance
# p < .05 = at least one group differs
# η² (eta-squared): small = .01, medium = .06, large = .14


# ── 10. KRUSKAL-WALLIS — THE PAPER'S MAIN TEST ─────────────

# This is what the paper actually uses
# 3 independent groups, non-normal data → Kruskal-Wallis

# Kruskal-Wallis test
kw_result <- kruskal.test(cookies_accepted ~ experimental_group,
                          data = df)
print(kw_result)

# Output:
# Kruskal-Wallis chi-squared = 43.725, df = 2, p-value < .001
# → We reject H0: at least one group differs significantly

# Effect size (epsilon-squared)
df |> kruskal_effsize(cookies_accepted ~ experimental_group)


# ── 11. DUNN'S POST-HOC + EFFECT SIZE r ─────────────────────

# Kruskal-Wallis only tells us "something differs"
# Dunn's test tells us WHICH pairs differ
# BH correction controls false discovery rate across 3 comparisons

dunn_result <- df |>
  dunn_test(cookies_accepted ~ experimental_group,
            p.adjust.method = "BH")

# Add effect size r = |Z| / sqrt(N)
dunn_result <- dunn_result |>
  mutate(r_effect = abs(statistic) / sqrt(n1 + n2))

print(dunn_result)

# Interpret:
# PPA vs PA:  Z = 2.15,  p = .03,   r ≈ .19  (small-medium)
# PPA vs CG:  Z = 6.51,  p < .001,  r ≈ .52  (large)
# PA  vs CG:  Z = 4.20,  p < .001,  r ≈ .37  (medium)
# r conventions: small = .1, medium = .3, large = .5


# ── 12. LINEAR REGRESSION ───────────────────────────────────

# Question: Can OPLIS score (privacy knowledge) predict
# how many cookies a participant accepts?
# Controlling for ATI score and age.

# Simple regression: one predictor
lm_simple <- lm(cookies_accepted ~ oplis_score, data = df)
summary(lm_simple)

# Multiple regression: several predictors simultaneously
lm_multi <- lm(cookies_accepted ~ oplis_score + ati_score + age,
               data = df)
summary(lm_multi)

# Standardised coefficients (compare predictors fairly)
standardize_parameters(lm_multi)

# Interpret:
# β (slope) = change in cookies per unit increase in predictor
# R² = proportion of variance explained
# p per predictor = is this variable significant controlling for others?


# ============================================================
# SECTION 7 — REPORTING
# ============================================================

# ── 13. APA REPORTING TEMPLATE ──────────────────────────────

# Overall test:
# "A Kruskal-Wallis test revealed a significant difference in the
# number of accepted cookies between the three groups,
# χ²(2) = 43.73, p < .001."

# Post-hoc:
# "Post-hoc pairwise comparisons (Dunn's test, BH correction)
# showed that the PPA group (Mdn = 65) accepted significantly
# fewer cookies than both the PA group (Mdn = 65; Z = 2.15,
# p = .03, r = .19) and the control group (Mdn = 231; Z = 6.51,
# p < .001, r = .52). The PA group also accepted significantly
# fewer cookies than the control group (Z = 4.20, p < .001,
# r = .37)."

# Checklist:
# ✓ Test name + statistic + df
# ✓ Exact p-value (or < .001)
# ✓ Group medians
# ✓ Effect size r
# ✓ Correction method (BH)

# Pull the numbers directly from your R output:
cat("Kruskal-Wallis: chi-sq =", round(kw_result$statistic, 3),
    ", df =", kw_result$parameter,
    ", p =", format.pval(kw_result$p.value, digits = 3), "\n")

cat("\nDunn post-hoc results:\n")
print(dunn_result |> select(group1, group2, statistic, p.adj, r_effect))
