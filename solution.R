# ============================================================
#  Quantitative Data Analysis in HCI
#  Guest Lecture — Mobile HCI
#
#  SOLUTION — SUS Analysis
#
#  Only open this after attempting the exercise yourself!
# ============================================================


library(tidyverse)
library(readxl)
library(rstatix)
library(dunn.test)
library(effectsize)

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

sus_desc <- df |>
  group_by(experimental_group) |>
  summarise(
    n          = n(),
    mean_sus   = round(mean(sus_score, na.rm = TRUE), 1),
    median_sus = median(sus_score, na.rm = TRUE),
    sd_sus     = round(sd(sus_score, na.rm = TRUE), 1)
  )

print(sus_desc)

# PPA has the highest mean SUS (~83.5) → most usable
# CG has the lowest (~63.1) → least usable
# Pattern mirrors the cookies result: PPA > PA > CG


# ============================================================
# TASK 2 — Boxplot
# ============================================================

ggplot(df, aes(x = experimental_group,
               y = sus_score,
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
    title = "SUS Scores by Experimental Group",
    x     = "Group",
    y     = "SUS Score (0 – 100)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# PPA box is highest and relatively compact
# CG box is lowest with the widest spread
# All three boxes are clearly separated — the effect is visible


# ============================================================
# TASK 3 — Normality Check
# ============================================================

# Shapiro Wilk Test
normality_sus <- df |>
  group_by(experimental_group) |>
  summarise(
    shapiro_p = shapiro.test(sus_score)$p.value
  )

print(normality_sus)

# QQ Plots
ggplot(df, aes(sample = sus_score)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  facet_wrap(~experimental_group) +
  labs(
    title = "Q-Q Plots — SUS Score by Group",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk shows p < .05 for at least one group
# → Use non-parametric test (Kruskal-Wallis)
# Note: even if p > .05, Kruskal-Wallis is a safe conservative choice


# ============================================================
# TASK 4 — Statistical Test
# ============================================================

kw_sus <- kruskal.test(sus_score ~ experimental_group,
                       data = df)

print(kw_sus)

# Expected output:
# Kruskal-Wallis chi-squared ≈ 29.80, df = 2, p < .001
# → We reject H0: SUS scores differ significantly across groups

# Effect size
df |> kruskal_effsize(sus_score ~ experimental_group)


# ============================================================
# TASK 5 — Post-Hoc Test + Effect Size
# ============================================================

dunn_sus <- df |>
  dunn_test(sus_score ~ experimental_group,
            p.adjust.method = "BH")

dunn_sus <- dunn_sus |>
  mutate(r_effect = abs(statistic) / sqrt(n1 + n2))

print(dunn_sus)

# All three pairs are significant (p.adj < .05):
# PPA vs PA:  p < .01,   r ≈ .31  (small-medium)
# PPA vs CG:  p < .001,  r ≈ .52  (medium-large)
# PA  vs CG:  p = .03,   r ≈ .21  (small-medium)

# APA result sentence:
# "A Kruskal-Wallis test revealed a significant difference in
# SUS scores across the three groups, χ²(2) = 29.80, p < .001.
# Post-hoc pairwise comparisons (Dunn's test, BH correction)
# showed that all three groups differed significantly: the PPA
# group reported the highest usability (M = 84.1; vs PA:
# Z = -3.12, p < .01, r = .27; vs CG: Z = -5.43, p < .001,
# r = .47), and the PA group reported higher usability than
# the control group (Z = -2.18, p = .03, r = .19)."

cat("\nAPA summary:\n")
cat("Kruskal-Wallis: chi-sq =", round(kw_sus$statistic, 2),
    ", df =", kw_sus$parameter,
    ", p =", format.pval(kw_sus$p.value, digits = 3), "\n")
cat("\nPost-hoc (Dunn, BH):\n")
print(dunn_sus |> select(group1, group2, statistic, p.adj, r_effect))


# ============================================================
# BONUS — Correlation
# ============================================================

# Scatter plot
ggplot(df, aes(x = oplis_score, y = sus_score)) +
  geom_point(alpha = 0.4, colour = "#A0714F") +
  geom_smooth(method = "lm", se = TRUE, colour = "#5B8A3C") +
  labs(title = "OPLIS Score vs. SUS Score",
       x = "OPLIS score (privacy knowledge)",
       y = "SUS score") +
  theme_minimal()

# Spearman correlation
cor_result <- cor.test(df$oplis_score, df$sus_score,
                       method = "spearman")
print(cor_result)

# Interpret:
# If r is positive → higher privacy knowledge → higher usability rating
# This makes sense: users who understand cookies better can appreciate
# the banner's design more
# But remember: correlation ≠ causation!
# A third variable (e.g. general tech affinity) could drive both
