# 0. Libraries
library(tidyverse); library(readxl)
library(rstatix);   library(effectsize)

# 1. Load real data (two-row header → skip=1)
df <- read_excel('cookie_study_data.xlsx',
                 sheet='ppa_exp_with_varexplanations', skip=1) |>
  select(experimental_group=`experimental group`,
         cookies_accepted=`number of accepted cookies`,
         sus_score=`sus score`, age, gender,
         ati_score=`ati score`, oplis_score=`oplis score`,
         privacy_knowledge=`privacy knowledge`) |>
  mutate(cookies_accepted=as.numeric(cookies_accepted),
         experimental_group=factor(experimental_group,
                                   levels=c('PPA','PA','CG')))

# 2. Descriptives + plots
df |> group_by(experimental_group) |>
  summarise(n=n(), mean=mean(cookies_accepted),
            median=median(cookies_accepted))
ggplot(df, aes(experimental_group, cookies_accepted,
               fill=experimental_group)) +
  geom_boxplot() + theme_minimal()

# 3. Normality check
## Shapiro Wilk Test
df |> group_by(experimental_group) |>
  summarise(sw=shapiro.test(cookies_accepted)$p.value)

## QQ Plots
ggplot(df, aes(sample = cookies_accepted)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  facet_wrap(~experimental_group) +
  labs(
    title = "Q-Q Plots — Cookies Accepted by Group",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal()


# 4. Kruskal-Wallis + effect size
kruskal.test(cookies_accepted~experimental_group, df)
df |> kruskal_effsize(cookies_accepted~experimental_group)

# 5. Post-hoc + r
df |> dunn_test(cookies_accepted~experimental_group,
                p.adjust.method='BH') |>
  mutate(r = abs(statistic)/sqrt(n1+n2))
