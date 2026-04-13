# Loading relevant packages
library(tidyr)
library(rstatix)
library(ggpubr)
library(readxl)

# Setting WD to data folder
setwd("~/Desktop/Project/Data&Results/Dataset")


# Narrowband angular deviation analysis:

# Bringing in Excel data set + assigning it to a data frame
NarrowAD <- read_excel("dataset.xlsx", sheet = 3, range = "A2:C46", col_names = c("id", "obliques", "cardinals"))

# Pivoting data from wide format to long format
NarrowAD <- pivot_longer(
  NarrowAD,
  cols = c(obliques, cardinals),
  names_to = "orientation", values_to = "ang_d") %>%
  convert_as_factor(id, orientation)

# Summary statistics
NarrowAD %>%
  group_by(orientation) %>%
  get_summary_stats(ang_d, type = c("mean_sd"))

# Checking for outliers
NarrowAD %>%
  group_by(orientation) %>%
  identify_outliers(ang_d)

ggboxplot(
  NarrowAD,
  x = "orientation",
  y = "ang_d",
  color = "orientation",
  add = "jitter",
  xlab = "Orientation",
  ylab = "Angular Deviation"
) +
  theme_classic() +
  theme(legend.position = "none")

# Testing for normality - Shapiro-Wilk test + Q-Q plots
NarrowAD %>%
  group_by(orientation) %>%
  shapiro_test(ang_d)

ggqqplot(NarrowAD, "ang_d", facet.by = "orientation")

# Repeated measures t-test
NarrowAD$orientation <- factor(NarrowAD$orientation,
                               levels = c("obliques", "cardinals"))
NarrowAD_Ttest <- NarrowAD %>%
  t_test(ang_d ~ orientation, paired = TRUE) %>%
  add_significance()
NarrowAD_Ttest

# Effect size calculation
NarrowAD %>% cohens_d(ang_d ~ orientation, paired = TRUE)

# Narrowband AD ANOVA + post-hoc - vertical + horizontal vs. obliques:

# Bringing in Excel data set + assigning it to a data frame
NarrowADPost <- read_excel("dataset.xlsx", sheet = 3, range = "E2:H46", col_names = c("id", "oblique", "vertical", "horizontal"))

# Pivoting data from wide format to long format
NarrowADPost <- pivot_longer(
  NarrowADPost,
  cols = c(oblique, vertical, horizontal),
  names_to = "orientation", values_to = "ang_d") %>%
  convert_as_factor(id, orientation)

NarrowADPost %>%
  group_by(orientation) %>%
  get_summary_stats(ang_d, type = c("mean_sd"))

# Testing for outliers
NarrowADPost %>%
  group_by(orientation) %>%
  identify_outliers(ang_d)

ggboxplot(
  NarrowADPost,
  x = "orientation",
  y = "ang_d",
  add = "jitter",
  xlab = "Orientation",
  ylab = "Angular Deviation"
) +
  theme_classic() +
  theme(legend.position = "none")

# Testing for normality - Shapiro-Wilk test + Q-Q plots
NarrowADPost %>%
  group_by(orientation) %>%
  shapiro_test(ang_d)

ggqqplot(NarrowADPost, "ang_d", facet.by = "orientation")

# Repeated measures ANOVA + testing for sphericity
NarrowADPostANOVA <- anova_test(data = NarrowADPost, dv = ang_d, wid = id, within = orientation)

get_anova_table(NarrowADPostANOVA)

# Pairwise comparisons
NarrowADpwc <- NarrowADPost %>%
  pairwise_t_test(ang_d ~ orientation, paired = TRUE, p.adjust.method = "bonferroni")

NarrowADpwc


# Narrowband bias analysis:

# Bringing in Excel data set + assigning it to a data frame
NarrowBias <- read_excel("dataset.xlsx", sheet = 4, range = "K2:M46", col_names = c("id", "max", "min"))

# Pivoting data from wide format to long format
NarrowBias <- pivot_longer(
  NarrowBias,
  cols = c(max, min),
  names_to = "expected_bias_magnitude", values_to = "perceptual_bias") %>%
  convert_as_factor(id, expected_bias_magnitude)

# Summary statistics
NarrowBias %>%
  group_by(expected_bias_magnitude) %>%
  get_summary_stats(perceptual_bias, type = "mean_sd")

# Testing for outliers
NarrowBias %>% 
  group_by(expected_bias_magnitude) %>%
  identify_outliers(perceptual_bias)

ggboxplot(
  NarrowBias,
  x = "expected_bias_magnitude",
  y = "perceptual_bias",
  add = "jitter",
  xlab = "Magnitude of Bias",
  ylab = "Perceptual Bias"
) +
  theme_classic() +
  theme(legend.position = "none")

# Testing for normality - Shapiro-Wilk test + Q-Q plots
NarrowBias %>% 
  group_by(expected_bias_magnitude) %>%
  shapiro_test(perceptual_bias)

ggqqplot(NarrowBias, "perceptual_bias", facet.by = "expected_bias_magnitude")

# Repeated measures t-test
NarrowBias_Ttest <- NarrowBias %>%
  t_test(perceptual_bias ~ expected_bias_magnitude, paired = TRUE) %>%
  add_significance()
NarrowBias_Ttest

# Effect size calculation
NarrowBias %>% cohens_d(perceptual_bias ~ expected_bias_magnitude, paired = TRUE)


# Narrowband bias ANOVA + post-hoc tests - vert max bias + horz max bias vs. min bias:

NarrowBiasPost <- read_excel("dataset.xlsx", sheet = 4, range = "P2:S46", col_names = c("id", "horz_max", "vert_max", "min"))

# Pivoting data from wide format to long format
NarrowBiasPost <- pivot_longer(
  NarrowBiasPost,
  cols = c(horz_max, vert_max, min),
  names_to = "expected_bias_magnitude", values_to = "perceptual_bias") %>%
  convert_as_factor(id, expected_bias_magnitude)

NarrowBiasPost %>%
  group_by(expected_bias_magnitude) %>%
  get_summary_stats(perceptual_bias, type = c("mean_sd"))

# Testing for outliers
NarrowBiasPost %>%
  group_by(expected_bias_magnitude) %>%
  identify_outliers(perceptual_bias)

ggboxplot(
  NarrowBiasPost,
  x = "expected_bias_magnitude",
  y = "perceptual_bias",
  add = "jitter",
  xlab = "Magnitude of Bias",
  ylab = "Perceptual Bias"
) +
  theme_classic() +
  theme(legend.position = "none")

# Testing for normality - Shapiro-Wilk test + Q-Q plots
NarrowBiasPost %>%
  group_by(expected_bias_magnitude) %>%
  shapiro_test(perceptual_bias)

ggqqplot(NarrowBiasPost, "perceptual_bias", facet.by = "expected_bias_magnitude")

# Repeated measures ANOVA + testing for sphericity
NarrowBiasPostANOVA <- anova_test(data = NarrowBiasPost, dv = perceptual_bias, wid = id, within = expected_bias_magnitude)

get_anova_table(NarrowBiasPostANOVA)

# Pairwise comparisons
NarrowBiaspwc <- NarrowBiasPost %>%
  pairwise_t_test(perceptual_bias ~ expected_bias_magnitude, paired = TRUE, p.adjust.method = "bonferroni")











