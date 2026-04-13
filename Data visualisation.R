# Loading relevant packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)
library(readxl)
library(rstatix)

# Setting WD to data folder
setwd("~/Desktop/Project/Data&Results/Dataset")


# Narrowband angular deviation graph creation:

# Bringing in Excel data set + assigning it to a data frame
NarrowADdf <- read_xlsx("dataset.xlsx", sheet = 9, range = "A1:FY46") 

# Renaming the first column to 'id'
names(NarrowADdf)[1] <- "id"

# Pivoting data from wide format to long format
NarrowADdf <- pivot_longer(
  NarrowADdf,
  cols = -id,
  names_to = "orientation", values_to = "response") %>%
  convert_as_factor(id) %>%
  mutate(
    orientation = as.numeric(gsub("^X", "", orientation))
  )

# Computing summaries at orientations
sum_df <- NarrowADdf %>%
  group_by(orientation) %>%
  summarise(
    mean = mean(response, na.rm = TRUE),
    sd = sd(response, na.rm = TRUE),
    .groups = "drop"
  )

# Fitting smooth models to mean and SD
fit_mean <- gam(mean ~ s(orientation, k = 25), data = sum_df)
fit_sd <- gam(sd ~ s(orientation, k = 25), data = sum_df)

# Predicting on grid
smooth_df <- tibble(
  orientation = seq(min(sum_df$orientation), max(sum_df$orientation), length.out = 500)
  ) %>%
  mutate(
    mean = predict(fit_mean, newdata = data.frame(orientation = orientation)),
    sd = predict(fit_sd, newdata = data.frame(orientation = orientation)),
    ymin = mean - sd,
    ymax = mean + sd
  )

# Plot
ggplot(smooth_df, aes(x = orientation)) +
  geom_ribbon( # Shaded SD region
    aes(ymin = ymin, ymax = ymax),
    fill = "#d99ba7",
    alpha = 0.6
  ) +
  geom_line( # Solid mean line
    aes(y = mean),
    color = "#8b2334",
    linewidth = 1.8,
    lineend = "round"
  ) +
  scale_x_continuous( # X-axis scale
    breaks = c(0, 45, 90, 135, 180)
  ) +
  scale_y_continuous( #Y-axis scale
    limits = c(5, 25)
  ) +
  theme_classic(base_size = 20) + 
  labs( # Setting theme
    x = "Orientation (degr.)",
    y = "Mean Angular Deviation (degr.)"
  )

# Narrowband bias graph creation:

# Bringing in Excel data set + assigning it to a data frame
NarrowBiasdf <- read_xlsx("dataset.xlsx", sheet = 10, range = "A1:FY46") 

# Renaming the first column to 'id'
names(NarrowBiasdf)[1] <- "id"

# Pivoting data from wide format to long format
NarrowBiasdf <- pivot_longer(
  NarrowBiasdf,
  cols = -id,
  names_to = "orientation", values_to = "response_bias") %>%
  convert_as_factor(id) %>%
  mutate(
    orientation = as.numeric(gsub("^X", "", orientation))
  )

# Computing summaries at orientations
sum_df <- NarrowBiasdf %>%
  group_by(orientation) %>%
  summarise(
    mean = mean(response_bias, na.rm = TRUE),
    sd = sd(response_bias, na.rm = TRUE),
    ymin = mean - sd,
    ymax = mean + sd,
    .groups = "drop"
  )

# Fitting smooth models to mean and SD
fit_mean <- gam(mean ~ s(orientation, k = 25), data = sum_df)
fit_min <- gam(ymin ~ s(orientation, k = 25), data = sum_df)
fit_max <- gam(ymax ~ s(orientation, k = 25), data = sum_df)

# Predict on grid
smooth_df <- tibble(
  orientation = seq(min(sum_df$orientation), max(sum_df$orientation), length.out = 500)
) %>%
  mutate(
    mean = predict(fit_mean, newdata = data.frame(orientation = orientation)),
    ymin = predict(fit_min, newdata = data.frame(orientation = orientation)),
    ymax = predict(fit_max, newdata = data.frame(orientation = orientation))
  )

# Plot
ggplot(smooth_df, aes(x = orientation)) +
  geom_ribbon( # Shaded SD region
    aes(ymin = ymin, ymax = ymax),
    fill = "#d99ba7",
    alpha = 0.6
  ) +
  geom_line( # Solid mean line
    aes(y = mean),
    color = "#8b2334",
    linewidth = 1.8,
    lineend = "round"
  ) +
  geom_hline( # Intercept line separating -/+
    yintercept = 0,
    color = "black",
    linewidth = 1
  ) +
  scale_x_continuous( # X-axis scale
    breaks = c(0, 45, 90, 135, 180)
  ) +
  scale_y_continuous( # Y-axis scale
    breaks = c(-5, 0, 5, 10, 15, 20)
  )
  theme_classic(base_size = 20) +
  labs( # Setting theme
    x = "Orientation (degr.)",
    y = "Bias (degr.)"
  )


