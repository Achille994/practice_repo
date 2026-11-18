library(ggplot2)
library(tidyverse)
data(diamonds)

#Clean the data selects y and clarity columns ----
newDiamonds <- diamonds |>
  filter(y != 0, !is.na(y)) |>
  select(clarity, y)

#Statistics for each clarity level ----
statVals <- newDiamonds |>
  group_by(clarity) |>
  summarise(
    Count = length(y),
    Min = min(y),
    Quintile1 = quantile(y, 0.20),
    Quintile2 = quantile(y, 0.40),
    Median = median(y),
    Quintile3 = quantile(y, 0.60),
    Quintile4 = quantile(y, 0.80),
    Max = max(y),
    Mean = mean(y),
    SD = sd(y),
    .groups = "drop"
  )
#Rounding stat values ----
statVals <- statVals |>
  mutate(
    across(
      .cols = -c(clarity, Count),
      .fns = ~round(.x, 2)
    )
  )

#Shows the table ----
print(statVals, n = 8)
View(statVals)