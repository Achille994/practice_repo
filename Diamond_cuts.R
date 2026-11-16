library(ggplot2)
library(tidyverse)
data(diamonds)

#Selecting ----
newDiamonds <- diamonds |>
  filter(x != 0, y != 0, z != 0) |>
  select(cut, x, y, z)

# Creating Function ----
targetStats <- function(dataVector) {
  c(
    Count = length(dataVector),
    Min = min(dataVector),
    Q1 = quantile(dataVector, 0.25),
    Median = median(dataVector),
    Q3 = quantile(dataVector, 0.75),
    Max = max(dataVector),
    MAD = mad(dataVector),
    Mean = mean(dataVector),
    SD = sd(dataVector)
  )
}

# Puts all dimensions in one column ----
longDiamonds <- newDiamonds |>
  pivot_longer(
    cols = c(x, y, z),
    names_to = "Variable",
    values_to = "value"
  )

# Calculates each cut and dimensions ----
statVals <- longDiamonds |>
  group_by(cut, Variable) |>
  summarise(
    Count = length(value),
    Min = min(value),
    Q1 = quantile(value, 0.25),
    Median = median(value),
    Q3 = quantile(value, 0.75),
    Max = max(value),
    MAD = mad(value),
    Mean = mean(value),
    SD = sd(value),
    .groups = "drop"
  )

#Rounds all the stat value to 2 decimal places ----
statVals <- statVals |>
  mutate(
    Count = round(Count, 2),
    Min = round(Min, 2),
    Q1 = round(Q1, 2),
    Median = round(Median, 2),
    Q3 = round(Q3, 2),
    Max = round(Max, 2),
    MAD = round(MAD, 2),
    Mean = round(Mean, 2),
    SD = round(SD, 2)
  )

# Displays the stat values ----
print(statVals, n = 15)

# Displays deptth z ----
depthStats <- statVals |>
  filter(Variable == "z")

print(depthStats, n = 5)