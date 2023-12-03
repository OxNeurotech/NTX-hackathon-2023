
library(dplyr)
library(forcats)
library(readxl)
library(tidyverse)


df <- read.csv("Muse_S01.csv")
df <- read.csv("Muse_J01.csv")
df <- read.csv("Muse_R01.csv")
df <- df[-1, ]

df <- df %>%
  mutate(result = Alpha_AF8 - Alpha_AF7)

mean_result <- median(df$result, na.rm = TRUE)

print(paste("Mean of the results:", mean_result))


df <- df %>%
  mutate(betaa = Beta_AF8 + Beta_AF7,
         thetaa = Theta_AF8 + Theta_AF7,
         ratio = thetaa / betaa)


median_values <- df %>%
  summarize(median_betaa = median(betaa, na.rm = TRUE),
            median_v = median(thetaa, na.rm = TRUE),
            median_ratio = median(ratio, na.rm = TRUE))

print(median_values)




