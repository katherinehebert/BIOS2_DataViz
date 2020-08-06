# Script to dump a bunch of plot types for the presentation
# based on Coffee & Code dataset

# load packages
library(ggplot2)
library(dplyr)
library(magritte)

# set ggplot theme
theme_set(theme_classic())

# import data
df <- read.csv("data/coffee_code.csv")
df$CoffeeCupsPerDay <- as.character(df$CoffeeCupsPerDay)
# plot of all variable pairs (super difficult to interpret/understand)
plot(df)

# Boxplot demo

# basic scatter plot - not interpretable, raw information
ggplot(df) +
  geom_point(aes(x = CodingWithoutCoffee, y = CodingHours)) +
  labs(x = "Coding without coffee", y = "Coding time (hours/day")

# basic boxplot
ggplot(df) +
  geom_boxplot(aes(x = CodingWithoutCoffee, y = CodingHours)) +
  labs(x = "Coding without coffee", y = "Coding time (hours/day") 

# what if we take the mean, and plot it with uncertainty measurements?
df_mean <- group_by(df, CodingWithoutCoffee) %>%
  summarise(mean_codinghours = mean(CodingHours),
            sd_codinghours = sd(CodingHours),
            se_codinghours = sd(CodingHours)/sqrt(length(CodingHours))) 

# violin plot with mean and error bars
ggplot() +
  geom_violin(data = df, aes(x = CodingWithoutCoffee, y = CodingHours)) +
  geom_linerange(data = df_mean,
                aes(x = CodingWithoutCoffee, 
                    ymin = mean_codinghours - se_codinghours,
                    ymax = mean_codinghours + se_codinghours)) +
  geom_point(data = df_mean, aes(x = CodingWithoutCoffee, y = mean_codinghours), size = 1.2) +
  labs(x = "Coding without coffee", y = "Coding time (hours/day")

# violin plot with jittered raw data
ggplot() +
  geom_violin(data = df, aes(x = CodingWithoutCoffee, y = CodingHours)) +
  geom_jitter(data = df, aes(x = CodingWithoutCoffee, y = CodingHours),
              alpha = .2, width = .05) +
  geom_linerange(data = df_mean,
                 aes(x = CodingWithoutCoffee, 
                     ymin = mean_codinghours - se_codinghours,
                     ymax = mean_codinghours + se_codinghours)) +
  geom_point(data = df_mean, aes(x = CodingWithoutCoffee, y = mean_codinghours), size = 1.2) +
  labs(x = "Coding without coffee", y = "Coding time (hours/day")

# standard error vs. standard deviation  (colours are to be changed)
ggplot(df_mean) +
  geom_errorbar(aes(x = CodingWithoutCoffee, 
                    ymin = mean_codinghours - 1.96*sd_codinghours,
                    ymax = mean_codinghours + 1.96*sd_codinghours), col = "dodgerblue") +
  geom_point(aes(x = CodingWithoutCoffee, y = mean_codinghours), size = 3) +
  labs(x = "Coding without coffee", y = "Coding time (hours/day")
# comparison
ggplot(df_mean) +
  geom_errorbar(aes(x = CodingWithoutCoffee, 
                    ymin = mean_codinghours - 1.96*sd_codinghours,
                    ymax = mean_codinghours + 1.96*sd_codinghours), col = "dodgerblue") +
  geom_errorbar(aes(x = CodingWithoutCoffee, 
                    ymin = mean_codinghours - se_codinghours,
                    ymax = mean_codinghours + se_codinghours), col = "magenta") +
  geom_point(aes(x = CodingWithoutCoffee, y = mean_codinghours), size = 3) +
  labs(x = "Coding without coffee", y = "Coding time (hours/day")


