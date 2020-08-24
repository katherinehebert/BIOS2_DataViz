# Script to dump a bunch test plot examples for the Data Visualization training
# based on Coffee & Code dataset
# from https://www.kaggle.com/devready/coffee-and-code/data

# load packages
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggridges)

# set ggplot theme
theme_set(theme_classic() +
            theme(axis.title = element_text(size = 11, face = "bold"),
                  axis.text = element_text(size = 11),
                  plot.title = element_text(size = 13, face = "bold"),
                  legend.title = element_text(size = 11, face = "bold"),
                  legend.text = element_text(size = 10)))

# import data
df <- read.csv("data/coffee_code.csv")

# plot of all variable pairs (super difficult to interpret/understand)
plot(df)

# Dimensions - overwhelming bubble plot example -----

# Plot 1: Dimensions example (bubble plot)

# can comment out col and size to see how the plot changes
ggplot(na.omit(df)) +
  geom_jitter(aes(y = CodingHours, 
                 x = CoffeeCupsPerDay,
                 col = CoffeeType,
                 shape = CoffeeSolveBugs,
  ), size = 3
  ) +
  # set labels for each plot element
  labs(title = "Does coffee help programmers code?",
       x = "Coffee consumption (cups/day)",
       y = "Productivity (hours/day)",
       col = "Coffee Type",
       shape = "CoffeeSolveBugs"
  ) +
  scale_color_brewer(palette = "Dark2") 
ggsave("figures/bubbleplot_coffee_4dimensions.png", 
       device = "png", width = 20, height = 15, units = "cm", dpi = "retina")


# Visualizing honestly - Boxplot example ----
# different ways to show raw data & uncertainty

# set labels to be used in all plots
boxplot_labels <- labs(title = "Does coffee help programmers code?",
                       x = "Coding without coffee", 
                       y = "Coding time (hours/day)") 

# this variable (CodingWithoutCoffee) is negative, which is harder to understand
# (i.e. "No" means they drink coffee...)
# so, let's transform it into a better variable to use for plots
df$CodingWithCoffee <- gsub("No", "Always", df$CodingWithoutCoffee)
df$CodingWithCoffee <- gsub("Yes", "Never", df$CodingWithCoffee)
# convert to factor and set levels so they show up in a logical order
df$CodingWithCoffee <- factor(df$CodingWithCoffee,
                              levels = c("Never", "Sometimes", "Always"))

# Basic scatter plot - not interpretable, raw information
ggplot(df) +
  # jitter introduces some slight noise to space out data points, which helps to
  # show overlapping points
  geom_jitter(aes(x = CodingWithCoffee, 
                  y = CodingHours), 
              alpha = .5, width = .05) +
  boxplot_labels

# basic boxplot
ggplot(df) +
  geom_boxplot(aes(x = CodingWithoutCoffee, 
                   y = CodingHours)) +
  boxplot_labels 

# what if we take the mean, and plot it with uncertainty measurements?
df_mean <- group_by(df, CodingWithoutCoffee) %>%
  summarise(mean_codinghours = mean(CodingHours),
            sd_codinghours = sd(CodingHours),
            se_codinghours = sd(CodingHours)/sqrt(length(CodingHours))) 

# violin plot with mean and error bars
ggplot() +
  geom_violin(data = df, aes(x = CodingWithoutCoffee, 
                             y = CodingHours)) +
  geom_linerange(data = df_mean,
                 aes(x = CodingWithoutCoffee, 
                     ymin = mean_codinghours - se_codinghours,
                     ymax = mean_codinghours + se_codinghours)) +
  geom_point(data = df_mean, aes(x = CodingWithoutCoffee, 
                                 y = mean_codinghours), size = 1.2) +
  boxplot_labels

# violin plot with jittered raw data
ggplot() +
  geom_violin(data = df, aes(x = CodingWithoutCoffee, 
                             y = CodingHours)) +
  geom_jitter(data = df, aes(x = CodingWithoutCoffee, 
                             y = CodingHours),
              alpha = .2, width = .05) +
  geom_linerange(data = df_mean,
                 aes(x = CodingWithoutCoffee, 
                     ymin = mean_codinghours - se_codinghours,
                     ymax = mean_codinghours + se_codinghours)) +
  geom_point(data = df_mean, 
             aes(x = CodingWithoutCoffee, 
                 y = mean_codinghours), size = 3) +
  boxplot_labels



# standard error vs. standard deviation  (colours are to be changed)
ggplot(df_mean) +
  geom_errorbar(aes(x = CodingWithoutCoffee, 
                    ymin = mean_codinghours - 1.96*sd_codinghours,
                    ymax = mean_codinghours + 1.96*sd_codinghours), 
                col = "dodgerblue", width = .2) +
  geom_point(aes(x = CodingWithoutCoffee, y = mean_codinghours), size = 3) +
  boxplot_labels

# plotting both error bars for comparison
ggplot(df_mean) +
  geom_errorbar(aes(x = CodingWithoutCoffee, 
                    ymin = mean_codinghours - 1.96*sd_codinghours,
                    ymax = mean_codinghours + 1.96*sd_codinghours), 
                col = "dodgerblue", width = .2) +
  geom_errorbar(aes(x = CodingWithoutCoffee, 
                    ymin = mean_codinghours - se_codinghours,
                    ymax = mean_codinghours + se_codinghours), 
                col = "magenta", width = .2) +
  geom_point(aes(x = CodingWithoutCoffee, 
                 y = mean_codinghours), 
             size = 3) +
  boxplot_labels