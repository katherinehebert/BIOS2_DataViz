# Script to generate plots with various ways of representing uncertainty, based 
# Coffee & Code dataset from https://www.kaggle.com/devready/coffee-and-code/data

# set-up & data manipulation ---------------------------------------------------


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

# set labels to be used in all plots
coffee_labels <- labs(title = "Does coffee help programmers code?",
                      x = "Coffee consumption \nwhile coding", 
                      y = "Coding time \n(hours/day)") 

# the variable CodingWithoutCoffee is negative, which is harder to understand
# (i.e. "No" means they drink coffee...). So, let's transform it into a more 
# intuitive variable!
df$CodingWithCoffee <- gsub("No", "Always", df$CodingWithoutCoffee)
df$CodingWithCoffee <- gsub("Yes", "Never", df$CodingWithCoffee)
# convert to factor and set levels so they show up in a logical order
df$CodingWithCoffee <- factor(df$CodingWithCoffee,
                              levels = c("Never", "Sometimes", "Always"))

# calculate summary statistics for the variable of choice
df_summary <- group_by(df, CodingWithoutCoffee) %>%
  summarise(
    # mean
    mean_codinghours = mean(CodingHours), 
    # standard deviation
    sd_codinghours = sd(CodingHours), 
    # standard error
    se_codinghours = sd(CodingHours)/sqrt(length(CodingHours)))


# 1. Error bars (standard error) -----------------------------------------------

ggplot(df_summary) +
  geom_errorbar(aes(x = CodingWithoutCoffee, 
                    ymin = mean_codinghours - se_codinghours,
                    ymax = mean_codinghours + se_codinghours), 
                width = .2) +
  geom_point(aes(x = CodingWithoutCoffee, y = mean_codinghours), 
             size = 3) +
  coffee_labels + ylim(0,10)
ggsave("figures/coffee_errorbars.png", width = 5, height = 3, units = "in")

# 2. Boxplot -------------------------------------------------------------------

ggplot(df) +
  geom_boxplot(aes(x = CodingWithoutCoffee, y = CodingHours)) +
  coffee_labels
ggsave("figures/coffee_boxplot.png", width = 5, height = 3, units = "in")


# 3. Jitter plot with violin ---------------------------------------------------



# 4. Density ridge plot --------------------------------------------------------

ggplot(df) + 
  aes(y = CodingWithCoffee, x = CodingHours, fill = stat(x)) +
  geom_density_ridges_gradient(scale = 1.9, size = .2, rel_min_height = 0.005) +
  # colour palette (gradient according to CodingHours)
  scale_fill_viridis_c(option = "magma", direction = -1) +
  # remove legend - it's not necessary here!
  theme(legend.position = "none") +
  labs(title = coffee_labels$title, 
       x = coffee_labels$y, 
       y = coffee_labels$x)
ggsave("figures/coffee_density_ridges.png", width = 5, height = 3, units = "in")