# Script to make animated plot of volcano eruptions over time

# Load libraries:
library(dplyr) # data manipulation
library(ggplot2) # plotting
library(gganimate) # animation
library(gifski) # creating gifs

# set ggplot theme
theme_set(theme_classic() +
            theme(axis.title = element_text(size = 13),
                  axis.text = element_text(size = 12),
                  plot.title = element_text(size = 15, face = "bold"),
                  ))


# function to floor a year to the decade
floor_decade = function(value){return(value - value %% 10)}

# read data 
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
# make a time series dataset (number of eruptions per year)
eruptions$start_decade = floor_decade(eruptions$start_year)
eruptions_ts <- table(eruptions$start_decade) %>% as.data.frame()
df <- data.frame(decade = as.numeric(as.character(eruptions_ts$Var1)), 
                 n_eruptions = eruptions_ts$Freq)

p <- ggplot(filter(df, between(decade, 0, 2019)), aes(x = decade, y = n_eruptions)) +
  geom_line(aes(col = decade), lwd = 1.2) +
  geom_point() +
  theme_classic() +
  scale_colour_viridis_c(option = "magma", end = .8) +
  labs(x = "", y = "Number of eruptions", title = "Volcano eruptions per decade") +
  theme(legend.position = "none") +
  # gganimate part:
  transition_reveal(decade) 
animate(p, duration = 7, fps = 20, width = 700, height = 300, renderer = gifski_renderer())
anim_save("figures/volcano_eruptions.gif")
