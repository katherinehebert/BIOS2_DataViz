# Script to generate plots to demonstrate how combinations of information dimensions
# can become overwhelming and difficult to interpret.

# set-up & data manipulation ---------------------------------------------------

# load packages
library(ggplot2) # for plots, built layer by layer
library(dplyr) # for data manipulation
library(magrittr) # for piping
library(plotly) # interactive plots

# set ggplot theme
theme_set(theme_classic() +
            theme(axis.title = element_text(size = 11, face = "bold"),
                  axis.text = element_text(size = 11),
                  plot.title = element_text(size = 13, face = "bold"),
                  legend.title = element_text(size = 11, face = "bold"),
                  legend.text = element_text(size = 10)))

# import data
# more info on this dataset: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-28/readme.md
penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

# 1. Ineffective combinations: Luminance & shading -----------------------------

# get some nice colours from viridis (magma)
sp_cols <- viridis::viridis_pal(option = "magma")(5)[2:4]

# create the plot
ggplot(penguins) +
  geom_point(aes(y = bill_length_mm, x = bill_depth_mm, 
                 col = species, # hue
                 alpha = log(body_mass_g)), # luminance
             size = 2.5) +
  labs(x = "Bill depth (mm)", y = "Bill length (mm)", 
       col = "Species", alpha = "Body mass (g)") +
  scale_color_manual(values = sp_cols)
ggsave("figures/penguins_incompatible1.png", width = 6, height = 3, units = "in")

# 2. Ineffective combinations: Sizes and shapes --------------------------------

ggplot(penguins) +
  geom_point(aes(y = bill_length_mm, x = bill_depth_mm, 
                 shape = species, # shape
                 size = log(body_mass_g)), alpha = .7) + # size
  scale_size(range = c(.1, 5)) + # make sure the sizes are scaled by area and not by radius
  labs(x = "Bill depth (mm)", y = "Bill length (mm)", 
       shape = "Species", size = "Body mass (g)") 
ggsave("figures/penguins_incompatible2.png", width = 6, height = 3, units = "in")

# 3. Cognitive overload --------------------------------------------------------

# get some nice colours from viridis (magma)
sex_cols <- viridis::viridis_pal(option = "magma")(8)[c(3,6)]

ggplot(na.omit(penguins)) +
  geom_point(aes(y = bill_length_mm, # dimension 1: position along y scale
                 x = bill_depth_mm, # dimension 2: position along x scale
                 shape = species, # dimension 3: shape
                 size = log(body_mass_g), # dimension 4: size
                 col = sex), # dimension 5: hue
             alpha = .7) + # size
  scale_size(range = c(.1, 5)) + # make sure the sizes are scaled by area and not by radius
  labs(x = "Bill depth (mm)", y = "Bill length (mm)", 
       shape = "Species", size = "Body mass (g)", col = "Sex") +
  scale_color_manual(values = sex_cols)
ggsave("figures/penguins_5dimensions.png", width = 7, height = 4, units = "in")


# 4. Facets -------------------------------------------------------------------

ggplot(na.omit(penguins)) +
  geom_point(aes(y = bill_length_mm, # dimension 1: position along y scale
                 x = bill_depth_mm, # dimension 2: position along x scale
                 col = log(body_mass_g)), # dimension 3: hue
             alpha = .7, size = 2) + 
  facet_wrap(~ species) + # dimention 4: species!
  # this will create a separate panel for each species
  # note: this also automatically uses the same axes for all panels! If you want 
  # axes to vary between panels, use the argument scales = "free"
  labs(x = "Bill depth (mm)", y = "Bill length (mm)", col = "Body mass (g)") +
  scale_color_viridis_c(option = "magma", end = .9, direction = -1) +
  theme_linedraw() + theme(panel.grid = element_blank()) # making the panels prettier
ggsave("figures/penguins_dimensions_facets.png", width = 7, height = 4, units = "in")


# 5. Interactive ---------------------------------------------------------------

p <- na.omit(penguins) %>%
  ggplot(aes(y = bill_length_mm, 
             x = bill_depth_mm, 
             col = log(body_mass_g))) +
  geom_point(size = 2, alpha = .7) + 
  facet_wrap(~ species) +
  labs(x = "Bill depth (mm)", y = "Bill length (mm)", col = "Body mass (g)") +
  scale_color_viridis_c(option = "magma", end = .9, direction = -1) +
  theme_linedraw() + theme(panel.grid = element_blank()) # making the panels prettier
p <- ggplotly(p)
setwd("figures")
htmlwidgets::saveWidget(as_widget(p), "penguins_interactive.html")


