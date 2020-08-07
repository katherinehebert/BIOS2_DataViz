# Script to dump a bunch test plot examples for the Data Visualization training
# based on Goodreads books dataset
# from https://www.kaggle.com/jealousleopard/goodreadsbooks

# set-up ----

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)

# set ggplot theme
theme_set(theme_classic() +
            theme(axis.title = element_text(size = 12, face = "bold"),
                  plot.title = element_text(size = 13, face = "bold"),
                  legend.title = element_text(size = 11, face = "bold"),
                  legend.text = element_text(size = 10)))

# import data
df <- read.csv("data/books.csv")

# wrangling -----

# convert columns to numeric
df <- mutate_at(df, 
                vars(average_rating, num_pages, ratings_count, text_reviews_count),
                as.numeric)
# deal with date column (I just want the year)
df$publication_date <- mdy(df$publication_date)
df$publication_year <- year(df$publication_date)

# create decade column
floor_decade <- function(year){return(year - year %% 10)}
df$decade = floor_decade(df$publication_year) %>% as.character()

# keep only the 1st author when there are multiple listed
df = separate(df, authors, sep = "/", into = as.character("author", NA))

# determine how many people have read (i.e. rated) each title in chosen time period
df_top = df %>% group_by(title) %>% 
  summarise(readers = max(ratings_count, na.rm = TRUE)) %>%
  arrange(desc(readers))
# subset df to these titles
df = df[which(df$title %in% df_top$title[1:50]),]
# remove duplicates
df = df[!duplicated(df$title),]

# Dimensions - overwhelming bubble plot example -----

# Plot 1: Dimensions example (bubble plot)

# can comment out col and size to see how the plot changes
ggplot(df) +
  geom_point(aes(y = num_pages, 
                 x = publication_date,
                 col = average_rating,
                 size = ratings_count/100000,
                 )
             ) +
  # set labels for each plot element
  labs(title = "Top 50 most-read books on Goodreads",
       subtitle = "Have popular books gotten longer over time?",
       x = "Publication date",
       y = "Book length (pages)",
       col = "Rating",
       size = "# Ratings \n(x 100,000)"
       ) +
  scale_size_area() +  # make bubble size scaled with area instead of radius (the default)
  scale_color_distiller(palette = "Purples", 
                        direction = 1,
                        limits = c(1,5)) # show full range of possible values!


             