install.packages(c("tidyverse","SimDesign","janitor"))
library(tidyverse)
library(SimDesign)
library(ggplot2)
library(janitor)
library(data.table)

winemag_aggregate_data <- read.csv("winemag_aggregate_data.csv")
winespec_aggregate_data <- read.csv("winespec_aggregate_data.csv")

# Clean winespec data set to have United States match winemag data set
winespec_aggregate_data[winespec_aggregate_data == "USA"] <- "US"

# Get entries from winemag data which have matching countries in winespec data
winemag_match_df <- match_df(winemag_aggregate_data,
                             winespec_aggregate_data, on="country")

# Compute bias between average Spectator score and Wine Mag score 
print(
  cor(winespec_aggregate_data["avg_points"], winemag_match_df["avg_points"])
)
print(
  bias(winespec_aggregate_data["avg_points"], winemag_match_df["avg_points"])
)

# Plot the two data sets to visualize the correlation in scores
combined_scores <- cbind(winemag_match_df["country"],
                         winemag_match_df["avg_points"],
                         winespec_aggregate_data["avg_points"])

combined_scores <- setNames(combined_scores,
                            make_clean_names(colnames(combined_scores)))

ggplot(data=combined_scores) +
  geom_point(mapping=aes(x=avg_points, y=avg_points_2)) +
  geom_smooth(mapping=aes(x=avg_points, y=avg_points_2), method="loess") +
  annotate('text', x = 88.36-0.125, y = 93.8+0.25, label = 'Portugal') +
  ggtitle("Wine Scores from Two Magazines") +
  xlab("Wine Magazine Score") + ylab("Wine Spectator Score")
