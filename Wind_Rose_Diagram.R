# This code visualizes monthly average wind speed and directions at 8 compass directions:
# North, Northeast, East, Southeast, South, Southwest, West, and Northwest

# Load Libraries
library(ggplot2) # To Plot Data

# Load the CSV File
wind_data <- read.csv("Wind_Dir_Speed.csv")

# Order Months Properly (Ensures Monthly Sequence in the Plot)
month_levels <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
wind_data$Month <- factor(wind_data$Month, levels = month_levels)

# Assign Breaks and Labels for 8 Compass Directions
breaks_dir <- seq(-22.5, 360, by = 45)
labels_dir <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

# Adjust directions near 360°
# Prevents wind directions between 337.5° and 360° from being assigned as NA 
# by converting them into negative angles that fit into the North bin

wind_data$Direction_adjusted <- ifelse(wind_data$Direction >= 337.5,
                                       wind_data$Direction - 360,
                                       wind_data$Direction)

# Assign Compass Bins
wind_data$dir_bin <- cut(wind_data$Direction_adjusted,
                         breaks = breaks_dir,
                         labels = labels_dir,
                         include.lowest = TRUE)

# Wind Speed Bins at Certain Intervals
wind_data$speed_bin <- cut(wind_data$Speed,
                           breaks = c(2, 3, 4, 5, 6),
                           labels = c("2-3", "3-4", "4-5", "5-6"),
                           include.lowest = TRUE)

# Prepare Dataframe for Plotting
wind_plot <- data.frame(
  Month = wind_data$Month,
  dir_bin = wind_data$dir_bin,
  speed_bin = wind_data$speed_bin,
  Speed = wind_data$Speed
)

# Map Direction Labels to Angles
dir_angles <- setNames(seq(0, 315, by = 45), labels_dir) # 45 degree interval
wind_plot$dir_angle <- dir_angles[as.character(wind_plot$dir_bin)]

# Plot the Diagram
ggplot(wind_plot, aes(x = dir_angle, y = Speed, fill = speed_bin)) +
  geom_bar(stat = "identity", width = 45, color = "black") +
  coord_polar(start = -pi/8) +
  scale_x_continuous(
    limits = c(-22.5, 337.5),
    breaks = seq(0, 315, by = 45),
    labels = labels_dir
  ) +
  scale_y_continuous(breaks = c(0, 2, 4, 6), limits = c(0, 6)) +
  scale_fill_brewer(palette = "YlGnBu",
                    name = "Wind Speed (m/s)") +
  facet_wrap(~Month, ncol = 4) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray70"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 15), face = "bold", size = 15, color = "black"),
    axis.title.y = element_text(margin = margin(r = 15), face = "bold", size = 15, color = "black"),
    axis.text = element_text(color = "black"),
    legend.title = element_text(face = "bold", size = 15, color = "black"),
    legend.text = element_text(size = 12, color = "black"),
    legend.spacing.y = unit(0.6, "cm"),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    x = "Wind Direction",  # X-axis Label
    y = "Wind Speed (m/s)" # Y-axis Label
  )

# Save the Diagram as png file
ggsave("Wind_Rose.png", width = 11, height = 7, dpi = 500)
