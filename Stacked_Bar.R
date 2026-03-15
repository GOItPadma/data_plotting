# Load Libraries
library(ggplot2) # To Plot Data

# Load Data from CSV
dune_data <- read.csv("Dune_Data.csv")  

# Concatenate the String Values from Two Columns and Stores the Values into a New Column "Group" (See Sample Data and Output Charts)
dune_data$Group <- paste(dune_data$Elevation, dune_data$Disturbance)

# Plot the Stacked Bar Chart
ggplot(dune_data, aes(x = Morpho, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "stack", 
           color = "black", linewidth = 0.3,
           width = 0.3) +
  labs(
    x = "Morpho-ecological Class",
    y = "Percentage (%)",
    fill = "Elevation & Disturbance Class"
  ) +
  
  scale_fill_manual(   # Set Colors for Each Class
    values = c(
      "High Dune Partially Degraded" = "#B0AA64",  
      "Low Dune Partially Degraded" = "#d9cc86",   
      "Low Dune Undisturbed" = "#6bc77f",          
      "Medium Dune Partially Degraded" = "#B2C26B", 
      "Medium Dune Undisturbed" = "#40854f"        
    ),
    breaks = c(
      "High Dune Partially Degraded",
      "Low Dune Partially Degraded",
      "Low Dune Undisturbed",
      "Medium Dune Partially Degraded",
      "Medium Dune Undisturbed"
    )
  ) +
  theme(
    # Font Styles
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black",face='bold', size = 14),
    
    # Legend Styles
    legend.position = "right",
    legend.justification = "center",
    legend.text = element_text(size = 12, margin = margin(t = 2, b = 2)),
    legend.title = element_text(size = 12, face = "bold", margin = margin(b = 8)),
    legend.background = element_rect(color = "black", linewidth = 0.2),
    legend.margin = margin(10, 15, 10, 15),
    legend.box.margin = margin(0, 0, 0, 15),
    
    # Set Legend Keys
    legend.key.size = unit(0.8, "cm"),
    legend.key = element_rect(color = NA, fill = NA),
    
    # Axis Settings
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 18)),
    axis.title.y = element_text(margin = margin(r = 8)),
    
    # Set Grid Lines
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "gray80"),
    
    # Panel and Border Styles
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_blank(),
    
    # Set Plot Margins
    plot.margin = margin(50, 40, 22, 28)
  ) +
  guides(fill = guide_legend(
    ncol = 1,
    byrow = TRUE,
    keyheight = unit(1.2, "lines"),
    default.unit = "lines"
  ))

# Export Bar Chart to a Png File
ggsave("Dune_Summary.png", width = 12, height = 8, dpi = 500)
