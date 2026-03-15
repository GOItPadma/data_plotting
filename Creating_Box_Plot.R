# This Notebook is Part of the Research Presented at American Geophysical Union's Annual Meeting 2024
# Please Cite the Research as
# Islam, R., Chowdhury, P. and Mahmud, M. (2024). A Synergistic Utility of Optical and SAR Imagery to Monitor Mangrove Ecosystem in Bangladesh. American Geophysical Union Annual Meeting Abstracts 2024 (B01-22), 9-13 December

# Here we visualize the training data collected from Sentinel-2 (RGB and NIR bands) and Sentinel-1 (VV and GLCM Parameter Bands) in Multiple Boxplots

# Load Libraries
library(ggplot2) # To Plot Data
library(reshape2) # To Change Data Format

# Load the CSV data
data <- read.csv("Data.csv")

# Remove any Unnecessary Column (if Necessary)
data[,c("OBJECTID",'ORIG_FID')] <- list(NULL)

# Summarize Class Values
result_summary <- as.data.frame(table(data$ClassName))
colnames(result_summary) <- c("Class", "Total")
result_summary

# Transform Data into a Long Format if Your Data is in Wide Format
data_long <- melt(data, id.vars = "ClassName",  variable.name = "Band", value.name = "Value")
data_long$ClassName <- gsub("_", " ", data_long$ClassName) # Replace the "_" from Column Names to a Whitespace ""
data_long$Band <- gsub("_", " ", data_long$Band)
data_long$ClassName[data_long$ClassName == "B2"] <- "Red" # Rename Columns if Necessary

# Rearrange the Class Levels
data_long$ClassName <- factor(data_long$ClassName, levels = c("Dense Mangrove", "Sparse Mangrove", "Water","Bare"))

# Plot Band Values in a Boxplot at Multiple Rows and Columns 
ggplot(data_long, aes(x=ClassName, y=Value,fill=ClassName)) + 
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.4,outlier.color="red") + 
  labs(fill = "Class Name")+
  xlab("")+ 
  ylab("")+
  facet_wrap(~Band,scale="free", ncol = 5, nrow = 3)+      # Set Column and Rows for the Plot
  theme(axis.text.x = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold",size =10),
        axis.ticks.length.x = unit(0,'cm'),
        panel.spacing = unit(9, "pt"))+
  scale_fill_manual(
    values = c("Dense Mangrove" = "#1e872e", # Set Colors for Each Class
               "Sparse Mangrove" = "#03fc0f",
               "Bare" = "#e6dbbe", 
               "Water" = "#03adfc"))

# Export the Boxplot to a JPEG File
ggsave(filename="Boxplot.jpeg", height=8.27, width=11.69, device='jpeg', dpi=720)
