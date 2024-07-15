setwd("C:\\Users\\nihar\\OneDrive\\Desktop\\Bootcamp\\SCMA 632\\Assignments\\A5")
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE, repos = 'http://cran.rstudio.com/')
    library(package, character.only = TRUE)
  }
}

libraries <- c("dplyr", "readr", "ggplot2", "sf", "viridis", "tidyverse", "ggspatial", "BSDA", "glue")
lapply(libraries, install_and_load)

# Load the data
filepath <- "C:/Users/nihar/OneDrive/Desktop/Bootcamp/SCMA 632/DataSet/NSSO68.csv"
data <- read.csv(filepath)

# Display dataset info
cat("Dataset Information:\n")
print(dim(data))

# Filter data for Maharashtra
maharashtra_data <- data %>% filter(state == 27)

# Display the structure of the filtered data
str(maharashtra_data)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(maharashtra_data)))

# Replace missing values in all columns with their respective median values
for (col in names(maharashtra_data)) {
  if (any(is.na(maharashtra_data[[col]]))) {
    maharashtra_data[[col]][is.na(maharashtra_data[[col]])] <- median(maharashtra_data[[col]], na.rm = TRUE)
  }
}

# Check for missing values after replacement
cat("Missing Values After Replacement:\n")
print(colSums(is.na(maharashtra_data)))

# Create a lookup table for district codes and names
district_lookup <- data.frame(
  District = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35),
  District_Name = c("Nandurbar", "Dhule", "Jalgaon", "Buldana", "Akola", "Washim", "Amravati", "Wardha", "Nagpur", "Bhandara", "Gondiya", "Gadchiroli", 
                    "Chandrapur", "Yavatmal", "Nanded", "Hingoli", "Parbhani", "Jalna", "Aurangabad", "Nashik", "Thane", "Mumbai (Suburban)", 
                    "Raigarh", "Pune", "Ahmadnagar", "Bid", "Latur", "Osmanabad", "Solapur", "Satara", "Ratnagiri", "Sindhudurg", "Kolhapur", "Sangli")
)

# Merge the lookup table with the data to replace district codes with names
maharashtra_data <- merge(maharashtra_data, district_lookup, by.x = "District", by.y = "District", all.x = TRUE)

# Display the structure to verify the changes
str(maharashtra_data)

# Calculate IQR and identify outliers for foodtotal_v
Q1 <- quantile(maharashtra_data$foodtotal_v, 0.25, na.rm = TRUE)
Q3 <- quantile(maharashtra_data$foodtotal_v, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define the lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove outliers in foodtotal_v
maharashtra_data <- maharashtra_data %>%
  filter(foodtotal_v >= lower_bound & foodtotal_v <= upper_bound)

# Summarize data by district to get total consumption per district
consumption_per_district <- maharashtra_data %>%
  group_by(District_Name) %>%
  summarize(total_consumption = sum(foodtotal_v, na.rm = TRUE))

# Create a custom color palette with sufficient colors
library(viridis)
colors <- viridis_pal()(length(unique(consumption_per_district$District_Name)))

# Plot histogram of total consumption by district
hist_plot <- ggplot(consumption_per_district, aes(x = reorder(District_Name, -total_consumption), y = total_consumption)) +
  geom_col(aes(fill = District_Name), color = "black") +
  scale_fill_manual(values = colors) +
  labs(title = "Total Consumption by District in Maharashtra",
       x = "District",
       y = "Total Consumption (in units)") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        legend.position = "none")

# Plot boxplot of total consumption by district
box_plot <- ggplot(maharashtra_data, aes(x = reorder(District_Name, -foodtotal_v), y = foodtotal_v)) +
  geom_boxplot(aes(fill = District_Name), color = "black", outlier.shape = NA) +  # Remove outliers from the plot
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  scale_fill_manual(values = colors) +
  labs(title = "Distribution of Total Consumption by District in Maharashtra",
       x = "District",
       y = "Total Consumption (in units)") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        legend.position = "none")

# Print the plots
print(hist_plot)
print(box_plot)

# Summarize data by district to get the values for the four variables
variables_per_district <- maharashtra_data %>%
  group_by(District_Name) %>%
  summarize(total_consumption = sum(foodtotal_v, na.rm = TRUE),
            avg_mpce_urp = mean(MPCE_URP, na.rm = TRUE),
            avg_mpce_mrp = mean(MPCE_MRP, na.rm = TRUE),
            avg_hh_size = mean(hhdsz, na.rm = TRUE))

# Display top and bottom 3 consuming districts
district_summary <- variables_per_district %>%
  arrange(desc(total_consumption))

cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

# Summarize consumption by region
region_summary <- maharashtra_data %>%
  group_by(Sector) %>%
  summarize(total_consumption = sum(foodtotal_v, na.rm = TRUE))

# Rename sectors for readability
region_summary$Sector <- ifelse(region_summary$Sector == 1, "RURAL", "URBAN")

cat("Region Consumption Summary:\n")
print(region_summary)

# Load the shapefile for Maharashtra
shapefile_path <- "C:/Users/nihar/OneDrive/Desktop/Bootcamp/SCMA 632/Assignments/A5/MAHARASHTRA_DISTRICTS.geojson"
maharashtra_shp <- st_read(shapefile_path)

# Verify the geometry type of the loaded shapefile
print(st_geometry_type(maharashtra_shp))

# Print column names of the shapefile
print(colnames(maharashtra_shp))

# The correct column for merging is "dtname" 
colnames(maharashtra_shp)[which(colnames(maharashtra_shp) == "dtname")] <- "District_Name"

# Merge the shapefile with the variables data
maharashtra_map <- merge(maharashtra_shp, variables_per_district, by = "District_Name")

# Define a plotting function with tidy evaluation
plot_variable <- function(data, variable, title, legend_title) {
  ggplot(data) +
    geom_sf(aes(fill = !!sym(variable)), color = "black") +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = title, fill = legend_title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Plot the maps for the four variables
plot1 <- plot_variable(maharashtra_map, "total_consumption", "Total Consumption by District", "Total Consumption")
plot2 <- plot_variable(maharashtra_map, "avg_mpce_urp", "Average MPCE (URP) by District", "Avg MPCE (URP)")
plot3 <- plot_variable(maharashtra_map, "avg_mpce_mrp", "Average MPCE (MRP) by District", "Avg MPCE (MRP)")
plot4 <- plot_variable(maharashtra_map, "avg_hh_size", "Average Household Size by District", "Avg Household Size")

# Print the plots
print(plot1)
print(plot2)
print(plot3)
print(plot4)