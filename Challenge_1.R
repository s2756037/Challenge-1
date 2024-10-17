# layout: Challenge 1
# title: Where are the red squirrels?
# student #: s2756037
# date: 17/10/24

# 1. DATA MANIPULATION

# Load necessary libraries
library(dplyr)

# Assume squirrel_data is your original dataset
squirrel_data <- read.csv("squirrels.csv")

names(squirrel_data)

# Clean the dataset
cleaned_data <- squirrel_data

squirrel_data$Start.date.year
cleaned_data$Start.date.year

  # Ensure Start.date.year is numeric
  cleaned_data <- cleaned_data %>%
    mutate(Start.date.year = as.numeric(Start.date.year)) %>%

  # filter based on the numeric year between 2008 and 2017
  cleaned_data <- cleaned_data %>%
    filter(Start.date.year >= 2008 & Start.date.year <= 2017) %>%

  # Rename Start.date.year to year
  year <- cleaned_data$Start.date.year 
    
  Common.name <- as.numeric(cleaned_data$Common.name == "Grey Squirrel") 
    
  # Remove observations that are not at the species level
  cleaned_data <- dplyr::select(cleaned_data$Common.name, cleaned_data$Start.date.year, cleaned_data$Individual.count)
    
  # Replace NA counts with 1
  mutate(ifelse(is.na(cleaned_data$Individual.count), 1, cleaned_data$Individual.count))
  
# View the cleaned data (optional)
head(cleaned_data)

# Calculate the size of the cleaned dataset
cleaned_size <- nrow(cleaned_data)

# Round to the nearest thousand
cleaned_size_rounded <- round(cleaned_size, -3)

# Print the size of the cleaned dataset
print(paste("Size of the cleaned dataset (to the nearest thousand):", cleaned_size_rounded))  

# 2. TEMPORAL TRENDS

install.packages("ggplot2")
library(ggplot2)

# Assuming cleaned_data is your cleaned dataset from previous steps
# Summarize the number of observations per species and year
summary_data <- cleaned_data %>%
  group_by(cleaned_data.Start.date.year, cleaned_data.Common.name) %>%
  summarise(total_observations = sum(cleaned_data.Individual.count), .groups = 'drop')

# View summary data
print(summary_data)

summary_data$cleaned_data.Common.name <- factor(summary_data$cleaned_data.Common.name)

summary_data %>% 
  rename(cleaned_data.Start.date.year = year, cleaned_data.Common.name = Common.name)

# Plot the number of observations over time for each species
ggplot(summary_data, aes(x = cleaned_data.Start.date.year, y = total_observations, color = cleaned_data.Common.name)) +
  geom_line() +
  geom_point() +
  labs(title = "Observations of Red and Grey Squirrels (2008-2017)",
       x = "Year",
       y = "Total Observations") +
  theme_minimal()

# Convert Common.name to a factor for the model
summary_data$cleaned_data.Common.name <- factor(summary_data$cleaned_data.Common.name)

# Create a linear model
lm_model <- lm(total_observations ~ cleaned_data.Start.date.year * cleaned_data.Common.name, data = summary_data)

# Summary of the model
summary(lm_model)

# Adjusted R-squared
adj_r_squared <- summary(lm_model)$adj.r.squared
print(adj_r_squared)  

# 1. Which species showed the strongest change over time?
# The grey squirrel had the strongest change over time due to it having a higher coefficient when looking at the year:Common.name interaction

# 2. What were your predictor variable(s) and their data type in the model?
# The predictor variables were year(numeric) and Common.name(factor)

# 3. What is the adjusted R-squared of the regression?
# 0.8644

# 4. Considering the nature of our response variable, what modeling approach would be the most appropriate?

# 5. What could be the reasons for this trend? Is it ecologically meaningful? Are there any biases in the data to be aware of?
# Since grey squirrels are invasive in the UK it makes sense for them to have the strongest change over time. However it is important to also recongnize that there could be biases due to habitat change, human impact, and conservation efforts.

# 3. DO GREY SQUIRRELS PREFER DIFFERENT HABITATS?

# Filter data for 2015-2017
filtered_data <- squirrel_data %>%
  group_by(OSGR, Common.name) %>%
  filter(year >= 2015 & year <= 2017) %>%
  summarise(total_count = sum(Individual.count, na.rm = TRUE), .groups = 'drop')
  filter(total_count <= 300)  # Remove counts greater than 300
  
# Moving Repository to Github
install.packages("devtools")
library(devtools)
install_github("s2756037/Challenge-1")
