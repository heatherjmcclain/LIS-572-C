# LIS 572 C Final Project
# Broadband Summary By Geography Type

#Load tools
library(dplyr)
library(stringr)
library("ggplot2")
library ("plotly")

#Renaming dataset with a shorter name
bdc_df = bdc_us_broadband_summary_by_geography_J23_16apr2024

# Number of Rows
num_rows <- nrow(bdc_df)
print(num_rows) #188121 rows

# Number of Columns
num_cols <- ncol(bdc_df)
print(num_cols) #224 columns

# Because the dataset is so large, we did some initial cleaning to make the file size more manageable and to be able to share. The cleaned dataset is on GitHub and can be accessed here: https://github.com/heatherjmcclain/LIS-572-C/blob/main/bdc_us_broadband_summary_by_geography_J23_05may2024_clean.csv

# Loading the dataset via GitHub for accessibility
bdc_df_clean <- read.csv("https://raw.githubusercontent.com/heatherjmcclain/LIS-572-C/main/bdc_us_broadband_summary_by_geography_J23_05may2024_clean.csv", stringsAsFactors = FALSE)

# To begin we wanted to look specifically at the totals for each state
total_state <- bdc_df_clean %>% 
  filter(area_data_type == "Total") %>% 
  filter(geography_type == "State")

#Cleaning up the data to only include columns that we are reviewing.
smaller_state <- total_state %>% select(area_data_type, geography_type, geography_id, geography_desc, total_units)

# Visualization for the states
ggplot(smaller_state) +
  geom_point(mapping = aes(x = geography_desc, y = total_units, fill = geography_desc)) +
  labs(x = "State", y = "Units", title = "Number of Broadband Units by State and Territory")

# Also interested to see as bar graph.
ggplot(smaller_state) +
  geom_col(mapping = aes(x = geography_desc, y = total_units, fill = geography_desc)) +
  labs(x = "State", y = "Units", title = "Number of Broadband Units by State and Territory")

# Visually this is a lot to look at so to clean it up even further we narrowed the dataset to the two states that we are interested in, Alaska and Texas.

# Data specific for State of Alaska
state_alaska <- bdc_df_clean %>% 
  filter(geography_desc == "Alaska")

# Because we do not need all of the columns, we wanted to make smaller dataset for Alaska.
smaller_alaska <- state_alaska %>% select(area_data_type, geography_type, geography_id, geography_desc, total_units)

# To clean it up even further, we wanted to look at just the Urban and Rural data.
urban_alaska <- smaller_alaska %>% 
  filter(area_data_type == "Urban") 

rural_alaska <- smaller_alaska %>% 
  filter(area_data_type == "Rural")

# Data specific for State of Texas
state_texas <- bdc_df_clean %>% 
  filter(geography_desc == "Texas")

# Similar to Alaska, we cleaned up the dataset.
smaller_texas <- state_texas %>% select(area_data_type, geography_type, geography_id, geography_desc, total_units)

# To clean it up even further, we wanted to look at just the Urban and Rural data.
urban_texas <- smaller_texas %>% 
  filter(area_data_type == "Urban") 

rural_texas <- smaller_texas %>% 
  filter(area_data_type == "Rural")

# Comparing Urban and Rural usage in Alaska and Texas
combined_data <- bind_rows(
  mutate(urban_alaska, dataset = "urban_alaska"),
  mutate(rural_alaska, dataset = "rural_alaska"),
  mutate(urban_texas, dataset = "urban_texas"),
  mutate(rural_texas, dataset = "rural_texas")
)

ggplot(combined_data, aes(x = geography_desc, y = total_units, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Community", y = "Broadband Units", title = "Broadband Usage in Urban and Rural Communities in Alaska and Texas") +
  scale_color_brewer(palette = "Set3") 

# Overall we can see that the broadband unit count is higher in Texas both in urban and rural communities, which can likely be attributed to population density in the Lower 48, but it is interesting to see that the numbers for urban and rural usage are much closer in Alaska. This could be attributed to  the fact that there are really only 3 urban communities in Alaska (Anchorage, Fairbanks, Juneau) where the majority of the population lives, but the data distribution between Texas and Alaska shows a significant spread. 
  
# Because many of Alaska's remote communities are Indigenous, we were curious to see what the national numbers are for Tribal communities by state.
total_tribal <- bdc_df_clean %>% 
  filter(area_data_type == "Tribal") %>% 
  filter(geography_type == "State")

# Out of curiosity, we wanted to see what the top 10 states were for broadband internet in Indigenous communities. And because we don't need to see all of the columns, we started by selecting the columns that we wanted to work with first.

smaller_tribal <- total_tribal %>% select(area_data_type, geography_type, geography_id, geography_desc, total_units)

top_tribal <- smaller_tribal %>% 
  slice_max(n=10, order_by = total_units)

#Visualization for the top tribal
bar_plot <- ggplot(top_tribal) +
  geom_col(mapping = aes(x = geography_desc, y = total_units, fill = geography_desc)) +
  labs(x = "State", y = "Broadband Units", title = "Top Tribal Broadband Data in the United States")

ggplotly(bar_plot)

# We decided to also evaluate the provider dataset available on the BDC website as a comparison [Do we still want to use this dataset? Totally fine if we do not!]

# Provider dataset
bdc_provider_df = bdc_us_provider_summary_by_geography_J23_05may2024

# Similar to the Boradband Summary dataset, we cleaned up this dataset to reduce the file size and to reflect data more specific to Alaska and Texas. 
bdc_provider_df_clean <- read.csv("https://raw.githubusercontent.com/heatherjmcclain/LIS-572-C/main/bdc_us_provider_summary_by_geography_J23_05may2024_clean.csv", stringsAsFactors = FALSE)

# How many distinct types of data are there?
distinct_data <- bdc_provider_df_clean %>% 
  distinct(data_type)

# Interesting to note that although the data in the providers dataset only distinguishes between "Fixed Broadband" and "Mobile Broadband", the output online displays the breakdown in technology type. This may be included in another dataset that we are not reviewing for this project.

# We also want to know how may distinct geography types there are
distinct_geography <- bdc_provider_df_clean %>% 
  distinct(geography_type)

# For our purposes, we are interested in the state data for Alaska and Texas.
providers_alaska <- bdc_provider_df_clean %>% 
  filter(geography_desc == "Alaska")

providers_texas <- bdc_provider_df_clean %>% 
  filter(geography_desc == "Texas")

# Looking for number of fixed and mobile broadband providers for each state.
broadband_providers_alaska <- providers_alaska %>% 
  group_by(provider_id) %>% 
  summarize(num_providers = n_distinct(data_type))

broadband_providers_texas <- providers_texas %>% 
  group_by(provider_id) %>% 
  summarize(num_providers = n_distinct(data_type))

# Once again, it is not surprising that Texas has the highest number of providers in the state, but it is interesting that only 7 offer both broadband and mobile services out of 243. Whereas 9 of the 32 providers in Alaska offer both. Mobile broadband tends to have a wider range in remote or rural communities in Alaska but fixed broadband is more reliable, so it makes sense that there are more providers that offer that option. 

