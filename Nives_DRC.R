# Code created by Elisa D'Amico on October 8, 2024
# This project investigates the impact of the emergence of Green Infrastructure Projects in DRC on population and resource conflict.
# The analysis employs the Difference-in-Differences (DiD) approach using the did package and visualizes the results with ggplot2.

# Data comes from the ACLED: https://acleddata.com/data-export-tool/
#      and Global Energy Monitor: https://globalenergymonitor.org/projects/global-hydropower-tracker/download-data/

# Install and load necessary libraries if they are not already installed
if (!require("did")) install.packages("did", dependencies = TRUE)  # For Difference-in-Differences analysis
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)  # For data visualization
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)  # For reading Excel files
if (!require("data.table")) install.packages("data.table", dependencies = TRUE)  # For data manipulation
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)  # For data manipulation and visualization
if (!require("plm")) install.packages("plm", dependencies = TRUE)  # For data manipulation and visualization
if (!require("zoo")) install.packages("zoo", dependencies = TRUE)

# Load the necessary libraries
library(readxl)  # For reading Excel files
library(did)     # For Difference-in-Differences analysis
library(ggplot2) # For data visualization
library(data.table) # For data manipulation
library(tidyverse) # For data manipulation and visualization
library(plm) # For Panel Linear Regression
library(zoo)

# Set the working directory 
# This should be the location of your "data.xlsx" file that you downloaded
getwd() # To check your current working directory
#setwd("/path/to/your/directory") # e.g., setwd("/Users/elisadamico/Documents/Basel")

# If having issues with this step, follow this tutorial: https://www.youtube.com/watch?v=2_tW7e4e_dM

# The following regions are included in the data: Adamaoua, Centre (incl Yaounde), Est, Extreme Nord, Littoral (incl Douala), Nord, Nord Ouest, Ouest, Sud, Sud Ouest.
# The data spans from 1990 to 2022

# Read in the data from an Excel file named "WorldBank.xlsx"

data <- read_excel("Nives/data.xlsx")

# Prepare and clean the data in a single step
data <- data %>%
  mutate(
    unit = as.numeric(as.factor(gns_name)),             # Encode region as numeric
    year = as.numeric(year),                            # Ensure year is numeric
    treatment = as.numeric(treatment),                  # Ensure treatment is numeric
    EventType_Count = as.numeric(EventType_Count),      # Ensure EventType_Count is numeric
    Average_Population_Best = as.numeric(Average_Population_Best)  # Ensure Average_Population_Best is numeric
  ) %>%
  group_by(unit) %>%
  mutate(
    Average_Population_Best = na.locf(Average_Population_Best, na.rm = FALSE)  # Fill down using na.locf
  ) %>%
  ungroup()  # Ungroup after filling down



# Define a list of dependent variables (indicators) for the analysis
dvs <- c("EventType_Count", "Average_Population_Best"
)

# Create a named vector for human-readable labels corresponding to each dependent variable
variable_labels <- c(
  "EventType_Count" = "Resource Conflict",
  "Average_Population_Best" = "Displacement"
)
# Initialize an empty list to store results for each dependent variable
results_list <- list()

# Loop through each dependent variable defined in 'dvs'
for (dv in dvs) {
  print(paste("Processing", dv))  # Print the current dependent variable being processed
  
  try({
    # Check if the dependent variable exists in the dataset
    if (!(dv %in% colnames(data))) {
      stop(paste("Dependent variable", dv, "not found in the dataset"))
    }
    
    # Estimate the Average Treatment Effect on the Treated (ATT) using the did package
    did_result <- did::att_gt(
      yname = dv,                     # Name of the dependent variable
      tname = "year",                 # Time variable
      idname = "unit",     # Identifier for units (countries)
      gname = "treatment",            # Treatment variable
      control_group = "notyettreated", # Specify control group
      data = data,                    # Dataset to use
      panel = FALSE, 
      bstrap = TRUE
    )
    
    # Compute DiD statistics with simple aggregation
    did_simple <- aggte(did_result, type = "simple", na.rm = TRUE)
    
    # Compute DiD statistics with event study aggregation
    did_event <- aggte(did_result, type = "dynamic", na.rm = TRUE)
    
    # Store the results in the results_list for later use
    results_list[[dv]] <- list(
      did_result = did_result,
      did_simple = did_simple,
      did_event = did_event
    )
    
    # Print simple statistics
    cat(paste("\nSimple DiD statistics for", variable_labels[dv], ":\n"))
    print(did_simple)
    
    # Print event study statistics
    cat(paste("\nEvent study DiD statistics for", variable_labels[dv], ":\n"))
    print(did_event)
    
    # Plot the event study results for the current dependent variable
    cat(paste("Plotting event study for", dv, "\n"))
    plot_obj <- ggdid(did_event) + 
      ggtitle(variable_labels[dv])  # Use the human-readable label as the plot title
    print(plot_obj)  # Display the plot
    
  }, silent = TRUE)  # Suppress errors and continue with the next variable
}

# Instructions for viewing the plots:
# Click on "Plots" in your R environment to view the outputs.
# Use the arrows to navigate between different plots.
# 
# Interpreting Event Study Plots:
# - The red line represents the values of the dependent variable before the "shock" (implementation of the green development project).
# - The blue line represents the values after the shock.
# - An upward trend in the blue line post-implementation suggests a positive impact, while a downward trend indicates a negative direction
#
# For further guidance on interpreting event study plots, please watch the following video: 
# https://www.youtube.com/watch?v=pAOhWWiuhM8



###### Overtime Plots ######

wb <- read_excel("Nives/WorldBank.xlsx")


###################
# "Conflict" plot

wb_long <- wb %>%
  pivot_longer(cols = starts_with("event_"),
               names_to = "Event",
               values_to = "Count")

ggplot(wb_long, aes(x = Year, y = Count, color = Event)) +
  geom_line() +
  labs(title = "Resource Conflict Variables Over Time",
       x = "Year",
       y = "Count",
       color = "Event Type") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5))


###################
# Displacement Plot

wb_long_displacement <- wb %>%
  pivot_longer(cols = c("Internally displaced persons, new displacement associated with conflict and violence (number of cases) [VC.IDP.NWCV]",
                        "Internally displaced persons, new displacement associated with disasters (number of cases) [VC.IDP.NWDS]",
                        "Internally displaced persons, total displaced by conflict and violence (number of people) [VC.IDP.TOCV]",
                        "Refugee population by country or territory of asylum [SM.POP.REFG]",
                        "Refugee population by country or territory of origin [SM.POP.REFG.OR]"),
               names_to = "Displacement_Type",
               values_to = "Count")

wb_long_displacement <- wb_long_displacement %>%
  mutate(Displacement_Type = recode(Displacement_Type,
                                    "Internally displaced persons, new displacement associated with conflict and violence (number of cases) [VC.IDP.NWCV]" = "Conflict and Violence (New)",
                                    "Internally displaced persons, new displacement associated with disasters (number of cases) [VC.IDP.NWDS]" = "Disasters (New)",
                                    "Internally displaced persons, total displaced by conflict and violence (number of people) [VC.IDP.TOCV]" = "Conflict and Violence (Total)",
                                    "Refugee population by country or territory of asylum [SM.POP.REFG]" = "Refugee (Asylum)",
                                    "Refugee population by country or territory of origin [SM.POP.REFG.OR]" = "Refugee (Origin)"))

wb_long_displacement <- wb_long_displacement %>%
  filter(Year >= 2008)

ggplot(wb_long_displacement, aes(x = Year, y = Count, color = Displacement_Type)) +
  geom_line() +
  labs(title = "Displacement Variables Over Time (2008 Onwards)",
       x = "Year",
       y = "Count",
       color = "Displacement Type") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5))


#######################
# Renewable Energy Plot

wb_long_renewable <- wb %>%
  pivot_longer(cols = c("Renewable electricity output (% of total electricity output) [EG.ELC.RNEW.ZS]",
                        "Renewable energy consumption (% of total final energy consumption) [EG.FEC.RNEW.ZS]",
                        "Renewable internal freshwater resources per capita (cubic meters) [ER.H2O.INTR.PC]"),
               names_to = "Renewable_Type",
               values_to = "Value")

wb_long_renewable <- wb_long_renewable %>%
  mutate(Renewable_Type = recode(Renewable_Type,
                                 "Renewable electricity output (% of total electricity output) [EG.ELC.RNEW.ZS]" = "Renewable Electricity Output (% of Total)",
                                 "Renewable energy consumption (% of total final energy consumption) [EG.FEC.RNEW.ZS]" = "Renewable Energy Consumption (% of Total)",
                                 "Renewable internal freshwater resources per capita (cubic meters) [ER.H2O.INTR.PC]" = "Renewable Freshwater Resources per Capita (Cubic Meters)"))

# Apply min-max normalization to better compare trends
wb_long_renewable <- wb_long_renewable %>%
  group_by(Renewable_Type) %>%
  mutate(Normalized_Value = (Value - min(Value, na.rm = TRUE)) / (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE))) %>%
  ungroup()

ggplot(wb_long_renewable, aes(x = Year, y = Normalized_Value, color = Renewable_Type)) +
  geom_line() +
  labs(title = "Renewable Energy Variables Over Time (Normalized)",
       x = "Year",
       y = "Normalized Value",
       color = "Renewable Type") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5))


