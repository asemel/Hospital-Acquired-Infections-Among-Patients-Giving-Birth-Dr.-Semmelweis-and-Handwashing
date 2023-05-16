# Import csv file to Global Environment

Deaths_by_clinic <- read.csv("yearly_deaths_by_clinic.csv", header = TRUE, sep = ",")

# Load in the tidyverse package
library(tidyverse)

# Read yearly_deaths_by_clinic.csv into yearly
yearly <- read_csv("yearly_deaths_by_clinic.csv")

# Print out yearly
yearly

# Add a new column to yearly with proportion of deaths per number of births
yearly <- yearly %>%
  mutate(proportion_deaths = deaths / births)

# Print out yearly
yearly

# Set the size of plots in notebook
options(repr.plot.width=7, repr.plot.height=4)

# Plot yearly proportion of deaths by clinic
ggplot(yearly, aes(x = year, y= proportion_deaths, color = clinic)) +
  geom_line()

# Read monthly_deaths.csv into monthly
monthly <- read_csv("monthly_deaths.csv")

# Add a new column with proportion of deaths per number of births
monthly <- monthly %>%
  mutate(proportion_deaths = deaths / births)

# Print out the first rows in monthly
head(monthly)

# Plot monthly proportion of deaths
ggplot(monthly, aes( x = date, y = proportion_deaths)) +
  geom_line()

# From this date handwashing was made mandatory at clinics
handwashing_start = as.Date('1847-06-01')

# Add TRUE/FALSE column to monthly called handwashing_mandatory
monthly <- monthly %>%
  mutate(handwashing_mandatory = date >= handwashing_start)

# Plot monthly proportion of deaths before and after handwashing
ggplot(monthly, aes(x = date, y = proportion_deaths, color = handwashing_mandatory)) +
  geom_line()

# Calculate the mean proportion of deaths before and after handwashing was made mandatory
monthly_summary <- monthly %>%
  group_by(handwashing_mandatory) %>%
  summarize(mean_proportion_deaths = mean(proportion_deaths))

# Print monthly_summary
monthly_summary

# Calculate a 95% Confidence Interval using a t.test
test_result <- t.test( proportion_deaths ~ handwashing_mandatory, data = monthly)
test_result



