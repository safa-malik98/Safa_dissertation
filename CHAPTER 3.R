# Load necessary libraries
library(survival)
library(ggplot2)
library(VennDiagram)

# Load your data
data <- read.csv("final_data2.csv")

# Define age bins and labels
age_bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
age_labels <- c('0-10', '10-20', '20-30', '30-40', '40-50', 
                '50-60', '60-70', '70-80', '80-90', '90-100')

# Create the age_group variable using cut
data$age_group <- cut(data$age, breaks = age_bins, labels = age_labels, right = FALSE)

# Ensure you have complete cases
data <- data[complete.cases(data$age_group, data$slider_sex, data$income), ]

# Filter for MACE == TRUE
mace_data <- data[data$MACE == 'True', ]

# Define the conditions to be included in the Venn diagram
condition1 <- mace_data$`Cardiac.arrest` == 'True'
condition2 <- mace_data$`Cardiac.Arrhythmia` == 'True'
condition3 <- mace_data$Stroke == 'True'
condition4 <- mace_data$`Heart.Failure` == 'True'
condition5 <- mace_data$`Myocardial.Injury` == 'True'

# Create the Venn diagram for 5 conditions
venn.plot <- draw.quintuple.venn(
  area1 = sum(condition1),
  area2 = sum(condition2),
  area3 = sum(condition3),
  area4 = sum(condition4),
  area5 = sum(condition5),
  n12 = sum(condition1 & condition2),
  n13 = sum(condition1 & condition3),
  n14 = sum(condition1 & condition4),
  n15 = sum(condition1 & condition5),
  n23 = sum(condition2 & condition3),
  n24 = sum(condition2 & condition4),
  n25 = sum(condition2 & condition5),
  n34 = sum(condition3 & condition4),
  n35 = sum(condition3 & condition5),
  n45 = sum(condition4 & condition5),
  n123 = sum(condition1 & condition2 & condition3),
  n124 = sum(condition1 & condition2 & condition4),
  n125 = sum(condition1 & condition2 & condition5),
  n134 = sum(condition1 & condition3 & condition4),
  n135 = sum(condition1 & condition3 & condition5),
  n145 = sum(condition1 & condition4 & condition5),
  n234 = sum(condition2 & condition3 & condition4),
  n235 = sum(condition2 & condition3 & condition5),
  n245 = sum(condition2 & condition4 & condition5),
  n345 = sum(condition3 & condition4 & condition5),
  n1234 = sum(condition1 & condition2 & condition3 & condition4),
  n1235 = sum(condition1 & condition2 & condition3 & condition5),
  n1245 = sum(condition1 & condition2 & condition4 & condition5),
  n1345 = sum(condition1 & condition3 & condition4 & condition5),
  n2345 = sum(condition2 & condition3 & condition4 & condition5),
  n12345 = sum(condition1 & condition2 & condition3 & condition4 & condition5),
  category = c("Cardiac Arrest", "Cardiac Arrhythmia", "Stroke", "Heart Failure", "Myocardial Injury"),
  fill = c("blue", "yellow", "green", "purple", "orange"),
  alpha = 0.5,
  lwd = 1.5,
  label.col = "black",
  cex = 1.1,
  cat.cex = 1.1,
  cat.dist = c(0.05, 0.05, 0.05, 0.05, 0.05),
  cat.pos = 0,
  cat.fontfamily = "sans"
)

grid.newpage()
# Display the Venn diagram
grid.draw(venn.plot)



# Filter for true MACE cases
true_mace_cases <- data %>% filter(MACE == "True")

# Calculate total number of true MACE cases
total_true_mace <- nrow(true_mace_cases)

# Define the conditions
conditions <- c('Cardiac.arrest', 'Cardiac.Arrhythmia', 'Stroke', 'Heart.Failure', 'Myocardial.Injury')

# Calculate the percentage of true MACE cases for each condition
condition_percentages <- sapply(conditions, function(condition) {
  condition_count <- sum(true_mace_cases[[condition]] == "True")
  percentage <- (condition_count / total_true_mace) * 100
  return(percentage)
})

# Create a data frame for plotting
condition_df <- data.frame(
  Condition = conditions,
  Percentage = condition_percentages
)

# Print the results
for (i in seq_along(conditions)) {
  cat(sprintf("Percentage of MACE cases with %s: %.2f%%\n", conditions[i], condition_percentages[i]))
}

# Rename the conditions in the data frame
condition_df$Condition <- c("Cardiac Arrest", "Cardiac Arrhythmia", "Heart Failure", "Myocardial Injury", "Stroke")

# Plotting the results with renamed bars
ggplot(condition_df, aes(x = Condition, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Complication", y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  # Increase label size and add space
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),  # Increase label size and add space
    axis.text = element_text(size = 12),  # Increase the size of the axis labels
  )


library(ggplot2)
library(dplyr)
library(broom)

# Function to calculate confidence intervals
calculate_confidence_interval <- function(p, n) {
  if (n == 0) {
    return(c(0, 0))
  }
  z <- qnorm(0.975)  # 1.96 for 95% confidence
  se <- sqrt(p * (1 - p) / n)
  ci_lower <- p - z * se
  ci_upper <- p + z * se
  return(c(ci_lower * 100, ci_upper * 100))
}

final_data2$MACE <- as.logical(final_data2$MACE)
# Calculate the percentage of True values for MACE by age group and their confidence intervals
age_group_stats <- final_data2 %>%
  group_by(age_group) %>%
  summarise(
    percentage = mean(MACE, na.rm = TRUE) * 100,
    ci_lower = calculate_confidence_interval(mean(MACE, na.rm = TRUE), n())[[1]],
    ci_upper = calculate_confidence_interval(mean(MACE, na.rm = TRUE), n())[[2]]
  )

# Keep the order of age groups as defined by age_labels
age_group_stats$age_group <- factor(age_group_stats$age_group, levels = age_labels)

# Remove rows with NA values
age_group_stats_clean <- na.omit(age_group_stats)

ggplot(age_group_stats_clean, aes(x = age_group, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Age Group", y = "Percentage (%)") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, max(age_group_stats_clean$ci_upper, na.rm = TRUE))) +
  theme(
    plot.title = element_blank(),  # Remove the title
    axis.text.x = element_text(size = 15, angle = 45, hjust = 1),  # Increase x-axis text size
    axis.text.y = element_text(size = 16),  # Increase y-axis text size
    axis.title.x = element_text(size = 16, margin = margin(t = 12)),  # Increase x-axis label size and add space
    axis.title.y = element_text(size = 16, margin = margin(r = 12))  # Increase y-axis label size and add space
  )


# Filter for patients who developed MACE and group by age group and sex
grouped_data <- final_data2 %>%
  filter(MACE == TRUE) %>%
  group_by(age_group, slider_sex) %>%
  summarise(count = n()) %>%
  ungroup()

grouped_data <- grouped_data[!is.na(grouped_data$age_group), ]

# Adjust the count for males (make them negative)
grouped_data <- grouped_data %>%
  mutate(count_adj = ifelse(slider_sex == "Male", -count, count))

# Create the pyramid plot
ggplot(grouped_data, aes(x = age_group, y = count_adj, fill = slider_sex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-6000, 6000)) +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"), name = "Sex") +
  labs(x = "Age Group", y = "Number of MACE Cases") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_blank(),
    axis.text.x = element_text(size = 13),  # Adjust x-axis text size and angle
    axis.text.y = element_text(size = 13),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  # Adjust x-axis title size and add space
    axis.title.y = element_text(size = 14, margin = margin(r = 12)),  # Adjust y-axis title size and add space
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 12)    # Adjust legend text size
  )





library(ggplot2)
library(dplyr)

# Define the columns of interest
columns_of_interest <- c('Cardiac.arrest', 'Cardiac.Arrhythmia', 'Stroke', 'Heart.Failure', 'Myocardial.Injury')

# Function to calculate confidence intervals
calculate_confidence_interval <- function(p, n) {
  if (n == 0) {
    return(c(0, 0))
  }
  z <- qnorm(0.975)  # 1.96 for 95% confidence
  se <- sqrt(p * (1 - p) / n)
  ci_lower <- p - z * se
  ci_upper <- p + z * se
  return(c(ci_lower * 100, ci_upper * 100))
}

# Initialize results data frames
results <- data.frame(Complication = rep(columns_of_interest, each = 2),
                      Sex = rep(c("Male", "Female"), length(columns_of_interest)),
                      Percentage = NA, CI_Lower = NA, CI_Upper = NA)

# Loop through each column and calculate percentages and confidence intervals for males and females
for (col in columns_of_interest) {
  
  male_data <- final_data2 %>% filter(slider_sex == "Male") %>% select(all_of(col)) %>% na.omit()
  female_data <- final_data2 %>% filter(slider_sex == "Female") %>% select(all_of(col)) %>% na.omit()
  
  # Calculate for males
  num_true_male <- sum(male_data == TRUE)
  total_male <- nrow(male_data)
  percentage_male <- (num_true_male / total_male) * 100
  ci_male <- calculate_confidence_interval(num_true_male / total_male, total_male)
  
  # Calculate for females
  num_true_female <- sum(female_data == TRUE)
  total_female <- nrow(female_data)
  percentage_female <- (num_true_female / total_female) * 100
  ci_female <- calculate_confidence_interval(num_true_female / total_female, total_female)
  
  # Store the results
  results[results$Complication == col & results$Sex == "Male", c("Percentage", "CI_Lower", "CI_Upper")] <- c(percentage_male, ci_male)
  results[results$Complication == col & results$Sex == "Female", c("Percentage", "CI_Lower", "CI_Upper")] <- c(percentage_female, ci_female)
}

# Plot using ggplot2
ggplot(results, aes(x = Complication, y = Percentage, fill = Sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(x = "Complication", y = "Percentage (%)") +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    legend.position = "bottom",  # Move the legend to the bottom
    axis.text.x = element_text(size = 11),  # Increase x-axis text size and rotate
    axis.text.y = element_text(size = 15),  # Increase y-axis text size
    axis.title.x = element_text(size = 13, margin = margin(t = 15)),  # Add space between x-axis label and axis
    axis.title.y = element_text(size = 13, margin = margin(r = 15))  # Add space between y-axis label and axis
  ) +
  scale_x_discrete(labels = c("Cardiac Arrest", "Cardiac Arrhythmia", "Stroke", "Heart Failure", "Myocardial Injury"))





library(dplyr)
library(ggplot2)
library(scales)

country_counts <- final_data2 %>%
  count(slider_country) %>%
  arrange(desc(n))

# Identify countries with fewer than 50 individuals
countries_to_replace <- country_counts %>%
  filter(n < 50) %>%
  pull(slider_country)

# Replace these country names with 'Other'
final_data2 <- final_data2 %>%
  mutate(slider_country = ifelse(slider_country %in% countries_to_replace, 'Other', slider_country))

# Verify the replacement
updated_country_counts <- final_data2 %>%
  count(slider_country) %>%
  arrange(desc(n))
column_name <- 'MACE'

# Calculate the percentage of TRUE values and their confidence intervals for each country
country_stats <- final_data2 %>%
  group_by(slider_country) %>%
  summarise(
    count_true = sum(get(column_name), na.rm = TRUE),
    count_total = sum(!is.na(get(column_name)))
  ) %>%
  mutate(
    proportion = count_true / count_total,
    se = sqrt(proportion * (1 - proportion) / count_total),
    ci_lower = proportion - qnorm(0.975) * se,
    ci_upper = proportion + qnorm(0.975) * se,
    proportion = proportion * 100,
    ci_lower = ci_lower * 100,
    ci_upper = ci_upper * 100
  ) %>%
  arrange(desc(proportion))

# Create the plot
ggplot(country_stats, aes(x = reorder(slider_country, -proportion), y = proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous( limits = c(0, 60)) +
  labs(x = "Country", y = "Percentage (%) ") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  # Increase x-axis text size and rotate
    axis.text.y = element_text(size = 12),  # Increase y-axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  # Add space between x-axis label and axis
    axis.title.y = element_text(size = 13, margin = margin(r = 12))  # Add space between y-axis label and axis
  )

library(dplyr)
library(ggplot2)

# Function to calculate confidence intervals
calculate_confidence_interval <- function(p, n) {
  if (n == 0) {
    return(c(NA, NA))  # Return NA if the sample size is 0
  }
  z <- qnorm(0.975)  # 1.96 for 95% confidence
  se <- sqrt(p * (1 - p) / n)
  ci_lower <- max(0, (p - z * se) * 100)  # Ensure lower bound is not negative
  ci_upper <- min(100, (p + z * se) * 100)  # Ensure upper bound does not exceed 100%
  return(c(ci_lower, ci_upper))
}

# Calculate the percentage of True values for MACE by income group and their confidence intervals
income_group_stats <- final_data2 %>%
  group_by(income) %>%
  summarize(
    count_true = sum(MACE == TRUE, na.rm = TRUE),
    count_total = sum(!is.na(MACE)),
    percentage = (count_true / count_total) * 100
  ) %>%
  rowwise() %>%
  mutate(
    ci_values = list(calculate_confidence_interval(percentage / 100, count_total)),
    ci_lower = ci_values[[1]][1],
    ci_upper = ci_values[[2]][1]
  ) %>%
  ungroup()

# Define the desired order for income groups
income_order <- c('Low income', 'Lower middle income', 'Upper middle income', 'High income')

# Reorder the income groups
income_group_stats$income <- factor(income_group_stats$income, levels = income_order)

# Plot the results
ggplot(income_group_stats, aes(x = income, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_x_discrete(labels = c("Low", "Lower middle", "Upper middle", "High")) +  # Relabel x-axis ticks
  labs(x = "Income", y = "Percentage (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),  # Set x-axis text size and position
    axis.text.y = element_text(size = 14),  # Set y-axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  # Add space between x-axis label and axis
    axis.title.y = element_text(size = 14, margin = margin(r = 12))  # Add space between y-axis label and axis
  )


# Load necessary libraries
library(ggplot2)
library(readr)

# Load the data from the CSV file
data <- read_csv("odds_ratio_data.csv")

# Replace comorbidity column names with more descriptive labels
data$Comorbidities <- recode(data$Comorbidities,
                             comorbid_aids_hiv = "AIDS/HIV",
                             comorbid_asthma = "Asthma",
                             comorbid_chronic_cardiac_disease = "Chronic Cardiac Disease",
                             comorbid_chronic_haematological_disease = "Chronic Haematological Disease",
                             comorbid_chronic_kidney_disease = "Chronic Kidney Disease",
                             comorbid_chronic_neurological_disorder = "Chronic Neurological Disorder",
                             comorbid_chronic_pulmonary_disease = "Chronic Pulmonary Disease",
                             comorbid_dementia = "Dementia",
                             comorbid_diabetes = "Diabetes",
                             comorbid_hypertension = "Hypertension",
                             comorbid_immunosuppression = "Immunosuppression",
                             comorbid_liver_disease = "Liver Disease",
                             comorbid_malignant_neoplasm = "Malignant Neoplasm",
                             comorbid_malnutrition = "Malnutrition",
                             comorbid_obesity = "Obesity",
                             comorbid_other = "Other",
                             comorbid_rare_diseases_and_inborn_errors_of_metabolism = "Rare Diseases",
                             comorbid_rheumatologic_disorder = "Rheumatologic Disorder",
                             comorbid_smoking = "Smoking",
                             comorbid_transplantation = "Transplantation",
                             comorbid_tuberculosis = "Tuberculosis",
                             comorbid_pregnancy = "Pregnancy")

# Create the plot
ggplot(data, aes(x = reorder(Comorbidities, Odds_Ratio), y = Odds_Ratio)) +
  geom_point(size = 2, color = "black") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "red") +
  coord_flip() +  # Flip the coordinates to make it horizontal
  theme_minimal() +
  labs(
    x = "Comorbidities",
    y = "Odds Ratio"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue") +  # Add a reference line at OR = 1
  theme(
    axis.text.x = element_text(size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 10),  # Increase y-axis text size
    axis.title.x = element_text(size = 13, margin = margin(t = 12)),  # Add space between x-axis label and axis
    axis.title.y = element_text(size = 13, margin = margin(r = 12))   # Add space between y-axis label and axis
  )



# Filter for patients who developed MACE and group by age group and sex
grouped_data <- final_data2 %>%
  filter(dsdecod == 'DEATH') %>%
  group_by(age_group, slider_sex) %>%
  summarise(count = n()) %>%
  ungroup()

grouped_data <- grouped_data[!is.na(grouped_data$age_group), ]

# Adjust the count for males (make them negative)
grouped_data <- grouped_data %>%
  mutate(count_adj = ifelse(slider_sex == "Male", -count, count))

# Create the pyramid plot
ggplot(grouped_data, aes(x = age_group, y = count_adj, fill = slider_sex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-12000, 10000)) +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"), name = "Sex") +
  labs(x = "Age Group", y = "Number of Deaths") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_blank(),
    axis.text.x = element_text(size = 13),  # Adjust x-axis text size
    axis.text.y = element_text(size = 13),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  # Adjust x-axis title size and add space
    axis.title.y = element_text(size = 14, margin = margin(r = 12)),  # Adjust y-axis title size and add space
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 12)    # Adjust legend text size
  )



library(dplyr)
library(ggplot2)

# Filter data to include only deaths and discharges
filtered_data <- final_data2 %>%
  filter(dsdecod %in% c("DEATH", "DISCHARGED"))

# Remove rows with NA in specified columns
filtered_data <- filtered_data[complete.cases(filtered_data[, c("age_group", "slider_sex", "income")]), ]

num_rows <- nrow(filtered_data)
print(num_rows)

# Calculate the total number of patients and the number of deaths by age group and sex
age_sex_summary <- filtered_data %>%
  group_by(age_group, slider_sex) %>%
  summarise(
    total = n(),
    deaths = sum(dsdecod == 'DEATH'),
    prop = deaths / total,
    se = sqrt(prop * (1 - prop) / total),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se
  ) %>%
  mutate(
    ci_lower = pmax(0, ci_lower),  # Ensure confidence intervals are within [0,1]
    ci_upper = pmin(1, ci_upper)
  )

# Separate the data for males and females to handle their confidence intervals correctly
male_data <- age_sex_summary %>%
  filter(slider_sex == "Male") %>%
  mutate(
    prop = -prop,
    ci_lower = -ci_lower,
    ci_upper = -ci_upper
  )

print(age_sex_summary)

female_data <- age_sex_summary %>%
  filter(slider_sex == "Female")

# Combine the male and female data back together
age_sex_summary <- bind_rows(male_data, female_data)



# Create the pyramid plot
ggplot(age_sex_summary, aes(x = age_group, y = prop, fill = slider_sex)) +
  geom_bar(stat = "identity", position = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
  coord_flip() +  # Flip coordinates to have age on the y-axis
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"), name = "Sex") +
  labs(x = "Age Group", y = "Proportion of Deaths") +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    plot.title = element_blank(),  # Remove the title
    axis.text.x = element_text(size = 13),  # Adjust x-axis text size and angle
    axis.text.y = element_text(size = 13),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  # Increase x-axis title size and add space
    axis.title.y = element_text(size = 14, margin = margin(r = 12)),  # Increase y-axis title size and add space
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 12)  # Adjust legend text size
  )


# Define the maximum value for the x-axis limit
max_value <- max(abs(c(age_sex_summary$prop, age_sex_summary$ci_lower, age_sex_summary$ci_upper)))



# Calculate median age and interquartile range for death among males
age_stats_male <- filtered_data %>%
  filter(dsdecod == "DEATH", slider_sex == "Male") %>%
  summarise(
    median_age = median(age, na.rm = TRUE),
    IQR_age = IQR(age, na.rm = TRUE)
  )

# Calculate median age and interquartile range for death among females
age_stats_female <- filtered_data %>%
  filter(dsdecod == "DEATH", slider_sex == "Female") %>%
  summarise(
    median_age = median(age, na.rm = TRUE),
    IQR_age = IQR(age, na.rm = TRUE)
  )

# Print the results
print(age_stats_male)
print(age_stats_female)







filtered_data <- final_data2 %>%
  filter(dsdecod %in% c("DEATH", "DISCHARGED"))

filtered_data <- filtered_data %>%
  mutate(income = factor(income, levels = c('Low income', 'Lower middle income',
                                            'Upper middle income', 'High income')))

# Create a dataframe with proportion of deaths and confidence intervals
prop_deaths <- filtered_data %>%
  group_by(income) %>%
  summarise(
    total = n(),
    deaths = sum(dsdecod == "DEATH"),
    prop = deaths / total,
    se = sqrt(prop * (1 - prop) / total),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se
  ) %>%
  mutate(
    ci_lower = pmax(0, ci_lower),  # Ensure confidence intervals are within [0,1]
    ci_upper = pmin(1, ci_upper)
  )

print(prop_deaths)

# Create the plot
ggplot(prop_deaths, aes(x = income, y = prop)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
  labs(
    x = "Income Group",
    y = "Proportion of Deaths"
  ) +
  scale_x_discrete(labels = c("Low", "Lower middle", "Upper middle", "High")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(size = 13),  # Adjust x-axis text size and angle
    axis.text.y = element_text(size = 13),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),  # Adjust x-axis title size and add space
    axis.title.y = element_text(size = 14, margin = margin(r = 15))   # Adjust y-axis title size and add space
  )



