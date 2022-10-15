library(tidyverse)
library(broom)
library(dplyr)
library(patchwork)
library(tidymodels)
library(gtools) # Import libraries

# Need to read.csv first in order to progress to codes below

employee_promotion_1_ = read.csv("employee_promotion.csv")


employee_promotion_filtered = employee_promotion_1_ %>%
  select(-region, -employee_id) %>%
  drop_na() # Load dataset and remove NAs, region and employee_id
write.csv(employee_promotion_filtered, "employee_promotion_filtered.csv") # Save as csv


# Create dataframe for comparing gender vs. score

gender_vs_score = data.frame(
  gender = c("f", "m"),
  number_in_30_40 = c(0,0),
  number_in_40_50 = c(0,0),
  number_in_50_60 = c(0,0),
  number_in_60_70 = c(0,0),
  number_in_70_80 = c(0,0),
  number_in_80_90 = c(0,0),
  number_in_90_100 = c(0,0)
)

# Calculate total number of each gender to prepare for percentage calculation

total_num_f = nrow(employee_promotion_filtered %>% filter(gender == "f"))
total_num_m = nrow(employee_promotion_filtered %>% filter(gender == "m"))

# Calculate percentage and pivot the dataframe in order to plot

for (i in c(2,3,4,5,6,7,8)) {
  gender_vs_score[1,i] <- nrow(employee_promotion_filtered %>% filter(gender == "f", avg_training_score > 30 + (i - 2) * 10, avg_training_score < 40 + (i - 2) * 10))/total_num_f
  gender_vs_score[2,i] <- nrow(employee_promotion_filtered %>% filter(gender == "m", avg_training_score > 30 + (i - 2) * 10, avg_training_score < 40 + (i - 2) * 10))/total_num_m
}

gender_vs_score = gender_vs_score %>%
  pivot_longer(
    cols = c(  "number_in_30_40",
               "number_in_40_50",
               "number_in_50_60",
               "number_in_60_70",
               "number_in_70_80",
               "number_in_80_90",
               "number_in_90_100"),
    names_to = "Range",
    values_to = "Number")

gender_vs_score["Range"] <- c("30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100") 

# Make bar plot

ggplot(data = gender_vs_score, aes(fill = gender, x = Range, y = Number)) +
  geom_bar(position="dodge", stat="identity") +
  labs(
    title = "Percentage of people vs. score range in different genders",
    x = "Score range",
    y = "Percentage"
    ) +
  scale_fill_viridis_d(name = "Gender", labels = c("Female", "Male")) +
  theme_light()

employee_promotion_filtered_extended = employee_promotion_filtered %>%
  mutate(range = case_when(
    avg_training_score > 30 & avg_training_score < 40 ~ "30-40",
    avg_training_score > 40 & avg_training_score < 50 ~ "40-50",
    avg_training_score > 50 & avg_training_score < 60 ~ "50-60",
    avg_training_score > 60 & avg_training_score < 70 ~ "60-70",
    avg_training_score > 70 & avg_training_score < 80 ~ "70-80",
    avg_training_score > 80 & avg_training_score < 90 ~ "80-90",
    avg_training_score > 90 & avg_training_score < 100 ~ "90-100"
  ))

# Calculate total number of people in each score range for further calculation

range_total_f  = employee_promotion_filtered_extended %>%
  filter(gender == "f") %>%
  count(range)


range_total_m  = employee_promotion_filtered_extended %>%
  filter(gender == "m") %>%
  count(range)

range_total = rbind(range_total_f, range_total_m) %>%
  drop_na()

# Create dataframe that links gender, score range and promotions

gender_score_promotion = employee_promotion_filtered_extended %>%
  select(gender, range, is_promoted) %>%
  drop_na() %>%
  group_by(gender, range) %>%
  summarize(num_promoted = sum(is_promoted)) %>%
  cbind(range_total) %>%
  mutate(percentage_promoted = num_promoted/n)

# Make bar plot

ggplot(data = gender_score_promotion, aes(fill = gender, x = range...2, y = percentage_promoted)) +
  geom_bar(position="dodge", stat="identity") +
  labs(
    title = "Percentage of promotion vs. score range in different genders",
    x = "Score range",
    y = "Percentage"
  ) +
  scale_fill_viridis_d(name = "Gender", labels = c("Female", "Male")) +
  theme_light()

# Calculate total number of people in each educational category

edu_total_f  = employee_promotion_filtered_extended %>%
  filter(gender == "f") %>%
  count(education)


edu_total_m  = employee_promotion_filtered_extended %>%
  filter(gender == "m") %>%
  count(education)

edu_total = rbind(edu_total_f, edu_total_m) %>%
  drop_na()

# Create dataframe

gender_education_promotion = employee_promotion_filtered_extended %>%
  select(gender, education, is_promoted) %>%
  group_by(gender, education) %>%
  summarize(num_promoted = sum(is_promoted)) %>%
  cbind(edu_total)

# Make bar plot

ggplot(data = gender_education_promotion, aes(fill = gender, x = factor(education...2, levels = c("Below Secondary", "Bachelor's", "Master's & above")), y = num_promoted/n)) +
  geom_bar(position="dodge", stat="identity") +
  labs(
    title = "Percentage of promotion vs. education in different genders",
    x = "Education",
    y = "Percentage"
  ) +
  scale_fill_viridis_d(name = "Gender", labels = c("Female", "Male")) +
  theme_light()

# Make regression mode

employee_promotion_filtered_extended_categorical = employee_promotion_filtered_extended
employee_promotion_filtered_extended_categorical$is_promoted = as.factor(employee_promotion_filtered_extended_categorical$is_promoted)

set.seed(9841)
data_split <- initial_split(employee_promotion_filtered_extended_categorical, prop = 0.8)
training_data = training(data_split)
testing_data = testing(data_split) # Seperating dataset into training set and testing set

data_rec <- recipe(
  is_promoted ~ gender,
  data = training_data
) %>%
  step_dummy(all_nominal(), -all_outcomes())

data_model <- logistic_reg() %>%
  set_engine("glm") # Add recipe and model

data_workflow <- workflow() %>%
  add_model(data_model) %>%
  add_recipe(data_rec)
data_fit <- data_workflow %>%
  fit(data = training_data)

data_predict <- predict(data_fit, testing_data, type = "prob") %>%
  bind_cols(testing_data)

print(data_predict)
