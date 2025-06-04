# fp2030_analysis.R

# ────────────────────────────────────────────────────────────────────────────────
# 1. Load libraries and inspect the raw CSV
# ────────────────────────────────────────────────────────────────────────────────

# (Install these packages if you don’t have them: install.packages(c("tidyverse","janitor","lubridate")))
library(tidyverse)
library(janitor)
library(lubridate)

# Read the CSV into a tibble
raw <- read_csv("FP2030_awareness.csv") %>% 
  clean_names() 
# clean_names() will convert column names to snake_case: 
#    e.g. "Age-group" → age_group, "State of Residence" → state_of_residence, etc.

# Quick peek
glimpse(raw)
# Columns should now be:
#  • sex                       (chr: "Male"/"Female")
#  • age_group                 (chr: "18-24","25-34","35-45","46-55","56 and above")
#  • state_of_residence        (chr: state names)
#  • area_type                 (chr: "Urban"/"Rural")
#  • fp2030_policy_awarenes    (chr: "Yes"/"No")
#  • barriers_to_fp2030_implementation (chr: pipe- or comma-separated barrier choices)


# Define the North East and South East vectors
north_east_states <- c("Adamawa","Bauchi","Borno","Gombe","Taraba","Yobe")
south_east_states <- c("Abia","Anambra","Ebonyi","Enugu","Imo")

# Filter the dataset for only NE + SE
df1 <- raw %>%
  filter(state_of_residence %in% c(north_east_states, south_east_states))

# Confirm how many rows remain
df1 %>% count(state_of_residence) %>% arrange(desc(n))


df1 <- df1 %>%
  mutate(
    age_group_clean = case_when(
      age_group %in% c("46-55", "56 and above", "56 and above ") ~ "Above 45",
      TRUE                                      ~ age_group
    )
  )
# Check
df1 %>% tabyl(age_group_clean)


# 1) Split multiple‐choice barriers into separate rows
df_barriers_long <- df1 %>%
  # Some CSVs use semicolons, some commas. We'll standardize by replacing commas with semicolons,
  # then using separate_rows() on semicolons (;) to explode multiple selections.
  mutate(
    barriers_clean = str_replace_all(barriers_to_fp2030_implementation, ",", ";")
  ) %>%
  separate_rows(barriers_clean, sep = ";") %>%
  mutate(
    barriers_clean = str_trim(barriers_clean)
  )

# 2) Rule A: If fp2030_policy_awarenes == "No", then barrier must be "No idea" (or NA).
#    - If they have any barrier other than "No idea", remove it.
#    - If they didn’t have barrier = "No idea", set it to "No idea".

df_barriers_long <- df_barriers_long %>%
  group_by(sex, age_group, state_of_residence, area_type, fp2030_policy_awarenes, row_id = row_number()) %>%
  # row_id is temporary to keep track of each respondent
  ungroup() %>%
  mutate(
    barriers_clean = case_when(
      fp2030_policy_awarenes == "No" ~ "No idea", 
      TRUE                            ~ barriers_clean
    )
  )

# 3) Rule B: If someone’s barrier list contains "No idea" **and** other barrier strings, drop the other strings.
df_barriers_long <- df_barriers_long %>%
  group_by(sex, age_group, state_of_residence, area_type, fp2030_policy_awarenes, row_id) %>%
  mutate(
    # Check if "No idea" appears anywhere for this respondent
    has_no_idea = any(str_to_lower(barriers_clean) == "no idea")
  ) %>%
  ungroup() %>%
  mutate(
    barriers_clean = if_else(
      has_no_idea, 
      "No idea", 
      barriers_clean
    )
  ) %>%
  select(-has_no_idea)

# 4) Collapse it back into one row per respondent, separating barriers by semicolon again
df_clean <- df_barriers_long %>%
  group_by(sex, age_group_clean, state_of_residence, area_type, fp2030_policy_awarenes, row_id) %>%
  summarize(
    barriers_final = paste(unique(barriers_clean), collapse = "; "), 
    .groups = "drop"
  ) %>%
  rename(age_group = age_group_clean)

# 5) Drop the row_id column since it was just for grouping
df_clean <- df_clean %>% select(-row_id)

# Final sanity check
glimpse(df_clean)
df_clean %>% count(fp2030_policy_awarenes, barriers_final) %>% print(n = 20)



# 2.1a: Count of respondents by state (descending)
state_counts <- df_clean %>%
  count(state_of_residence) %>%
  arrange(desc(n))
print(state_counts)

# 2.1b: Distribution by sex and by age group
sex_counts <- df_clean %>%
  count(sex) %>%
  mutate(pct = round(100 * n / sum(n), 1))
print(sex_counts)

age_counts <- df_clean %>%
  count(age_group) %>%
  mutate(pct = round(100 * n / sum(n), 1))
print(age_counts)



library(ggplot2)

# Bar chart: Number of respondents by sex
ggplot(sex_counts, aes(x = sex, y = n, fill = sex)) +
  geom_col() +
  labs(
    title = "Respondent Count by Sex (NE + SE)", 
    x = "Sex", y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Bar chart: Number of respondents by age group
ggplot(age_counts, aes(x = age_group, y = n, fill = age_group)) +
  geom_col() +
  labs(
    title = "Respondent Count by Age Group (NE + SE)", 
    x = "Age Group", y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# 2.2a: Overall percentage who have heard of FP2030
awareness_overall <- df_clean %>%
  count(fp2030_policy_awarenes) %>%
  mutate(pct = round(100 * n / sum(n), 1))
print(awareness_overall)
# e.g.  “Yes:  1,675 (65.6%)”  , “No:  878 (34.4%)”

# 2.2b: Awareness by state (ranked)
awareness_by_state <- df_clean %>%
  group_by(state_of_residence) %>%
  summarize(
    n_total = n(),
    n_yes = sum(fp2030_policy_awarenes == "Yes"),
    pct_yes = round(100 * n_yes / n_total, 1)
  ) %>%
  arrange(desc(pct_yes))
print(awareness_by_state)

# 2.2c: Compare sex vs area_type as predictors
#   – We’ll compute the awareness percentage for each sex, and for each area_type.

awareness_by_sex <- df_clean %>%
  group_by(sex) %>%
  summarize(
    n_total = n(),
    n_yes = sum(fp2030_policy_awarenes == "Yes"),
    pct_yes = round(100 * n_yes / n_total, 1)
  )
print(awareness_by_sex)

awareness_by_area <- df_clean %>%
  group_by(area_type) %>%
  summarize(
    n_total = n(),
    n_yes = sum(fp2030_policy_awarenes == "Yes"),
    pct_yes = round(100 * n_yes / n_total, 1)
  )
print(awareness_by_area)



ggplot(awareness_by_state, 
       aes(x = reorder(state_of_residence, -pct_yes), y = pct_yes, fill = pct_yes)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "FP2030 Awareness Rate by State (NE + SE)",
    x = "State of Residence",
    y = "Percent Aware"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Split the cleaned barriers_final into separate rows again
barriers_long <- df_clean %>%
  filter(barriers_final != "") %>%
  separate_rows(barriers_final, sep = ";") %>%
  mutate(barrier = str_trim(barriers_final)) %>%
  select(-barriers_final)

# 2.3a: Count occurrences of each barrier (overall)
barrier_counts_overall <- barriers_long %>%
  count(barrier) %>%
  arrange(desc(n)) 
print(barrier_counts_overall)

# Top 3 overall:
barrier_counts_overall %>% slice(1:3)

# 2.3b: Top 3 barriers by area_type (Urban vs Rural)
barrier_by_area <- barriers_long %>%
  group_by(area_type, barrier) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(area_type, desc(count))

# For each area_type, pick top 3
top3_by_area <- barrier_by_area %>%
  group_by(area_type) %>%
  slice_max(order_by = count, n = 3) %>%
  ungroup()
print(top3_by_area)


ggplot(top3_by_area, aes(x = reorder(barrier, count), y = count, fill = area_type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ area_type) +
  coord_flip() +
  labs(
    title = "Top 3 FP2030 Implementation Barriers by Area Type",
    x = "Barrier",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


write_csv(df_clean, "FP2030_cleaned_NE_SE.csv")


# Example: Counts of respondents by state
state_counts <- df_clean %>%
  count(state_of_residence) %>%
  arrange(desc(n))

state_counts

write_csv(state_counts, "state_counts.csv")

library(writexl)
write_xlsx(state_counts, "state_counts.xlsx")




