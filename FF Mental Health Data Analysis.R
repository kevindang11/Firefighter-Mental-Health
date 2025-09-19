install.packages('dplyr')
install.packages('tidyr')
install.packages('janitor')
install.packages("ggplot2")
install.packages("rms")
install.packages("reshape2")
install.packages("MASS")
install.packages("car")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("broom")
install.packages("lmtest")
install.packages("table1")
install.packages("purrr")
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(rms)
library(reshape2)
library(MASS)
library(car)
library(pROC)
library(ResourceSelection)
library(broom)
library(splines)
library(lmtest)
library(table1)
library(purrr)

#importing csv data
FF <- read.csv("/Users/kevindang/Downloads/FF Dataset (Data Analysis).csv", check.names = FALSE)

#removing all n/a
clean_FF <- na.omit(FF)

#keeping only ID, independent, and dependent variables
clean_FF <- clean_FF %>% dplyr::select(ID, Age, years, Weight, Height, Gender, Race, Ethnicity, Smoking, Alcohol, Exercise, Fire_Incident:Divorce_C, 
                                CESD, CESD_bi, FAST, FAST_bi,ASI, ASI_bi, PCL, PCL_bi, CFQ, CFQ_bi)

#Transforms verbatim (other) answer choices into binary
#clean_FF <- clean_FF %>% mutate(across(c("Other", "Other_C"), ~ as.integer(. != 0)))

#transform numeric data into numeric datatype and creating summary statistics
clean_FF <- clean_FF %>% mutate(across(c(Age, Weight, Height, years), as.numeric))

#creating BMI variable from Weight and Height
clean_FF <- clean_FF %>% 
  mutate(BMI = (Weight / (Height^2)) * 703)

#Replacing newly created NAs with 0s
clean_FF <- clean_FF %>%
  mutate(BMI = replace_na(BMI, 0))

clean_FF <- clean_FF %>%
  mutate(years = replace_na(years, 0))

#total numeric summary statistics (n=132) -> not stratified
clean_FF %>% dplyr::select(Age, BMI, years) %>%
  summarise(across(everything(),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      SD = ~ sd(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Min = ~ min(., na.rm = TRUE),
      Max = ~ max(., na.rm = TRUE),
      N = ~ sum(!is.na(.))))) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_sep = "_") %>%
  pivot_wider(
    names_from = "Statistic",
    values_from = "value") %>%
  mutate(across(c(Mean, SD, Median, Min, Max), ~ round(., 2)))

#completed responses (no zeros, n=101) <- also not stratified
complete_numeric <- clean_FF %>% dplyr::select(Age, BMI, years) %>%
  filter(if_all(everything(), ~ . != 0)) 
complete_numeric %>% dplyr::select(c("Age", "BMI", "years")) %>%
  summarise(across(everything(),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      SD = ~ sd(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Min = ~ min(., na.rm = TRUE),
      Max = ~ max(., na.rm = TRUE),
      N = ~ sum(!is.na(.))))) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_sep = "_") %>%
  pivot_wider(
    names_from = "Statistic",
    values_from = "value") %>%
  mutate(across(c(Mean, SD, Median, Min, Max), ~ round(., 2)))


#Numeric Summary Statistics (stratified by outcome status for each outcome variable)
stratified_stats <- clean_FF %>%
  pivot_longer(
    cols = all_of(c("CESD_bi", "FAST_bi", "ASI_bi", "PCL_bi", "CFQ_bi")),
    names_to = "Outcome",
    values_to = "Status"
  ) %>%
  filter(Status %in% c(0, 1)) %>%
  group_by(Outcome, Status) %>%
  summarise(
    across(
      all_of(c("Age", "BMI", "years")),
      list(
        mean = function(x) mean(x[x != 0], na.rm = TRUE),
        sd = function(x) sd(x[x != 0], na.rm = TRUE),
        median = function(x) median(x[x != 0], na.rm = TRUE),
        min = function(x) min(x[x != 0], na.rm = TRUE),
        max = function(x) max(x[x != 0], na.rm = TRUE),
        n_nonzero = function(x) sum(x != 0, na.rm = TRUE),
        n_total = function(x) sum(!is.na(x))
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(Outcome, Status),
    names_to = c("Variable", ".value"),
    names_sep = "__"
  ) %>%
  mutate(
    across(c(mean, sd, median, min, max), ~round(., 2)),
    `Mean (SD)` = ifelse(n_nonzero > 0, sprintf("%.2f (%.2f)", mean, sd), NA_character_),
    `Median [Range]` = ifelse(n_nonzero > 0, sprintf("%.2f [%.2f-%.2f]", median, min, max), NA_character_),
    `N (Nonzero/Total)` = sprintf("%d/%d", n_nonzero, n_total)
  ) %>%
  dplyr::select(Outcome, Status, Variable, `Mean (SD)`, `Median [Range]`, `N (Nonzero/Total)`) %>%
  arrange(Outcome, Status, Variable)

#Numeric Summary Statistics (stratified by outcome variables)
outcome_numeric <- clean_FF %>%
  pivot_longer(cols = all_of(c("CESD_bi", "FAST_bi", "ASI_bi", "PCL_bi", "CFQ_bi")),
               names_to = "Outcome", values_to = "Status") %>%
  filter(!is.na(Status)) %>%
  group_by(Outcome) %>%
  summarise(across(all_of(c("Age", "BMI", "years")), list(
    mean = function(x) mean(x[x != 0], na.rm = TRUE),
    sd = function(x) sd(x[x != 0], na.rm = TRUE),
    median = function(x) median(x[x != 0], na.rm = TRUE),
    min = function(x) min(x[x != 0], na.rm = TRUE),
    max = function(x) max(x[x != 0], na.rm = TRUE),
    n_nonzero = function(x) sum(x != 0, na.rm = TRUE),
    n_total = function(x) sum(!is.na(x))),
    .names = "{.col}__{.fn}"), .groups = "drop") %>%
  pivot_longer(cols = -Outcome, names_to = c("Variable", ".value"), names_sep = "__") %>%
  mutate(across(c(mean, sd, median, min, max), ~round(., 2)),
         `Mean (SD)` = ifelse(n_nonzero > 0, sprintf("%.2f (%.2f)", mean, sd), NA_character_),
         `Median [Range]` = ifelse(n_nonzero > 0, sprintf("%.2f [%.2f-%.2f]", median, min, max), NA_character_),
         `N (Nonzero/Total)` = sprintf("%d/%d", n_nonzero, n_total)) %>%
  dplyr::select(Outcome, Variable, `Mean (SD)`, `Median [Range]`, `N (Nonzero/Total)`) %>%
  arrange(Outcome, Variable)

#pearson correlation matrix for numeric variables
numeric_vars <- c("Age", "BMI", "years", "CESD", "FAST", "ASI", "PCL", "CFQ")
cor_matrix <- cor(clean_FF[, numeric_vars], use = "complete.obs")
melted_cormat <- melt(cor_matrix, na.rm = TRUE)

ggplot(melted_cormat, aes(
  x = factor(Var1, levels = rev(numeric_vars)),
  y = factor(Var2, levels = rev(levels(Var2))),
  fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(
    low = "#6D9EC1", high = "#E46726", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Pearson\nCorrelation") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = -45, vjust = 1, hjust = 1, face = "bold"),
    axis.text.y = element_text(angle = -45, face = "bold", margin = margin(l=10)),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)) +
  coord_fixed() +
  geom_text(
    aes(label = ifelse(is.na(value), "", sprintf("%.2f", value))),
    color = "black", size = 3.5, angle = -45,
    vjust = 0.5, hjust = 0.5) +
  labs(x = NULL, y = NULL)

#categorizing Age
summary(clean_FF$Age) #getting min-max range
clean_FF <- clean_FF %>%
  mutate(age_categories = case_when(
    Age >= 18 & Age <= 40 ~ "18-40",
    Age > 40 & Age <= 60 ~ "41-60",
    Age > 60 & Age <= 80 ~ "61-80",
    Age == 0 | is.na(Age) ~ "Unknown",
    TRUE ~ "Unknown") %>%
      factor(levels = c("18-40", "41-60", "61-80", "Unknown")))
summary(clean_FF $age_categories) # 7 'Unknown'

#total categorical summary statistics
categorical_sum <- clean_FF %>% mutate(across(c("Gender", "Race", "Ethnicity", "Smoking", "Alcohol", "Exercise", "Fire_Incident":"Divorce_C", "age_categories"), as.character)) %>%
  pivot_longer(c("Gender", "Race", "Ethnicity", "Smoking", "Alcohol", "Exercise", "Fire_Incident":"Divorce_C", "age_categories")) %>%
  count(name, value) %>%
  group_by(name) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(name, desc(n))

#stratified categorical summary statistics (by outcome and outcome status)
outcome_vars <- c("CESD_bi", "FAST_bi", "ASI_bi", "PCL_bi", "CFQ_bi")
categorical_vars <- c("Gender", "Race", "Ethnicity", "Smoking", "Alcohol", "Exercise", "Fire_Incident",
                      "Disaster", "Death", "Accidents", "Illness", "Bullying", "Abuse_C", "Neglect_C", "Dom_Violence_C",
                      "Disasters_C","Death_C", "Accidents_C", "Displacement_C", "Illness_C", "Bullying_C", "Divorce_C","age_categories")

stratified_cat <- clean_FF %>%
  mutate(across(all_of(categorical_vars), as.character)) %>%
  pivot_longer(cols = all_of(categorical_vars), 
               names_to = "variable", 
               values_to = "category") %>%
  pivot_longer(cols = all_of(outcome_vars), 
               names_to = "outcome", 
               values_to = "outcome_status") %>%
  count(variable, category, outcome, outcome_status) %>%
  group_by(variable, outcome, outcome_status) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(outcome, variable, outcome_status, desc(n)) %>%
  filter(!is.na(category))

# Create a list to store the individual outcome tables
outcome_tables <- list()

# Loop through each outcome variable
for (outcome_var in outcome_vars) {
  # Filter for the current outcome
  outcome_data <- stratified_cat %>%
    dplyr::filter(outcome == outcome_var)
  
  # Create the table for this outcome
  outcome_table <- outcome_data %>%
    # Combine n and percent into a single column
    dplyr::mutate(n_percent = paste0(n, " (", round(percent, 1), "%)")) %>%
    # Pivot wider
    tidyr::pivot_wider(
      id_cols = c(variable, category),
      names_from = outcome_status,
      values_from = n_percent,
      names_glue = "status_{outcome_status}",
      values_fill = "0 (0.0%)"
    ) %>%
    dplyr::arrange(variable, category)
  
  # Store the table in the list with the outcome name
  outcome_tables[[outcome_var]] <- outcome_table
}

# Access individual outcome tables
cesd_table <- outcome_tables[["CESD_bi"]]
fast_table <- outcome_tables[["FAST_bi"]]
asi_table <- outcome_tables[["ASI_bi"]]
ptsd_table <- outcome_tables[["PCL_bi"]]
cfq_table <- outcome_tables[["CFQ_bi"]]

#stratified categorical summary statistics only by outcome - total column in categorical summary statistics
outcome_stratified_summary <- clean_FF %>%
  # Convert categorical variables to character
  mutate(across(all_of(categorical_vars), as.character)) %>%
  # Pivot categorical variables to long format
  pivot_longer(cols = all_of(categorical_vars), 
               names_to = "variable", 
               values_to = "category") %>%
  # Pivot outcome variables to long format (but don't keep values)
  pivot_longer(cols = all_of(outcome_vars),
               names_to = "outcome_name",
               values_to = "outcome_value",
               values_drop_na = TRUE) %>%
  # Count combinations (ignoring outcome_value)
  count(outcome_name, variable, category) %>%
  # Calculate percentages within each outcome-variable combination
  group_by(outcome_name, variable) %>%
  mutate(percent = n / sum(n) * 100) %>%
  # Arrange results
  arrange(outcome_name, variable, desc(n)) %>%
  # Remove NA categories
  filter(!is.na(category))


#p-values for predictor variables

#t-tests
# Depression
t.test(Age ~ CESD_bi, data = clean_FF)
t.test(BMI ~ CESD_bi, data = clean_FF)
t.test(years ~ CESD_bi, data = clean_FF)

# Stress
t.test(Age ~ FAST_bi, data = clean_FF)
t.test(BMI ~ FAST_bi, data = clean_FF)
t.test(years ~ FAST_bi, data = clean_FF)

# Anxiety
t.test(Age ~ ASI_bi, data = clean_FF)
t.test(BMI ~ ASI_bi, data = clean_FF)
t.test(years ~ ASI_bi, data = clean_FF)

# PTSD
t.test(Age ~ PCL_bi, data = clean_FF)
t.test(BMI ~ PCL_bi, data = clean_FF)
t.test(years ~ PCL_bi, data = clean_FF)

# CFQ
t.test(Age ~ CFQ_bi, data = clean_FF)
t.test(BMI ~ CFQ_bi, data = clean_FF)
t.test(years ~ CFQ_bi, data = clean_FF)

#Chi Square Tests

#counting 0s/NAs in continuous Age variable to ensure 'Unknown' count is correct
num_zeros <- sum(clean_FF$Age == 0, na.rm = TRUE)
print(num_zeros)

outcome_vars <- c("CESD_bi", "FAST_bi", "ASI_bi", "PCL_bi", "CFQ_bi")
numeric_vars <- c("Age", "BMI", "years")
categorical_vars <- c("Gender", "Race", "Ethnicity", "Smoking", "Alcohol", "Exercise", "Fire_Incident",
                      "Disaster", "Death", "Accidents", "Illness", "Bullying", "Abuse_C", "Neglect_C", "Dom_Violence_C",
                      "Disasters_C","Death_C", "Accidents_C", "Displacement_C", "Illness_C", "Bullying_C", "Divorce_C", "age_categories")

results <- list()
for(outcome in outcome_vars) for(predictor in categorical_vars) {
  test_name <- paste(outcome, "by", predictor)
  if(!(outcome %in% names(clean_FF)) | !(predictor %in% names(clean_FF))) {
    results[[test_name]] <- "Variable not found"; next
  }
  
  tbl <- table(clean_FF[[predictor]], clean_FF[[outcome]])
  if(sum(tbl) == 0) { results[[test_name]] <- "Empty table"; next }
  
  expected <- suppressWarnings(chisq.test(tbl)$expected)
  small_cells <- any(expected < 5)
  test <- if(!small_cells) chisq.test(tbl) else fisher.test(tbl)
  
  results[[test_name]] <- list(
    test_type = ifelse(!small_cells, "Chi-square", "Fisher's exact"),
    table = tbl,
    p_value = test$p.value,
    stat = if(!small_cells) c(test$statistic, test$parameter) else NA
  )
  
  cat("\n", outcome, "~", predictor, 
      "\nTest:", results[[test_name]]$test_type,
      "\np =", round(test$p.value, 4), 
      if(!small_cells) sprintf("\nχ²(%s) = %.2f", test$parameter, test$statistic),
      "\n")
  print(tbl)
}

#Table of Covariates' p-values for each outcome variable

# Function to get p-value for numeric variables (t-test)
get_ttest_p <- function(outcome, covariate) {
  formula <- as.formula(paste(covariate, "~", outcome))
  test_result <- t.test(formula, data = clean_FF)
  return(test_result$p.value)
}

# Function to get p-value for categorical variables (chi-square/fisher)
get_chisq_p <- function(outcome, predictor) {
  tbl <- table(clean_FF[[predictor]], clean_FF[[outcome]])
  if (sum(tbl) == 0) return(NA)
  
  expected <- suppressWarnings(chisq.test(tbl)$expected)
  small_cells <- any(expected < 5)
  
  if (!small_cells) {
    test_result <- chisq.test(tbl)
  } else {
    test_result <- fisher.test(tbl)
  }
  return(test_result$p.value)
}

# Create dataframe for numeric variables
numeric_pvals <- expand_grid(
  Variable = numeric_vars,
  Type = "Numeric",
  Outcome = outcome_vars
) %>%
  rowwise() %>%
  mutate(
    p_value = get_ttest_p(Outcome, Variable),
    p_value = round(p_value, 4)
  )

# Create dataframe for categorical variables  
categorical_pvals <- expand_grid(
  Variable = categorical_vars,
  Type = "Categorical", 
  Outcome = outcome_vars
) %>%
  rowwise() %>%
  mutate(
    p_value = get_chisq_p(Outcome, Variable),
    p_value = round(p_value, 4)
  )

# Combine both dataframes
combined_pvals <- bind_rows(numeric_pvals, categorical_pvals) %>%
  mutate(
    p_value_formatted = case_when(
      is.na(p_value) ~ "NA",
      p_value < 0.001 ~ "<0.001",
      TRUE ~ as.character(p_value)
    )
  )

# Create the final wide table
final_table <- map_dfr(outcome_vars, ~map_dfr(c(numeric_vars, categorical_vars), function(v) {
  type <- ifelse(v %in% numeric_vars, "Numeric", "Categorical")
  p_val <- if (type == "Numeric") {
    t.test(as.formula(paste(v, "~", .x)), data = clean_FF)$p.value
  } else {
    tbl <- table(clean_FF[[v]], clean_FF[[.x]])
    if (sum(tbl) == 0) NA else {
      expected <- suppressWarnings(chisq.test(tbl)$expected)
      (if (any(expected < 5)) fisher.test else chisq.test)(tbl)$p.value
    }
  }
  data.frame(Outcome = .x, Variable = v, Type = type, p_value = p_val)
})) %>% mutate(p_value = ifelse(p_value < 0.05 & !is.na(p_value), 
                                sprintf("%.3f*", p_value),
                                sprintf("%.3f", p_value))) %>%
  pivot_wider(names_from = Outcome, values_from = p_value)

print(final_table, n = Inf)



#re-level categorical variables
clean_FF$Gender <- factor(clean_FF$Gender, levels = c("F", "M"))
# Other is Indigenous (e.g. American Indian, Native Hawaiian)
clean_FF$Race <- factor(clean_FF$Race, levels = c("White", "Asian", "Black", "Other", "Unknown")) 
clean_FF$Ethnicity <- factor(clean_FF$Ethnicity, levels = c("Non-Hispanic", "Hispanic", "Unknown"))
clean_FF$Smoking <- factor(clean_FF$Smoking, levels = c("Non-user", "Former", "Current"))
clean_FF$Alcohol <- factor(clean_FF$Alcohol, levels = c("Light/No Drinking", "Moderate Drinking", "Heavy Drinking", "Unknown"))
clean_FF$Exercise <- factor(clean_FF$Exercise, levels = c("Active", "Fairly active", "Less active", "Unknown"))
clean_FF$age_categories <- factor(clean_FF$age_categories, levels = c("18-40", "41-60", "61-80", "Unknown"))

#Preliminary Logistic Regression
#Fire-related incidents and Physical/Emotional Abuse are reference groups
# Depression
fit_d <- glm(CESD_bi ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
                  + Death + Accidents + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                  + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(fit_d)
#step-wise selection
fit_final_d <- step(fit_d, direction = "both")
summary(fit_final_d)
#95% CI
confint(fit_final_d)

# Stress
fit_s <- glm(FAST_bi ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
             + Death + Accidents + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
             + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(fit_s)
#step-wise selection
fit_final_s <- step(fit_s, direction = "both")
summary(fit_final_s)
#95% CI
confint(fit_final_s)

# Anxiety
fit_a <- glm(ASI_bi ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
             + Death + Accidents + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
             + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(fit_a)
#step-wise selection
fit_final_a <- step(fit_a, direction = "both")
summary(fit_final_a)
#95% CI
confint(fit_final_a)

# PTSD
fit_p <- glm(PCL_bi ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
             + Death + Accidents + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
             + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(fit_p)
#step-wise selection
fit_final_p <- step(fit_p, direction = "both")
summary(fit_final_p)
#95% CI
confint(fit_final_p)

# CFQ
fit_c <- glm(CFQ_bi ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
             + Death + Accidents + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
             + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(fit_c)
#step-wise selection
fit_final_c <- step(fit_c, direction = "both")
summary(fit_final_c)
#95% CI
confint(fit_final_c)
# NEW logistic regression - refined from filtering (t-test and chi2 test p-values) and splines (categorizing continuous variables)
# Depression
new_fit_d <- glm(CESD_bi ~ factor(age_categories) + years + factor(Gender) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(new_fit_d)
#stepwise selection based on BIC
stepwise_D <- step(new_fit_d, direction = "both", k = log(nrow(clean_FF)))
summary(stepwise_D)
confint.default(stepwise_D, level = 0.95)

# Previous model with continuous age variable
old_fit_d <- glm(CESD_bi ~ Age + years + factor(Gender) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(old_fit_d)
#stepwise selection based on BIC
old_stepwise_D <- step(old_fit_d, direction = "both", k = log(nrow(clean_FF)))
summary(old_stepwise_D)

# Stress - Ethnicity was significant
new_fit_s <- glm(FAST_bi ~ factor(age_categories) + years + factor(Gender) + factor(Ethnicity) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(new_fit_s)
#stepwise selection based on BIC
stepwise_S <- step(new_fit_s, direction = "both", k = log(nrow(clean_FF)))
summary(stepwise_S)
confint.default(stepwise_S, level = 0.95)


# Previous model with continuous age variable
old_fit_s <- glm(FAST_bi ~ Age + years + factor(Gender) + factor(Ethnicity) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(old_fit_s)
#stepwise selection based on BIC
old_stepwise_S <- step(old_fit_s, direction = "both", k = log(nrow(clean_FF)))
summary(old_stepwise_S)

# Anxiety - Ethnicity was significant
new_fit_a <- glm(ASI_bi ~ factor(age_categories) + years + factor(Gender) + factor(Ethnicity) + factor(Smoking)+ factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(new_fit_a)
#stepwise selection based on BIC
stepwise_A <- step(new_fit_a, direction = "both", k = log(nrow(clean_FF)))
summary(stepwise_A)
confint.default(stepwise_A, level = 0.95)


# Previous model with continuous age variable
old_fit_a <- glm(ASI_bi ~ Age + years + factor(Gender) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(old_fit_a)
#stepwise selection based on BIC
old_stepwise_A <- step(old_fit_a, direction = "both", k = log(nrow(clean_FF)))
summary(old_stepwise_A)


# PTSD
new_fit_p <- glm(PCL_bi ~ factor(age_categories) + years + factor(Gender) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(new_fit_p)
#stepwise selection based on BIC
stepwise_P <- step(new_fit_p, direction = "both", k = log(nrow(clean_FF)))
summary(stepwise_P)
confint.default(stepwise_P, level = 0.95)

# Previous model with continuous age variable
old_fit_p <- glm(PCL_bi ~ Age + years + factor(Gender) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(old_fit_p)
#stepwise selection based on BIC
old_stepwise_P <- step(old_fit_p, direction = "both", k = log(nrow(clean_FF)))
summary(old_stepwise_P)

# CFQ
new_fit_c <- glm(CFQ_bi ~ factor(age_categories) + years + factor(Gender) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(new_fit_c)
#stepwise selection based on BIC
stepwise_C <- step(new_fit_c, direction = "both", k = log(nrow(clean_FF)))
summary(stepwise_C)
confint.default(stepwise_C, level = 0.95)

# Previous model with continuous age variable
old_fit_c <- glm(CFQ_bi ~ Age + years + factor(Gender) + factor(Smoking) + factor(Exercise) + Disaster + Death + Accidents 
                 + Illness + Bullying + Neglect_C + Dom_Violence_C + Disasters_C + Death_C + Accidents_C 
                 + Displacement_C + Illness_C + Bullying_C + Divorce_C,data = clean_FF, family=binomial(link="logit"))
summary(old_fit_c)
#stepwise selection based on BIC
old_stepwise_C <- step(old_fit_c, direction = "both", k = log(nrow(clean_FF)))
summary(old_stepwise_C)

#VIFs to measure multicollinearity
vif(new_fit_d)
vif(new_fit_s)
vif(new_fit_a)
vif(new_fit_p)
vif(new_fit_c)

#Splines
# Spline for Depression - AGE
clean_FF$Age_Knots <- cut(clean_FF$Age,
                breaks = quantile(clean_FF$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                include.lowest = TRUE,
                labels = c("Q1", "Q2", "Q3", "Q4"))
linage <- glm(CESD_bi ~ Age_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "age_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Age (Depression)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis = 0.8)

# Spline for Stress - AGE
linage <- glm(FAST_bi ~ Age_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "age_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Age (Stress)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis = 0.8)

# Splines for Anxiety - AGE
linage <- glm(ASI_bi ~ Age_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "age_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Age (Anxiety)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis = 0.8)

# Splines for PTSD - AGE
linage <- glm(PCL_bi ~ Age_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "age_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Age (PTSD)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)

# Splines for CFQ - AGE
linage <- glm(CFQ_bi ~ Age_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "age_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Age (CFQ)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Spline for Depression - BMI
clean_FF$BMI_Knots <- cut(clean_FF$BMI,
                    breaks = quantile(clean_FF$BMI, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                    include.lowest = TRUE,
                    labels = c("Q1", "Q2", "Q3", "Q4"))
linage <- glm(CESD_bi ~ BMI_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "BMI_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of BMI (Depression)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Spline for Stress - BMI
linage <- glm(FAST_bi ~ BMI_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "BMI_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of BMI (Stress)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Splines for Anxiety - BMI
linage <- glm(ASI_bi ~ BMI_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "BMI_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of BMI (Anxiety)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Splines for PTSD - BMI
linage <- glm(PCL_bi ~ BMI_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "BMI_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of BMI (PTSD)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Splines for CFQ - BMI
linage <- glm(CFQ_bi ~ BMI_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "BMI_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of BMI (CFQ)",
     cex.lab = 0.8,
     cex.main = 0.9,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Spline for Depression - Years Worked
clean_FF$years_Knots <- cut(clean_FF$years,
                    breaks = quantile(clean_FF$years, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                    include.lowest = TRUE,
                    labels = c("Q1", "Q2", "Q3", "Q4"))
linage <- glm(CESD_bi ~ years_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "Years_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Years Worked (Depression)",
     cex.lab = 0.8,
     cex.main = 0.8,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Spline for Stress - Years Worked
linage <- glm(FAST_bi ~ years_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "Years_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Years Worked (Stress)",
     cex.lab = 0.8,
     cex.main = 0.8,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)

# Splines for Anxiety - Years Worked
linage <- glm(ASI_bi ~ years_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "Years_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Years Worked (Anxiety)",
     cex.lab = 0.8,
     cex.main = 0.8,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Splines for PTSD - Years Worked
linage <- glm(PCL_bi ~ years_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "Years_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Years Worked (PTSD)",
     cex.lab = 0.8,
     cex.main = 0.8,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


# Splines for CFQ - Years Worked
linage <- glm(CFQ_bi ~ years_Knots, data=clean_FF, family=binomial())
summary(linage)
ageco <- coef(linage)
ageco[1] <- 0
labels3 <- names(ageco)
labels3[1] <- "Years_qQ1"
estimates3 <- as.numeric(ageco)
plot(estimates3,
     pch = 16,
     xaxt = "n",
     xlab = "Model Terms",
     ylab = "Log Odds Estimate",
     main = "Logistic Regression Coefficients of Years Worked (CFQ)",
     cex.lab = 0.8,
     cex.main = 0.8,
     cex.axis = 0.8)
# Add horizontal line at 0
lines(1:length(estimates3), estimates3, type = "b", pch = 16, col = "black")
# Label the x-axis
axis(1, at = 1:length(estimates3), labels = labels3, cex.axis=0.8)


#Forest plots

# Depression
ci_d <- confint.default(stepwise_D, level = 0.95)
results_d <- broom::tidy(stepwise_D, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    conf.low = exp(ci_d[term, 1]),
    conf.high = exp(ci_d[term, 2]),
    OR = estimate,
    OR_lower = conf.low,
    OR_upper = conf.high,
    OR_text = sprintf("%.2f (%.2f-%.2f)", OR, OR_lower, OR_upper))
ggplot(results_d, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_text(aes(label = OR_text), x = 10, hjust = 0, size = 2.5) +
  coord_cartesian(xlim = c(-5, 15)) +
  labs(
    x = "Odds Ratio", 
    y = "Predictor",
    title = "Forest Plot of Odds Ratio (Depression)") +
  theme_minimal() +
  theme(
    # Adjust text sizes
    axis.title = element_text(size = 8),          # Axis titles (x & y)
    axis.text = element_text(size = 8),            # Axis labels (tick marks)
    plot.title = element_text(size = 10, face = "bold"),  # Plot title
    strip.text = element_text(size = 9))            # Facet labels (if used)

# Stress
ci_s <- confint.default(stepwise_S, level = 0.95)
results_s <- broom::tidy(stepwise_S, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    conf.low = exp(ci_s[term, 1]),
    conf.high = exp(ci_s[term, 2]),
    OR = estimate,
    OR_lower = conf.low,
    OR_upper = conf.high,
    OR_text = sprintf("%.2f (%.2f-%.2f)", OR, OR_lower, OR_upper))
ggplot(results_s, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_text(aes(label = OR_text), x = 7, hjust = 0, size = 2.5) +
  coord_cartesian(xlim = c(-5, 10)) +
  labs(
    x = "Odds Ratio", 
    y = "Predictor",
    title = "Forest Plot of Odds Ratio (Stress)") +
  theme_minimal() +
  theme(
    # Adjust text sizes
    axis.title = element_text(size = 8),          # Axis titles (x & y)
    axis.text = element_text(size = 8),            # Axis labels (tick marks)
    plot.title = element_text(size = 10, face = "bold"),  # Plot title
    strip.text = element_text(size = 9))            # Facet labels (if used)

# Anxiety
ci_a <- confint.default(stepwise_A, level = 0.95)
results_a <- broom::tidy(stepwise_A, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    conf.low = exp(ci_a[term, 1]),
    conf.high = exp(ci_a[term, 2]),
    OR = estimate,
    OR_lower = conf.low,
    OR_upper = conf.high,
    OR_text = sprintf("%.2f (%.2f-%.2f)", OR, OR_lower, OR_upper))
ggplot(results_a, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_text(aes(label = OR_text), x = 15, hjust = 0, size = 2.5) +
  coord_cartesian(xlim = c(-5, 25)) +
  labs(
    x = "Odds Ratio", 
    y = "Predictor",
    title = "Forest Plot of Log-Odds (Anxiety)") +
  theme_minimal() +
  theme(
    # Adjust text sizes
    axis.title = element_text(size = 8),          # Axis titles (x & y)
    axis.text = element_text(size = 8),            # Axis labels (tick marks)
    plot.title = element_text(size = 10, face = "bold"),  # Plot title
    strip.text = element_text(size = 9))            # Facet labels (if used)

# PTSD
ci_p <- confint.default(stepwise_P, level = 0.95)
results_p <- broom::tidy(stepwise_P, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    conf.low = exp(ci_p[term, 1]),
    conf.high = exp(ci_p[term, 2]),
    OR = estimate,
    OR_lower = conf.low,
    OR_upper = conf.high,
    OR_text = sprintf("%.2f (%.2f-%.2f)", OR, OR_lower, OR_upper))
ggplot(results_p, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_text(aes(label = OR_text), x = 22, hjust = 0, size = 2.5) +
  coord_cartesian(xlim = c(-5, 30)) +
  labs(
    x = "Odds Ratio", 
    y = "Predictor",
    title = "Forest Plot of Log-Odds (PTSD)") +
  theme_minimal() +
  theme(
    # Adjust text sizes
    axis.title = element_text(size = 8),          # Axis titles (x & y)
    axis.text = element_text(size = 8),            # Axis labels (tick marks)
    plot.title = element_text(size = 10, face = "bold"),  # Plot title
    strip.text = element_text(size = 9))            # Facet labels (if used)

# CFQ
ci_c <- confint.default(stepwise_C, level = 0.95)
results_c <- broom::tidy(stepwise_C, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    conf.low = exp(ci_c[term, 1]),
    conf.high = exp(ci_c[term, 2]),
    OR = estimate,
    OR_lower = conf.low,
    OR_upper = conf.high,
    OR_text = sprintf("%.2f (%.2f-%.2f)", OR, OR_lower, OR_upper))
ggplot(results_c, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_text(aes(label = OR_text), x = 10, hjust = 0, size = 2.5) +
  coord_cartesian(xlim = c(-5, 15)) +
  labs(
    x = "Odds Ratio", 
    y = "Predictor",
    title = "Forest Plot of Log-Odds (CFQ)") +
  theme_minimal() +
  theme(
    # Adjust text sizes
    axis.title = element_text(size = 8),          # Axis titles (x & y)
    axis.text = element_text(size = 8),            # Axis labels (tick marks)
    plot.title = element_text(size = 10, face = "bold"),  # Plot title
    strip.text = element_text(size = 9))            # Facet labels (if used)



#Influential Points

# Depression
#-----------
# leverage
leverage_D <- hatvalues(stepwise_D)
leverage.threshold_D <- 2*5 / 132 #2p/n
high.leverage_D <- which(leverage_D > leverage.threshold_D)
print("High leverage points:")
print(leverage_D[high.leverage_D])

# Standardized Pearson residuals
std.residuals_D <- rstandard(stepwise_D)
# |residual| > 2 as problematic
large.residuals_D <- which(abs(std.residuals_D) > 2)
print("Large standardized residuals:")
print(std.residuals_D[large.residuals_D])

# Cook's distance
cooks.d_D <- cooks.distance(stepwise_D)
# Rule of thumb: > 4/n is noteworthy
cooks.threshold_D <- 4/132
high.cooks_D <- which(cooks.d_D > cooks.threshold_D)
print("High Cook's distance points:")
print(cooks.d_D[high.cooks_D])


#Goodness of Fit - HL Test
# View the most influential points
n_D <- nobs(stepwise_D)
problem_indices_D <- unique(c(which(abs(std.residuals_D) > 2),
                              which(leverage_D > leverage.threshold_D),
                              which(cooks.d_D > cooks.threshold_D)))
print(problem_indices_D)

#1 obs removed
to_remove <- c(65)

#all obs removed
to_remove <- c(65, 93, 1, 60, 77, 90, 92, 96, 101)

clean_data_final <- clean_FF[-to_remove, ]
stepwise_D2 <- update(stepwise_D, data = clean_data_final)

hl_test_D <- hoslem.test(clean_FF$CESD_bi, fitted(stepwise_D), g=4)
print(hl_test_D)

hl_test_D2 <- hoslem.test(clean_data_final$CESD_bi, fitted(stepwise_D2), g=4)
print(hl_test_D2)

#Visualizations (Depression)
# influential measure vs estimated probabilities
estimated_probs_D <- fitted(stepwise_D)
par(mar = c(4, 4, 3, 2))  # Adjust margins (bottom, left, top, right)

# Leverage vs. Estimated Probabilities
plot(estimated_probs_D, leverage_D,
     main = "Leverage vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Leverage (Hat Values)",
     pch = 19, col = ifelse(leverage_D > leverage.threshold_D, "red", "black"))
abline(h = leverage.threshold_D, col = "red", lwd = 2)

# Plot Chi-Square Contributions vs. Estimated Probabilities
# Convert to chi-square contributions
pearson_chisq_D <- std.residuals_D^2
plot(estimated_probs_D, pearson_chisq_D,
     main = "Change in Pearson Chi-Square vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Pearson Chi-Square Contribution",
     pch = 19,
     cex.main=0.8,
     col = ifelse(pearson_chisq_D > 4, "red", "black"))

# Cook's Distance vs. Estimated Probabilities
plot(estimated_probs_D, cooks.d_D,
     main = "Cook's Distance vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Cook's D",
     pch = 19, col = ifelse(cooks.d_D > cooks.threshold_D, "red", "black"))
abline(h = cooks.threshold_D, col = "red", lwd = 2)
text(x = max(estimated_probs_D)*0.9, y = cooks.threshold_D*1.05)

# Change in Deviance vs Estimated Probabilities
influence_stats_D <- influence(stepwise_D)
delta_deviance_D <- residuals(stepwise_D, type = "deviance")^2 
plot(estimated_probs_D, delta_deviance_D,
     main = "Change in Deviance vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Delta Deviance",
     pch = 19,
     cex.main = 1.2)


# Stress
#-----------
# leverage
leverage_S <- hatvalues(stepwise_S)
leverage.threshold_S <- 2*3 / 132 #2p/n
high.leverage_S <- which(leverage_S > leverage.threshold_S)
print("High leverage points:")
print(leverage_S[high.leverage_S])

# Standardized Pearson residuals
std.residuals_S <- rstandard(stepwise_S)
# |residual| > 2 as problematic
large.residuals_S <- which(abs(std.residuals_S) > 2)
print("Large standardized residuals:")
print(std.residuals_S[large.residuals_S])

# Cook's distance
cooks.d_S <- cooks.distance(stepwise_S)
# Rule of thumb: > 4/n is noteworthy
cooks.threshold_S <- 4/132
high.cooks_S <- which(cooks.d_S > cooks.threshold_S)
print("High Cook's distance points:")
print(cooks.d_S[high.cooks_S])

#Goodness of Fit - HL Test
# View the most influential points
n_S <- nobs(stepwise_S)
problem_indices_S <- unique(c(which(abs(std.residuals_S) > 2),
                              which(leverage_S > leverage.threshold_S),
                              which(cooks.d_S > cooks.threshold_S)))
print(problem_indices_S)

#1 obs removed
to_remove <- c(76)

#all obs removed
to_remove <- c(76, 1, 3, 4, 20, 55, 61, 63,
               78, 80, 81, 88, 92, 93, 94,
               96, 98, 110, 120, 123, 130)

clean_data_final <- clean_FF[-to_remove, ]
stepwise_S2 <- update(stepwise_S, data = clean_data_final)

hl_test_S <- hoslem.test(clean_FF$FAST_bi, fitted(stepwise_S), g=10)
print(hl_test_S)

hl_test_S2 <- hoslem.test(clean_data_final$FAST_bi, fitted(stepwise_S2), g=8)
print(hl_test_S2)

#Visualizations
# influential measure vs estimated probabilities
estimated_probs_S <- fitted(stepwise_S)
par(mar = c(4, 4, 3, 2))  # Adjust margins (bottom, left, top, right)

# Leverage vs. Estimated Probabilities
plot(estimated_probs_S, leverage_S,
     main = "Leverage vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Leverage (Hat Values)",
     pch = 19, col = ifelse(leverage_S > leverage.threshold_S, "red", "black"))
abline(h = leverage.threshold_S, col = "red", lwd = 2)

# Plot Chi-Square Contributions vs. Estimated Probabilities
# Convert to chi-square contributions
pearson_chisq_S <- std.residuals_S^2
plot(estimated_probs_S, pearson_chisq_S,
     main = "Change in Pearson Chi-Square vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Pearson Chi-Square Contribution",
     pch = 19,
     cex.main=0.8,
     col = ifelse(pearson_chisq_S > 4, "red", "black"))

# Cook's Distance vs. Estimated Probabilities
plot(estimated_probs_S, cooks.d_S,
     main = "Cook's Distance vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Cook's D",
     pch = 19, col = ifelse(cooks.d_S > cooks.threshold_S, "red", "black"))
abline(h = cooks.threshold_S, col = "red", lwd = 2)
text(x = max(estimated_probs_S)*0.9, y = cooks.threshold_S*1.05)

# Change in Deviance vs Estimated Probabilities
influence_stats_S <- influence(stepwise_S)
delta_deviance_S <- residuals(stepwise_S, type = "deviance")^2 
plot(estimated_probs_S, delta_deviance_S,
     main = "Change in Deviance vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Delta Deviance",
     pch = 19,
     cex.main = 1.2)


# Anxiety
#-----------
# leverage
leverage_A <- hatvalues(stepwise_A)
leverage.threshold_A <- 2*7 / 132 #2p/n
high.leverage_A <- which(leverage_A > leverage.threshold_A)
print("High leverage points:")
print(leverage_A[high.leverage_A])

# Standardized Pearson residuals
std.residuals_A <- rstandard(stepwise_A)
# |residual| > 2 as problematic
large.residuals_A <- which(abs(std.residuals_A) > 2)
print("Large standardized residuals:")
print(std.residuals_A[large.residuals_A])

# Cook's distance
cooks.d_A <- cooks.distance(stepwise_A)
# Rule of thumb: > 4/n is noteworthy
cooks.threshold_A <- 4/132
high.cooks_A <- which(cooks.d_A > cooks.threshold_A)
print("High Cook's distance points:")
print(cooks.d_A[high.cooks_A])

#Goodness of Fit - HL Test
# View the most influential points
n_A <- nobs(stepwise_A)
problem_indices_A <- unique(c(which(abs(std.residuals_A) > 2),
                              which(leverage_A > leverage.threshold_A),
                              which(cooks.d_A > cooks.threshold_A)))
print(problem_indices_A)

#1 obs removed - slight decrease GOF
to_remove <- c(87)

#all obs removed - larger decrease in GOF
to_remove <- c(87, 1, 2, 3, 4, 19, 30, 42, 61, 111)

clean_data_final <- clean_FF[-to_remove, ]
stepwise_A2 <- update(stepwise_A, data = clean_data_final)

hl_test_A <- hoslem.test(clean_FF$ASI_bi, fitted(stepwise_A), g=10)
print(hl_test_A)

hl_test_A2 <- hoslem.test(clean_data_final$ASI_bi, fitted(stepwise_A2), g=10)
print(hl_test_A2)

#Visualizations - Anxiety
# influential measure vs estimated probabilities
estimated_probs_A <- fitted(stepwise_A)
par(mar = c(4, 4, 3, 2))  # Adjust margins (bottom, left, top, right)

# Leverage vs. Estimated Probabilities
plot(estimated_probs_A, leverage_A,
     main = "Leverage vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Leverage (Hat Values)",
     pch = 19, col = ifelse(leverage_A > leverage.threshold_A, "red", "black"))
abline(h = leverage.threshold_A, col = "red", lwd = 2)

# Plot Chi-Square Contributions vs. Estimated Probabilities
# Convert to chi-square contributions
pearson_chisq_A <- std.residuals_A^2
plot(estimated_probs_A, pearson_chisq_A,
     main = "Change in Pearson Chi-Square vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Pearson Chi-Square Contribution",
     pch = 19,
     cex.main=0.8,
     col = ifelse(pearson_chisq_A > 4, "red", "black"))

# Cook's Distance vs. Estimated Probabilities
plot(estimated_probs_A, cooks.d_A,
     main = "Cook's Distance vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Cook's D",
     pch = 19, col = ifelse(cooks.d_A > cooks.threshold_A, "red", "black"))
abline(h = cooks.threshold_A, col = "red", lwd = 2)
text(x = max(estimated_probs_A)*0.9, y = cooks.threshold_A*1.05)

# Change in Deviance vs Estimated Probabilities
influence_stats_A <- influence(stepwise_A)
delta_deviance_A <- residuals(stepwise_A, type = "deviance")^2 
plot(estimated_probs_A, delta_deviance_A,
     main = "Change in Deviance vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Delta Deviance",
     pch = 19,
     cex.main = 1.2)


# PTSD
#-----------
# leverage
leverage_P <- hatvalues(stepwise_P)
leverage.threshold_P <- 2*6 / 132 #2p/n
high.leverage_P <- which(leverage_P > leverage.threshold_P)
print("High leverage points:")
print(leverage_P[high.leverage_P])

# Standardized Pearson residuals
std.residuals_P <- rstandard(stepwise_P)
# |residual| > 2 as problematic
large.residuals_P <- which(abs(std.residuals_P) > 2)
print("Large standardized residuals:")
print(std.residuals_P[large.residuals_P])

# Cook's distance
cooks.d_P <- cooks.distance(stepwise_P)
# Rule of thumb: > 4/n is noteworthy
cooks.threshold_P <- 4/132
high.cooks_P <- which(cooks.d_P > cooks.threshold_P)
print("High Cook's distance points:")
print(cooks.d_P[high.cooks_P])

#Goodness of Fit - HL Test
# View the most influential points
n_P <- nobs(stepwise_P)
problem_indices_P <- unique(c(which(abs(std.residuals_P) > 2),
                              which(leverage_P > leverage.threshold_P),
                              which(cooks.d_P > cooks.threshold_P)))
print(problem_indices_P)

#1 obs removed
to_remove <- c(12)

#all obs removed
to_remove <- c(12, 32, 34, 51, 56, 70, 82, 86, 1, 2, 4, 20, 24, 35, 41, 55,
               61, 65, 79, 92, 95, 96, 98, 99, 110, 116, 119, 120, 121, 131, 71) 

clean_data_final <- clean_FF[-to_remove, ]
stepwise_P2 <- update(stepwise_P, data = clean_data_final)

hl_test_P <- hoslem.test(clean_FF$PCL_bi, fitted(stepwise_P), g=3)
print(hl_test_P)

hl_test_P2 <- hoslem.test(clean_data_final$PCL_bi, fitted(stepwise_P2), g=3)
print(hl_test_P2)

#Visualizations - (PTSD)
# influential measure vs estimated probabilities
estimated_probs_P <- fitted(stepwise_P)
par(mar = c(4, 4, 3, 2))  # Adjust margins (bottom, left, top, right)

# Leverage vs. Estimated Probabilities
plot(estimated_probs_P, leverage_P,
     main = "Leverage vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Leverage (Hat Values)",
     pch = 19, col = ifelse(leverage_P > leverage.threshold_P, "red", "black"))
abline(h = leverage.threshold_P, col = "red", lwd = 2)

# Plot Chi-Square Contributions vs. Estimated Probabilities
# Convert to chi-square contributions
pearson_chisq_P <- std.residuals_P^2
plot(estimated_probs_P, pearson_chisq_P,
     main = "Change in Pearson Chi-Square vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Pearson Chi-Square Contribution",
     pch = 19,
     cex.main=0.8,
     col = ifelse(pearson_chisq_P > 4, "red", "black"))

# Cook's Distance vs. Estimated Probabilities
plot(estimated_probs_P, cooks.d_P,
     main = "Cook's Distance vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Cook's D",
     pch = 19, col = ifelse(cooks.d_P > cooks.threshold_P, "red", "black"))
abline(h = cooks.threshold_P, col = "red", lwd = 2)
text(x = max(estimated_probs_P)*0.9, y = cooks.threshold_P*1.05)

# Change in Deviance vs Estimated Probabilities
influence_stats_P <- influence(stepwise_P)
delta_deviance_P <- residuals(stepwise_P, type = "deviance")^2 
plot(estimated_probs_P, delta_deviance_P,
     main = "Change in Deviance vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Delta Deviance",
     pch = 19,
     cex.main = 1.2)


# CFQ
#-----------
# leverage
leverage_C <- hatvalues(stepwise_C)
leverage.threshold_C <- 2*4 / 132 #2p/n
high.leverage_C <- which(leverage_C > leverage.threshold_C)
print("High leverage points:")
print(leverage_C[high.leverage_C])

# Standardized Pearson residuals
std.residuals_C <- rstandard(stepwise_C)
# |residual| > 2 as problematic
large.residuals_C <- which(abs(std.residuals_C) > 2)
print("Large standardized residuals:")
print(std.residuals_C[large.residuals_C])

# Cook's distance
cooks.d_C <- cooks.distance(stepwise_C)
# Rule of thumb: > 4/n is noteworthy
cooks.threshold_C <- 4/132
high.cooks_C <- which(cooks.d_C > cooks.threshold_C)
print("High Cook's distance points:")
print(cooks.d_C[high.cooks_C])

#Goodness of Fit - HL Test
# View the most influential points
n_C <- nobs(stepwise_C)
problem_indices_C <- unique(c(which(abs(std.residuals_C) > 2),
                              which(leverage_C > leverage.threshold_C),
                              which(cooks.d_C > cooks.threshold_C)))
print(problem_indices_C)

#1 obs removed
to_remove <- c(12)

#all obs removed
to_remove <- c(12, 18, 121, 38, 107)

clean_data_final <- clean_FF[-to_remove, ]
stepwise_C2 <- update(stepwise_C, data = clean_data_final)

hl_test_C <- hoslem.test(clean_FF$CFQ_bi, fitted(stepwise_C), g=10)
print(hl_test_C)

hl_test_C2 <- hoslem.test(clean_data_final$CFQ_bi, fitted(stepwise_C2), g=10)
print(hl_test_C2)

#Visualizations - CFQ
# influential measure vs estimated probabilities
estimated_probs_C <- fitted(stepwise_C)
par(mar = c(4, 4, 3, 2))  # Adjust margins (bottom, left, top, right)

# Leverage vs. Estimated Probabilities
plot(estimated_probs_C, leverage_C,
     main = "Leverage vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Leverage (Hat Values)",
     pch = 19, col = ifelse(leverage_C > leverage.threshold_C, "red", "black"))
abline(h = leverage.threshold_C, col = "red", lwd = 2)

# Plot Chi-Square Contributions vs. Estimated Probabilities
# Convert to chi-square contributions
pearson_chisq_C <- std.residuals_C^2
plot(estimated_probs_C, pearson_chisq_C,
     main = "Change in Pearson Chi-Square vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Pearson Chi-Square Contribution",
     pch = 19,
     cex.main=0.8,
     col = ifelse(pearson_chisq_C > 4, "red", "black"))

# Cook's Distance vs. Estimated Probabilities
plot(estimated_probs_C, cooks.d_C,
     main = "Cook's Distance vs. Estimated Probabilities",
     xlab = "Estimated Probability", 
     ylab = "Cook's D",
     pch = 19, col = ifelse(cooks.d_C > cooks.threshold_C, "red", "black"))
abline(h = cooks.threshold_C, col = "red", lwd = 2)
text(x = max(estimated_probs_C)*0.9, y = cooks.threshold_C*1.05)

# Change in Deviance vs Estimated Probabilities
influence_stats_C <- influence(stepwise_C)
delta_deviance_C <- residuals(stepwise_C, type = "deviance")^2 
plot(estimated_probs_C, delta_deviance_C,
     main = "Change in Deviance vs. Estimated Probability",
     xlab = "Estimated Probability", 
     ylab = "Delta Deviance",
     pch = 19,
     cex.main = 1.2)


#POWER - ROC/AUC 

# Depression - ROC Curve and AUC
rocplotf_D <- roc(CESD_bi ~ fitted(stepwise_D), data=clean_FF)
auc_with_ci <- ci.auc(rocplotf_D)
print(auc_with_ci)
plot.roc(rocplotf_D, legacy.axes=TRUE, main = "ROC Curve for Depression Model (CESD)")
text(0.4, 0.2, 
     paste0("AUC = ", round(auc(rocplotf_D), 3), 
            "\n95% CI = ", round(auc_with_ci[1], 3), "-", round(auc_with_ci[3], 3)),
     col = "blue")

# Stress - ROC Curve and AUC
rocplotf_S <- roc(FAST_bi ~ fitted(stepwise_S), data=clean_FF)
auc_with_ci <- ci.auc(rocplotf_S)
print(auc_with_ci)
plot.roc(rocplotf_S, legacy.axes=TRUE, main="ROC Curve for Stress Model (FAST)")
text(0.4, 0.2, 
     paste0("AUC = ", round(auc(rocplotf_S), 3), 
            "\n95% CI = ", round(auc_with_ci[1], 3), "-", round(auc_with_ci[3], 3)),
     col = "blue")

# Anxiety - ROC Curve and AUC
rocplotf_A <- roc(ASI_bi ~ fitted(stepwise_A), data=clean_FF)
auc_with_ci <- ci.auc(rocplotf_A)
print(auc_with_ci)
plot.roc(rocplotf_A, legacy.axes=TRUE, main="ROC Curve for Anxiety Model (ASI)")
text(0.4, 0.2, 
     paste0("AUC = ", round(auc(rocplotf_A), 3), 
            "\n95% CI = ", round(auc_with_ci[1], 3), "-", round(auc_with_ci[3], 3)),
     col = "blue")

# PTSD - ROC Curve and AUC
rocplotf_P <- roc(PCL_bi ~ fitted(stepwise_P), data=clean_FF)
auc_with_ci <- ci.auc(rocplotf_P)
print(auc_with_ci)
plot.roc(rocplotf_P, legacy.axes=TRUE, main="ROC Curve for PTSD Model (PCL)")
text(0.4, 0.2, 
     paste0("AUC = ", round(auc(rocplotf_P), 3), 
            "\n95% CI = ", round(auc_with_ci[1], 3), "-", round(auc_with_ci[3], 3)),
     col = "blue")

# CFQ - ROC Curve and AUC
rocplotf_C <- roc(CFQ_bi ~ fitted(stepwise_C), data=clean_FF)
auc_with_ci <- ci.auc(rocplotf_C)
print(auc_with_ci)
plot.roc(rocplotf_C, legacy.axes=TRUE, main="ROC Curve for CFQ Model (CFQ)")
text(0.4, 0.2, 
     paste0("AUC = ", round(auc(rocplotf_C), 3), 
            "\n95% CI = ", round(auc_with_ci[1], 3), "-", round(auc_with_ci[3], 3)),
     col = "blue")



#Possible linear regression?
#------------------------------------------------------------------------------------------------------

#Linear Regression

# checking normality of Age
# histogram
hist(clean_FF$Age, 
     main = "Histogram of Age",
     xlab = "Age",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$Age, 
       main = "QQ Plot of Age",
       ylim = c(min(clean_FF$Age, na.rm = TRUE) - 1, 
                max(clean_FF$Age, na.rm = TRUE) + 1))
qqline(clean_FF$Age, col = "red")

# boxplot
boxplot(clean_FF$Age, 
        main = "Boxplot of Age",
        ylim = c(min(clean_FF$Age, na.rm = TRUE) - 1, 
                 max(clean_FF$Age, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_Age <- shapiro.test(clean_FF$Age)
print(shapiro_Age)



# checking normality of BMI
# histogram
hist(clean_FF$BMI, 
     main = "Histogram of BMI",
     xlab = "BMI",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$BMI, 
       main = "QQ Plot of BMI",
       ylim = c(min(clean_FF$BMI, na.rm = TRUE) - 1, 
                max(clean_FF$BMI, na.rm = TRUE) + 1))
qqline(clean_FF$BMI, col = "red")

# boxplot
boxplot(clean_FF$BMI, 
        main = "Boxplot of BMI",
        ylim = c(min(clean_FF$BMI, na.rm = TRUE) - 1, 
                 max(clean_FF$BMI, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_BMI <- shapiro.test(clean_FF$BMI)
print(shapiro_BMI)


# checking normality of Years Worked
# histogram
hist(clean_FF$years, 
     main = "Histogram of Years Worked",
     xlab = "Years Worked",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$years, 
       main = "QQ Plot of Years Worked",
       ylim = c(min(clean_FF$years, na.rm = TRUE) - 1, 
                max(clean_FF$years, na.rm = TRUE) + 1))
qqline(clean_FF$years, col = "red")

# boxplot
boxplot(clean_FF$years, 
        main = "Boxplot of Years Worked",
        ylim = c(min(clean_FF$years, na.rm = TRUE) - 1, 
                 max(clean_FF$years, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_years <- shapiro.test(clean_FF$years)
print(shapiro_years)

# checking normality of Years Worked
# histogram
hist(clean_FF$years, 
     main = "Histogram of Years Worked",
     xlab = "Years Worked",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$years, 
       main = "QQ Plot of Years Worked",
       ylim = c(min(clean_FF$years, na.rm = TRUE) - 1, 
                max(clean_FF$years, na.rm = TRUE) + 1))
qqline(clean_FF$years, col = "red")

# boxplot
boxplot(clean_FF$years, 
        main = "Boxplot of Years Worked",
        ylim = c(min(clean_FF$years, na.rm = TRUE) - 1, 
                 max(clean_FF$years, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_years <- shapiro.test(clean_FF$years)
print(shapiro_years)


#Normality of Mental Health Outcomes
# checking normality of CESD
# histogram
hist(clean_FF$CESD, 
     main = "Histogram of CESD",
     xlab = "CESD",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$CESD, 
       main = "QQ Plot of CESD",
       ylim = c(min(clean_FF$CESD, na.rm = TRUE) - 1, 
                max(clean_FF$CESD, na.rm = TRUE) + 1))
qqline(clean_FF$CESD, col = "red")

# boxplot
boxplot(clean_FF$CESD, 
        main = "Boxplot of CESD",
        ylim = c(min(clean_FF$CESD, na.rm = TRUE) - 1, 
                 max(clean_FF$CESD, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_CESD <- shapiro.test(clean_FF$CESD)
print(shapiro_CESD)

# checking normality of FAST
# histogram
hist(clean_FF$FAST, 
     main = "Histogram of FAST",
     xlab = "FAST",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$FAST, 
       main = "QQ Plot of FAST",
       ylim = c(min(clean_FF$FAST, na.rm = TRUE) - 1, 
                max(clean_FF$FAST, na.rm = TRUE) + 1))
qqline(clean_FF$FAST, col = "red")

# boxplot
boxplot(clean_FF$FAST, 
        main = "Boxplot of FAST",
        ylim = c(min(clean_FF$FAST, na.rm = TRUE) - 1, 
                 max(clean_FF$FAST, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_FAST <- shapiro.test(clean_FF$FAST)
print(shapiro_FAST)

# checking normality of ASI
# histogram
hist(clean_FF$ASI, 
     main = "Histogram of ASI",
     xlab = "ASI",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$ASI, 
       main = "QQ Plot of ASI",
       ylim = c(min(clean_FF$ASI, na.rm = TRUE) - 1, 
                max(clean_FF$ASI, na.rm = TRUE) + 1))
qqline(clean_FF$ASI, col = "red")

# boxplot
boxplot(clean_FF$ASI, 
        main = "Boxplot of ASI",
        ylim = c(min(clean_FF$ASI, na.rm = TRUE) - 1, 
                 max(clean_FF$ASI, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_ASI <- shapiro.test(clean_FF$ASI)
print(shapiro_ASI)

# checking normality of PCL
# histogram
hist(clean_FF$PCL, 
     main = "Histogram of PCL",
     xlab = "PCL",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$PCL, 
       main = "QQ Plot of PCL",
       ylim = c(min(clean_FF$PCL, na.rm = TRUE) - 1, 
                max(clean_FF$PCL, na.rm = TRUE) + 1))
qqline(clean_FF$PCL, col = "red")

# boxplot
boxplot(clean_FF$PCL, 
        main = "Boxplot of PCL",
        ylim = c(min(clean_FF$PCL, na.rm = TRUE) - 1, 
                 max(clean_FF$PCL, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_PCL <- shapiro.test(clean_FF$PCL)
print(shapiro_PCL)

# checking normality of CFQ
# histogram
hist(clean_FF$CFQ, 
     main = "Histogram of CFQ",
     xlab = "CFQ",
     ylab = "Frequency",
     ylim = c(0, 70))

# qqplot
qqnorm(clean_FF$CFQ, 
       main = "QQ Plot of CFQ",
       ylim = c(min(clean_FF$CFQ, na.rm = TRUE) - 1, 
                max(clean_FF$CFQ, na.rm = TRUE) + 1))
qqline(clean_FF$CFQ, col = "red")

# boxplot
boxplot(clean_FF$CFQ, 
        main = "Boxplot of CFQ",
        ylim = c(min(clean_FF$CFQ, na.rm = TRUE) - 1, 
                 max(clean_FF$CFQ, na.rm = TRUE) + 1),
        horizontal = TRUE)

# shapiro-wilks test
shapiro_CFQ <- shapiro.test(clean_FF$CFQ)
print(shapiro_CFQ)

#Linear Regressions

# Depression
model_D <- glm(CESD ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
              + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
              + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
summary(model_D)

#Diagnosis

# Linearity
vif(model_D)

# Non-constant Variance (Breush-Pagan Test)
bptest(model_D)

# Normality (SW test)
raw_resid <- residuals(model_D)
shapiro.test((raw_resid))

#shift CESD by 1 for boxcox method
clean_FF$CESD_shifted <- clean_FF$CESD + 1
model_D2 <- glm(CESD_shifted ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
               + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
               + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)

# Fixing violations
BC_CESD <- boxcox(model_D2)
optimal_lambda <- BC_CESD$x[which.max(bc_result$y)]
print(paste("Optimal lambda:", round(optimal_lambda, 3)))


# Stress
model_S <- glm(FAST ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
              + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
              + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
summary(model_S)

#Diagnosis

# Linearity
vif(model_S)

# Non-constant Variance (Breush-Pagan Test)
bptest(model_S)

# Normality (SW test) <- non-normal
raw_resid <- residuals(model_S)
shapiro.test((raw_resid))

# Fixing violations
#shift FAST by 1 for boxcox method
clean_FF$FAST_shifted <- clean_FF$FAST + 1
model_S2 <- glm(FAST_shifted ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
                + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
                + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
BC_FAST <- boxcox(model_S2)
optimal_lambda <- BC_FAST$x[which.max(bc_result$y)]
print(paste("Optimal lambda:", round(optimal_lambda, 3)))


# Anxiety
model_A <- glm(ASI ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
              + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
              + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
summary(model_A)

#Diagnosis

# Linearity
vif(model_A)

# Non-constant Variance (Breush-Pagan Test)
bptest(model_A)

# Normality (SW test)
raw_resid <- residuals(model_A)
shapiro.test((raw_resid))

# Fixing violations
#shift FAST by 1 for boxcox method
clean_FF$ASI_shifted <- clean_FF$ASI + 1
model_A2 <- glm(ASI_shifted ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
                + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
                + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
BC_ASI <- boxcox(model_A2)
optimal_lambda <- BC_ASI$x[which.max(bc_result$y)]
print(paste("Optimal lambda:", round(optimal_lambda, 3)))

# PTSD
model_P <- glm(PCL ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
              + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
              + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
summary(model_P)

#Diagnosis

# Linearity
vif(model_P)

# Non-constant Variance (Breush-Pagan Test)
bptest(model_P)

# Normality (SW test)
raw_resid <- residuals(model_P)
shapiro.test((raw_resid))

# Fixing violations
#shift FAST by 1 for boxcox method
clean_FF$PCL_shifted <- clean_FF$PCL + 1
model_P2 <- glm(PCL_shifted ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
                + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
                + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
BC_PCL <- boxcox(model_P2)
optimal_lambda <- BC_PCL$x[which.max(bc_result$y)]
print(paste("Optimal lambda:", round(optimal_lambda, 3)))

# CFQ
model_C <- glm(CFQ ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
              + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
              + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
summary(model_C)

#Diagnosis

# Linearity
vif(model_C)

# Non-constant Variance (Breush-Pagan Test)
bptest(model_C)

# Normality (SW test)
raw_resid <- residuals(model_C)
shapiro.test((raw_resid))

# Fixing violations
#shift FAST by 1 for boxcox method
clean_FF$CFQ_shifted <- clean_FF$CFQ + 1
model_C2 <- glm(CFQ_shifted ~ Age + BMI + years + factor(Gender) + factor(Race) + factor(Ethnicity) + factor(Smoking) + factor(Alcohol) + factor(Exercise) + Disaster 
                + Death + Accidents + Illness + Other + Neglect + Dom_Violence + Disasters_C + Death_C + Accidents_C 
                + Displacement + Illness_C + Bullying + Other_C,data = clean_FF)
BC_CFQ <- boxcox(model_C2)
optimal_lambda <- BC_CFQ$x[which.max(bc_result$y)]
print(paste("Optimal lambda:", round(optimal_lambda, 3)))