####
## This script does the following:
# 1. Import released disclosure-safe output data
# 2. Create various tables, plots and figures
####

# Import libraries ----
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(gt)
library(purrr)
library(stringr)
library(ggplot2)
library(forestplot)

####
# Baseline table ----
####
# Load the data
df_desc <- read_csv(here("descriptives.csv"))

# Fill down Variable names where NA for further processing
df_tbl <- df_desc %>%
  fill(Variable, .direction = "down")  

# Add proportions in brackets for the rest (protect the num_var)
num_var <- c("age", "bmi", "imd", "d_admission_treat", "calendar_day", "covid_test_positive_date_d", "d_vaccinate_treat", "HR")
add_proportion <- function(value, total) {
  if (!is.na(value) && !is.na(total) && total > 0 && !grepl("\\(", value)) {
    proportion <- round((as.numeric(value) / as.numeric(total)) * 100, 1)
    return(paste0(value, " (", proportion, "%)"))  # Format: value (percentage%)
  }
  return(value)
}
N_values <- df_tbl %>% # Extract denominators
  filter(Variable == "N") %>%
  select(tocilizumab, sarilumab, total) %>%
  unlist() %>%
  as.numeric()
df_tbl <- df_tbl %>% # exclude N and HR (other numeric variables are protected by different format)
  mutate(
    tocilizumab = if_else(!is.na(tocilizumab) & !Variable %in% num_var,
                          add_proportion(tocilizumab, N_values[1]), 
                          tocilizumab),
    sarilumab = if_else(!is.na(sarilumab) & !Variable %in% num_var,
                        add_proportion(sarilumab, N_values[2]), 
                        sarilumab),
    total = if_else(!is.na(total) & !Variable %in% num_var,
                    add_proportion(total, N_values[3]), 
                    total)
  )

# Function to extract the Age & BMI values and standard deviations (from string to numeric), then round
round_values <- function(x) {
  if (!is.na(x)) {
    parts <- str_match(x, "([0-9.]+) \\(([^)]+)\\)")
    if (!is.na(parts[1, 1])) {
      value <- round(as.numeric(parts[1, 2]), 2)
      sd <- round(as.numeric(parts[1, 3]), 2)
      return(paste(value, " (", sd, ")", sep = ""))
    }
  }
  return(x)
}
# Apply to 'age' and 'bmi'
df_tbl <- df_tbl %>%
  mutate(across(c(tocilizumab, sarilumab, total), 
                ~ if_else(Variable %in% c("age", "bmi"), sapply(.x, round_values), .x)))

# Same for HR and CI (has a different format)
round_hr_values <- function(x) {
  if (!is.na(x)) {
    # If the string contains parentheses (indicating a confidence interval)
    if (str_detect(x, "\\(")) {
      # Extract the HR and the confidence interval separately
      parts <- str_match(x, "^([0-9.]+) \\(([^-]+)-([^)]+)\\)$")
      if (!is.na(parts[1, 1])) {
        hr <- round(as.numeric(parts[1, 2]), 2)
        ci_lower <- round(as.numeric(parts[1, 3]), 2)
        ci_upper <- round(as.numeric(parts[1, 4]), 2)
        return(paste(hr, " (", ci_lower, "-", ci_upper, ")", sep = ""))
      }
    } else {
      # If there is no confidence interval (as in sarilumab), just round the value
      return(round(as.numeric(x), 2))
    }
  }
  return(x)  # return the original value if no change needed
}
df_tbl <- df_tbl %>%
  mutate(across(c(tocilizumab, sarilumab), 
                ~ if_else(Variable == "HR", sapply(.x, round_hr_values), .x)))

## Define the baseline variables to be displayed (incl. order)
variable_order <- c(
  "N",
  "age",
  "age_group3",
  "sex",
  "ethnicity",
  "imd_1",
  "region_covid_therapeutics",
  "rural_urban",
  "d_admission_treat", 
  "calendar_day",
  "bmi_group4",
  "ckd_3_5", "liver_disease", "diabetes", "chronic_cardiac_disease", "hypertension", "chronic_respiratory_disease", 
  "solid_cancer_ever", "haema_disease_ever", "imid", "immunosupression", "solid_organ",
  "covid_test_positive_date_d", "d_vaccinate_treat", "vaccination_status", "omicron", "previous_drug", "covid_reinfection"
)

# Rename the group labels before ordering and renaming the variables
group_labels_dict <- list(
  age_group3 = c("0" = "18-39", "1" = "40-59", "2" = ">=60"),
  sex = c("0" = "Male", "1" = "Female"),
  ethnicity = c("1" = "White", "2" = "Mixed", "3" = "South Asian", "4" = "Black", "5" = "Other", "6" = "Unknown"),
  imd_1 = c("0" = "Other", "1" = "Most deprived"),
  region_covid_therapeutics = c("1" = "Region 1", "2" = "Region 2", "3" = "Region 3", "4" = "Region 4",
                 "5" = "Region 5", "6" = "Region 6", "7" = "Region 7", "8" = "Region 8", "9" = "Region 9"),
  rural_urban = c("1" = "Urban major conurbation", "2" = "Urban minor conurbation", "3" = "Urban city and town", "5" = "Rural town and fringe", "7" = "Rural village and dispersed"),
  bmi_group4 = c("0" = "underweight", "1" = "normal", "2" = "overweight", "3" = "obese"),
  vaccination_status = c("0" = "Un-vaccinated", "1" = "One vaccination", "2" = "Two vaccinations", "3" = "Three or more vaccinations")
  )
df_tbl <- df_tbl %>%
  mutate(
    Level_Description = if_else(
      Variable %in% names(group_labels_dict) & !is.na(group_labels),
      map2_chr(Variable, as.character(group_labels), ~ {
        if (.y %in% names(group_labels_dict[[.x]])) {
          group_labels_dict[[.x]][.y]
        } else {
          NA_character_
        }
      }),
      NA_character_
    )
  )

# Order the Variables
df_tbl <- df_tbl %>%
  mutate(Variable = factor(Variable, 
                           levels = c(variable_order, setdiff(unique(Variable), variable_order)), 
                           ordered = TRUE)) %>%
  arrange(Variable) # Unspecified variables are kept but pushed to the end

# Remove unncessary levels of binary Variables, but first fill p-value to keep it
df_tbl <- df_tbl %>%
  fill(P, .direction = "down") 

# restric to df_tbl1
df_tbl1 <- df_tbl %>%
  filter(!(Variable %in% c("ckd_3_5", "liver_disease", "diabetes", "chronic_cardiac_disease", 
                           "hypertension", "chronic_respiratory_disease", "solid_cancer_ever", 
                           "haema_disease_ever", "imid", "immunosupression", "solid_organ",
                           "omicron", "previous_drug", "covid_reinfection", "HR") 
           & group_labels == 0))


# filter the table with only those we want to display
df_tbl1 <- df_tbl1 %>%
  filter(Variable %in% variable_order)

# Rename these Variables
rename_vars <- c(
  "age" = "Age",
  "age_group3" = "Age groups",
  "sex" = "Sex",
  "ethnicity" = "Ethnicity",
  "imd_1" = "Index of Multiple Deprivation",
  "region_covid_therapeutics" = "Region NHS",
  "rural_urban" = "Rural/Urban",
  "d_admission_treat" = "Days since hospital admission", 
  "calendar_day" = "Days since 01.07.2021 (calendar period)",
  "bmi_group4" = "Body mass index",
  "ckd_3_5" = "History of chronic kidney disease stage 3-5", 
  "liver_disease" = "History of severe liver disease", 
  "diabetes" = "History of diabetes mellitus", 
  "hypertension" = "History of arterial hypertension",
  "chronic_cardiac_disease" = "History of chronic cardiac disease", 
  "chronic_respiratory_disease" = "History of chronic respiratory disease", 
  "solid_cancer_ever" = "History of solid cancer", 
  "haema_disease_ever" = "History of haematological diseases", 
  "imid" = "History of immunosuppressive treatment", 
  "immunosupression" = "History of immunosuppressive disease", 
  "solid_organ" = "History of solid organ transplant",
  "covid_test_positive_date_d" = "Days since positive test", 
  "d_vaccinate_treat" = "Days since vaccination", 
  "vaccination_status" = "Vaccination status", 
  "omicron" = "Treated during Omicron (after 06.12.2021)", 
  "previous_drug" = "History of COVID-19 treatment", 
  "covid_reinfection" = "COVID-19 re-infection (COVID-19 event >= 3 months)"
)
# Rename the variables and only keep the label for the first row of each group
df_tbl1 <- df_tbl1 %>%
  mutate(
    Variable = recode(Variable, !!!rename_vars),
    Variable = if_else(
      row_number() == 1 | Variable != lag(Variable),
      Variable,
      NA_character_
    )
  )

# Only keep the label for the first P value row of each group
df_tbl1 <- df_tbl1 %>%
  mutate(
    P = if_else(
      row_number() == 1 | P != lag(P) | is.na(lag(P)),
      P,
      NA
    )
  )

# Replace NA Variables with empty string
df_tbl1 <- df_tbl1 %>%
  mutate(Variable = replace_na(Variable, "")) %>% 
  mutate(Level_Description = replace_na(Level_Description, ""))

# Create the table
table_gt <- df_tbl1 %>%
  select(Variable, Level_Description, tocilizumab, sarilumab, total, P) %>%
  gt() %>%
  tab_header(title = "Baseline Characteristics") %>%
  fmt_number(
    columns = all_of("P"), decimals = 3  # Ensure column reference
  ) %>%
  fmt_missing(
    columns = all_of("P"), missing_text = ""  # Display empty instead of NA
  ) %>%
  cols_label(
    Variable = "Characteristic",
    Level_Description = "",  # Empty label
    tocilizumab = "Tocilizumab",
    sarilumab = "Sarilumab",
    total = "Total",
    P = "P-Value"
  ) %>%
  tab_options(
    table.font.size = px(12)
  )


####
# Love plot for unweighted vs weighted SMD ----
####
# Load the data
df_psw <- read_csv(here("psw_check.csv"))
# add the covariate labels; double-check in the logs!
covariates_psw <- c(
  "Age", "Sex (Female)", "Region 1", "Region 2",
  "Region 3","Region 4","Region 5","Region 6","Region 7",
  "Solid Cancer", "Haematological Disease", "CKD (Stage 3-5)", "Liver Disease",
  "IMID", "Immunosuppression", "Solid Organ Transplant", "Diabetes",
  "Chronic Cardiac Disease", "Hypertension", "Chronic Respiratory Disease",
  "BMI Category 1", "BMI Category 2","BMI Category 3","BMI Category 4", 
  "Ethnicity 1", "Ethnicity 2", "Ethnicity 3", "Ethnicity 4", "Ethnicity 5", "Ethnicity 6",
  "IMD 1", "IMD 2","IMD 3","IMD 4","IMD 5","IMD 6",
  "Vaccination Status 1", "Vaccination Status 2", "Vaccination Status 3", "Vaccination Status 4",
  "Calendar Day", "COVID Reinfection",
  "Previous Drug Exposure"
)

df_psw$Covariate <- covariates_psw

# Transform data to long format for ggplot
df_psw_long <- df_psw %>%
  pivot_longer(cols = c(raw_std_diff, weighted_std_diff), 
               names_to = "Type", 
               values_to = "SMD")

# Love plot with 0.1 threshold line
ggplot(df_psw_long, aes(x = SMD, y = factor(Covariate, levels = rev(unique(Covariate))), color = Type)) +
  geom_point(size = 4) +  # Dots for each SMD value
  geom_line(aes(group = Covariate), color = "gray") +  # Connecting lines
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dotted", color = "black") +  # SMD threshold lines
  theme_minimal() +
  scale_color_manual(values = c("raw_std_diff" = "red", "weighted_std_diff" = "blue"),
                     labels = c("Raw SMD", "Weighted SMD")) +
  labs(x = "Standardized Mean Difference (SMD)", 
       y = "Covariates",
       title = "Love Plot: SMDs Before and After Weighting",
       color = "SMD Type") +
  theme(legend.position = "top")


####
# Results plots ----
####
# Load the main data
df_primsec <- read_csv(here("cox.csv"))
df_sens <- read_csv(here("cox_subgroup.csv")) # at the bottom there are other sens-analyses attached (extract and attach to main df_primsec)

# Function to extract HR and CI
extract_hr_ci <- function(hr_string) {
  hr_value <- as.numeric(sub(" \\(.*", "", hr_string))  # Extract HR before '('
  ci_values <- gsub(".*\\(|\\)", "", hr_string)  # Extract content within '(...)'
  ci_split <- strsplit(ci_values, "-")[[1]]  # Split by '-'
  
  ci_lower <- as.numeric(ci_split[1])  # Extract lower bound
  ci_upper <- as.numeric(ci_split[2])  # Extract upper bound
  
  return(tibble(HR = hr_value, CI_Lower = ci_lower, CI_Upper = ci_upper))
}

# Extract HR and CI values for each model
df_estimates <- df_primsec %>%
  select(ends_with("_HR")) %>%
  map_dfr(~ map_dfr(., extract_hr_ci), .id = "Model") %>% 
  mutate(Model = gsub("_HR", "", Model)) # Remove _HR from model

# add the outcome labels
outcomes <- c(
  "28-day mortality", "28-day mortality (PS weighted)", 
  "90-day mortality", "180-day mortality", "1-year mortality", "2-year mortality",
  "Time to discharge within 28 days",
  "28-day mortality", "28-day mortality (PS weighted)", 
  "90-day mortality", "180-day mortality", "1-year mortality", "2-year mortality",
  "Time to discharge within 28 days",
  "28-day mortality", "28-day mortality (PS weighted)", 
  "90-day mortality", "180-day mortality", "1-year mortality", "2-year mortality",
  "Time to discharge within 28 days"
)
df_estimates$Outcome <- outcomes

# Extract P-values
df_p_values <- df_primsec %>%
  select(ends_with("_P")) %>%
  pivot_longer(cols = everything(), names_to = "Model", values_to = "P") %>%
  mutate(Model = gsub("_P", "", Model))  # Remove _P

# add the outcome labels
outcomes_p <- c(
  "28-day mortality", "28-day mortality", "28-day mortality",
  "28-day mortality (PS weighted)", "28-day mortality (PS weighted)", "28-day mortality (PS weighted)",
  "90-day mortality", "90-day mortality", "90-day mortality", 
  "180-day mortality", "180-day mortality", "180-day mortality", 
  "1-year mortality", "1-year mortality", "1-year mortality", 
  "2-year mortality", "2-year mortality", "2-year mortality",
  "Time to discharge within 28 days", "Time to discharge within 28 days", "Time to discharge within 28 days"
)
df_p_values$Outcome <- outcomes_p

# Merge HR, CI, and P-values back into one dataframe in the long format
df_primsec_long <- left_join(df_estimates, df_p_values, by = c("Model", "Outcome"))



### Add from subgroup set: Extract HR and CI values
df_sens_extr <- df_sens %>%
  filter(Variable %in% c("add_adjust", "mi", "COVID_specific_mortality", "poisson")) %>% 
  select(ends_with("_HR")) %>%
  map_dfr(~ map_dfr(., extract_hr_ci), .id = "Model")
outcomes_sens <- c(
  "28-day mortality (Multiple imputation; Model 2)", 
  "28-day mortality (Multiple imputation; Model 3)",
  "28-day mortality (Additional adjustments)",
  "28-day COVID-mortality",
  "28-day mortality (Baysian Cox)"
)
df_sens_extr$Outcome <- outcomes_sens
df_sens_extr <- df_sens_extr %>%
  mutate(Model = case_when(Outcome == "28-day mortality (Multiple imputation; Model 2)" ~ "Model2",
                           Outcome == "28-day mortality (Multiple imputation; Model 3)" ~ "Model3",
                           Outcome == "28-day mortality (Additional adjustments)" ~ "Model4",
                           Outcome == "28-day COVID-mortality" ~ "Model3",
                           Outcome == "28-day mortality (Baysian Cox)" ~ "Model3"))
# Extract P-values
df_sens_p_values <- df_sens %>%
  filter(Variable %in% c("add_adjust", "mi", "COVID_specific_mortality", "poisson")) %>% 
  select(ends_with("_P")) %>%
  pivot_longer(cols = everything(), names_to = "Model", values_to = "P") %>% 
  select(P)
df_sens_p_values$Outcome <- outcomes_sens
# Merge
df_sens_long <- left_join(df_sens_extr, df_sens_p_values, by = c("Outcome"))


# Append to main set
df_results_long <- rbind(df_primsec_long, df_sens_long)

# Add the absolute numbers from df_tbl
df_results_abs <- df_tbl %>% 
  select(Variable, tocilizumab, sarilumab, group_labels) %>%
  filter(Variable %in% c("failure", "failure_covid", "failure_90d", "failure_180d", "failure_1y", "failure_2y", "event_discharge")) %>% 
  filter(group_labels == 1) %>% 
  rename("Outcome" = "Variable") %>% 
  mutate(Outcome = case_when(Outcome == "failure" ~ "28-day mortality",
                             Outcome == "failure_covid" ~ "28-day COVID-mortality",
                             Outcome == "failure_90d" ~ "90-day mortality",
                             Outcome == "failure_180d" ~ "180-day mortality",
                             Outcome == "failure_1y" ~ "1-year mortality",
                             Outcome == "failure_2y" ~ "2-year mortality",
                             Outcome == "event_discharge" ~ "Time to discharge within 28 days"
                             )) %>% 
  select(!group_labels)

df_results_long <- left_join(df_results_long, df_results_abs, by = c("Outcome"))

# Fill up the swiss cheese
mort28_toci <- df_results_abs %>% 
  filter(Outcome == "28-day mortality") %>% 
  select(tocilizumab) %>% 
  pull()
mort28_sari <- df_results_abs %>% 
  filter(Outcome == "28-day mortality") %>% 
  select(sarilumab) %>% 
  pull()

df_results_long <- df_results_long %>%
  mutate(tocilizumab = case_when(is.na(tocilizumab) & (Outcome == "28-day mortality (PS weighted)" | 
                                                         Outcome == "28-day mortality (Multiple imputation; Model 2)" | 
                                                         Outcome == "28-day mortality (Multiple imputation; Model 3)" | 
                                                         Outcome == "28-day mortality (Additional adjustments)" | 
                                                         Outcome == "28-day mortality (Baysian Cox)") ~ mort28_toci,
                                 TRUE ~ tocilizumab)) %>% 
  mutate(sarilumab = case_when(is.na(sarilumab) & (Outcome == "28-day mortality (PS weighted)" | 
                                                         Outcome == "28-day mortality (Multiple imputation; Model 2)" | 
                                                         Outcome == "28-day mortality (Multiple imputation; Model 3)" | 
                                                         Outcome == "28-day mortality (Additional adjustments)" | 
                                                         Outcome == "28-day mortality (Baysian Cox)") ~ mort28_sari,
                                 TRUE ~ sarilumab))

# Round the p-value and convert to char for forestplot
df_results_long <- df_results_long %>%
  mutate(P = round(P, 3))
df_results_long$P_char <- as.character(df_results_long$P)


####
# MAIN RESULTS ----
####
# Filter estimates for main results plot
df_results_main <- df_results_long %>% 
  filter(Model == "Model3",
         Outcome != "28-day mortality (PS weighted)",
         Outcome != "28-day mortality (Multiple imputation; Model 2)",
         Outcome != "28-day mortality (Multiple imputation; Model 3)",
         Outcome != "28-day mortality (Additional adjustments)",
         Outcome != "28-day mortality (Baysian Cox)"
         )
# build forestplot displaying the different outcomes
base_data <- tibble(
  mean = df_results_main$HR,
  lower = df_results_main$CI_Lower,
  upper = df_results_main$CI_Upper,
  outcome = as.character(df_results_main$Outcome),
  events_i = as.character(df_results_main$sarilumab),
  events_c = as.character(df_results_main$tocilizumab),
  # p = df_results_main$P_char,
  estimates = paste0(formatC(df_results_main$HR, format = "f", digits = 2), 
                     " (", formatC(df_results_main$CI_Lower, format = "f", digits = 2), 
                     " - ", formatC(df_results_main$CI_Upper, format = "f", digits = 2), ")"))
header <- tibble(
  outcome = "Outcome",
  events_i = "Events Sarilumab",
  events_c = "Events Tocilizumab",
  # p = "p-value",
  estimates = "aHR (95% CI)",
  mean = NA, lower = NA, upper = NA)

fp <- bind_rows(header, base_data)
font <- "sans"

# Create the forest plot

pdf(file = "Fp_results_main.pdf",
    width = 15,
    height = 8)
fp %>%
  forestplot(
    labeltext = c(outcome, events_i, events_c, 
                  # p, 
                  estimates),
    mean = mean, lower = lower, upper = upper,  # Numeric columns for the plot
    txt_gp = fpTxtGp(
      label = gpar(fontfamily = font, cex = 1),
      ticks = gpar(cex = 0.88),
      xlab = gpar(cex = 0.88)
    ),
    graph.pos = 4, 
    hrzl_lines = list("2" = gpar(lty = 2),
                      "3" = gpar(lty = 2),
                      "4" = gpar(lty = 2),
                      "5" = gpar(lty = 2),
                      "6" = gpar(lty = 2),
                      "7" = gpar(lty = 2),
                      "8" = gpar(lty = 2)),
    xlog = TRUE,
    xticks = log(c(0.81, 1, 1.25)),
    lty.ci = 1,
    col = fpColors(
      box = "maroon4",
      line = "maroon1",
      summary = "magenta4",
      hrz_lines = "gray63"
    ),
    vertices = TRUE,
    xlab = "Favours Sarilumab < > Favours Tocilizumab",
    zero = 1
  )
dev.off()


####
# MODEL 2 RESULTS ----
####
df_results_2 <- df_results_long %>% 
  filter(Model == "Model2",
         Outcome != "28-day mortality (PS weighted)",
         Outcome != "28-day mortality (Multiple imputation; Model 2)",
         Outcome != "28-day mortality (Multiple imputation; Model 3)",
         Outcome != "28-day mortality (Additional adjustments)",
         Outcome != "28-day mortality (Baysian Cox)"
         )
# build forestplot displaying the different outcomes
base_data <- tibble(
  mean = df_results_2$HR,
  lower = df_results_2$CI_Lower,
  upper = df_results_2$CI_Upper,
  outcome = as.character(df_results_2$Outcome),
  events_i = as.character(df_results_2$sarilumab),
  events_c = as.character(df_results_2$tocilizumab),
  # p = df_results_2$P_char,
  estimates = paste0(formatC(df_results_2$HR, format = "f", digits = 2), 
                     " (", formatC(df_results_2$CI_Lower, format = "f", digits = 2), 
                     " - ", formatC(df_results_2$CI_Upper, format = "f", digits = 2), ")"))
header <- tibble(
  outcome = "Outcome",
  events_i = "Events Sarilumab",
  events_c = "Events Tocilizumab",
  # p = "p-value",
  estimates = "aHR (95% CI)",
  mean = NA, lower = NA, upper = NA)

fp <- bind_rows(header, base_data)
font <- "sans"

# Create the forest plot
pdf(file = "Fp_results_model2.pdf",
    width = 15,
    height = 8)
fp %>%
  forestplot(
    labeltext = c(outcome, events_i, events_c, 
                  # p, 
                  estimates),
    mean = mean, lower = lower, upper = upper,
    txt_gp = fpTxtGp(
      label = gpar(fontfamily = font, cex = 1),
      ticks = gpar(cex = 0.88),
      xlab = gpar(cex = 0.88)
    ),
    graph.pos = 4, 
    hrzl_lines = list("2" = gpar(lty = 2),
                      "3" = gpar(lty = 2),
                      "4" = gpar(lty = 2),
                      "5" = gpar(lty = 2),
                      "6" = gpar(lty = 2),
                      "7" = gpar(lty = 2)),
    xlog = TRUE,
    xticks = log(c(0.81, 1, 1.25)),
    lty.ci = 1,
    col = fpColors(
      box = "maroon4",
      line = "maroon1",
      summary = "magenta4",
      hrz_lines = "gray63"
    ),
    vertices = TRUE,
    xlab = "Favours Sarilumab < > Favours Tocilizumab",
    zero = 1
  )
dev.off()


####
# MODEL 1 RESULTS ----
####
df_results_1 <- df_results_long %>% 
  filter(Model == "Model1",
         Outcome != "28-day mortality (PS weighted)",
         Outcome != "28-day mortality (Multiple imputation; Model 2)",
         Outcome != "28-day mortality (Multiple imputation; Model 3)",
         Outcome != "28-day mortality (Additional adjustments)",
         Outcome != "28-day mortality (Baysian Cox)"
         )
# build forestplot displaying the different outcomes
base_data <- tibble(
  mean = df_results_1$HR,
  lower = df_results_1$CI_Lower,
  upper = df_results_1$CI_Upper,
  outcome = as.character(df_results_1$Outcome),
  events_i = as.character(df_results_1$sarilumab),
  events_c = as.character(df_results_1$tocilizumab),
  # p = df_results_1$P_char,
  estimates = paste0(formatC(df_results_1$HR, format = "f", digits = 2), 
                     " (", formatC(df_results_1$CI_Lower, format = "f", digits = 2), 
                     " - ", formatC(df_results_1$CI_Upper, format = "f", digits = 2), ")"))
header <- tibble(
  outcome = "Outcome",
  events_i = "Events Sarilumab",
  events_c = "Events Tocilizumab",
  # p = "p-value",
  estimates = "aHR (95% CI)",
  mean = NA, lower = NA, upper = NA)

fp <- bind_rows(header, base_data)
font <- "sans"

# Create the forest plot

pdf(file = "Fp_results_model1.pdf",
    width = 15,
    height = 8)
fp %>%
  forestplot(
    labeltext = c(outcome, events_i, events_c, 
                  # p, 
                  estimates),
    mean = mean, lower = lower, upper = upper,
    txt_gp = fpTxtGp(
      label = gpar(fontfamily = font, cex = 1),
      ticks = gpar(cex = 0.88),
      xlab = gpar(cex = 0.88)
    ),
    graph.pos = 4, 
    hrzl_lines = list("2" = gpar(lty = 2),
                      "3" = gpar(lty = 2),
                      "4" = gpar(lty = 2),
                      "5" = gpar(lty = 2),
                      "6" = gpar(lty = 2),
                      "7" = gpar(lty = 2)),
    xlog = TRUE,
    xticks = log(c(0.81, 1, 1.25)),
    lty.ci = 1,
    col = fpColors(
      box = "maroon4",
      line = "maroon1",
      summary = "magenta4",
      hrz_lines = "gray63"
    ),
    vertices = TRUE,
    xlab = "Favours Sarilumab < > Favours Tocilizumab",
    zero = 1
  )
dev.off()



####
# PROPENSITY SCORE RESULTS ----
####
# All PS models PLUS primary outcome
df_results_ps <- df_results_long %>% 
  filter(Outcome == "28-day mortality (PS weighted)" | (Outcome == "28-day mortality" & Model == "Model3")) %>% 
  mutate(
    Outcome = case_when(
      Model == "Model3" & Outcome == "28-day mortality" ~ "Primary outcome, primary model, fully adjusted",
      Model == "Model3" & Outcome == "28-day mortality (PS weighted)" ~ "Primary outcome, PS weighted, fully adjusted",
      Model == "Model2" ~ "Primary outcome, PS weighted, less adjusted",
      Model == "Model1" ~ "Primary outcome, PS weighted, minimally adjusted"
    ))
df_results_ps <- df_results_ps %>% 
  mutate(Outcome = factor(Outcome, levels = c("Primary outcome, primary model, fully adjusted", 
                                     "Primary outcome, PS weighted, fully adjusted",
                                     "Primary outcome, PS weighted, less adjusted",
                                     "Primary outcome, PS weighted, minimally adjusted")))
df_results_ps <- df_results_ps %>% arrange(Outcome)

# build forestplot displaying the different outcomes
base_data <- tibble(
  mean = df_results_ps$HR,
  lower = df_results_ps$CI_Lower,
  upper = df_results_ps$CI_Upper,
  outcome = as.character(df_results_ps$Outcome),
  events_i = as.character(df_results_ps$sarilumab),
  events_c = as.character(df_results_ps$tocilizumab),
  # p = df_results_ps$P_char,
  estimates = paste0(formatC(df_results_ps$HR, format = "f", digits = 2), 
                     " (", formatC(df_results_ps$CI_Lower, format = "f", digits = 2), 
                     " - ", formatC(df_results_ps$CI_Upper, format = "f", digits = 2), ")"))
header <- tibble(
  outcome = "Outcome",
  events_i = "Events Sarilumab",
  events_c = "Events Tocilizumab",
  # p = "p-value",
  estimates = "aHR (95% CI)",
  mean = NA, lower = NA, upper = NA)

fp <- bind_rows(header, base_data)
font <- "sans"

# Create the forest plot

pdf(file = "Fp_results_PS.pdf",
    width = 15,
    height = 8)
fp %>%
  forestplot(
    labeltext = c(outcome, events_i, events_c, 
                  # p, 
                  estimates),
    mean = mean, lower = lower, upper = upper,
    txt_gp = fpTxtGp(
      label = gpar(fontfamily = font, cex = 1),
      ticks = gpar(cex = 0.88),
      xlab = gpar(cex = 0.88)
    ),
    graph.pos = 4, 
    hrzl_lines = list("2" = gpar(lty = 2),
                      "3" = gpar(lty = 2),
                      "4" = gpar(lty = 2),
                      "5" = gpar(lty = 2)),
    xlog = TRUE,
    xticks = log(c(0.68, 1, 1.5)),
    lty.ci = 1,
    col = fpColors(
      box = "maroon4",
      line = "maroon1",
      summary = "magenta4",
      hrz_lines = "gray63"
    ),
    vertices = TRUE,
    xlab = "Favours Sarilumab < > Favours Tocilizumab",
    zero = 1
  )
dev.off()


####
# SENS RESULTS ----
####
df_results_sens <- df_results_long %>% 
  filter(Outcome == "28-day mortality (Multiple imputation; Model 2)" | 
           Outcome == "28-day mortality (Multiple imputation; Model 3)" |
           Outcome == "28-day mortality (Additional adjustments)" |
           Outcome == "28-day mortality (Baysian Cox)"
  )
# build forestplot displaying the different outcomes
base_data <- tibble(
  mean = df_results_sens$HR,
  lower = df_results_sens$CI_Lower,
  upper = df_results_sens$CI_Upper,
  outcome = as.character(df_results_sens$Outcome),
  events_i = as.character(df_results_sens$sarilumab),
  events_c = as.character(df_results_sens$tocilizumab),
  # p = df_results_sens$P_char,
  estimates = paste0(formatC(df_results_sens$HR, format = "f", digits = 2), 
                     " (", formatC(df_results_sens$CI_Lower, format = "f", digits = 2), 
                     " - ", formatC(df_results_sens$CI_Upper, format = "f", digits = 2), ")"))
header <- tibble(
  outcome = "Outcome",
  events_i = "Events Sarilumab",
  events_c = "Events Tocilizumab",
  # p = "p-value",
  estimates = "aHR (95% CI)",
  mean = NA, lower = NA, upper = NA)

fp <- bind_rows(header, base_data)
font <- "sans"

# Create the forest plot

pdf(file = "Fp_results_sens.pdf",
    width = 15,
    height = 8)
fp %>%
  forestplot(
    labeltext = c(outcome, events_i, events_c, 
                  # p, 
                  estimates),
    mean = mean, lower = lower, upper = upper,
    txt_gp = fpTxtGp(
      label = gpar(fontfamily = font, cex = 1),
      ticks = gpar(cex = 0.88),
      xlab = gpar(cex = 0.88)
    ),
    graph.pos = 4, 
    hrzl_lines = list("2" = gpar(lty = 2),
                      "3" = gpar(lty = 2),
                      "4" = gpar(lty = 2),
                      "5" = gpar(lty = 2)),
    xlog = TRUE,
    xticks = log(c(0.68, 1, 1.5)),
    lty.ci = 1,
    col = fpColors(
      box = "maroon4",
      line = "maroon1",
      summary = "magenta4",
      hrz_lines = "gray63"
    ),
    vertices = TRUE,
    xlab = "Favours Sarilumab < > Favours Tocilizumab",
    zero = 1
  )
dev.off()

# df_results_sens <- df_results_sens %>% # for correct ordering
#   mutate(Model = factor(Model, levels = unique(Model)),
#          Outcome = factor(Outcome, levels = unique(Outcome)))
# ggplot(df_results_sens, aes(x = HR, y = interaction(Outcome))) +
#   geom_point(size = 4, color = "blue") +
#   geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, color = "black") +
#   geom_text(aes(x = CI_Upper + 0.05, label = paste0("P = ", format(P, digits = 3))),
#             hjust = 0, size = 4, color = "red") +
#   scale_x_log10(breaks = c(0.75, 1, 1.25, 1.5), limits = c(0.75, 1.5)) +  # Max set to 1.5
#   scale_y_discrete(
#     limits = rev(levels(interaction(df_results_sens$Outcome))),  # Reverse order
#     # labels = c("28-day mortality.Model1" = "Model 1 - Outcome 1", 
#     #            "28-day mortality.Model2" = "Model 2 - Outcome 1") 
#   ) +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.5) +  # Add dashed line at x = 1
#   theme_minimal() +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 12)
#   ) +
#   labs(
#     title = "Adjusted Hazard Ratios with 95% Confidence Intervals", 
#     x = "Adjusted Hazard Ratio (aHR)"
#   )


####
# SUBGROUP RESULTS ----
####
# Load the data
df_subgroup <- read_csv(here("cox_subgroup.csv"))

df_subgroup_estimates <- df_subgroup %>%
  filter(!Variable %in% c("add_adjust", "mi", "COVID_specific_mortality", "poisson")) %>% 
  select(ends_with("_HR")) %>%
  map_dfr(~ map_dfr(., extract_hr_ci), .id = "Model")
labels_subgroups <- c( ### CAVE: double-check if new export!!!
  "Delta wave", "Omicron wave",
  "Sex: Male", "Sex: Female",
  "Age: Below 60 years", "Age: 60 years or above",
  "Ethnicity: Non-White", "Ethnicity: White",
  "Vaccines: Less than 3", "Vaccines: 3 or more",
  "BMI: Below 30", "BMI: 30 or above",
  "Solid cancer: No history", "Solid cancer: history",
  "Haematological disease: No history", "Haematological disease: history",
  "Immunosuppressive treatment: No history", "Immunosuppressive treatment: history",
  "Diabetes: No history", "Diabetes: history",
  "Chronic cardiac disease: No history", "Chronic cardiac disease: history",
  "Hypertension: No history", "Hypertension: history",
  "Chronic respiratory disease: No history", "Chronic respiratory disease: history",
  "Timeband: 0-28", "Timeband: 28-90", "Timeband: 90-180", "Timeband: 180-365", "Timeband: 365-730"
)
df_subgroup_estimates$Subgroup <- labels_subgroups

df_subgroup_rest <- df_subgroup %>%
  filter(!Variable %in% c("add_adjust", "mi", "COVID_specific_mortality", "poisson")) %>% 
  select(N, events, P_interaction)

df_subgroup_tot <- cbind(df_subgroup_estimates, df_subgroup_rest)

### Build forestplot
# First, work on P-int: Replace NA Variables with empty string and keep p-int as character
df_subgroup_tot <- df_subgroup_tot %>%
  mutate(P_interaction = as.numeric(P_interaction))
df_subgroup_tot <- df_subgroup_tot %>%
  mutate(P_interaction = round(P_interaction, 3))
df_subgroup_tot$p_int <- as.character(df_subgroup_tot$P_interaction)
df_subgroup_tot <- df_subgroup_tot %>%
  mutate(p_int = replace_na(p_int, ""))

# Second reorder and cut the list in two (for presentation)
# reorder
df_subgroup_tot <- df_subgroup_tot %>% 
  mutate(Subgroup = factor(Subgroup, levels = c(
    "Delta wave", "Omicron wave",
    "Sex: Male", "Sex: Female",
    "Age: Below 60 years", "Age: 60 years or above",
    "Ethnicity: Non-White", "Ethnicity: White",
    "Vaccines: Less than 3", "Vaccines: 3 or more",
    "Timeband: 0-28", "Timeband: 28-90", "Timeband: 90-180", "Timeband: 180-365", "Timeband: 365-730",
    "BMI: Below 30", "BMI: 30 or above",
    "Solid cancer: No history", "Solid cancer: history",
    "Haematological disease: No history", "Haematological disease: history",
    "Immunosuppressive treatment: No history", "Immunosuppressive treatment: history",
    "Diabetes: No history", "Diabetes: history",
    "Chronic cardiac disease: No history", "Chronic cardiac disease: history",
    "Hypertension: No history", "Hypertension: history",
    "Chronic respiratory disease: No history", "Chronic respiratory disease: history"
    )))
df_subgroup_tot <- df_subgroup_tot %>% arrange(Subgroup)
# cut
df_subgroup_fp1 <- df_subgroup_tot %>% 
  filter(Subgroup %in% c("Delta wave", "Omicron wave",
    "Sex: Male", "Sex: Female",
    "Age: Below 60 years", "Age: 60 years or above",
    "Ethnicity: Non-White", "Ethnicity: White",
    "Vaccines: Less than 3", "Vaccines: 3 or more",
    "Timeband: 0-28", "Timeband: 28-90", "Timeband: 90-180", "Timeband: 180-365", "Timeband: 365-730"))
df_subgroup_fp2 <- df_subgroup_tot %>% 
  filter(Subgroup %in% c("BMI: Below 30", "BMI: 30 or above",
                         "Solid cancer: No history", "Solid cancer: history",
                         "Haematological disease: No history", "Haematological disease: history",
                         "Immunosuppressive treatment: No history", "Immunosuppressive treatment: history",
                         "Diabetes: No history", "Diabetes: history",
                         "Chronic cardiac disease: No history", "Chronic cardiac disease: history",
                         "Hypertension: No history", "Hypertension: history",
                         "Chronic respiratory disease: No history", "Chronic respiratory disease: history"))

# pull overall treatment estimate from df_results_long
mort_28_OR <- df_results_long %>% 
  filter(Model == "Model3" & Outcome == "28-day mortality") %>% 
  pull(HR)
mort_28_ci_lower <- df_results_long %>% 
  filter(Model == "Model3" & Outcome == "28-day mortality") %>% 
  pull(CI_Lower)
mort_28_ci_upper <- df_results_long %>% 
  filter(Model == "Model3" & Outcome == "28-day mortality") %>% 
  pull(CI_Upper)

# Forestplot 1
base_data <- tibble(mean = df_subgroup_fp1$HR,
                    lower = df_subgroup_fp1$CI_Lower,
                    upper = df_subgroup_fp1$CI_Upper,
                    subgroup = as.character(df_subgroup_fp1$Subgroup),
                    # tot_i = as.character(tot_i),
                    # events_i = as.character(events_i),
                    # tot_c = as.character(tot_c),
                    # events_c = as.character(events_c),
                    p_int = df_subgroup_fp1$p_int)
summary <- tibble(mean = mort_28_OR,
                  lower = mort_28_ci_lower,
                  upper = mort_28_ci_upper,
                  subgroup = "Overall treatment effect (aHR)",
                  summary = TRUE)
header <- tibble(subgroup = c("Subgroup"),
                 # tot_i = c("Sarilumab"),
                 # events_i = c("Events Sarilumab"),
                 # tot_c = c("Tocilizumab"),
                 # events_c = c("Events Tocilizumab"),
                 p_int = c("p-int"),
                 summary = TRUE)

fp <- bind_rows(header,base_data,summary)

font <- "sans"

pdf(file = "Fp_subgroups1.pdf",
    width = 10,
    height = 8)

fp %>%
  forestplot(labeltext = c(subgroup, p_int),
             mean = mean, lower = lower, upper = upper,
             txt_gp = fpTxtGp(label = gpar(fontfamily = font, cex=1),
                              ticks = gpar(cex=0.88),
                              summary = gpar(cex=1),
                              xlab = gpar(cex=0.88)),
             is.summary = summary,
             graph.pos = 2,
             # clip = c(0.85, 1.3),
             hrzl_lines = list("2" = gpar(lty = 2),
                               "4" = gpar(lty = 2),
                               "6" = gpar(lty = 2),
                               "8" = gpar(lty = 2),
                               "10" = gpar(lty = 2),
                               "12" = gpar(lty = 2)
                               ),
             xlog = T,
             xticks = log(c(0.55, 1, 1.07, 1.75)),
             lty.ci = c(1),
             col = fpColors(box = "maroon4",
                            line = "maroon1",
                            summary = "magenta4",
                            hrz_lines = "gray63"),
             vertices = TRUE,
             xlab = "         Favours Sarilumab < > Favours Tocilizumab",
             zero = 1,
             grid = structure(c(1.07), gp = gpar(lty = 1, col = "gray63")), # ADAPT if new point estimate!
             # graphwidth = unit(100, "mm"), colgap = unit(2.5, "mm")
  )

dev.off()

# Forestplot 2
base_data <- tibble(mean = df_subgroup_fp2$HR,
                    lower = df_subgroup_fp2$CI_Lower,
                    upper = df_subgroup_fp2$CI_Upper,
                    subgroup = as.character(df_subgroup_fp2$Subgroup),
                    # tot_i = as.character(tot_i),
                    # events_i = as.character(events_i),
                    # tot_c = as.character(tot_c),
                    # events_c = as.character(events_c),
                    p_int = df_subgroup_fp2$p_int)
summary <- tibble(mean = mort_28_OR,
                  lower = mort_28_ci_lower,
                  upper = mort_28_ci_upper,
                  subgroup = "Overall treatment effect (aHR)",
                  summary = TRUE)
header <- tibble(subgroup = c("Subgroup"),
                 # tot_i = c("Sarilumab"),
                 # events_i = c("Events Sarilumab"),
                 # tot_c = c("Tocilizumab"),
                 # events_c = c("Events Tocilizumab"),
                 p_int = c("p-int"),
                 summary = TRUE)

fp <- bind_rows(header,base_data,summary)

font <- "sans"

pdf(file = "Fp_subgroups2.pdf",
    width = 10,
    height = 8)

fp %>%
  forestplot(labeltext = c(subgroup, p_int),
             mean = mean, lower = lower, upper = upper,
             txt_gp = fpTxtGp(label = gpar(fontfamily = font, cex=1),
                              ticks = gpar(cex=0.88),
                              summary = gpar(cex=1),
                              xlab = gpar(cex=0.88)),
             is.summary = summary,
             graph.pos = 2,
             # clip = c(0.85, 1.3),
             hrzl_lines = list("2" = gpar(lty = 2),
                               "4" = gpar(lty = 2),
                               "6" = gpar(lty = 2),
                               "8" = gpar(lty = 2),
                               "10" = gpar(lty = 2),
                               "12" = gpar(lty = 2),
                               "14" = gpar(lty = 2),
                               "16" = gpar(lty = 2)
             ),
             xlog = T,
             xticks = log(c(0.55, 1, 1.07, 1.75)),
             lty.ci = c(1),
             col = fpColors(box = "maroon4",
                            line = "maroon1",
                            summary = "magenta4",
                            hrz_lines = "gray63"),
             vertices = TRUE,
             xlab = "         Favours Sarilumab < > Favours Tocilizumab",
             zero = 1,
             grid = structure(c(1.07), gp = gpar(lty = 1, col = "gray63")), # ADAPT if new point estimate!
             # graphwidth = unit(100, "mm"), colgap = unit(2.5, "mm")
  )

dev.off()
