n_distinct(data_b$id)

smoking_colors <- c("0" = "#1E90FF", "1" = "#FF6347")

summary(data_b)
describe(data_b)

## Descriptive Analysis
vars_to_include <- c("age", "sex", "education", "n_cigarettes", "CVD", 
                     "dementia", "diuretics", "bmi", "bmi_ch_percent")

categorical_vars1 <- c("sex", "education", "CVD", "dementia","diuretics")
continuous_vars1 <- c("age", "n_cigarettes", "bmi", "bmi_ch_percent")

table1 <- CreateTableOne(
  vars = vars_to_include,
  strata = "smoking_cessation", 
  data = data_b,
  factorVars = categorical_vars1,
  addOverall = TRUE     
)

print(table1, 
      showAllLevels = TRUE,    
      nonnormal = c("bmi_change"),
      test = TRUE,
      smd = TRUE)

# Density and Categories plotting functions
density_plots1 <- function(data_b, continuous_vars1, smoking_colors) {
  plots <- list()
  for (var in continuous_vars1) {
    p <- ggplot(data_b, aes(x = .data[[var]], fill = factor(smoking_cessation))) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = smoking_colors, name = "Smoking Cessation", labels = c("No", "Yes")) +
      theme_bw() +
      labs(title = var, x = "Value", y = "Density") +
      theme(legend.position = "none", plot.title = element_text(size = 10))
    plots[[var]] <- p
  }
  return(plots)
}

cat_plots1 <- function(data_b, categorical_vars1, smoking_colors) {
  plots <- list()
  for (var in categorical_vars1) {
    p <- ggplot(data_b, aes(x = factor(.data[[var]]), fill = factor(smoking_cessation))) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = smoking_colors, name = "Smoking Cessation", labels = c("No", "Yes")) +
      theme_bw() +
      labs(y = "Count") +
      theme(legend.position = "none", plot.title = element_text(size = 10))
    
    if (var == "sex") {
      p <- p + scale_x_discrete(labels = c("0" = "Male", "1" = "Female")) + labs(title = "Sex", x = "")
    } else if (var %in% c("CVD", "dementia", "diuretics")) {
      p <- p + scale_x_discrete(labels = c("0" = "Absence", "1" = "Presence")) + labs(title = var, x = "")
    } else {
      p <- p + labs(title = var, x = "Value")
    }
    plots[[var]] <- p
  }
  return(plots)
}

# Generate the plots
density_plots_list <- density_plots1(data_b, continuous_vars1, smoking_colors)
categorical_plots_list <- cat_plots1(data_b, categorical_vars1, smoking_colors)

combined_plot1 <- function(density_plots1, cat_plots1) {
  combined <- (density_plots1[[1]] | density_plots1[[2]] | density_plots1[[3]]) /
    (density_plots1[[4]] | cat_plots1[[1]] | cat_plots1[[2]]) /
    (cat_plots1[[3]] | cat_plots1[[4]] | cat_plots1[[5]]) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")
  return(combined)
}

combined1 <- combined_plot1(density_plots_list, categorical_plots_list)

final_plot1 <- combined1 +
  plot_annotation(
    title = "Distribution of Variables by Smoking Cessation",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
  )

print(final_plot1)


data_b$smoking_cessation <- factor(data_b$smoking_cessation)
data_b$sex <- factor(data_b$sex)
data_b$education <- factor(data_b$education)
data_b$CVD <- factor(data_b$CVD)
data_b$dementia <- factor(data_b$dementia)
data_b$diuretics <- factor(data_b$diuretics)


sex_labels <- c("1" = "Male", "0" = "Female")
binary_labels <- c("0" = "Absence", "1" = "Presence")



## Outcome regression model
# Unadjusted model

model1 <- lm(bmi_ch_percent ~ smoking_cessation, data = data_b)
summary(model1)


# Adjusted model (with confounders)
model2 <- lm(bmi_ch_percent ~ smoking_cessation + sex + age + 
               education + n_cigarettes + CVD + 
               dementia + diuretics + bmi, 
             data = data_b)
summary(model2)


model3_noed <- lm(bmi_ch_percent ~ smoking_cessation + sex + age + n_cigarettes + CVD + 
                    dementia + diuretics + bmi, 
                  data = data_b)
summary(model3_noed)

model4_noage <- lm(bmi_ch_percent ~ smoking_cessation + sex  + 
                     education + n_cigarettes + CVD + 
                     dementia + diuretics + bmi, 
                   data = data_b)
summary(model4_noage)

model5_nodiur <- lm(bmi_ch_percent ~ smoking_cessation + sex + age + 
                      education + n_cigarettes + CVD + 
                      dementia  + bmi, 
                    data = data_b)
summary(model5_nodiur)

model6_noCVD <- lm(bmi_ch_percent ~ smoking_cessation + sex + age + 
                     education + n_cigarettes  + 
                     dementia + diuretics + bmi, 
                   data = data_b)
summary(model6_noCVD)


summary(model2)
summary(model3_noed)
summary(model4_noage)
summary(model5_nodiur)
summary(model6_noCVD)

## IPW Analysis ## 
# Propensity score model

propensity_model <- glm(smoking_cessation ~ age + sex + education + 
                          n_cigarettes + CVD + dementia + diuretics + bmi,
                        family = binomial(), data = data_b)
summary(propensity_model)

# Calculate weights for both sets
data_b$ps <- predict(propensity_model, type = "response")

# For smoking cessation = 1 
weight = 1/data_b$ps

# For smoking cessation = 0): weight = 1/(1-ps)
data_b$ipw <- ifelse(data_b$smoking_cessation == 1, 
                     1/data_b$ps, 
                     1/(1-data_b$ps))
summary(data_b$ipw)

data_b$ipw_truncated <- pmin(data_b$ipw, 20)
summary(data_b$ipw_truncated)


# Estimate the effect of smoking_cessation on BMI_change (without additional confounders)
ipw_model_unadj <- glm(bmi_ch_percent ~ smoking_cessation, 
                       data = data_b, 
                       weights = ipw_truncated)
summary(ipw_model_unadj)

# Adjusted model (with confounders)
ipw_model_adj <- glm(bmi_ch_percent ~ smoking_cessation + age + sex + education + 
                       n_cigarettes + CVD + dementia + diuretics + bmi,
                     data = data_b, 
                     weights = ipw_truncated)
summary(ipw_model_adj)



## G-formula ##
data_0 <- data_b
data_1 <- data_b
data_0$smoking_cessation <- 0 
data_1$smoking_cessation <- 1


data_0$smoking_cessation <- factor(0)
data_0$sex <- factor(data_b$sex)
data_0$education <- factor(data_b$education)
data_0$CVD <- factor(data_b$CVD)
data_0$dementia <- factor(data_b$dementia)
data_0$diuretics <- factor(data_b$diuretics)

data_1$smoking_cessation <- factor(1)
data_1$sex <- factor(data_b$sex)
data_1$education <- factor(data_b$education)
data_1$CVD <- factor(data_b$CVD)
data_1$dementia <- factor(data_b$dementia)
data_1$diuretics <- factor(data_b$diuretics)

# Outcome on smoking cessation = 0 
outcome_model <- glm(
  bmi_ch_percent ~ smoking_cessation + age + sex + 
    education + n_cigarettes + CVD + dementia + 
    diuretics + bmi,
  data = data_b
)
summary(outcome_model)


# Predicting outcome 
pred_0 <- predict(outcome_model, newdata = data_0)
pred_1 <- predict(outcome_model, newdata = data_1)

summary(pred_0)
summary(pred_1)


# Calculate average causal effect
g_formula_effect <- mean(pred_1 - pred_0)

boot_g_formula <- function(data, indices) {
  # Create a bootstrap sample using the provided indices
  boot_data <- data[indices, ]
  boot_model <- glm(
    bmi_ch_percent ~ smoking_cessation + age + sex + 
      education + n_cigarettes + CVD + dementia + 
      diuretics + bmi,
    data = boot_data
  )
  pred_0 <- predict(boot_model, newdata = data_0)
  pred_1 <- predict(boot_model, newdata = data_1)
  return(mean(pred_1 - pred_0))
}
print(g_formula_effect)

# Bootstrap CI
set.seed(133323)
boot_results <- boot(
  data = data_b,
  statistic = boot_g_formula,
  R = 100
)
normal_ci <- boot.ci(boot_results, type = "norm")
print(normal_ci)

# IPW
ipw_model_adj <- glm(bmi_ch_percent ~ smoking_cessation + age + sex + education + 
                       n_cigarettes + CVD + dementia + diuretics + bmi,
                     data = data_b, 
                     weights = ipw_truncated)

summary(ipw_model_adj)

outcome_model <- glm(
  bmi_ch_percent ~ smoking_cessation + age + sex + 
    education + n_cigarettes + CVD + dementia + 
    diuretics + bmi,
  data = data_b
)
summary(outcome_model)

