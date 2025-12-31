# A/B Test Analysis
library(dplyr)
library(ggplot2)

ab_data <- read.csv("ab_test_sample.csv")

# 1. Basic comparison
variant_summary <- ab_data %>%
  group_by(variant) %>%
  summarise(
    users = n(),
    conversions = sum(converted),
    conversion_rate = mean(converted),
    avg_pages = mean(pages_viewed),
    avg_time = mean(time_on_site)
  )

# 2. Statistical significance test
# Proportion test for conversion rates
prop_test_result <- prop.test(
  x = c(variant_summary$conversions[1], variant_summary$conversions[2]),
  n = c(variant_summary$users[1], variant_summary$users[2])
)

# 3. Visualization of results
ggplot(variant_summary, aes(x = variant, y = conversion_rate, fill = variant)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = conversion_rate - 1.96 * sqrt(conversion_rate*(100-conversion_rate)/users),
                    ymax = conversion_rate + 1.96 * sqrt(conversion_rate*(100-conversion_rate)/users)),
                width = 0.2) +
  geom_text(aes(label = paste0(round(conversion_rate, 2), "%")), 
            vjust = -0.5) +
  labs(title = "Conversion Rate by Variant",
       subtitle = paste("p-value:", round(prop_test_result$p.value, 4)),
       x = "Variant", y = "Conversion Rate (%)") +
  theme_minimal()

# 4. Bayesian A/B testing (alternative approach)
library(bayesAB)
# Convert to appropriate format for bayesAB
A_data <- ab_data %>% filter(variant == "A") %>% pull(converted)
B_data <- ab_data %>% filter(variant == "B") %>% pull(converted)

# Run Bayesian test
bayes_test <- bayesTest(A_data, B_data,
                        priors = c('alpha' = 1, 'beta' = 1),
                        n_samples = 1e5,
                        distribution = 'bernoulli')

# Plot posterior distributions
plot(bayes_test)

# 5. Calculate lift and confidence intervals
calculate_lift <- function(control_conv, test_conv, control_n, test_n) {
  lift <- (test_conv/test_n - control_conv/control_n) / (control_conv/control_n)  100
  
  # Calculate confidence interval
  se <- sqrt((control_conv/control_n)(1-control_conv/control_n)/control_n + 
               (test_conv/test_n)(1-test_conv/test_n)/test_n)
  ci_lower <- lift - 1.96 * se 
  ci_upper <- lift + 1.96 * se 
  
  return(data.frame(
    lift = lift,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    significant = ci_lower > 0 | ci_upper < 0
  ))
}

lift_result <- calculate_lift(
  variant_summary$conversions[1], variant_summary$conversions[2],
  variant_summary$users[1], variant_summary$users[2]
)
