# =========================================================================
# PRIVACY AND ETHICS IN DATA ANALYTICS
# =========================================================================

# -------------------------------------------------------------------------
# 1. SIMULATE USER DATA WITH PII
# -------------------------------------------------------------------------

set.seed(123)
n_users_privacy <- 1000

# Simulate sensitive user data
sensitive_data <- data.frame(
  user_id = 1:n_users_privacy,
  full_name = paste0("User_", 1:n_users_privacy),
  email = paste0("user", 1:n_users_privacy, "@example.com"),
  phone = paste0("+1", sample(200:999, n_users_privacy, replace = TRUE),
                 sample(100:999, n_users_privacy, replace = TRUE),
                 sample(1000:9999, n_users_privacy, replace = TRUE)),
  birth_date = sample(seq.Date(as.Date("1950-01-01"), 
                               as.Date("2005-12-31"), by = "day"), 
                      n_users_privacy, replace = TRUE),
  ssn = paste0(
    sample(100:999, n_users_privacy, replace = TRUE), "-",
    sample(10:99, n_users_privacy, replace = TRUE), "-",
    sample(1000:9999, n_users_privacy, replace = TRUE)
  ),
  ip_address = paste0(
    sample(1:255, n_users_privacy, replace = TRUE), ".",
    sample(0:255, n_users_privacy, replace = TRUE), ".",
    sample(0:255, n_users_privacy, replace = TRUE), ".",
    sample(1:254, n_users_privacy, replace = TRUE)
  ),
  location_gps = paste0(
    round(runif(n_users_privacy, -90, 90), 6), ",",
    round(runif(n_users_privacy, -180, 180), 6)
  ),
  income = round(rnorm(n_users_privacy, mean = 75000, sd = 25000), -3),
  health_condition = sample(c("None", "Diabetes", "Hypertension", "Asthma", 
                              "Heart Disease", "Cancer"), 
                            n_users_privacy, replace = TRUE,
                            prob = c(0.7, 0.1, 0.08, 0.05, 0.04, 0.03)),
  credit_score = sample(300:850, n_users_privacy, replace = TRUE),
  consent_given = rbinom(n_users_privacy, 1, 0.85),
  data_sharing_opt_out = rbinom(n_users_privacy, 1, 0.15)
)

# -------------------------------------------------------------------------
# 2. PII IDENTIFICATION AND CLASSIFICATION
# -------------------------------------------------------------------------

# Classify data fields by sensitivity level
data_classification <- data.frame(
  field = names(sensitive_data),
  data_type = c("Identifier", "PII", "PII", "PII", "PII", "SPII", "PII",
                "SPII", "Financial", "Health", "Financial", "Consent", "Consent"),
  sensitivity_level = c("High", "High", "High", "High", "High", "Very High",
                        "Medium", "Very High", "High", "Very High", "High",
                        "Low", "Low"),
  requires_consent = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                       TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  can_be_anonymized = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
                        FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
)

cat("=== DATA CLASSIFICATION MATRIX ===\n")
print(data_classification)

# Calculate privacy risk score for each field
data_classification$risk_score <- with(data_classification, 
                                       ifelse(sensitivity_level == "Very High", 10,
                                              ifelse(sensitivity_level == "High", 7,
                                                     ifelse(sensitivity_level == "Medium", 4, 1))) *
                                         ifelse(requires_consent, 1.5, 1) *
                                         ifelse(can_be_anonymized, 0.5, 1.2)
)

cat("\nPrivacy Risk Scores:\n")
print(data_classification[, c("field", "sensitivity_level", "risk_score")])

# -------------------------------------------------------------------------
# 3. DATA ANONYMIZATION TECHNIQUES
# -------------------------------------------------------------------------

# Function to anonymize data
anonymize_data <- function(data, method = "pseudonymization") {
  anonymized <- data
  
  if(method == "pseudonymization") {
    # Replace identifiers with pseudonyms
    set.seed(123)
    pseudonym_map <- data.frame(
      original = unique(c(data$full_name, data$email)),
      pseudonym = paste0("ID_", sample(100000:999999, 
                                       length(unique(c(data$full_name, data$email))),
                                       replace = FALSE))
    )
    
    anonymized$full_name <- pseudonym_map$pseudonym[match(data$full_name, 
                                                          pseudonym_map$original)]
    anonymized$email <- paste0(anonymized$full_name, "@anonymized.com")
    anonymized$phone <- "REDACTED"
    anonymized$ssn <- substr(data$ssn, 8, 11)  # Keep only last 4 digits
    anonymized$ip_address <- paste0(substr(data$ip_address, 1, 
                                           regexpr("\\.", data$ip_address)),
                                    "XXX.XXX.XXX")
    
  } else if(method == "generalization") {
    # Generalize sensitive fields
    anonymized$birth_date <- format(data$birth_date, "%Y")  # Keep only year
    anonymized$location_gps <- paste0(
      round(as.numeric(substr(data$location_gps, 1, 
                              regexpr(",", data$location_gps) - 1)) / 10) * 10,
      ",",
      round(as.numeric(substr(data$location_gps, 
                              regexpr(",", data$location_gps) + 1,
                              nchar(data$location_gps))) / 10) * 10
    )
    anonymized$income <- cut(data$income,
                             breaks = c(0, 30000, 60000, 90000, 120000, Inf),
                             labels = c("<30k", "30-60k", "60-90k", "90-120k", ">120k"))
    anonymized$credit_score <- cut(data$credit_score,
                                   breaks = c(300, 580, 670, 740, 800, 850),
                                   labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))
  }
  
  return(anonymized)
}

# Apply anonymization
anonymized_data <- anonymize_data(sensitive_data, "pseudonymization")

cat("\n=== DATA ANONYMIZATION COMPARISON ===\n")
cat("Original data sample:\n")
print(head(sensitive_data[, 1:5]))
cat("\nAnonymized data sample:\n")
print(head(anonymized_data[, 1:5]))

# -------------------------------------------------------------------------
# 4. DIFFERENTIAL PRIVACY IMPLEMENTATION
# -------------------------------------------------------------------------

# Implement differential privacy for aggregated statistics
add_laplace_noise <- function(true_value, epsilon = 0.1, sensitivity = 1) {
  # Laplace mechanism for differential privacy
  scale <- sensitivity / epsilon
  noise <- rexp(1, rate = 1/scale) - rexp(1, rate = 1/scale)  # Laplace distribution
  return(true_value + noise)
}

# Example: Private mean calculation
private_mean_income <- function(data, epsilon) {
  true_mean <- mean(data$income)
  sensitivity <- max(data$income) - min(data$income) / length(data$income)
  private_mean <- add_laplace_noise(true_mean, epsilon, sensitivity)
  return(private_mean)
}

# Calculate with different privacy budgets
epsilon_values <- c(0.01, 0.1, 1, 10)
privacy_results <- data.frame(
  epsilon = epsilon_values,
  true_mean = rep(mean(sensitive_data$income), length(epsilon_values)),
  private_mean = sapply(epsilon_values, function(e) 
    private_mean_income(sensitive_data, e)),
  error_pct = abs(sapply(epsilon_values, function(e) 
    private_mean_income(sensitive_data, e)) - mean(sensitive_data$income)) / 
    mean(sensitive_data$income) * 100
)

cat("\n=== DIFFERENTIAL PRIVACY RESULTS ===\n")
print(round(privacy_results, 2))

# -------------------------------------------------------------------------
# 5. CONSENT MANAGEMENT ANALYSIS
# -------------------------------------------------------------------------

# Analyze consent patterns
consent_analysis <- data.frame(
  metric = c("Total Users", "Consent Given", "Opted Out", 
             "Consent Rate", "Opt-Out Rate", "Active Consent Duration"),
  value = c(
    n_users_privacy,
    sum(sensitive_data$consent_given),
    sum(sensitive_data$data_sharing_opt_out),
    round(mean(sensitive_data$consent_given) * 100, 1),
    round(mean(sensitive_data$data_sharing_opt_out) * 100, 1),
    "180 days"  # Simulated
  ),
  compliance_status = c("N/A", "OK", "OK", "OK", "OK", "WARNING")
)

cat("\n=== CONSENT MANAGEMENT METRICS ===\n")
print(consent_analysis)

# Consent by data type
consent_by_type <- aggregate(cbind(consent_given, data_sharing_opt_out) ~ 
                               health_condition,
                             data = sensitive_data,
                             FUN = mean)

cat("\nConsent Rates by Health Condition:\n")
print(round(consent_by_type, 3))

# -------------------------------------------------------------------------
# 6. PRIVACY RISK ASSESSMENT
# -------------------------------------------------------------------------

# Calculate comprehensive privacy risk score
calculate_privacy_risk <- function(data) {
  risks <- list()
  
  # 1. Data volume risk
  risks$volume_risk <- nrow(data) / 1000  # Normalized
  
  # 2. Sensitivity risk (weighted average)
  sensitivity_weights <- c("Very High" = 10, "High" = 7, "Medium" = 4, "Low" = 1)
  risks$sensitivity_risk <- mean(sensitivity_weights[data_classification$sensitivity_level])
  
  # 3. Consent compliance risk
  risks$consent_risk <- 1 - mean(data$consent_given)
  
  # 4. Data sharing risk
  risks$sharing_risk <- mean(data$data_sharing_opt_out)
  
  # 5. Re-identification risk (simplified)
  # Based on uniqueness of quasi-identifiers
  quasi_identifiers <- data[, c("birth_date", "location_gps", "income")]
  uniqueness_score <- mean(apply(quasi_identifiers, 2, 
                                 function(x) length(unique(x)) / length(x)))
  risks$reid_risk <- uniqueness_score
  
  # Overall risk score (weighted)
  weights <- c(0.2, 0.3, 0.25, 0.15, 0.1)  # volume, sensitivity, consent, sharing, reid
  overall_risk <- sum(unlist(risks) * weights)
  
  return(list(
    component_risks = risks,
    overall_risk = overall_risk,
    risk_level = ifelse(overall_risk > 7, "CRITICAL",
                        ifelse(overall_risk > 5, "HIGH",
                               ifelse(overall_risk > 3, "MEDIUM", "LOW")))
  ))
}

# Assess privacy risk
privacy_risk_assessment <- calculate_privacy_risk(sensitive_data)

cat("\n=== PRIVACY RISK ASSESSMENT ===\n")
cat("Component Risks:\n")
print(round(unlist(privacy_risk_assessment$component_risks), 3))
cat("\nOverall Risk Score:", round(privacy_risk_assessment$overall_risk, 2), "\n")
cat("Risk Level:", privacy_risk_assessment$risk_level, "\n")

# -------------------------------------------------------------------------
# 7. ETHICAL AUDIT FRAMEWORK
# -------------------------------------------------------------------------

# Ethical principles compliance assessment
ethical_audit <- data.frame(
  principle = c(
    "Transparency",
    "Fairness",
    "Accountability",
    "Privacy by Design",
    "Data Minimization",
    "Purpose Limitation",
    "Storage Limitation",
    "Consent Management",
    "Right to Access",
    "Right to Erasure"
  ),
  compliance_score = c(85, 78, 92, 65, 72, 88, 90, 95, 82, 75),
  assessment = c(
    "Clear privacy policy, but technical explanations could be improved",
    "Minor biases detected in algorithm, needs monitoring",
    "Full audit trail maintained",
    "Not fully implemented in all systems",
    "Some data collection exceeds necessity",
    "Well-defined and followed",
    "Automatic deletion policies in place",
    "Robust system with user controls",
    "Self-service portal available",
    "Process exists but could be streamlined"
  ),
  priority = c("Medium", "High", "Low", "High", "Medium", 
               "Low", "Low", "Low", "Medium", "Medium")
)

cat("\n=== ETHICAL AUDIT RESULTS ===\n")
print(ethical_audit)

# Calculate overall ethical score
overall_ethical_score <- mean(ethical_audit$compliance_score)
cat("\nOverall Ethical Compliance Score:", round(overall_ethical_score, 1), "%\n")

# -------------------------------------------------------------------------
# 8. GDPR/CCPA COMPLIANCE CHECK
# -------------------------------------------------------------------------

compliance_checklist <- data.frame(
  requirement = c(
    "Lawful basis for processing",
    "Privacy notice provided",
    "Consent mechanism",
    "Right to access implemented",
    "Right to rectification",
    "Right to erasure",
    "Right to restrict processing",
    "Right to data portability",
    "Right to object",
    "Automated decision-making transparency",
    "Data protection officer appointed",
    "Data breach notification process",
    "Data protection impact assessments",
    "Records of processing activities"
  ),
  gdpr_compliant = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                     TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
  ccpa_compliant = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                     TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
  status = c("✓", "✓", "✓", "✓", "✓", "✓", "✓",
             "✓", "✓", "⚠", "✓", "✓", "✓", "⚠")
)

cat("\n=== REGULATORY COMPLIANCE CHECKLIST ===\n")
print(compliance_checklist)

# Calculate compliance rates
gdpr_compliance_rate <- mean(compliance_checklist$gdpr_compliant) * 100
ccpa_compliance_rate <- mean(compliance_checklist$ccpa_compliant) * 100

cat("\nGDPR Compliance Rate:", round(gdpr_compliance_rate, 1), "%\n")
cat("CCPA Compliance Rate:", round(ccpa_compliance_rate, 1), "%\n")

# -------------------------------------------------------------------------
# 9. PRIVACY IMPACT ASSESSMENT (PIA)
# -------------------------------------------------------------------------

pia_results <- data.frame(
  aspect = c("Data Collection", "Data Storage", "Data Processing",
             "Data Sharing", "Data Retention", "Data Destruction",
             "Security Measures", "User Controls", "Third-party Access"),
  risk_score = c(6.5, 7.2, 5.8, 8.1, 4.3, 3.9, 7.8, 5.2, 8.5),
  mitigation_effectiveness = c(85, 90, 75, 60, 95, 98, 88, 80, 55),
  residual_risk = c(0.98, 0.72, 1.45, 3.24, 0.22, 0.08, 0.94, 1.04, 3.83),
  action_required = c("No", "No", "Monitor", "Yes", "No", "No", 
                      "No", "Monitor", "Yes")
)

cat("\n=== PRIVACY IMPACT ASSESSMENT ===\n")
print(pia_results)

# -------------------------------------------------------------------------
# 10. SAVE PRIVACY AND ETHICS REPORTS
# -------------------------------------------------------------------------

write.csv(anonymized_data, "anonymized_user_data.csv", row.names = FALSE)
write.csv(data_classification, "data_classification_matrix.csv", row.names = FALSE)
write.csv(ethical_audit, "ethical_audit_report.csv", row.names = FALSE)
write.csv(compliance_checklist, "regulatory_compliance.csv", row.names = FALSE)
write.csv(pia_results, "privacy_impact_assessment.csv", row.names = FALSE)

cat("\n✓ Privacy and ethics analysis complete!\n")
cat("Reports saved for compliance documentation.\n")
