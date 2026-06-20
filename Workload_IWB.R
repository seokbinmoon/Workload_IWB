sink("Result.txt", split = TRUE)

library(readxl)
library(dplyr)
library(lavaan)
library(semTools)

df_raw <- read_excel("Survey_2024.xlsx", sheet = 1)


df <- df_raw %>%
  select(q1,
         q31_1, q31_2, q31_3,   # Role Conflict
         q31_4, q31_5, q31_6,   # Role Ambiguity
         q4_1,  q4_2,  q4_3,    # Job Autonomy
         q33_1, q33_2, q33_3, q33_4,  # IWB
         dq1, dq2, dq5_2, dq9_2, DM1) %>%
  
  filter(dq9_2 >= 3) %>%
  mutate(
    female  = ifelse(dq1 == 2, 1, 0),
    age   = 2024 - dq2 + 1,                   
    edu     = ifelse(dq5_2 >= 3, 1, 0),
    grade   = ifelse(dq9_2 <= 5, 1, 0),
    central = ifelse(DM1 == 1, 1, 0)   
  )

cat("N =", nrow(df), "\n")
cat("female:\n");  print(table(df$female))
cat("age:\n");   print(table(df$age))
cat("edu:\n");     print(table(df$edu))
cat("grade:\n");   print(table(df$grade))
cat("central:\n"); print(table(df$central))


# Demographic characteristics table
cat(" Final Sample Demographic Characteristics\n")
cat(sprintf("Total N = %d\n\n", nrow(df)))

demo_table <- function(varname, var, labels) {
  tab <- table(var)
  pct <- round(prop.table(tab) * 100, 2)
  cat(sprintf("[ %s ]\n", varname))
  for (lv in names(tab)) {
    lbl <- if (!is.null(labels) && lv %in% names(labels)) labels[[lv]] else lv
    cat(sprintf("  %-20s  n=%4d  (%.2f%%)\n", lbl, tab[[lv]], pct[[lv]]))
  }
  cat("\n")
}

demo_table("Gender (female)", df$female,
           list("0" = "Male", "1" = "Female"))
demo_table("Education (edu, 1 = 4yr univ or higher)", df$edu,
           list("0" = "Below 4yr univ", "1" = "4yr univ or higher"))
demo_table("Grade (grade, 1 = below mid-manager)", df$grade,
           list("0" = "Mid-manager or higher", "1" = "Below mid-manager"))
demo_table("Centralization (central, 1 = central gov.)", df$central,
           list("0" = "Local/Other", "1" = "Central gov."))

cat("[ Age (continuous) ]\n")
cat(sprintf("  Mean = %.2f   SD = %.2f   Min = %d   Max = %d\n\n",
            mean(df$age), sd(df$age), min(df$age), max(df$age)))


analysis_vars <- c("q1",
                   "q31_1","q31_2","q31_3",
                   "q31_4","q31_5","q31_6",
                   "q4_1","q4_2","q4_3",
                   "q33_1","q33_2","q33_3","q33_4",
                   "female","age","edu","grade","central")

# Descriptive statistics
for (v in analysis_vars) {
  m  <- round(mean(df[[v]]), 3)
  s  <- round(sd(df[[v]]),   3)
  md <- round(median(df[[v]]), 3)
  mn <- round(min(df[[v]]), 3)
  mx <- round(max(df[[v]]), 3)
  cat(sprintf("  %-8s  M=%.3f  SD=%.3f  Median=%.3f  Min=%.3f  Max=%.3f\n", v, m, s, md, mn, mx))
}


# Correlation table (main variables as composites + controls)
cat(" Correlation Table (composites + controls)\n")

df_corr <- df %>%
  mutate(
    Workload              = q1,
    Role_Conflict            = rowMeans(across(c(q31_1, q31_2, q31_3))),
    Role_Ambiguity           = rowMeans(across(c(q31_4, q31_5, q31_6))),
    Job_Autonomy             = rowMeans(across(c(q4_1,  q4_2,  q4_3))),
    Innovative_Work_Behavior = rowMeans(across(c(q33_1, q33_2, q33_3, q33_4)))
  ) %>%
  select(Workload, Role_Conflict, Role_Ambiguity, Job_Autonomy,
         Innovative_Work_Behavior, female, age, edu, grade, central)

cor_mat <- cor(df_corr, use = "pairwise.complete.obs")
print(round(cor_mat, 3))

# Correlation table with significance stars
cat("\n--- Correlations with significance stars (* p<.05, ** p<.01, *** p<.001) ---\n")
n_vars <- ncol(df_corr)
p_mat  <- matrix(NA, n_vars, n_vars,
                 dimnames = list(colnames(df_corr), colnames(df_corr)))
for (i in seq_len(n_vars)) {
  for (j in seq_len(n_vars)) {
    if (i != j) {
      p_mat[i, j] <- cor.test(df_corr[[i]], df_corr[[j]])$p.value
    }
  }
}

star <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01)  return("**")
  if (p < .05)  return("*")
  return("")
}

cor_str <- matrix("", n_vars, n_vars,
                  dimnames = list(colnames(df_corr), colnames(df_corr)))
for (i in seq_len(n_vars)) {
  for (j in seq_len(n_vars)) {
    if (i == j) {
      cor_str[i, j] <- "1.000"
    } else {
      cor_str[i, j] <- sprintf("%.3f%s", cor_mat[i, j], star(p_mat[i, j]))
    }
  }
}
print(noquote(cor_str))


# CFA
cfa_model <- '
  Role_Conflict            =~ q31_1 + q31_2 + q31_3
  Role_Ambiguity           =~ q31_4 + q31_5 + q31_6
  Job_Autonomy             =~ q4_1  + q4_2  + q4_3
  Innovative_Work_Behavior =~ q33_1 + q33_2 + q33_3 + q33_4
'

fit_cfa <- cfa(cfa_model, data = df, estimator = "ML")

print(fitMeasures(fit_cfa,
                  c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower",
                    "rmsea.ci.upper","srmr")))

print(standardizedSolution(fit_cfa) %>%
        filter(op == "=~") %>%
        select(lhs, rhs, est.std, se, z, pvalue))

rel <- reliability(fit_cfa)
print(rel)

# Convergent validity
cv <- data.frame(
  CR  = round(rel["omega",  ], 3),
  AVE = round(rel["avevar", ], 3)
)
print(cv)

# Discriminant validity
fcor <- lavInspect(fit_cfa, "cor.lv")
print(round(fcor, 3))

# Fornell-Larcker criterion:
ave_vals <- rel["avevar", ]
fl <- fcor
diag(fl) <- sqrt(ave_vals[rownames(fl)])
print(round(fl, 3))



# SEM
sem_model <- '
  Role_Conflict            =~ q31_1 + q31_2 + q31_3
  Role_Ambiguity           =~ q31_4 + q31_5 + q31_6
  Job_Autonomy             =~ q4_1  + q4_2  + q4_3
  Innovative_Work_Behavior =~ q33_1 + q33_2 + q33_3 + q33_4

  Role_Conflict  ~ a1*q1 + female + age + edu + grade + central
  Role_Ambiguity ~ a2*q1 + female + age + edu + grade + central
  Job_Autonomy   ~ a3*q1 + female + age + edu + grade + central

  Innovative_Work_Behavior ~ b1*Role_Conflict + b2*Role_Ambiguity + b3*Job_Autonomy + c*q1
  Innovative_Work_Behavior ~ female + age + edu + grade + central

  Role_Conflict  ~~ Role_Ambiguity
  Role_Conflict  ~~ Job_Autonomy
  Role_Ambiguity ~~ Job_Autonomy

  ind_RC    := a1*b1
  ind_RA    := a2*b2
  ind_JA    := a3*b3
  total_ind := ind_RC + ind_RA + ind_JA
  total     := c + total_ind

  # Pairwise contrasts of specific indirect effects (bootstrap)
  # signed difference (direction included)
  d_JA_RC := ind_JA - ind_RC
  d_JA_RA := ind_JA - ind_RA
  d_RC_RA := ind_RC - ind_RA

  # magnitude difference (|effect|) -- tests JA dominance claim directly
  m_JA_RC := abs(ind_JA) - abs(ind_RC)
  m_JA_RA := abs(ind_JA) - abs(ind_RA)
  m_RC_RA := abs(ind_RC) - abs(ind_RA)
'

set.seed(1227)

fit_sem <- sem(sem_model, data = df, estimator = "ML",
               se = "bootstrap", bootstrap = 5000)

print(fitMeasures(fit_sem,
                  c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower",
                    "rmsea.ci.upper","srmr")))

print(standardizedSolution(fit_sem, ci = TRUE, level = 0.95) %>%
        filter(op == "~") %>%
        select(lhs, rhs, est.std, ci.lower, ci.upper, pvalue))


print(parameterEstimates(fit_sem, boot.ci.type = "bca.simple", ci = TRUE) %>%
        filter(label %in% c("ind_RC","ind_RA","ind_JA","total_ind","total",
                            "d_JA_RC","d_JA_RA","d_RC_RA",
                            "m_JA_RC","m_JA_RA","m_RC_RA")) %>%
        select(label, est, ci.lower, ci.upper, pvalue))

sink()