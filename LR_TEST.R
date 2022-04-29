
# Uncomment to install packages
#install.packages("data.table")
#install.packages("bit64")
#install.packages("gnm")
#install.packages('lmtest')

library('data.table')
library('bit64')
library('gnm')
library('lmtest')

# Load data and excluded non_cohort patients
sccs_full <-fread("full_model/full_cohort/full_cohort.csv")
sccs_full1 <- sccs_full[ENTRY_COHORT_IsExcluded==FALSE, ]

sccs_reduced <-fread("reduced_model/full_cohort/full_cohort.csv")
sccs_reduced1 <- sccs_reduced[ENTRY_COHORT_IsExcluded==FALSE, ]


# fit the full conditional poisson model (3 exposure windows)
m_full = gnm(OUTCOME_CASES_0 ~ SELF_CONTROLLED_PERIOD_RISK_1 + 
         SELF_CONTROLLED_PERIOD_RISK_2 + SELF_CONTROLLED_PERIOD_RISK_3 +
         ENTRY_COVARIATE_0 + ENTRY_COVARIATE_1 + ENTRY_COVARIATE_2,
         eliminate=factor(PID), data=sccs_full1, family=poisson, offset=I(log(INTERVAL_LENGTH)))

# fit the reduced conditional poisson model (1 exposure window)
m_reduced = gnm(OUTCOME_CASES_0 ~ SELF_CONTROLLED_PERIOD_RISK_1 + 
               ENTRY_COVARIATE_0 + ENTRY_COVARIATE_1 + ENTRY_COVARIATE_2,
             eliminate=factor(PID), data=sccs_full1, family=poisson, offset=I(log(INTERVAL_LENGTH)))

# Likelihood ratio test lmtest implementation
lrtest(m_reduced, m_full)

# Likelihood ratio test anova implementation. 
#This should yeild the same p-value as above
anova(m_reduced, m_full, test = "LRT")





