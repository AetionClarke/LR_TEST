
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
sccs <-fread("MACE/model_3/full_cohort/full_cohort.csv")
sccs1 <- sccs[ENTRY_COHORT_IsExcluded==FALSE, ]

# Add nested base model covariate 
sccs1[["EXPOSED"]] = sccs1$SELF_CONTROLLED_PERIOD_RISK_1 | sccs1$SELF_CONTROLLED_PERIOD_RISK_2 | sccs1$SELF_CONTROLLED_PERIOD_RISK_3


# fit the full conditional poisson model (3 exposure windows)
m_full = gnm(OUTCOME_CASES_0 ~ SELF_CONTROLLED_PERIOD_RISK_1 + 
               SELF_CONTROLLED_PERIOD_RISK_2 + SELF_CONTROLLED_PERIOD_RISK_3 +
               ENTRY_COVARIATE_0 + ENTRY_COVARIATE_1 + ENTRY_COVARIATE_2,
             eliminate=factor(PID), data=sccs1, family=poisson, offset=I(log(INTERVAL_LENGTH)))

# fit the reduced conditional poisson model (1 exposure window)
m_base = gnm(OUTCOME_CASES_0 ~ EXPOSED + 
                  ENTRY_COVARIATE_0 + ENTRY_COVARIATE_1 + ENTRY_COVARIATE_2,
                eliminate=factor(PID), data=sccs1, family=poisson, offset=I(log(INTERVAL_LENGTH)))

lrtest(m_base, m_full)





