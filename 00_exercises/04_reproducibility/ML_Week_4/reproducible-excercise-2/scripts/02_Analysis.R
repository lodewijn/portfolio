# Analysis using only the complete cases (no missing values)
# 1. Blood pressure as a function of Glycohemoglobin, age and sex 

lm1 <- lm(bp ~ HbA1C + age + as.factor(sex), data=dc)
#confint(lm(bp ~ HbA1C + age + as.factor(sex), data=dc))

# 2. Blood pressure as a function of Glycohemoglobin, age, sex and bmi
lm2 <- lm(bp ~ HbA1C + bmi + age + as.factor(sex), data=dc)
#confint(lm(bp ~ HbA1C + bmi + age + as.factor(sex), data=dc))

# ====================================================================
