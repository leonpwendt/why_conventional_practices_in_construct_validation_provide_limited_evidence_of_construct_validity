# ================================================================
# Load packages
# ================================================================

# Reproducibility -------------------------------------------------
# Exact package versions are documented in renv.lock.
# To restore the environment, place renv.lock in the project
# directory and run:
#   install.packages("renv")
#   library(renv)
#   renv::restore()

# packages
library(lavaan)
library(semTools)

# Set seed
set.seed(2026)         

# specify data-generating model (measure with high construct validity)
gen.1 <- 'target =~ .895*i1 + .895*i2 + .895*i3 + .895*i4 + .895*i5'

# simulate fake data (without sampling error, so that the sample matches the model-implied population moments)
dat.1 <- simulateData(model = gen.1, sample.nobs = 1000, empirical = T)

# model specification (unidimensional factor model)
mod.1 <- 'eta1 =~ i1 + i2 + i3 + i4 + i5'

# estimate model
fit.1 <- cfa(mod.1, dat.1, std.lv = T, estimator = "MLR")

# model summary
summary(fit.1, standardized = T, fit.measures = T) # p = 1.000, CFI = 1.000, RMSEA = 0.000, SRMR = 0.000

# internal consistency
compRelSEM(fit.1, tau.eq = F) # reliability = .802

# specify data-generating model (measure with low construct validity)
gen.2 <- 'target =~ .447*i1 + .447*i2 + .447*i3 + .447*i4 + .447*i5
          error  =~ .774*i1 + .774*i2 + .774*i3 + .774*i4 + .774*i5'

# simulate data
dat.2 <- simulateData(model = gen.2, sample.nobs = 1000, empirical = T)

# model specification (unidimensional factor model)
mod.2 <- 'eta1 =~ i1 + i2 + i3 + i4 + i5'

# estimate model
fit.2 <- cfa(mod.2, dat.2, std.lv = T, estimator = "MLR")

# model summary
summary(fit.2, standardized = T, fit.measures = T) # p = 1.000, CFI = 1.000, RMSEA = 0.000, SRMR = 0.000

# internal consistency
compRelSEM(fit.2, tau.eq = F) # .800

