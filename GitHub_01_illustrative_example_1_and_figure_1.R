# ================================================================
# Illustrative Example 1:
# Deriving and testing predictions from a nomological network
# ================================================================

# This script illustrates a confirmatory, theory-driven approach to
# construct validation using formal modeling.
#
# The workflow follows the logic described in the manuscript:
#
# 1. Formalize the nomological network as a structural equation model,
#    where a range of plausible parameters is considered.
# 2. Derive predictions from this range of plausible nomological networks 
#    using simulation.
# 3. Use empirical data to test the predictions.
#
# In this example, we use an equivalence test and a minimal-effects test to 
# evaluate whether the correlation between t1 and t3 lies inside or outside 
# the interval predicted by the nomological network.


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

library(simstandard)  # Specify models, derive model-implied correlations, and simulate data.
library(TOSTER)       # Conduct equivalence tests.

# ================================================================
# STEP 1. Formalize the nomological network
# ================================================================

# In this first step, the nomological network is formalized as a 
# structural equation model.
#
# The target construct is construct_t. The other constructs in the
# nomological network are construct_x and construct_z.
#
# In the proposed construct interpretation, t1, t2, and t3 are interpreted
# as indicators of construct_t and t1 is the test score for which construct
# validity is tested.
#
# The nomological network is expressed as a set of standardized factor
# loadings and structural paths. Rather than treating these parameters as
# known point values, we specify lower and upper bounds to represent theoretical
# uncertainty. These bounds represent the range of population parameters that 
# researchers regard as plausible under the construct validity hypothesis.


# ------------------------------------------------------------
# STEP 1.1. Specify lower bounds for plausible parameters
# ------------------------------------------------------------

plausible_lower <- c(  # Store the lower bounds for all plausible parameters in one named vector.
  
  # Factor loadings linking construct_t to its indicators:
  # construct_t =~ t1 + t2 + t3
  t_t1 = .75,          # Lower bound for the loading of t1 on construct_t.
  t_t2 = .60,          # Lower bound for the loading of t2 on construct_t.
  t_t3 = .55,          # Lower bound for the loading of t3 on construct_t.
  
  # Factor loadings linking construct_x to its indicators:
  # construct_x =~ x1 + x2 + x3
  x_x1 = .40,          # Lower bound for the loading of x1 on construct_x.
  x_x2 = .45,          # Lower bound for the loading of x2 on construct_x.
  x_x3 = .50,          # Lower bound for the loading of x3 on construct_x.
  
  # Factor loadings linking construct_z to its indicators:
  # construct_z =~ z1 + z2 + z3
  z_z1 = .35,          # Lower bound for the loading of z1 on construct_z.
  z_z2 = .50,          # Lower bound for the loading of z2 on construct_z.
  z_z3 = .40,          # Lower bound for the loading of z3 on construct_z.
  
  # Structural path from construct_z to construct_t:
  # construct_t ~ construct_z
  z_t = .20,           # Lower bound for the path from construct_z to construct_t.
  
  # Structural paths from construct_z and construct_t to construct_x:
  # construct_x ~ construct_z + construct_t
  z_x = .10,           # Lower bound for the path from construct_z to construct_x.
  t_x = .00            # Lower bound for the path from construct_t to construct_x.
)


# ------------------------------------------------------------
# STEP 1.2. Specify upper bounds for plausible parameters
# ------------------------------------------------------------

plausible_upper <- c(  # Store the upper bounds for all plausible parameters in one named vector.
  
  # Factor loadings linking construct_t to its indicators:
  # construct_t =~ t1 + t2 + t3
  t_t1 = .95,          # Upper bound for the loading of t1 on construct_t.
  t_t2 = .80,          # Upper bound for the loading of t2 on construct_t.
  t_t3 = .75,          # Upper bound for the loading of t3 on construct_t.
  
  # Factor loadings linking construct_x to its indicators:
  # construct_x =~ x1 + x2 + x3
  x_x1 = .60,          # Upper bound for the loading of x1 on construct_x.
  x_x2 = .65,          # Upper bound for the loading of x2 on construct_x.
  x_x3 = .70,          # Upper bound for the loading of x3 on construct_x.
  
  # Factor loadings linking construct_z to its indicators:
  # construct_z =~ z1 + z2 + z3
  z_z1 = .55,          # Upper bound for the loading of z1 on construct_z.
  z_z2 = .70,          # Upper bound for the loading of z2 on construct_z.
  z_z3 = .60,          # Upper bound for the loading of z3 on construct_z.
  
  # Structural path from construct_z to construct_t:
  # construct_t ~ construct_z
  z_t = .40,           # Upper bound for the path from construct_z to construct_t.
  
  # Structural paths from construct_z and construct_t to construct_x:
  # construct_x ~ construct_z + construct_t
  z_x = .30,           # Upper bound for the path from construct_z to construct_x.
  t_x = .20            # Upper bound for the path from construct_t to construct_x.
)


# ================================================================
# STEP 2. Derive predicted observations
# ================================================================

# In this second step, predicted observations are derived from the range
# of plausible nomological networks via simulation.
#
# The code first draws a random combination of plausible parameter sets 
# from the bounds specified in STEP 1. It then translates each parameter set 
# into a plausible nomological network and derives the observed-variable 
# correlations implied by each network.


# ------------------------------------------------------------
# STEP 2.1. Draw plausible parameter sets
# ------------------------------------------------------------

set.seed(2026)         # Make the simulation reproducible.

n_draws <- 10000        # Define how many combinations are drawn.
                        # Increase the number for greater precision.

plausible_parameter_sets <- as.data.frame(  # Create a data frame to store all sampled parameter sets.
  matrix(                 # Start by creating an empty matrix.
    NA,                   # Fill the matrix with missing values before replacing them.
    nrow = n_draws,       # Use one row for each plausible nomological network.
    ncol = length(plausible_lower)  # Use one column for each parameter in the network.
  )
)

names(plausible_parameter_sets) <- names(plausible_lower)  # Give each column its parameter name.

for (parameter_name in names(plausible_lower)) {  # Loop over all parameters in the nomological network.
  
  plausible_parameter_sets[[parameter_name]] <- runif(  # Draw plausible values for the current parameter from a uniform distribution.
    n = n_draws,                              # Draw one value for each plausible nomological network.
    min = plausible_lower[parameter_name],    # Use the lower bound of the current parameter.
    max = plausible_upper[parameter_name]     # Use the upper bound of the current parameter.
  )
}

# Inspect the first plausible parameter set.
plausible_parameter_sets[1,]


# ------------------------------------------------------------
# STEP 2.2. Translate parameter sets into plausible nomological networks
# ------------------------------------------------------------

plausible_nomological_networks <- character(n_draws)  # Create an empty character vector for model syntax strings.

for (draw_id in seq_len(n_draws)) {  # Loop over all sampled parameter sets.
  
  plausible_parameters <- as.numeric(plausible_parameter_sets[draw_id, ])  # Extract one parameter set.
  
  names(plausible_parameters) <- names(plausible_parameter_sets)  # Restore the parameter names.
  
  plausible_nomological_networks[draw_id] <- sprintf(  # Insert the sampled values into SEM syntax.
    '
    construct_t =~ %.3f*t1 + %.3f*t2 + %.3f*t3
    construct_x =~ %.3f*x1 + %.3f*x2 + %.3f*x3
    construct_z =~ %.3f*z1 + %.3f*z2 + %.3f*z3

    construct_t ~ %.3f*construct_z
    construct_x ~ %.3f*construct_z + %.3f*construct_t
    ',
    plausible_parameters["t_t1"],  # Insert the loading of t1 on construct_t.
    plausible_parameters["t_t2"],  # Insert the loading of t2 on construct_t.
    plausible_parameters["t_t3"],  # Insert the loading of t3 on construct_t.
    plausible_parameters["x_x1"],  # Insert the loading of x1 on construct_x.
    plausible_parameters["x_x2"],  # Insert the loading of x2 on construct_x.
    plausible_parameters["x_x3"],  # Insert the loading of x3 on construct_x.
    plausible_parameters["z_z1"],  # Insert the loading of z1 on construct_z.
    plausible_parameters["z_z2"],  # Insert the loading of z2 on construct_z.
    plausible_parameters["z_z3"],  # Insert the loading of z3 on construct_z.
    plausible_parameters["z_t"],   # Insert the path from construct_z to construct_t.
    plausible_parameters["z_x"],   # Insert the path from construct_z to construct_x.
    plausible_parameters["t_x"]    # Insert the path from construct_t to construct_x.
  )
}

# Inspect the first plausible nomological network in lavaan syntax.
plausible_nomological_networks[1]


# ------------------------------------------------------------
# STEP 2.3. Derive predicted correlations
# ------------------------------------------------------------

correlations_by_network <- vector("list", n_draws)  # Create a list to store predicted correlations from each network.

for (draw_id in seq_len(n_draws)) {  # Loop over all plausible nomological networks.
  
  implied_correlations <- get_model_implied_correlations(  # Derive the model-implied correlation matrix.
    plausible_nomological_networks[draw_id],  # Use the current plausible nomological network.
    observed = TRUE                           # Return correlations among the observed variables.
  )
  
  model_implied_correlations <- implied_correlations[  # Extract only the unique correlations.
    lower.tri(implied_correlations)                    # Use the lower triangle and omit the diagonal.
  ]
  
  names(model_implied_correlations) <- apply(  # Assign readable names to the extracted correlations.
    combn(colnames(implied_correlations), 2),  # Generate all pairs of observed-variable names.
    2,                                         # Apply the naming function to each pair of names.
    paste,                                     # Combine the two variable names in each pair.
    collapse = " ~~ "                          # Use lavaan-style covariance notation for the names.
  )
  
  correlations_by_network[[draw_id]] <- model_implied_correlations  # Store correlations for this network.
}

# Inspect the predictions from the first plausible nomological network.
correlations_by_network[1]


# ------------------------------------------------------------
# STEP 2.4. Summarize predicted observations as intervals
# ------------------------------------------------------------

predicted_observations <- do.call(  # Combine all prediction vectors into one matrix.
  rbind,                            # Stack the vectors row-wise.
  correlations_by_network           # Use the list of correlations from all plausible networks.
)

rownames(predicted_observations) <- paste0(  # Give each row a readable name.
  "network_",                               # Prefix each row name with "network_".
  seq_len(n_draws)                          # Add the network number.
)

interval_predictions <- data.frame(  # Store the lower and upper bounds of each predicted observation.
  lower = apply(                     # Compute the lower bound for each predicted observation.
    predicted_observations,          # Use the matrix of model-implied correlations.
    2,                               # Apply the function column-wise.
    min                              # Use the minimum value across plausible networks.
  ),
  upper = apply(                     # Compute the upper bound for each predicted observation.
    predicted_observations,          # Use the matrix of model-implied correlations.
    2,                               # Apply the function column-wise.
    max                              # Use the maximum value across plausible networks.
  ),
  row.names = colnames(predicted_observations)  # Use correlation names as row names.
)

# Inspect the interval predictions based on all plausible nomological networks.
interval_predictions


# The nomological network predicts that the correlation coefficient between
# t1 and t3 should be in the interval from .413050000 to .7117500.


# ================================================================
# STEP 3. Draw empirical observations and test the prediction
# ================================================================

# In this third step, a sample of empirical observations is drawn and used
# to test the predicted observation.
#
# In real applications, these empirical observations would come from an
# actual empirical data set. In this demonstration, we use a simulated dataset
# that is sampled from a true data-generating model. The true data-generating
# model can be specified to be consistent with the plausible nomological
# network or to deviate from it.

# We assume that the nomological network is correctly specified and the test 
# score valid so that the parameters of the data-generating model lie inside 
# the range of plausible values from the nomological network.


# ------------------------------------------------------------
# STEP 3.1. Specify the true data-generating model
# ------------------------------------------------------------

true_parameters <- c(  # Store the true parameters used to generate the validation sample.
  
  # Factor loadings linking construct_t to its indicators:
  # construct_t =~ t1 + t2 + t3
  t_t1 = .85,          # True loading of t1 on construct_t.
  t_t2 = .70,          # True loading of t2 on construct_t.
  t_t3 = .65,          # True loading of t3 on construct_t.
  
  # Factor loadings linking construct_x to its indicators:
  # construct_x =~ x1 + x2 + x3
  x_x1 = .50,          # True loading of x1 on construct_x.
  x_x2 = .55,          # True loading of x2 on construct_x.
  x_x3 = .60,          # True loading of x3 on construct_x.
  
  # Factor loadings linking construct_z to its indicators:
  # construct_z =~ z1 + z2 + z3
  z_z1 = .45,          # True loading of z1 on construct_z.
  z_z2 = .60,          # True loading of z2 on construct_z.
  z_z3 = .50,          # True loading of z3 on construct_z.
  
  # Structural path from construct_z to construct_t:
  # construct_t ~ construct_z
  z_t = .30,           # True path from construct_z to construct_t.
  
  # Structural paths from construct_z and construct_t to construct_x:
  # construct_x ~ construct_z + construct_t
  z_x = .20,           # True path from construct_z to construct_x.
  t_x = .10            # True path from construct_t to construct_x.
)


true_data_generating_model <- sprintf(  # Insert the true parameter values into SEM syntax.
  '
  construct_t =~ %.3f*t1 + %.3f*t2 + %.3f*t3
  construct_x =~ %.3f*x1 + %.3f*x2 + %.3f*x3
  construct_z =~ %.3f*z1 + %.3f*z2 + %.3f*z3

  construct_t ~ %.3f*construct_z
  construct_x ~ %.3f*construct_z + %.3f*construct_t
  ',
  true_parameters["t_t1"],  # Insert the true loading of t1 on construct_t.
  true_parameters["t_t2"],  # Insert the true loading of t2 on construct_t.
  true_parameters["t_t3"],  # Insert the true loading of t3 on construct_t.
  true_parameters["x_x1"],  # Insert the true loading of x1 on construct_x.
  true_parameters["x_x2"],  # Insert the true loading of x2 on construct_x.
  true_parameters["x_x3"],  # Insert the true loading of x3 on construct_x.
  true_parameters["z_z1"],  # Insert the true loading of z1 on construct_z.
  true_parameters["z_z2"],  # Insert the true loading of z2 on construct_z.
  true_parameters["z_z3"],  # Insert the true loading of z3 on construct_z.
  true_parameters["z_t"],   # Insert the true path from construct_z to construct_t.
  true_parameters["z_x"],   # Insert the true path from construct_z to construct_x.
  true_parameters["t_x"]    # Insert the true path from construct_t to construct_x.
)

true_population_correlations <- get_model_implied_correlations(  # Derive the true population correlations for reference.
  true_data_generating_model,  # Use the true data-generating model.
  observed = TRUE              # Return the correlations among observed variables.
)

# The true population correlation between t1 and t3 is .5525,
# which the manuscript rounds to .55.
true_population_correlations["t1", "t3"]

# ------------------------------------------------------------
# STEP 3.2. Draw a validation sample
# ------------------------------------------------------------

set.seed(2026)  # Make the simulated validation sample reproducible.

n_validation_sample <- 500  # Define the size of the validation sample.

validation_sample <- sim_standardized(  # Simulate data from the true data-generating model.
  m = true_data_generating_model,       # Use the true data-generating model in lavaan syntax.
  n = n_validation_sample,              # Draw N observations.
  latent = FALSE,                       # Return only observed variables, not latent scores.
  errors = FALSE                        # Do not return the error terms.
)


# ------------------------------------------------------------
# STEP 3.3. Equivalence test
# ------------------------------------------------------------

# The observed correlation between t1 and t3 is 0.5402382.
cor(validation_sample$t1, validation_sample$t3)

# Interval predictions from the nomological network were:
#              lower      upper
# t1 ~~ t3   0.413050000   0.7117500

# The predicted interval is entered manually in the hypothesis test below.

# H0: rho <= lower or rho >= upper
# The population correlation lies outside the interval predicted by the
# nomological network.

# H1: lower < rho < upper
# The population correlation lies inside the interval predicted by the
# nomological network.

# A significant result indicates that the observed result is consistent
# with the proposed construct interpretation.

equivalence_test <- TOSTER::z_cor_test(
  x = validation_sample$t1,        # Use t1 as the first observed variable.
  y = validation_sample$t3,        # Use t3 as the second observed variable.
  method = "pearson",              # Estimate a Pearson correlation.
  alternative = "equivalence",     # Test whether the correlation is inside the predicted interval.
  null = c(0.413050000, 0.7117500),    # Manually enter the predicted lower and upper bounds.
  alpha = .05                      # Use alpha = .05 for the equivalence test.
)

# Inspect the result.
equivalence_test

# ------------------------------------------------------------
# STEP 3.4. Minimal-effects test and three-way decision rule
# ------------------------------------------------------------

# A non-significant equivalence test does not show that the population
# correlation lies outside the predicted interval. It only shows that
# containment could not be demonstrated with the available data. A negative
# result therefore requires its own test, in which non-containment is the
# alternative hypothesis.
#
# The minimal-effects test reverses the hypotheses of the equivalence test.
#
# H0: lower <= rho <= upper
# The population correlation lies inside the interval predicted by the
# nomological network.
#
# H1: rho < lower or rho > upper
# The population correlation lies outside the interval predicted by the
# nomological network.
#
# A significant result indicates that the population correlation lies outside
# the predicted interval, which falsifies the prediction. The two one-sided
# p-values in the output indicate which bound was crossed.

minimal_effects_test <- TOSTER::z_cor_test(  # Store the result for the decision rule.
  x = validation_sample$t1,                  # Use t1 as the first observed variable.
  y = validation_sample$t3,                  # Use t3 as the second observed variable.
  method = "pearson",                        # Estimate a Pearson correlation.
  alternative = "minimal.effect",            # Test whether the correlation is outside the predicted interval.
  null = c(0.413050000, 0.7117500),    # Manually enter the predicted lower and upper bounds.
  alpha = .05                                # Use the same alpha as the equivalence test.
)

# Inspect the result.
minimal_effects_test

# Combine both tests into a three-way decision. 

if (equivalence_test$p.value < .05) {              # Check whether containment was demonstrated.
  decision <- "Consistent: the prediction is corroborated."
} else if (minimal_effects_test$p.value < .05) {   # Check whether non-containment was demonstrated.
  decision <- "Inconsistent: the prediction is falsified."
} else {                                           # Neither test was significant.
  decision <- "Inconclusive: the data cannot distinguish containment from non-containment."
}

# Inspect the decision.
decision



