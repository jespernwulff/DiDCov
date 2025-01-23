test_obj <- list(
  widest_interval = data.frame(lb = 0.0182, ub = 0.148, method = "constant", Delta = "DeltaSD", M = 0.02, parameter = 0.6, interval_width = 0.13),
  narrowest_interval = data.frame(lb = 0.0996, ub = 0.186, method = "decay_linear", Delta = "DeltaSD", M = 0.02, parameter = 0.0, interval_width = 0.0865),
  all_intervals = data.frame(
    lb = c(0.0182, 0.0996),
    ub = c(0.148, 0.186),
    method = c("constant", "decay_linear"),
    Delta = c("DeltaSD", "DeltaSD"),
    M = c(0.02, 0.02),
    parameter = c(0.6, 0.0),
    interval_width = c(0.13, 0.0865)
  ),
  average_width = 0.1183,
  sd_width = 0.0110
)
class(test_obj) <- "sensitivity_intervals"

# Test object with honest_data
test_obj_with_honest <- list(
  all_intervals = data.frame(
    lb = c(0.0182, 0.0996),
    ub = c(0.148, 0.186),
    method = c("constant", "decay_linear"),
    Delta = c("DeltaSD", "DeltaSD"),
    Mbar = c(0.02, 0.02),
    parameter = c(0.6, 0.0),
    interval_width = c(0.13, 0.0865)
  ),
  average_width = 0.1183,
  sd_width = 0.0110
)
class(test_obj_with_honest) <- "sensitivity_intervals"

# Honest data
honest_data <- data.frame(
  lb = 0.1,
  ub = 0.17,
  Mbar = 0.02
)

# Test object with multiple Mbar values
test_obj_multi_mbar <- list(
  all_intervals = data.frame(
    lb = c(0.0182, 0.0996, 0.0500, 0.1200),
    ub = c(0.148, 0.186, 0.139, 0.210),
    method = c("constant", "decay_linear", "linear", "quadratic"),
    Delta = c("DeltaSD", "DeltaSD", "DeltaSD", "DeltaSD"),
    Mbar = c(0.02, 0.02, 0.05, 0.05),
    parameter = c(0.6, 0.0, 0.3, 0.7),
    interval_width = c(0.13, 0.0865, 0.089, 0.11)
  ),
  average_width = 0.1008,
  sd_width = 0.0210
)
class(test_obj_multi_mbar) <- "sensitivity_intervals"

# Optional honest data for multi-Mbar scenario
honest_data_multi <- data.frame(
  lb = c(0.11, 0.15),
  ub = c(0.19, 0.22),
  Mbar = c(0.02, 0.05)
)

generate_multi_mbar_test_obj <- function() {
  Mbar_values <- seq(0.1, 2.1, by = 0.1)

  # Generate synthetic data with increasing complexity for each Mbar
  test_obj <- list(
    all_intervals = data.frame(
      lb = sapply(Mbar_values, function(m) m * runif(1, 0.01, 0.2)),
      ub = sapply(Mbar_values, function(m) m * runif(1, 0.2, 0.4)),
      method = rep(c("constant", "decay_linear", "linear", "quadratic"), length.out = length(Mbar_values)),
      Delta = rep("DeltaSD", length(Mbar_values)),
      Mbar = Mbar_values,
      parameter = runif(length(Mbar_values), 0, 1),
      interval_width = sapply(Mbar_values, function(m) m * runif(1, 0.05, 0.15))
    ),
    average_width = mean(sapply(Mbar_values, function(m) m * runif(1, 0.05, 0.15))),
    sd_width = sd(sapply(Mbar_values, function(m) m * runif(1, 0.05, 0.15)))
  )

  class(test_obj) <- "sensitivity_intervals"

  # Optional honest data matching Mbar values
  honest_data <- data.frame(
    lb = sapply(Mbar_values, function(m) m * runif(1, 0.1, 0.3)),
    ub = sapply(Mbar_values, function(m) m * runif(1, 0.3, 0.5)),
    Mbar = Mbar_values
  )

  return(list(test_obj = test_obj, honest_data = honest_data))
}

#result <- generate_multi_mbar_test_obj()
#test_obj_multi_mbar <- result$test_obj
#honest_data_multi <- result$honest_data

#plot(test_obj_multi_mbar,honest_data_multi)
