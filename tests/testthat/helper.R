# Create a simple `sensitivity_intervals` object

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
