> Q. I have done my REMLF90 analysis with real data. I have log convergence logs over iterations with both variances and residuals. Now is there any way I can produce a 2d or 3d surface plot. I am really curious to see if there is any only one global maximum or multiple local maxima?

> Excellent question! This is a very insightful approach to understanding your REML convergence. Yes, you can absolutely reconstruct the likelihood surface from your REMLF90 convergence logs! Here's how:


# 1. First, Let's Understand Your Data Structure
What your REMLF90 log typically contains:

```text
Iteration  Variance1  Variance2  Residual  LogLikelihood
1          2.5        1.8        3.2       -1250.5
2          2.1        1.9        3.0       -1245.2
3          1.8        2.0        2.8       -1240.1
...
```

# 2. Method 1: 2D Surface Reconstruction from Convergence Path
```r
library(ggplot2)
library(plotly)
library(akima)  # For interpolation

# Read your REMLF90 convergence log
# Assuming you have a CSV with columns: iteration, var1, var2, residual, loglik
reml_data <- read.csv("your_remlf90_convergence_log.csv")

# Create a 2D likelihood surface around your convergence path
create_2d_surface <- function(reml_data, grid_size = 50) {
  # Use your actual variance parameter names
  var1_range <- range(reml_data$var1) * c(0.5, 1.5)  # Expand range by 50%
  var2_range <- range(reml_data$var2) * c(0.5, 1.5)
  
  # Create grid
  var1_grid <- seq(var1_range[1], var1_range[2], length.out = grid_size)
  var2_grid <- seq(var2_range[1], var2_range[2], length.out = grid_size)
  
  # Interpolate likelihood surface from your convergence points
  # Note: This is an approximation since we don't have the full surface
  interpolated_surface <- with(reml_data, {
    interp(x = var1, y = var2, z = loglik, 
           xo = var1_grid, yo = var2_grid,
           linear = FALSE, extrap = TRUE)
  })
  
  # Convert to data frame for plotting
  surface_df <- expand.grid(var1 = var1_grid, var2 = var2_grid)
  surface_df$loglik <- as.vector(interpolated_surface$z)
  
  return(surface_df)
}

# Create the surface
surface_2d <- create_2d_surface(reml_data)

# Plot 2D contour
p1 <- ggplot(surface_2d, aes(x = var1, y = var2, z = loglik)) +
  geom_contour_filled(bins = 20, alpha = 0.7) +
  geom_contour(color = "white", bins = 15, size = 0.3) +
  geom_path(data = reml_data, aes(x = var1, y = var2, color = iteration), 
            size = 1.5, alpha = 0.8) +
  geom_point(data = reml_data, aes(x = var1, y = var2, fill = iteration), 
             size = 3, shape = 21) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(title = "REML Likelihood Surface (Reconstructed from Convergence Path)",
       subtitle = "Lines show your actual convergence path through parameter space",
       x = "Variance Component 1", y = "Variance Component 2",
       fill = "Iteration", color = "Iteration") +
  theme_minimal()

print(p1)
```
# 3. Method 2: Direct 3D Plot from Your Data

```r
## Create interactive 3D plot of your actual convergence path
fig_3d_path <- plot_ly() %>%
  # Add your convergence path as a 3D line
  add_trace(
    data = reml_data,
    x = ~var1,
    y = ~var2, 
    z = ~loglik,
    type = 'scatter3d',
    mode = 'lines+markers',
    line = list(width = 6, color = ~iteration, colorscale = 'Viridis'),
    marker = list(size = 4, color = ~iteration, colorscale = 'Viridis'),
    name = "Convergence Path"
  ) %>%
  # Add starting point
  add_trace(
    data = reml_data[1, ],
    x = ~var1,
    y = ~var2,
    z = ~loglik,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 8, color = 'red', symbol = 'circle'),
    name = "Start"
  ) %>%
  # Add final point
  add_trace(
    data = reml_data[nrow(reml_data), ],
    x = ~var1,
    y = ~var2,
    z = ~loglik,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 8, color = 'green', symbol = 'diamond'),
    name = "Final"
  ) %>%
  layout(
    title = "3D View: Your Actual REML Convergence Path",
    scene = list(
      xaxis = list(title = "Variance Component 1"),
      yaxis = list(title = "Variance Component 2"),
      zaxis = list(title = "Log-Likelihood"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
    )
  )

fig_3d_path
```

# 4. Method 3: Enhanced Analysis - Check for Multiple Maxima

```r
# Analyze your convergence pattern for signs of multiple maxima
analyze_convergence_pattern <- function(reml_data) {
  # Calculate improvements in log-likelihood
  reml_data$lik_improvement <- c(NA, diff(reml_data$loglik))
  reml_data$abs_improvement <- abs(reml_data$lik_improvement)
  
  # Detect potential local maxima by looking for:
  # 1. Sudden drops in likelihood followed by recovery
  # 2. Plateaus in convergence
  # 3. Oscillations in parameter estimates
  
  # Plot convergence diagnostics
  p1 <- ggplot(reml_data, aes(x = iteration, y = loglik)) +
    geom_line(size = 1, color = "blue") +
    geom_point(size = 2, color = "red") +
    labs(title = "Log-Likelihood Convergence",
         x = "Iteration", y = "Log-Likelihood") +
    theme_minimal()
  
  p2 <- ggplot(reml_data, aes(x = iteration, y = var1)) +
    geom_line(size = 1, color = "darkgreen") +
    geom_point(size = 2, color = "orange") +
    labs(title = "Variance Component 1 Convergence",
         x = "Iteration", y = "Variance 1") +
    theme_minimal()
  
  p3 <- ggplot(reml_data, aes(x = iteration, y = var2)) +
    geom_line(size = 1, color = "purple") +
    geom_point(size = 2, color = "brown") +
    labs(title = "Variance Component 2 Convergence",
         x = "Iteration", y = "Variance 2") +
    theme_minimal()
  
  library(gridExtra)
  grid.arrange(p1, p2, p3, ncol = 3)
  
  # Return analysis summary
  summary <- list(
    final_loglik = tail(reml_data$loglik, 1),
    total_iterations = nrow(reml_data),
    max_improvement = max(reml_data$abs_improvement, na.rm = TRUE),
    final_improvement = tail(reml_data$abs_improvement, 1),
    convergence_quality = ifelse(tail(reml_data$abs_improvement, 1) < 0.001, 
                                "Good", "Possible issues")
  )
  
  return(summary)
}

# Run the analysis
convergence_analysis <- analyze_convergence_pattern(reml_data)
print(convergence_analysis)
```

# 5. Method 4: Practical Test for Multiple Maxima

```r
# Since you can't see the full surface from one convergence path,
# here's how to test if you might have multiple maxima:

test_multiple_maxima <- function(reml_data) {
  # Look for signs in your convergence pattern:
  
  signs <- list()
  
  # Sign 1: Non-monotonic likelihood improvement
  lik_changes <- diff(reml_data$loglik)
  signs$non_monotonic <- any(lik_changes < 0)
  
  # Sign 2: Parameter oscillations
  var1_changes <- diff(reml_data$var1)
  var2_changes <- diff(reml_data$var2)
  signs$parameter_oscillations <- any(diff(sign(var1_changes)) != 0) | 
                                any(diff(sign(var2_changes)) != 0)
  
  # Sign 3: Slow final convergence
  final_improvements <- tail(lik_changes, 5)
  signs$slow_convergence <- all(abs(final_improvements) < 0.01) & 
                           length(final_improvements) >= 3
  
  # Create warning message
  warning_msg <- ""
  if(signs$non_monotonic) {
    warning_msg <- paste0(warning_msg, "⚠️ Non-monotonic likelihood changes detected\n")
  }
  if(signs$parameter_oscillations) {
    warning_msg <- paste0(warning_msg, "⚠️ Parameter oscillations detected\n")
  }
  if(signs$slow_convergence) {
    warning_msg <- paste0(warning_msg, "⚠️ Very slow final convergence\n")
  }
  
  if(warning_msg == "") {
    warning_msg <- "✅ No strong signs of multiple local maxima"
  }
  
  cat("Multiple Maxima Analysis:\n")
  cat(warning_msg)
  
  return(signs)
}

# Run the test
maxima_test <- test_multiple_maxima(reml_data)
```

# 6. What Your Results Might Look Like:

Single Global Maximum (Good case):

Smooth, monotonic convergence

Parameters approach stable values

Log-likelihood increases steadily

Possible Multiple Maxima (Warning signs):

Likelihood decreases then increases

Parameters oscillate significantly

Very slow final convergence

Different starting values give different results

# 7. To Get More Definitive Evidence:
If you're really concerned, you could:

```r
# Rerun REMLF90 with different starting values
# Compare final log-likelihoods and parameter estimates

compare_starting_values <- function() {
  # You would need to run REMLF90 multiple times with different starting values
  # Then compare the results:
  
  results_comparison <- data.frame(
    start_var1 = c(0.1, 1.0, 5.0, 10.0),  # Different starting values
    start_var2 = c(0.1, 1.0, 5.0, 10.0),
    final_var1 = c(2.1, 2.1, 2.1, 2.1),   # Your actual final estimates
    final_var2 = c(1.5, 1.5, 1.5, 1.5),   # from different runs
    final_loglik = c(-1240.1, -1240.1, -1240.1, -1240.1)  # Should be identical
  )
  
  # If all final_loglik are the same → Single maximum
  # If different → Multiple maxima
  
  return(results_comparison)
}
```

```text
Next Steps for You:
Share your actual convergence data structure so I can tailor the code

Run the 2D surface reconstruction with your data

Check the warning signs for multiple maxima

If concerned, rerun with different starting values to test robustness
```
