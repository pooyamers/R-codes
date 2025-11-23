# Assignment 1) Pooya Mers, Erin Breur


## Load and prepare data
# Read tritium level data from CSV file 
data <- read.csv("C:/Users/4dmer/Desktop/Study material/Year 2/Data Science Methods/Assignments/tritium.csv")
# Convert data column to vector format for analysis
x <- as.vector(t(data["tritium"]))
# Get sample size
n = length(x)

## Gaussian Kernel Density Estimation
# Calculate optimal bandwidth using Silverman's rule of thumb for Gaussian kernel
gauss_h <- sd(x)*(4/(3*n))^(1/5)
# Compute Gaussian kernel density estimate
d_gauss <- density(x, bw = gauss_h, kernel = "gaussian")
# Plot estimated function
plot(d_gauss$x, d_gauss$y, type = "l", xlab = "Tritium level", 
     ylab = "Kernel Density", main = "Gaussian Kernel Density Estimator")

## Confidence Intervals for Gaussian KDE
K22 <- 1/(2*sqrt(pi))  # Theoretical value
# Calculate 95% confidence intervals using normal approximation
ci_up <- d_gauss$y + qnorm(0.975)*sqrt(d_gauss$y*K22)/sqrt(n*gauss_h)
ci_lo <- d_gauss$y - qnorm(0.975)*sqrt(d_gauss$y*K22)/sqrt(n*gauss_h)
# Plot density with confidence intervals and data
plot(d_gauss$x, d_gauss$y, type="l", xlab="Tritium level", 
     ylab="Kernel Density", ylim = c(0, max(ci_up)),
     main = "Gaussian Kernel Density Estimator with Confidence Intervals")
points(x, rep(0,n), pch = 3)  # Add data points
points(d_gauss$x, ci_up, type="l", lty=2)  # Upper CI
points(d_gauss$x, ci_lo, type="l", lty=2)  # Lower CI

## Epanechnikov Kernel Density Estimation
# Define Epanechnikov kernel function x z^2
Epanechnikov <- function(z){
  (3/4)* (1-z^2) * z^2  #To integrate and standardize density
}
# Calculate scaling constant for bandwidth adjustment
a = integrate(f = Epanechnikov, lower = -1, upper = 1)$value
print(a)  # Verify integral value 0.6)
# Calculate "standardized" Epanechnikov bandwidth 
h_epan <- gauss_h * sqrt(a)
h_epan  # Display calculated bandwidth
# Compute Epanechnikov kernel density estimate
d_epan <- density(x, bw = h_epan, kernel= "epanechnikov")
# Plot Epanechnikov density estimate function
plot(d_epan, ylim = c(0, 0.6), main = "Epanechnikov Kernel Density Estimator")

## Confidence Intervals for Epanechnikov KDE
# Use theoretical K22 value for standard Epanechnikov kernel (0.6)
K22_epan <- 0.6  #As per the calculations in report
ci_up_epan <- d_epan$y + qnorm(0.975)*sqrt(d_epan$y*K22_epan)/sqrt(n*h_epan)
ci_lo_epan <- d_epan$y - qnorm(0.975)*sqrt(d_epan$y*K22_epan)/sqrt(n*h_epan)
# Plot with confidence intervals
plot(d_epan$x, d_epan$y, type="l", xlab="Tritium level", 
     ylab="Kernel Density", ylim = c(0, max(ci_up_epan)),
     main = "Epanechnikov Kernel Density Estimator with Confidence Intervals")
points(x, rep(0,n), pch = 3)
points(d_epan$x, ci_up_epan, type="l", lty=2)
points(d_epan$x, ci_lo_epan, type="l", lty=2)

## Histogram Comparison
# Calculate bin width using  rule of thumb
h1 = 3.5/n^(1/3)
# Create breaks centered around mean for better visualization
m <- mean(x)
nl <- ceiling((m-min(x))/h1)
nu <- ceiling((max(x)-m)/h1)
breaks <- seq(m-nl*h1, m+nu*h1, length.out = (nl+nu+1))
# Plot histogram with both density estimators
hist(x, breaks = breaks, probability = TRUE, 
     main= "Histogram of data and density plots", xlab = "Tritium level")
lines(d_epan, col = 2, lwd = 2)  # Epanechnikov in red
lines(d_gauss, col = 4, lwd = 2)  # Gaussian in blue
legend(x = 4, y = 0.8, legend = c("Gaussian Density", "Epanechnikov"),
       col = c(4, 2), lty = 1, lwd = 2)
