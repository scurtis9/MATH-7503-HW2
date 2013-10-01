##############################
# Homework 6, Problem 1 (problem 7.2 in book)
##############################
## Generate 100 observations from an N(0,1) 
## distribution
## Compute a 95% confidence band for the CDF F
## Repeat 1000 times and see how often the
## confidence band contains the true distribution
## Repeat with data from a Cauchy
# Function generates a sample of a given size from
# a given data source, optionally plots the CI
# band, and returns whether or not the band
# encloses the truth
# Inputs: sample size, whether to display
# bands, generating distribution, confidence
# level
# Side-effect: displays true CDF (in blue),
# empirical CDF (black) and confidence
# band (dashed lines)
# Output: whether true CDF is within confidence
# band
q.7.2 <- function(n=100,display=TRUE,source="normal",confidence=0.95) {
  # Generate random sample
  x = switch(source,
             normal = rnorm(n),
             cauchy = rcauchy(n),
             stop("unknown distribution"))
  # Empirical CDF
  x.ecdf = ecdf(x)
  # Use confidence band from p. 99
  alpha = 1-confidence
  # Get the correction term
  epsilon = sqrt(log(2/alpha)/(2*n))
  # Define the functions for the limits
  # R note: need to distribute min/max across
  # multiple terms
  L = function(y) {
    raw = x.ecdf(y)-epsilon
    return(ifelse(raw < 0, 0, raw))
  }
  U = function(y) {
    raw = x.ecdf(y)+epsilon
    return(ifelse(raw > 1, 1, raw))
  }
  # Calculate lower and upper bounds at the
  # observations
  lb.CDF = L(x)
  ub.CDF = U(x)
  # Calculate the true CDF at the observations
  true.CDF = switch(source,
                    normal = pnorm(x),
                    cauchy = pcauchy(x))
  # Where did the bounds fail?
  lb.bad = (lb.CDF > true.CDF)
  ub.bad = (ub.CDF < true.CDF)
  num.errors = lb.bad + ub.bad
  # Did the bands cover the truth?
  if (sum(lb.bad) + sum(ub.bad)> 0) {
    covered = FALSE
  }
  else (covered = TRUE)
  # Should we display stuff?
  if (display) {
    plot(x.ecdf)
    curve(L(x),lty=2,add=TRUE)
    curve(U(x),lty=2,add=TRUE)
    if (source=="cauchy") {
      curve(pcauchy(x),col="blue",add=TRUE)
    } else {
      curve(pnorm(x),col="blue",add=TRUE)
    }
  }
  # Return whether the band covered the truth or
  # not
  return(covered)
}

# Estimate true coverage fraction
est.coverage.normal = sum(replicate(1000,q.7.2(display=FALSE)))/1000
### My run gives 0.965 on the first attempt

# And again for Cauchy
est.coverage.cauchy = sum(replicate(1000,q.7.2(display=FALSE,source="cauchy")))/1000
### My run gives 0.954 on the first attempt