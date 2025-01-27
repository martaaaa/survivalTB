#' Process Single Run for Interval Data
#'
#' This function processes a single run of interval survival data using `icfit`.
#'
#' @param left Numeric vector of left interval boundaries.
#' @param right Numeric vector of right interval boundaries (can include `NA` for right-censored data).
#' @return A data frame with columns: `left`, `right`, `weight`, and `survival`.
#' @keywords internal

.process_single_run <- function(left, right) {
  if (length(left) != length(right)){
    stop("'left' and 'right' must have the same length")
  }
  res <- icfit(survival::Surv(left, right, type = "interval2") ~ 1)
  sink(tempfile())  # Redirect output to a temporary file
  summary_res <- summary(res)
  sink()

  ob <- data.frame(
    left = as.numeric(gsub("\\(([^,]+),.*", "\\1", summary_res$Interval)),
    right = suppressWarnings(as.numeric(gsub(".*,(.*)\\]", "\\1", summary_res$Interval))),
    weight = as.numeric(summary_res$Probability),
    survival = 1 - cumsum(as.numeric(summary_res$Probability))
  )
  ob[is.na(ob)]<-Inf
  return(ob)
}

#' Calculate Interval Survival Estimates
#'
#' This function estimates survival probabilities from interval-censored data and supports bootstrap replication.
#'
#' @param left Numeric vector of left interval boundaries.
#' @param right Numeric vector of right interval boundaries (can include `NA` for right-censored data).
#' @param nboot Integer, number of bootstrap replications. Default is 1 (no bootstrap).
#' @return A list containing:
#'   \item{original}{Data frame with survival estimates for the original data.}
#'   \item{bootstrap}{List of survival estimates for bootstrap replications (if `nboot > 1`).}
#' @examples
#' # Example data
#' left <- c(2, 4, 6, 8)
#' right <- c(4, 6, 8, NA)
#'
#' # Calculate survival intervals
#' intervals <- TNBintervals(left, right, nboot = 10)
#' print(intervals$original)  # Original data survival estimates
#' @importFrom interval icfit
#' @export

TNBintervals <- function(left, right, nboot = 1) {
  # Validate that left and right are numeric vectors
  if (!is.numeric(left) || !is.numeric(right)) {
    stop("'left' and 'right' must be numeric vectors.")
  }

  # Validate that left and right have the same length
  if (length(left) != length(right)) {
    stop("'left' and 'right' must have the same length.")
  }

  # Validate that left is strictly less than right for all defined pairs
  invalid_intervals <- !is.na(left) & !is.na(right) & (left >= right)
  if (any(invalid_intervals)) {
    stop("Each value in 'left' must be strictly less than the corresponding value in 'right'.")
  }

  # Replace infinite values with NA, as expected by icfit
  left[is.infinite(left)] <- NA
  right[is.infinite(right)] <- NA

  # Process the original data
  original_result <- .process_single_run(left, right)

  # Perform bootstrap if nboot > 1
  bootstrap_results <- NULL
  if (nboot > 1) {
    bootstrap_results <- replicate(nboot, {
      indices <- sample(seq_along(left), replace = TRUE)
      .process_single_run(left[indices], right[indices])
    }, simplify = FALSE)
  }

  # Return a list with the original and bootstrap results

  result <- list(original = original_result, bootstrap = bootstrap_results)
  class(result) <- c("list","TB")
  return(invisible(result))
}
