#' Estimate Survival for Specific Times
#'
#' This function estimates survival probabilities for a given set of times using interval-censored data.
#'
#' @param data A data frame with survival estimates (output of `.process_single_run`).
#' @param times Numeric vector of times for which survival estimates are required.
#' @return A numeric vector of survival probabilities corresponding to the input times.
#' @keywords internal

.single.TNBsurvival <- function(data, times) {
  # Ordenar os dados pelos limites inferiores dos intervalos
  data <- data[order(data$left), ]
  aux <- data[1, "left"]

  # Substituir NAs em `right` por um valor muito grande (Inf, por exemplo)
  data$right[is.na(data$right)] <- Inf

  # Adicionar um ponto inicial antes do primeiro intervalo
  data <- rbind(data.frame(left = -Inf, right = min(data$left), weight = 0, survival = 1), data)

  # Inicializar o vetor de sobrevivência
  survival_estimates <- numeric(length(times))

  # Calcular a sobrevivência para cada tempo fornecido
  for (i in seq_along(times)) {
    time <- times[i]

    # Caso 1: Tempo menor ou igual ao início do primeiro intervalo
    if (time <= min(data$left)) {
      survival_estimates[i] <- 1
      next
    }

    # Caso 2: Tempo maior ou igual ao final do último intervalo
    if (time >= max(data$right)) {
      survival_estimates[i] <- data$survival[nrow(data)]
      next
    }

    # Caso 3: Tempo dentro de um intervalo (incluindo limites exatos)
    for (j in 2:nrow(data)) {
      if (time >= data$left[j] && time <= data$right[j]) {
        if (time == data$left[j]) {
          survival_estimates[i] <- data$survival[j - 1]
        } else if (time == data$right[j]) {
          survival_estimates[i] <- data$survival[j]
        } else {
          # Interpolação linear
          survival_start <- data$survival[j - 1]
          survival_end <- data$survival[j]
          time_start <- data$left[j]
          time_end <- data$right[j]
          survival_estimates[i] <- survival_start +
            (survival_end - survival_start) * (time - time_start) / (time_end - time_start)
        }
        break
      }
    }

    # Case 4: Time not in any range (use survival of the last interval before the time)
    aux0 <- length(which(data$left <= time & data$right >= time))
    aux1 <- max(which(data$left <= time))
    if (aux0 == 0) survival_estimates[i] <- data$survival[aux1]
  }

  # Garantir que todos os tempos abaixo do menor limite inferior tenham sobrevivência = 1
  survival_estimates[times < aux] <- 1

  return(survival_estimates)
}

#' Calculate Survival Estimates with Confidence Intervals
#'
#' This function calculates survival probabilities for a given set of times and optionally adds confidence intervals.
#'
#' @param data A list containing survival estimates (output of `TNBintervals`).
#' @param times Numeric vector of times for which survival estimates are required.
#' @param conf Logical, whether to calculate confidence intervals. Default is `FALSE`.
#' @param conf.level Numeric, confidence level for intervals (e.g., 0.95 for 95%). Default is 0.95.
#' @return A data frame with columns:
#'   \item{time}{Times at which survival is estimated.}
#'   \item{survival}{Survival probabilities at the specified times.}
#'   \item{ci_lower}{Lower bound of the confidence interval (if `conf = TRUE`).}
#'   \item{ci_upper}{Upper bound of the confidence interval (if `conf = TRUE`).}
#' @examples
#' # Example data
#' left <- c(2, 4, 6, 8)
#' right <- c(4, 6, 8, NA)
#'
#' # Create intervals with bootstrap
#' intervals <- TNBintervals(left, right, nboot = 10)
#'
#' # Estimate survival probabilities
#' times <- c(1, 3, 5, 7)
#' survival_estimates <- TNBsurvival(intervals, times, conf = TRUE, conf.level = 0.95)
#' print(survival_estimates)
#' @export

TNBsurvival <- function(data, times, conf = FALSE, conf.level = 0.95) {
  # Verificar se os dados têm a estrutura esperada
  if (!is.list(data) || !all(c("original", "bootstrap") %in% names(data))) {
    stop("Input 'data' must be the result of 'TNBintervals', containing 'original' and 'bootstrap' elements.")
  }

  # Calcular as estimativas de sobrevivência para os dados originais
  survival_estimates <- .single.TNBsurvival(data$original, times)

  # Se limites de confiança não forem solicitados, retornar apenas as estimativas
  if (!conf) {
    result <- data.frame(time = times, survival = survival_estimates)
    class(result) <- c("data.frame","TB")
    return(result)
  }

  # Validar se os dados bootstrap estão disponíveis
  if (is.null(data$bootstrap)) {
    stop("Bootstrap data is required to calculate confidence intervals. Ensure 'nboot > 1' in TNBintervals.")
  }

  # Inicializar matriz para armazenar sobrevivências bootstrap
  bootstrap_survival <- matrix(NA, nrow = length(times), ncol = length(data$bootstrap))

  # Calcular sobrevivência para cada réplica bootstrap
  for (b in seq_along(data$bootstrap)) {
    bootstrap_survival[, b] <- .single.TNBsurvival(data$bootstrap[[b]], times)
  }

  # Calcular limites de confiança
  alpha <- (1 - conf.level) / 2
  ci_lower <- apply(bootstrap_survival, 1, quantile, probs = alpha)
  ci_upper <- apply(bootstrap_survival, 1, quantile, probs = 1 - alpha)

  # Retornar estimativas com limites de confiança
  result <- data.frame(
    time = times,
    survival = survival_estimates,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )

  class(result) <- c("data.frame","TB")
  return(invisible(result))
}

