.process_single_run <- function(left, right) {
  res <- interval::icfit(survival::Surv(left, right, type = "interval2") ~ 1)
  sink(tempfile())  # Redirecionar saída para um arquivo temporário
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

# Usar findInterval para melhorar a funcao
# interval_index <- findInterval(times, vec = c(data$left, Inf), rightmost.closed = TRUE)

single.TNBsurvival <- function(data, times) {
  # Ordenar os dados pelos limites inferiores dos intervalos
  data <- data[order(data$left), ]
  aux <- data[1, "left"]
  
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
  return(invisible(result))
}


TNBsurvival <- function(data, times, conf = FALSE, conf.level = 0.95) {
  # Verificar se os dados têm a estrutura esperada
  if (!is.list(data) || !all(c("original", "bootstrap") %in% names(data))) {
    stop("Input 'data' must be the result of 'TNBintervals', containing 'original' and 'bootstrap' elements.")
  }
  
  # Calcular as estimativas de sobrevivência para os dados originais
  survival_estimates <- single.TNBsurvival(data$original, times)
  
  # Se limites de confiança não forem solicitados, retornar apenas as estimativas
  if (!conf) {
    result <- data.frame(time = times, survival = survival_estimates)
    class(result) <- c("data.frame","TBS")
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
    bootstrap_survival[, b] <- single.TNBsurvival(data$bootstrap[[b]], times)
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
  
  class(result) <- c("data.frame","TBS")
  return(invisible(result))
}


# Plot Function
plot.TB <- function(x, conf=FALSE, conf.level=0.95, line.col="blue", ribbon.col="grey", line.width=2,
                    main="Survival Estimate", xlab="Time", ylab="Survival", 
                    add.legend = FALSE, legend.position="topright",...){
  
  # Verificar se as colunas necessárias estão presentes no data
  if (!all(c("left", "right", "weight", "survival") %in% colnames(x$original))) {
    stop("O objeto 'data' deve conter as colunas 'left', 'right', 'weight' e 'survival'.")
  }
  
  data <- x$original
  
  # (i) Obter todos os tempos únicos das colunas 'left' e 'right', ordenados
  all_times <- sort(unique(c(0, data$left, data$right)))
  all_times <- all_times[is.finite(all_times)] # Remover Inf se existir
  ntimes <- seq(0, max(all_times), length=100)
  
  # (ii) Obter estimativas de sobrevivência usando a função single.TNBsurvival
  survival_estimates <- single.TNBsurvival(data, all_times)
  est <- TNBsurvival(data=x, times=ntimes, conf=conf, conf.level = conf.level)
  
  # (iii) Fazer o plot das linhas
  matplot(
    x = est[,1],
    y = est[,2:4],
    type = "l",  # Tipo linha
    col = line.col,  # Cor da linha
    lwd = line.width,  # Largura da linha
    xlab = xlab,  # Rótulo do eixo X
    ylab = ylab,  # Rótulo do eixo Y
    main = main  # Título do gráfico
  )
  
  # Preencher a área entre as bandas de confiança
  polygon(
    x = c(est[,1], rev(est[,1])),  # Combinar os tempos (ida e volta)
    y = c(est[,3], rev(est[,4])),  # Combinar limites inferiores e superiores
    col = ribbon.col,  # Cor de preenchimento (cinza)
    border = NA  # Sem bordas
  )
  
  # Replotar a curva principal para que fique visível acima da área preenchida
  lines(
    x = est[,1],
    y = est[,2],
    col = line.col,
    lwd = line.width
  )
  
  # Adicionar legenda opcional
  if (add.legend) {
    legend(
      legend.position,
      legend = c("Survival", "Confidence Interval"),
      col = c(line.col, ribbon.col),
      lty = c(1, NA),   # Tipo de linha (1 para curva, NA para banda)
      fill = c(NA, ribbon.col),  # Cor de preenchimento para a banda
      border = NA,      # Sem bordas para a legenda
      bty = "n"         # Sem borda ao redor da legenda
    )
  }
  
  # Adicionar grades no gráfico (opcional)
  grid()
  
}


library(plotly)

plot.TBL <- function(x, conf=FALSE, conf.level=0.95, main="Survival Estimate",
                     xlab = "Time", ylab = "Survival", line.col ="blue", 
                     showlegend = TRUE, main.line="survival", filled="CI",
                     line.width = 2, fillcolor = "rgba(128, 128, 128, 0.3)",...) {
  
  # Verificar se as colunas necessárias estão presentes no data
  if (!all(c("left", "right", "weight", "survival") %in% colnames(x$original))) {
    stop("O objeto 'data' deve conter as colunas 'left', 'right', 'weight' e 'survival'.")
  }
  
  data <- x$original
  
  # (i) Obter todos os tempos únicos das colunas 'left' e 'right', ordenados
  all_times <- sort(unique(c(0, data$left, data$right)))
  all_times <- all_times[is.finite(all_times)] # Remover Inf se existir
  ntimes <- seq(0, max(all_times), length=100)
  
  # (ii) Obter estimativas de sobrevivência usando a função single.TNBsurvival
  survival_estimates <- single.TNBsurvival(data, all_times)
  est <- TNBsurvival(data=x, times=ntimes, conf=conf, conf.level=conf.level)
  
  # Separar colunas do objeto est
  times <- est[, 1]
  survival <- est[, 2]
  lower_band <- est[, 3]
  upper_band <- est[, 4]
  
  # (iii) Criar o gráfico interativo com plotly
  plot <- plotly::plot_ly() %>%
    # Adicionar as bandas de confiança como um preenchimento
    add_ribbons(
      x = ~times,
      ymin = ~lower_band,
      ymax = ~upper_band,
      #fillcolor = paste0("rgba(128, 128, 128, ", ribbon.opacity=ribbon.opacity, ")"),
      fillcolor = fillcolor,
      line = list(width = 0),
      name = filled
    ) %>%
    # Adicionar a linha principal de sobrevivência
    add_lines(
      x = ~times,
      y = ~survival,
      line = list(color = line.col, width = line.width),
      name = main.line
    ) %>%
    # Personalizar os eixos e o layout
    layout(
      title = main,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab),
      showlegend = showlegend
    )
  
  return(plot)
}



# Example data
left <- c(4, 2, 6, 4, 2, 6, 6, 6, 2, 6)
right <- c(6, 4, NA, 6, NA, 8, 8, NA, 6, 8)

tbdata <- TNBintervals(left, right, nboot = 10)
times <- c(1, 2, 3, 3.5, 4, 4.5, 5, 5.5, 6, 8, 7)
est <- TNBsurvival(data=tbdata, times=times, conf=FALSE);est
est <- TNBsurvival(data=tbdata, times=times, conf=TRUE, conf.level = 0.9); est

plot.TB(tbdata, conf=TRUE, conf.level=0.95)
windows()
plot.TB(tbdata, conf=TRUE, conf.level=0.95, line.width = c(2,0,0), add.legend = TRUE)
plot.TBL(tbdata, conf=TRUE, conf.level=0.95)
plot.TBL(tbdata, conf=TRUE, conf.level=0.95, showlegend =FALSE)

# Example data 2
left <- c(2, 2, 6, 2, 2, 6, 6, 6, 2, 6)
right <- c(6, 4, NA, 6, NA, 8, 8, NA, 6, 8)

tbdata <- TNBintervals(left, right, nboot = 10)
times <- c(1, 2, 3, 3.5, 4, 4.5, 5, 5.5, 6, 8, 7)
est <- TNBsurvival(data=tbdata, times=times, conf=FALSE);est
est <- TNBsurvival(data=tbdata, times=times, conf=TRUE, conf.level = 0.9); est
plot.TNB(tbdata)
plot.TBL(tbdata, conf=TRUE, conf.level=0.95, showlegend =FALSE)


# Melhorias a considerar:
# Utilizar vetorização para substituir loops sempre que possivel.
# Usar findInterval para melhorar a funcao single.TNBsurvival
# interval_index <- findInterval(times, vec = c(data$left, Inf), rightmost.closed = TRUE)
