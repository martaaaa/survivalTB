#' Plot Survival Estimates with Confidence Bands
#'
#' This function generates a survival plot with optional confidence bands.
#'
#' @param x A list containing survival estimates (output of `TNBintervals`).
#' @param conf Logical, whether to include confidence bands. Default is `FALSE`.
#' @param conf.level Numeric, confidence level for the bands (e.g., 0.95 for 95%). Default is 0.95.
#' @param line.col Character, color of the survival curve. Default is "blue".
#' @param ribbon.col Character, color of the confidence bands. Default is "grey".
#' @param line.width Numeric, line width for the survival curve. Default is 2.
#' @param main Character, title of the plot.
#' @param xlab Character, label for the x-axis.
#' @param ylab Character, label for the y-axis.
#' @param add.legend Logical, whether to add a legend. Default is `FALSE`.
#' @param legend.position Character, position of the legend. Default is "topright".
#' @param ... Additional graphical parameters.
#' @return A base R plot.
#' @examples
#' # Example data
#' left <- c(2, 4, 6, 8)
#' right <- c(4, 6, 8, NA)
#'
#' # Create intervals with bootstrap
#' intervals <- TNBintervals(left, right, nboot = 10)
#'
#' # Plot survival estimates
#' survivalTB:::plot.TB(intervals, conf = TRUE, conf.level = 0.95)
#' @importFrom stats update predict vcov quantile qnorm
#' @importFrom graphics grid legend lines matplot polygon
#' @importFrom grDevices rgb
#' @import survivalTB
#' @export

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

  # (ii) Obter estimativas de sobrevivência usando a função .single.TNBsurvival
  survival_estimates <- .single.TNBsurvival(data, all_times)
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


#' Interactive Survival Plot with Plotly
#'
#' This function generates an interactive survival plot using `plotly`, including optional confidence bands.
#'
#' @param x A list containing survival estimates (output of `TNBintervals`).
#' @param conf Logical, whether to include confidence bands. Default is `FALSE`.
#' @param conf.level Numeric, confidence level for the bands (e.g., 0.95 for 95%). Default is 0.95.
#' @param main Character, title of the plot.
#' @param xlab Character, label for the x-axis.
#' @param ylab Character, label for the y-axis.
#' @param line.col Character, color of the survival curve. Default is "blue".
#' @param showlegend Logical, whether to show the legend. Default is `TRUE`.
#' @param line.width Numeric, line width for the survival curve. Default is 2.
#' @param fillcolor Character, color for the confidence band fill (e.g., "rgba(128, 128, 128, 0.3)"). Default is grey.
#' @param main.line Character, the label for the main survival curve in the plot legend. Default is "survival".
#' @param filled Character, the label for the confidence intervals ribbon in the plot legend. Default is "CI".
#' @param ... Additional graphical parameters.
#' @return An interactive `plotly` object representing the survival plot.
#' @name plot.TBL
#' @examples
#' # Example data
#' left <- c(2, 4, 6, 8)
#' right <- c(4, 6, 8, NA)
#'
#' # Create intervals with bootstrap
#' intervals <- TNBintervals(left, right, nboot = 10)
#'
#' # Interactive plot
#' survivalTB:::plot.TBL(intervals, conf = TRUE, conf.level = 0.95)
#' @importFrom plotly plot_ly add_ribbons add_lines layout
#' @import dplyr survivalTB plotly
#' @export

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

  # (ii) Obter estimativas de sobrevivência usando a função .single.TNBsurvival
  survival_estimates <- .single.TNBsurvival(data, all_times)
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
    layout(
      title = list(text = main),  # Corrigido para lista
      xaxis = list(title = xlab),
      yaxis = list(title = ylab),
      showlegend = showlegend  # Este argumento é aceito corretamente agora
    )

  return(plot)
}
