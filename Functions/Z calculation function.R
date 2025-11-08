calculate_z <- function(sample, control, method = "traditional", detailed_output = FALSE, plot_data = FALSE) {
  # Verificações iniciais
  if (!is.numeric(sample) || !is.numeric(control)) {
    stop("Ambos 'sample' e 'control' devem ser vetores numéricos.")
  }
  
  if (length(sample) < 2 || length(control) < 2) {
    stop("Ambos 'sample' e 'control' devem ter pelo menos 2 elementos.")
  }
  
  if (!method %in% c("traditional", "robust")) {
    stop("O método deve ser 'traditional' ou 'robust'.")
  }
  
  # Plotagem dos dados se solicitado
  if (plot_data) {
    par(mfrow=c(2,2))
    hist(sample, main="Histograma da Amostra", xlab="Valores", ylab="Frequência")
    hist(control, main="Histograma do Controle", xlab="Valores", ylab="Frequência")
    qqnorm(sample, main="Q-Q Plot da Amostra")
    qqline(sample, col = "red")
    qqnorm(control, main="Q-Q Plot do Controle")
    qqline(control, col = "red")
    par(mfrow=c(1,1))  # Resetar o layout do plot
  }
  
  # Cálculo do Z'
  if (method == "traditional") {
    mean_s <- mean(sample)
    mean_c <- mean(control)
    sd_s <- sd(sample)
    sd_c <- sd(control)
    
    z_prime <- 1 - (3 * (sd_s + sd_c)) / abs(mean_s - mean_c)
    
    if (detailed_output) {
      cat("Método: Tradicional\n")
      cat("Média da amostra:", mean_s, "\n")
      cat("Média do controle:", mean_c, "\n")
      cat("Desvio padrão da amostra:", sd_s, "\n")
      cat("Desvio padrão do controle:", sd_c, "\n")
    }
  } else {
    median_s <- median(sample)
    median_c <- median(control)
    mad_s <- mad(sample)
    mad_c <- mad(control)
    
    z_prime <- 1 - (3 * (mad_s + mad_c)) / abs(median_s - median_c)
    
    if (detailed_output) {
      cat("Método: Robusto\n")
      cat("Mediana da amostra:", median_s, "\n")
      cat("Mediana do controle:", median_c, "\n")
      cat("MAD da amostra:", mad_s, "\n")
      cat("MAD do controle:", mad_c, "\n")
    }
  }
  
  # Interpretação do Z'
  quality <- if (z_prime >= 0.5 && z_prime <= 1) {
    "Separation band is large. An excelent signal."
  } else if (z_prime > 0 && z_prime < 0.5) {
    "Separation band is small. Be carefull!!"
  } else if (z_prime <= 0) {
    "No separtiaon band."
  } else {
    "Inválido"
  }
  
  if (detailed_output) {
    cat("Z' calculado:", z_prime, "\n")
    cat("Qualidade do ensaio:", quality, "\n")
  }
  
  return(list(
    z_prime = z_prime,
    method = method,
    quality = quality
  ))
}