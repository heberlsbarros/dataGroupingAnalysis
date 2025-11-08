# Carregar pacotes necessários
library(ggplot2)
library(car)
library(dplyr)
library(moments)
library(lsr)
library(FSA)
library(multcomp)
library(rstatix)

# Função para avaliar normalidade, homogeneidade, identificar outliers e refazer os testes
avaliar_mais_de_dois_grupos <- function(data, grupo, resposta, controle = NULL) {
  # Separar os grupos
  grupos <- unique(data[[grupo]])
  if (length(grupos) < 3) {
    stop("O número de grupos deve ser pelo menos 3.")
  }
  
  if (!is.null(controle) && !(controle %in% grupos)) {
    stop("O grupo controle especificado não está presente nos dados.")
  }
  
  # Verificar o tamanho dos grupos para o teste de Shapiro-Wilk
  grupo_data <- data %>% group_by(!!sym(grupo)) %>% summarise(count = n())
  if (any(grupo_data$count < 3)) {
    stop("O tamanho da amostra de cada grupo deve ser pelo menos 3.")
  }
  
  # Teste de Shapiro-Wilk para normalidade da variável dependente
  shapiro_result <- shapiro.test(data[[resposta]])$p.value
  
  cat("Resultado do teste de Shapiro-Wilk para normalidade da variável dependente:\n")
  print(shapiro_result)
  
  normalidade <- shapiro_result > 0.05
  
  # QQ plot para a variável dependente
  qqplot <- ggplot(data, aes(sample = .data[[resposta]])) + 
    stat_qq() + stat_qq_line() + ggtitle("QQ Plot - Variável Dependente")
  print(qqplot)
  
  # Converter o grupo para fator
  data[[grupo]] <- as.factor(data[[grupo]])
  
  # Teste de Levene para homogeneidade de variâncias
  levene <- leveneTest(as.formula(paste(resposta, "~", grupo)), data = data)
  
  cat("Resultado do teste de Levene para homogeneidade de variâncias:\n")
  print(levene)
  
  homogeneidade <- levene$`Pr(>F)`[1] > 0.05
  
  # Identificação de outliers
  if (normalidade) {
    # Dados são normais - Usar método Z-score
    z_scores <- scale(data[[resposta]])
    outliers <- abs(z_scores) > 3
  } else {
    # Dados não são normais - Usar método do Boxplot
    Q1 <- quantile(data[[resposta]], 0.25)
    Q3 <- quantile(data[[resposta]], 0.75)
    IQR <- Q3 - Q1
    outliers <- (data[[resposta]] < (Q1 - 1.5 * IQR)) | (data[[resposta]] > (Q3 + 1.5 * IQR))
  }
  
  data$outliers <- outliers
  data_sem_outliers <- data[!data$outliers, ]
  
  cat("Outliers identificados:\n")
  print(data[data$outliers, ])
  
  # Calcular estatísticas descritivas com outliers
  stats <- data %>%
    group_by(!!sym(grupo)) %>%
    summarise(
      n = n(),
      mean = mean(.data[[resposta]]),
      sd = sd(.data[[resposta]]),
      median = median(.data[[resposta]]),
      kurtosis = kurtosis(.data[[resposta]]),
      skewness = skewness(.data[[resposta]])
    )
  
  cat("Estatísticas descritivas com outliers:\n")
  print(stats)
  
  if (any(outliers)) {
    # Calcular estatísticas descritivas sem outliers
    stats_sem_outliers <- data_sem_outliers %>%
      group_by(!!sym(grupo)) %>%
      summarise(
        n = n(),
        mean = mean(.data[[resposta]]),
        sd = sd(.data[[resposta]]),
        median = median(.data[[resposta]]),
        kurtosis = kurtosis(.data[[resposta]]),
        skewness = skewness(.data[[resposta]])
      )
    
    cat("Estatísticas descritivas sem outliers:\n")
    print(stats_sem_outliers)
  }
  
  # Realizar ANOVA inicial para obter o valor de F
  resultado_anova_inicial <- aov(as.formula(paste(resposta, "~", grupo)), data = data)
  cat("Resultado da ANOVA inicial:\n")
  print(summary(resultado_anova_inicial))
  
  # Box plot original
  boxplot_original <- ggplot(data, aes(x = .data[[grupo]], y = .data[[resposta]], fill = .data[[grupo]])) + 
    geom_boxplot() + ggtitle("Box Plot - Grupos com Outliers")
  print(boxplot_original)
  
  # Escolha do teste baseado nos resultados com outliers
  if (normalidade && homogeneidade) {
    cat("Os grupos são normalmente distribuídos e têm variâncias homogêneas. Usando ANOVA.\n")
    resultado_teste <- aov(as.formula(paste(resposta, "~", grupo)), data = data)
  } else if (normalidade && !homogeneidade) {
    cat("Os grupos são normalmente distribuídos, mas têm variâncias diferentes. Usando ANOVA de Welch.\n")
    resultado_teste <- oneway.test(as.formula(paste(resposta, "~", grupo)), data = data, var.equal = FALSE)
  } else {
    cat("Pelo menos um dos grupos não é normalmente distribuído ou têm variâncias diferentes. Usando teste de Kruskal-Wallis.\n")
    resultado_teste <- kruskal.test(as.formula(paste(resposta, "~", grupo)), data = data)
  }
  
  # Realizar teste pós-hoc se p-valor do teste for menor que 0.05 com outliers
  if (inherits(resultado_teste, "aov")) {
    p_value <- summary(resultado_teste)[[1]]$`Pr(>F)`[1]
  } else {
    p_value <- resultado_teste$p.value
  }
  
  if (p_value < 0.05) {
    cat("Realizando teste pós-hoc porque p-valor do teste é menor que 0.05.\n")
    if (normalidade) {
      # Post-hoc Tukey
      post_hoc <- TukeyHSD(aov(as.formula(paste(resposta, "~", grupo)), data = data))
      # Adicionar coluna de significância
      post_hoc_result <- as.data.frame(post_hoc[[1]])
      post_hoc_result$significance <- ifelse(post_hoc_result$`p adj` < 0.05, "*", "ns")
      print(post_hoc_result)
    } else {
      # Post-hoc Dunn com correção de Bonferroni
      post_hoc <- dunn_test(data, as.formula(paste(resposta, "~", grupo)), p.adjust.method = "bonferroni")
      post_hoc <- post_hoc %>%
        dplyr::select(.y., group2, group1, n1, n2, statistic, p, p.adj, p.adj.signif)
      print(post_hoc)
    }
  }
  
  cat("\nResultados com outliers:\n")
  print(resultado_teste)
  
  if (any(outliers)) {
    # Box plot sem outliers
    boxplot_sem_outliers <- ggplot(data_sem_outliers, aes(x = .data[[grupo]], y = .data[[resposta]], fill = .data[[grupo]])) + 
      geom_boxplot() + ggtitle("Box Plot - Grupos sem Outliers")
    print(boxplot_sem_outliers)
    
    # Teste de Shapiro-Wilk para normalidade sem outliers
    shapiro_result_sem_outliers <- shapiro.test(data_sem_outliers[[resposta]])$p.value
    
    cat("Resultado do teste de Shapiro-Wilk para normalidade da variável dependente sem outliers:\n")
    print(shapiro_result_sem_outliers)
    
    normalidade_sem_outliers <- shapiro_result_sem_outliers > 0.05
    
    # QQ plot para a variável dependente sem outliers
    qqplot_sem_outliers <- ggplot(data_sem_outliers, aes(sample = .data[[resposta]])) + 
      stat_qq() + stat_qq_line() + ggtitle("QQ Plot - Variável Dependente sem Outliers")
    print(qqplot_sem_outliers)
    
    # Teste de Levene para homogeneidade de variâncias sem outliers
    levene_sem_outliers <- leveneTest(as.formula(paste(resposta, "~", grupo)), data = data_sem_outliers)
    
    cat("Resultado do teste de Levene para homogeneidade de variâncias sem outliers:\n")
    print(levene_sem_outliers)
    
    homogeneidade_sem_outliers <- levene_sem_outliers$`Pr(>F)`[1] > 0.05
    
    # Escolha do teste baseado nos resultados sem outliers
    if (normalidade_sem_outliers && homogeneidade_sem_outliers) {
      cat("Os grupos são normalmente distribuídos e têm variâncias homogêneas sem outliers. Usando ANOVA.\n")
      resultado_teste_sem_outliers <- aov(as.formula(paste(resposta, "~", grupo)), data = data_sem_outliers)
    } else if (normalidade_sem_outliers && !homogeneidade_sem_outliers) {
      cat("Os grupos são normalmente distribuídos, mas têm variâncias diferentes sem outliers. Usando ANOVA de Welch.\n")
      resultado_teste_sem_outliers <- oneway.test(as.formula(paste(resposta, "~", grupo)), data = data_sem_outliers, var.equal = FALSE)
    } else {
      cat("Pelo menos um dos grupos não é normalmente distribuído ou têm variâncias diferentes sem outliers. Usando teste de Kruskal-Wallis.\n")
      resultado_teste_sem_outliers <- kruskal.test(as.formula(paste(resposta, "~", grupo)), data = data_sem_outliers)
    }
    
    # Realizar teste pós-hoc se p-valor do teste for menor que 0.05 sem outliers
    if (inherits(resultado_teste_sem_outliers, "aov")) {
      p_value_sem_outliers <- summary(resultado_teste_sem_outliers)[[1]]$`Pr(>F)`[1]
    } else {
      p_value_sem_outliers <- resultado_teste_sem_outliers$p.value
    }
    
    if (p_value_sem_outliers < 0.05) {
      cat("Realizando teste pós-hoc porque p-valor do teste é menor que 0.05 sem outliers.\n")
      if (is.null(controle)) {
        post_hoc_sem_outliers <- TukeyHSD(aov(as.formula(paste(resposta, "~", grupo)), data = data_sem_outliers))
        print(post_hoc_sem_outliers)
      } else {
        # Comparação múltipla com o grupo controle usando teste de Dunnett
        resultado_teste_aov_sem_outliers <- aov(as.formula(paste(resposta, "~", grupo)), data = data_sem_outliers)
        dunnett_test_sem_outliers <- glht(resultado_teste_aov_sem_outliers, linfct = mcp(grupo = "Dunnett"))
        post_hoc_sem_outliers <- summary(dunnett_test_sem_outliers, test = adjusted("none"))
        print(post_hoc_sem_outliers)
      }
    }
    
    cat("\nResultados sem outliers:\n")
    print(resultado_teste_sem_outliers)
  }
}

# Exemplo de uso
# data <- data.frame(
#   grupo = rep(c("A", "B", "C", "D"), each = 10),
#   resposta = c(rnorm(10, mean = 5), rnorm(10, mean = 5.5), rnorm(10, mean = 6), rnorm(10, mean = 6.5))
# )
# avaliar_mais_de_dois_grupos(data, "grupo", "resposta", controle = "A")
