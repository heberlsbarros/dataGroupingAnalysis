library(ggplot2)
library(car)
library(rstatix)
library(multcomp)

conferir_tabela <- function(data, resposta, grupo) {
  # Tabela com n, média e desvio padrão para cada grupo
  cat("\n### Resumo Descritivo por Grupo ###\n")
  resumo_grupos <- data %>%
    group_by(!!sym(grupo)) %>%
    summarise(
      n = n(),
      media = mean(!!sym(resposta), na.rm = TRUE),
      desvio_padrao = sd(!!sym(resposta), na.rm = TRUE)
    ) %>%
    ungroup()
  print(resumo_grupos)
  
  # Teste de Shapiro-Wilk por Grupo
  cat("\n### Teste de Normalidade (Shapiro-Wilk) por Grupo ###\n")
  resultados_shapiro <- by(data[[resposta]], data[[grupo]], function(x) {
    if (length(x) > 2) {
      return(shapiro.test(x))
    } else {
      return(NULL)  # Retorna NULL se o grupo não tiver dados suficientes
    }
  })
  
  # Criar uma tabela com os resultados válidos
  tabela_shapiro <- do.call(rbind, lapply(1:length(resultados_shapiro), function(i) {
    res <- resultados_shapiro[[i]]
    if (!is.null(res)) {
      return(data.frame(
        Grupo = names(resultados_shapiro)[i],
        W = res$statistic,
        p_value = res$p.value
      ))
    } else {
      return(NULL)  # Ignora grupos inválidos
    }
  }))
  
  print(tabela_shapiro)  # Exibe a tabela no console
  
  # Boxplot
  cat("\n### Boxplot ###\n")
  p <- ggplot(data, aes_string(x = grupo, y = resposta, fill = grupo)) +
    geom_boxplot() +
    labs(title = "Boxplot dos grupos", x = grupo, y = resposta) +
    theme_minimal()
  print(p)
  
  # Teste de Levene
  cat("\n### Teste de Levene ###\n")
  levene_result <- leveneTest(as.formula(paste(resposta, "~", grupo)), data = data)
  print(levene_result)
  
  # Escolha do modelo com base na homogeneidade de variância
  if (levene_result$`Pr(>F)`[1] >= 0.05) {
    cat("\n### ANOVA Padrão ###\n")
    modelo <- aov(as.formula(paste(resposta, "~", grupo)), data = data)
    print(summary(modelo))
    
    cat("\n### Teste de Tukey ###\n")
    tukey_result <- TukeyHSD(modelo)
    print(tukey_result)
  } else {
    cat("\n### ANOVA com Correção de Welch ###\n")
    modelo <- oneway.test(as.formula(paste(resposta, "~", grupo)), data = data, var.equal = FALSE)
    print(modelo)
    
    cat("\n### Teste de Games-Howell ###\n")
    gh_result <- data %>%
      games_howell_test(as.formula(paste(resposta, "~", grupo)))
    print(gh_result)
  }
  
  # Identificação de outliers
  data$outlier <- NA  # Coluna para marcar os outliers
  
  for (grupo_atual in unique(data[[grupo]])) {
    grupo_dados <- data[data[[grupo]] == grupo_atual, resposta]
    
    if (length(grupo_dados) > 2) {
      shapiro_p <- tabela_shapiro[tabela_shapiro$Grupo == grupo_atual, "p_value"]
      
      if (!is.na(shapiro_p) && shapiro_p > 0.05) {
        # Método Z-Score para dados normalmente distribuídos
        z_scores <- (grupo_dados - mean(grupo_dados)) / sd(grupo_dados)
        outliers <- abs(z_scores) > 3
      } else {
        # Método IQR para dados não normalmente distribuídos
        Q1 <- quantile(grupo_dados, 0.25)
        Q3 <- quantile(grupo_dados, 0.75)
        IQR <- Q3 - Q1
        outliers <- (grupo_dados < (Q1 - 1.5 * IQR)) | (grupo_dados > (Q3 + 1.5 * IQR))
      }
      
      data$outlier[data[[grupo]] == grupo_atual] <- outliers
    }
  }
  
 
  # Teste de Kruskal-Wallis
  cat("\n### Teste de Kruskal-Wallis ###\n")
  kruskal_result <- kruskal.test(as.formula(paste(resposta, "~", grupo)), data = data)
  print(kruskal_result)
  
  # Teste de Dunn com correção de Holm
  if (kruskal_result$p.value < 0.05) {
    cat("\n### Teste de Dunn (com correção de Holm) ###\n")
    dunn_result <- data %>%
      dunn_test(as.formula(paste(resposta, "~", grupo)), p.adjust.method = "holm")
    print(dunn_result)
  } else {
    cat("\nOs grupos não diferem significativamente (p > 0.05 no teste de Kruskal-Wallis), portanto, o teste de Dunn não foi realizado.\n")
    dunn_result <- NULL
  }
  
  cat("\n### Outliers Identificados ###\n")
  print(data[data$outlier == TRUE, ])
  
  # Retorna uma lista com os resultados importantes
  return(list(
    #resumo_grupos = resumo_grupos,
    #levene = levene_result,
    #tabela_shapiro = tabela_shapiro,
    #outliers = data[data$outlier == TRUE, ],
    anova = modelo,
    tukey = if (levene_result$`Pr(>F)`[1] >= 0.05) tukey_result else NULL,
    games_howell = if (levene_result$`Pr(>F)`[1] < 0.05) gh_result else NULL,
    kruskal = kruskal_result,
    dunn = dunn_result
  ))
  print (p)
}
