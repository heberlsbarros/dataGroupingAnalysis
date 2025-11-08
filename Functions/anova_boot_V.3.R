# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(lsr)
library(boot)
library(multcomp)
library(tibble)
library(effsize)

# Função para realizar ANOVA com opção de bootstrapping e intervalos de confiança
anova_boot <- function(data, grupo, resposta, bootstrap = FALSE, n_bootstrap = 1000, ci_type = c("normal", "bca")) {
  
  # Validar tipo de intervalo de confiança
  ci_type <- match.arg(ci_type)
  
  # Converter o grupo para fator e a resposta para numérico
  data[[grupo]] <- as.factor(data[[grupo]])
  data[[resposta]] <- as.numeric(data[[resposta]])
  
  # Função auxiliar para calcular estatísticas ANOVA
  anova_fun <- function(data, indices) {
    d <- data[indices, ] # Subamostra dos dados
    aov_res <- aov(as.formula(paste(resposta, "~", grupo)), data = d)
    summary(aov_res)[[1]][["F value"]][1] # Retornar o valor F
  }
  
  # ANOVA tradicional
  anova_res <- aov(as.formula(paste(resposta, "~", grupo)), data = data)
  cat("Resultado da ANOVA:\n")
  print(summary(anova_res))
  
  # Calcular o tamanho do efeito
  tamanho_efeito <- etaSquared(anova_res)
  cat("Tamanho do efeito (eta squared):\n")
  print(tamanho_efeito)
  
  if (bootstrap) {
    # Bootstrapping para ANOVA geral
    set.seed(123) # Para reprodutibilidade
    boot_res <- boot(data, anova_fun, R = n_bootstrap)
    cat("Resultado do bootstrapping para ANOVA:\n")
    print(boot_res)
    
    # Calcular intervalo de confiança normal
    original <- boot_res$t0
    std_error <- sd(boot_res$t)
    ci_lower_normal <- original - 1.96 * std_error
    ci_upper_normal <- original + 1.96 * std_error
    cat("Intervalo de confiança de 95% para o valor F (Normal): [", ci_lower_normal, ", ", ci_upper_normal, "]\n")
    
    # Calcular intervalo de confiança BCa se solicitado
    if (ci_type == "bca") {
      bca_ci <- boot.ci(boot_res, type = "bca")
      cat("Intervalo de confiança de 95% para o valor F (BCa):\n")
      print(bca_ci)
    }
  }
  
  # Realizar testes post hoc tradicionais
  tukey_res <- TukeyHSD(anova_res)
  cat("\nResultados dos testes post hoc (Tukey):\n")
  print(tukey_res)
  
  # Criar uma tabela para armazenar os resultados dos testes post hoc com intervalos de confiança
  posthoc_results <- tibble(
    Comparacao = character(),
    Diferenca = numeric(),
    Lower_Tukey = numeric(),
    Upper_Tukey = numeric(),
    Cohen_d = numeric(),
    Lower_BCA = numeric(),
    Upper_BCA = numeric()
  )
  
  # Obter os nomes das comparações do TukeyHSD
  comparacoes <- rownames(tukey_res[[1]])
  tukey_data <- as.data.frame(tukey_res[[1]])
  
  for (i in seq_along(comparacoes)) {
    # Calcular o tamanho do efeito de Cohen d para cada comparação
    grupos <- strsplit(comparacoes[i], "-")[[1]]
    grupo1 <- as.numeric(unlist(data[data[[grupo]] == grupos[1], resposta]))
    grupo2 <- as.numeric(unlist(data[data[[grupo]] == grupos[2], resposta]))
    cohen_d <- cohen.d(grupo1, grupo2)$estimate
    
    # Normalizar Cohen d para estar entre 0 e 1
    cohen_d_normalizado <- cohen_d / (abs(cohen_d) + 1)
    
    posthoc_results <- posthoc_results %>%
      add_row(
        Comparacao = comparacoes[i],
        Diferenca = tukey_data$diff[i],
        Lower_Tukey = tukey_data$lwr[i],
        Upper_Tukey = tukey_data$upr[i],
        Cohen_d = cohen_d_normalizado,
        Lower_BCA = NA,
        Upper_BCA = NA
      )
  }
  
  if (ci_type == "bca" && bootstrap) {
    # Função auxiliar para bootstrapping nos testes post hoc
    posthoc_fun <- function(data, indices) {
      d <- data[indices, ]
      aov_res <- aov(as.formula(paste(resposta, "~", grupo)), data = d)
      tukey_res <- TukeyHSD(aov_res)
      return(as.vector(tukey_res[[1]][, "diff"])) # Retornar as diferenças entre os grupos
    }
    
    boot_posthoc_res <- boot(data, posthoc_fun, R = n_bootstrap)
    cat("Resultado do bootstrapping para diferenças de médias nos testes post hoc:\n")
    print(boot_posthoc_res)
    
    for (i in 1:ncol(boot_posthoc_res$t)) {
      bca_ci <- boot.ci(boot_posthoc_res, type = "bca", index = i)
      posthoc_results$Lower_BCA[i] <- bca_ci$bca[4]
      posthoc_results$Upper_BCA[i] <- bca_ci$bca[5]
    }
  }
  
  cat("\nTabela de resultados dos testes post hoc (Tukey) com intervalos de confiança de 95% (BCa):\n")
  print(posthoc_results)
}

# # Exemplo de uso com dados que não têm distribuição normal e variâncias diferentes
# set.seed(123) # Para reprodutibilidade
# 
# data_example <- data.frame(
#   grupo = rep(letters[1:3], each = 50),
#   resposta = c(rnorm(50), rnorm(50, mean = 2), rnorm(50, mean = 5))
# )
# 
# # Usar a função com os dados de exemplo
# anova_boot(data_example, "grupo", "resposta", bootstrap = TRUE, n_bootstrap = 1000, ci_type = "bca")
# anova_boot(data_example, "grupo", "resposta", bootstrap = FALSE)
