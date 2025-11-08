valores_aberrantes <- function (dados, resposta){ #Essa função possui uma forma diferente de remoção de valores discrepantes que muitas vezes não são detectados pelas fórmulas padrão de detecção de outliers. Cabe lembrar que o que está neste script não é derivado de uma teoria matemática, se trata de uma fórmula criada arbitrariamente com o objetivo de corrigir discrepâncias aparentes.
  ##! Uma breve explicação a respeito dos inputs da função: o primeiro input é a planilha com todos os valores de leitura (output do script de transposição)
  ######### PRIMEIRA ETAPA: INSTALAÇÃO E CARREGAMENTO DOS PACOTES RELEVANTES ###########
  ## Listando os pacotes necessários
  pacotes <- c("dplyr", "tibble", "writexl", "readxl", "ggsignif", "ggplot2", "rlang", "rstatix", "Hmisc", "tidyr", "car", "multcomp", "RColorBrewer", "ggbreak", "stringr", "ggpattern", "purrr", "openxlsx")
  ### Checando se os pacotes estão instalados. Aqueles que não estão, serão automaticamente instalados.
  pacotes_instalados <- pacotes %in% rownames(installed.packages())
  if (any(pacotes_instalados == FALSE)) {
    install.packages(pacotes[!pacotes_instalados])
  }
  ### Carregar pacotes necessários
  lapply(pacotes, library, character.only = T)
  ### Removendo variáveis do processo de instalação e carregamento de pacotes.
  rm (pacotes, pacotes_instalados)
  ######### SEGUNDA ETAPA: IDENTIFICAÇÃO DE GRUPOS DIFERENTES ############
  ##Criando a coluna "nome_comb"
  dados$nome_comb <- NA
  ##Obtendo o nome da droga 1
  nome_droga_1 <- dados %>% filter (droga1 != "CT DMSO" & droga1 != "Nada") #Criando um dataframe no qual foram excluídos os valores "CT DMSO" e "Nada" da coluna "droga1". Dessa forma, ficaram somente os grupos que possuem o nome da droga 1.
  nome_droga_1 <- as.character(unique (nome_droga_1$droga1)) #Obtendo os valores que estão presentes na coluna "droga1", que será somente o nome da droga 1. Convertendo esse output para o formato de caracteres.
  concentracao_droga_1 <- dados %>% filter (droga1 == nome_droga_1)
  concentracao_droga_1 <- as.character(unique (concentracao_droga_1$concentracao_droga1))
  cat(paste("###\nA droga 1 deste grupo é: **", nome_droga_1, "**\n A concentração dessa droga é: **", concentracao_droga_1, "**")) #Confirmação do conteúdo do objeto "nome_droga_1"
  nome_ind1 <- paste(nome_droga_1, " (", concentracao_droga_1, ")", sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  
  ##Obtendo o nome da droga 2
  nome_droga_2 <- dados %>% filter (droga2 != "Nada" & droga2 != "droga2 sozinha" & droga2 != "CT DMSO") #Criando um dataframe no qual foram excluídos os valores "droga2 sozinha" e "Nada" da coluna "droga2". Dessa forma, ficaram somente os grupos que possuem o nome da droga 2.
  nome_droga_2 <- as.character(unique (nome_droga_2$droga2)) #Obtendo os valores que estão presentes na coluna "droga2", que será somente o nome da droga 2. Convertendo esse output para o formato de caracteres.
  concentracao_droga_2 <- dados %>% filter (droga2 == nome_droga_2)
  concentracao_droga_2 <- as.character(unique (concentracao_droga_2$concentracao_droga2))
  cat(paste("###\nA droga 2 deste grupo é: **", nome_droga_2, "**\n A concentração dessa droga é: **", concentracao_droga_2, "**")) #Confirmação do conteúdo do objeto "nome_droga_2"
  nome_ind2 <- paste(nome_droga_2, " (", concentracao_droga_2, ")", sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  
  ##Obtendo o nome da droga 3
  nome_droga_3 <- dados %>% filter (droga3 != "Nada" & droga3 != "droga3 sozinha") #Criando um dataframe no qual foram excluídos os valores "droga3 sozinha" e "Nada" da coluna "droga3". Dessa forma, ficaram somente os grupos que possuem o nome da droga 3.
  nome_droga_3 <- as.character(unique (nome_droga_3$droga3)) #Obtendo os valores que estão presentes na coluna "droga2", que será somente o nome da droga 3. Convertendo esse output para o formato de caracteres.
  concentracao_droga_3 <- dados %>% filter (droga3 == nome_droga_3)
  concentracao_droga_3 <- as.character(unique (concentracao_droga_3$concentracao_droga3))
  cat(paste("###\nA droga 3 deste grupo é: **", nome_droga_3, "**\n A concentração dessa droga é: **", concentracao_droga_3, "**")) #Confirmação do conteúdo do objeto "nome_droga_3"
  nome_ind3 <- paste(nome_droga_3, " (", concentracao_droga_3, ")", sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  
  ##Grupos CT duplos e combinação
  nome_dup12 <- paste(nome_ind1, " + ", nome_ind2, sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  nome_dup13 <- paste(nome_ind1, " + ", nome_ind3, sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  nome_dup23 <- paste(nome_ind2, " + ", nome_ind3, sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  nome_comb <- paste(nome_ind1, " + ", nome_ind2, " + ", nome_ind3, sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  
  #Criando a coluna nome_comb no dados:
  grupo_ct_dmso <- dados %>% filter (droga1 == "CT DMSO")
  grupo_ct_dmso$nome_comb <- "CT DMSO"
  grupo_ct_droga1 <- dados %>% filter (droga1 != "CT DMSO" & droga1 != "Nada" & droga2 == "Nada" & droga3 == "Nada")
  grupo_ct_droga1$nome_comb <- nome_ind1
  grupo_ct_droga2 <- dados %>% filter (droga1 == "Nada" & droga2 != "Nada" & droga3 == "Nada")
  grupo_ct_droga2$nome_comb <- nome_ind2
  grupo_ct_droga3 <- dados %>% filter (droga1 == "Nada" & droga2 == "Nada" & droga3 != "Nada")
  grupo_ct_droga3$nome_comb <- nome_ind3
  grupo_ct_droga12 <- dados %>% filter (droga1 != "CT DMSO" & droga1 != "Nada" & droga2 != "Nada" & droga3 == "Nada")
  grupo_ct_droga12$nome_comb <- nome_dup12
  grupo_ct_droga13 <- dados %>% filter (droga1 != "CT DMSO" & droga1 != "Nada" & droga2 == "Nada" & droga3 != "Nada")
  grupo_ct_droga13$nome_comb <- nome_dup13
  grupo_ct_droga23 <- dados %>% filter (droga1 == "Nada" & droga2 != "Nada" & droga3 != "Nada")
  grupo_ct_droga23$nome_comb <- nome_dup23
  grupo_comb <- dados %>% filter (droga1 != "CT DMSO" & droga1 != "Nada", droga2 != "Nada" & droga3 != "Nada")
  grupo_comb$nome_comb <- nome_comb
  dados <- rbind (grupo_ct_dmso, grupo_ct_droga1, grupo_ct_droga2, grupo_ct_droga3, grupo_ct_droga12, grupo_ct_droga13, grupo_ct_droga23, grupo_comb)
  
}#Final da função