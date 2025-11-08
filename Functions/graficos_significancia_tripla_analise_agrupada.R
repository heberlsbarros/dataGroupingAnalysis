graficos_significancia_tripla_analise_agrupada <- function (nome_da_planilha, tituloImagem, resposta) {
  ######################==========INSTALAÇÃO E CARREGAMENTO DE PACOTES===========######################
  #Instalação e carregamento de pacotes necessários
  ## Listando os pacotes necessários
  pacotes <- c("dplyr", "tibble", "writexl", "readxl", "ggsignif", "ggplot2", "rlang", "rstatix", "Hmisc", "tidyr", "car", "multcomp", "RColorBrewer", "ggbreak", "stringr", "ggpattern", "purrr", "openxlsx")
  ## Checando se os pacotes estão instalados. Aqueles que não estão, serão automaticamente instalados.
  pacotes_instalados <- pacotes %in% rownames(installed.packages())
  if (any(pacotes_instalados == FALSE)) {
    install.packages(pacotes[!pacotes_instalados])
  }
  ## Carregar pacotes necessários
  lapply(pacotes, library, character.only = T)
  ## Removendo variáveis do processo de instalação e carregamento de pacotes.
  rm (pacotes, pacotes_instalados)
  
  ######################==========FORÇANDO O SOFTWARE A FORNECER NÚMEROS EM FORMATO USUAL, NÃO EM NOTAÇÃO CIENTÍFICA===========######################
  options(scipen = 100, digits = 4) # Será útil quando forem realizadas as anotações gráficas
  ######################==========TROCA DE IDENTIFICAÇÕES DOS GRUPOS===========######################
  ##Retirando underlines dos nomes
  nome_da_planilha$nome_comb <- NA
  nome_da_planilha$droga1 <- gsub("Tanshinona_IA", "Tanshinona IA",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("Tanshinona_IA", "Tanshinona IA",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("Tanshinona_IA", "Tanshinona IA",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("Tanshinona_IIA", "Tanshinona IIA",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("Tanshinona_IIA", "Tanshinona IIA",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("Tanshinona_IIA", "Tanshinona IIA",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("S_equol", "S equol",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("S_equol", "S equol",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("S_equol", "S equol",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("Sequol", "S equol",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("Sequol", "S equol",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("Sequol", "S equol",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("T_5224", "T 5224",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("T_5224", "T 5224",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("T_5224", "T 5224",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("AKTinhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("AKTinhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("AKTinhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("AKTInhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("AKTInhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("AKTInhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("AKT_Inhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("AKT_Inhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("AKT_Inhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("AKT_inhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("AKT_inhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("AKT_inhibitor_VIII", "AKT Inhibitor VIII",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("JNKIN7", "JNK IN 7",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("JNKIN7", "JNK IN 7",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("JNKIN7", "JNK IN 7",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("JNK_IN_7", "JNK IN 7",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("JNK_IN_7", "JNK IN 7",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("JNK_IN_7", "JNK IN 7",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("UO126", "UO 126",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("UO126", "UO 126",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("UO126", "UO 126",nome_da_planilha$droga3)
  nome_da_planilha$droga1 <- gsub("UO_126", "UO 126",nome_da_planilha$droga1)
  nome_da_planilha$droga2 <- gsub("UO_126", "UO 126",nome_da_planilha$droga2)
  nome_da_planilha$droga3 <- gsub("UO_126", "UO 126",nome_da_planilha$droga3)
  nome_da_planilha$nome_comb <- NA
  # nome_da_planilha$nome_comb <- gsub("Tanshinona_IA", "Tanshinona IA",nome_da_planilha$nome_comb)
  nome_da_planilha$droga1 <- gsub("Tanshinona_IA", "Tanshinona IA",nome_da_planilha$droga1)
  # nome_da_planilha$nome_comb <- gsub("Tanshinona_IIA", "Tanshinona IIA",nome_da_planilha$nome_comb)
  nome_da_planilha$droga1 <- gsub("Tanshinona_IIA", "Tanshinona IIA",nome_da_planilha$droga1)
  nome_da_planilha$droga3 <- gsub("S_equol", "S equol", nome_da_planilha$droga3)
  
  ##Obtendo o nome da droga 1
  nome_droga_1 <- nome_da_planilha %>% filter (droga1 != "CT DMSO" & droga1 != "Nada") #Criando um dataframe no qual foram excluídos os valores "CT DMSO" e "Nada" da coluna "droga1". Dessa forma, ficaram somente os grupos que possuem o nome da droga 1.
  nome_droga_1 <- as.character(unique (nome_droga_1$droga1)) #Obtendo os valores que estão presentes na coluna "droga1", que será somente o nome da droga 1. Convertendo esse output para o formato de caracteres.
  concentracao_droga_1 <- nome_da_planilha %>% filter (droga1 == nome_droga_1)
  concentracao_droga_1 <- as.character(unique (concentracao_droga_1$concentracao_droga1))
  cat(paste("###\nA droga 1 deste grupo é: **", nome_droga_1, "**\n A concentração dessa droga é: **", concentracao_droga_1, "**")) #Confirmação do conteúdo do objeto "nome_droga_1"
  nome_ind1 <- paste(nome_droga_1, " (", concentracao_droga_1, ")", sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  
  ##Obtendo o nome da droga 2
  nome_droga_2 <- nome_da_planilha %>% filter (droga2 != "Nada" & droga2 != "droga2 sozinha" & droga2 != "CT DMSO") #Criando um dataframe no qual foram excluídos os valores "droga2 sozinha" e "Nada" da coluna "droga2". Dessa forma, ficaram somente os grupos que possuem o nome da droga 2.
  nome_droga_2 <- as.character(unique (nome_droga_2$droga2)) #Obtendo os valores que estão presentes na coluna "droga2", que será somente o nome da droga 2. Convertendo esse output para o formato de caracteres.
  concentracao_droga_2 <- nome_da_planilha %>% filter (droga2 == nome_droga_2)
  concentracao_droga_2 <- as.character(unique (concentracao_droga_2$concentracao_droga2))
  cat(paste("###\nA droga 2 deste grupo é: **", nome_droga_2, "**\n A concentração dessa droga é: **", concentracao_droga_2, "**")) #Confirmação do conteúdo do objeto "nome_droga_2"
  nome_ind2 <- paste(nome_droga_2, " (", concentracao_droga_2, ")", sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  
  ##Obtendo o nome da droga 3
  nome_droga_3 <- nome_da_planilha %>% filter (droga3 != "Nada" & droga3 != "droga3 sozinha") #Criando um dataframe no qual foram excluídos os valores "droga3 sozinha" e "Nada" da coluna "droga3". Dessa forma, ficaram somente os grupos que possuem o nome da droga 3.
  nome_droga_3 <- as.character(unique (nome_droga_3$droga3)) #Obtendo os valores que estão presentes na coluna "droga2", que será somente o nome da droga 3. Convertendo esse output para o formato de caracteres.
  concentracao_droga_3 <- nome_da_planilha %>% filter (droga3 == nome_droga_3)
  concentracao_droga_3 <- as.character(unique (concentracao_droga_3$concentracao_droga3))
  cat(paste("###\nA droga 3 deste grupo é: **", nome_droga_3, "**\n A concentração dessa droga é: **", concentracao_droga_3, "**")) #Confirmação do conteúdo do objeto "nome_droga_3"
  nome_ind3 <- paste(nome_droga_3, " (", concentracao_droga_3, ")", sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  
  ##Grupos CT duplos e combinação
  nome_dup12 <- paste(nome_ind1, " + ", nome_ind2, sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  nome_dup13 <- paste(nome_ind1, " + ", nome_ind3, sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  nome_dup23 <- paste(nome_ind2, " + ", nome_ind3, sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  nome_comb <- paste(nome_ind1, " + ", nome_ind2, " + ",  nome_ind3, sep = "") #ESSA LINHA COMPÕE O NOME DA COMBINAÇÃO
  
  
  #Criando a coluna nome_comb no nome_da_planilha:
  grupo_ct_dmso <- nome_da_planilha %>% filter (droga1 == "CT DMSO")
  grupo_ct_dmso$nome_comb <- "CT DMSO"
  grupo_ct_droga1 <- nome_da_planilha %>% filter (droga1 != "CT DMSO" & droga1 != "Nada" & droga2 == "Nada" & droga3 == "Nada")
  grupo_ct_droga1$nome_comb <- nome_ind1
  grupo_ct_droga2 <- nome_da_planilha %>% filter (droga1 == "Nada" & droga2 != "Nada" & droga3 == "Nada")
  grupo_ct_droga2$nome_comb <- nome_ind2
  grupo_ct_droga3 <- nome_da_planilha %>% filter (droga1 == "Nada" & droga2 == "Nada" & droga3 != "Nada")
  grupo_ct_droga3$nome_comb <- nome_ind3
  # grupo_ct_droga12 <- nome_da_planilha %>% filter (droga1 != "CT DMSO" & droga1 != "Nada" & droga2 != "Nada" & droga3 == "Nada")
  # grupo_ct_droga12$nome_comb <- nome_dup12
  grupo_ct_droga13 <- nome_da_planilha %>% filter (droga1 != "CT DMSO" & droga1 != "Nada" & droga2 == "Nada" & droga3 != "Nada")
  grupo_ct_droga13$nome_comb <- nome_dup13
  grupo_ct_droga23 <- nome_da_planilha %>% filter (droga1 == "Nada" & droga2 != "Nada" & droga3 != "Nada")
  grupo_ct_droga23$nome_comb <- nome_dup23
  grupo_comb <- nome_da_planilha %>% filter (droga1 != "CT DMSO" & droga1 != "Nada", droga2 != "Nada" & droga3 != "Nada")
  grupo_comb$nome_comb <- nome_comb
  
  nome_da_planilha <- rbind (grupo_ct_dmso, grupo_ct_droga3, grupo_ct_droga1, grupo_ct_droga23, grupo_comb)
  
  # 
  # ## Criando uma coluna com nomes combinados
  # nome_da_planilha$nome_comb <- paste(nome_da_planilha$droga1, nome_da_planilha$concentracao_droga1, nome_da_planilha$droga2, nome_da_planilha$concentracao_droga2, nome_da_planilha$droga3,  nome_da_planilha$concentracao_droga3,  sep=" ")
  # 
  # ##Retirando, na planilha original, o nome da linhagem celular:
  # 
  # nome_da_planilha$nome_comb <- gsub("(0,1%)", "",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("(0)", "",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("()", "",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("Nada", "",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("Tanshinona_IA", "Tanshinona IA",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("Tanshinona_IIA", "Tanshinona IIA",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("()", "", nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("$ ", "", nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub(paste(nome_da_planilha$tipo_celula[1], "_", sep=""), "",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- sub("_"," ", nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("_10"," 10", nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("_1 "," 1 ", nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("IIA_","IIA ", nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("IA_","IA ", nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("M_E","M E", nome_da_planilha$nome_comb)
    # nome_da_planilha$nome_comb <- gsub(paste("_",concentracao_droga_3,"$", sep = ""), paste(" ",concentracao_droga_3,sep=""), nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("_"," + ",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("1 nM","(1 nM)",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("10 nM","(10 nM)",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("1 uM","(1 uM)",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("10 uM","(10 uM)",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub("8 uM", "(8 uM)",nome_da_planilha$nome_comb)
  # nome_da_planilha$nome_comb <- gsub(" 0,1%", "",nome_da_planilha$nome_comb)
  
  ######################==========ESTATÍSTICA DESCRITIVA BÁSICA===========######################
  #Criação de tabela com valores de n amostral, média, desvio padrão e identificação dos grupos:
  cat("\n### Resumo Descritivo por Grupo ###\n")
  resumo_grupos <- nome_da_planilha %>%
    group_by(!!sym("nome_comb")) %>%
    summarise(
      n = n(),
      media = mean(!!sym(resposta), na.rm = TRUE),
      desvio_padrao = sd(!!sym(resposta), na.rm = TRUE)
    ) %>%
    ungroup()
  print(resumo_grupos)
  
  #Criação e incorporação de coluna com valores médios de porcentagem da resposta para grupos, usando o CT DMSO como referência (de 100% de resposta):
  conversao_grupo <- list() #Cria objeto temporário para loop a seguir
  conversao_sd <- list() #Cria objeto temporário para loop a seguir
  coleta_media <- list() #Cria objeto temporário para loop a seguir
  coleta_sd <- list() #Cria objeto temporário para loop a seguir
  for (z in 1:length(resumo_grupos$nome_comb)){ #Loop para calcular todos valores de conversão para porcentagem, levando a média de resposta do CT DMSO como referência, tanto para conversão de média quanto para desvio padrão.
    conversao_grupo <- (resumo_grupos$media[z] *100)/resumo_grupos$media[1] #
    conversao_sd <- (resumo_grupos$desvio_padrao[z] * 100/resumo_grupos$media[1])
    coleta_media <- c(coleta_media, conversao_grupo)
    coleta_sd <- c(coleta_sd, conversao_sd)
  }
  
  ##Agrupamento dos dados de porcentagem dos grupos a um único dataframe
  porcentagens_media <- data.frame()
  porcentagens_media <- as.numeric(c(coleta_media[1], coleta_media[2], coleta_media[3], coleta_media[4], coleta_media[5])) #Tirando o resultado do loop do formato de lista e colocando no formato númerico
  porcentagens_media <- as.data.frame(porcentagens_media) #Tirando o resultado do formato númerico e colocando em formato de dataframe
  porcentagens_grupos <- cbind(resumo_grupos$nome_comb, porcentagens_media) #Agregando o resultado de porcentagens à identificação dos grupos
  colnames (porcentagens_grupos) <- c("Id grupos", "Porcentagens referentes ao CT DMSO")
    
  #Criação e incorporação de coluna com valores individuais de porcentagem da resposta para grupos, usando o CT DMSO como referência (de 100% de resposta):
  conversao_media_individual <- list()
  coleta_media_individual <- list()
  
  for (z in 1:length(nome_da_planilha$droga3)){ #Loop para calcular todos valores de conversão para porcentagem, levando a média de resposta do CT DMSO como referência, tanto para conversão de média quanto para desvio padrão.
    conversao_media_individual <- (nome_da_planilha$fluorescencia_ajustada[z] *100)/resumo_grupos$media[1] # Aqui é uma fórmula que assume os diferentes valores de resposta, multiplica por 100 e divide pelo valor da média do grupo CT DMSO.
    coleta_media_individual <- c(coleta_media_individual, conversao_media_individual) #Agrupa os dados de porcentagem resultantes da conversão, usando a média da resposta de CT DMSO como referência
  }
  porcentagens_individual <- data.frame()
  porcentagens_individual <- as.numeric(coleta_media_individual) #Tirando o resultado do loop do formato de lista e colocando no formato númerico
  porcentagens_individual <- as.data.frame(porcentagens_individual) #Tirando o resultado do formato númerico e colocando em formato de dataframe
  porcentagens_individual <- cbind(nome_da_planilha$nome_comb, porcentagens_individual) #Agregando o resultado de porcentagens à identificação dos grupos
  colnames(porcentagens_individual) <- c("nome_comb", "porcentagem")
  
  ##Aplicando o mesmo raciocínio de estatística descritiva dos valores absolutos, os valores, em porcentagem, relativos ao CT DMSO, serão calculados e salvos em um dataframe
  resumo_porcentagens <- porcentagens_individual %>%
    group_by(!!sym("nome_comb")) %>%
    summarise(
      n = n(),
      media = mean(!!sym("porcentagem"), na.rm = TRUE),
      desvio_padrao = sd(!!sym("porcentagem"), na.rm = TRUE)
    ) %>%
    ungroup()
  
  ##Remoção dos objetos temporários
  rm(conversao_grupo, conversao_sd, coleta_media, coleta_sd, porcentagens_media, porcentagens_grupos, conversao_media_individual, coleta_media_individual)
  
  ######################==========ANÁLISES DE NORMALIDADE E HETEROGENEIDADE===========######################
  # Teste de Shapiro-Wilk por Grupo: identificará se o grupo tem distribuição paramétrica ou não paramétrica.
  cat("\n### Teste de Normalidade (Shapiro-Wilk) por Grupo ###\n")
  resultados_shapiro <- by(nome_da_planilha[[resposta]], nome_da_planilha["nome_comb"], function(x) {
    if (length(x) > 2) { #Verificar se o grupo tem mais de 2 valores.
      return(shapiro.test(x)) #Se passar na condição fixada na linha anterior, aquele grupo receberá o valor de p do teste de shapiro wilk.
    } else {
      return(NULL)  # Retorna NULL se o grupo não tiver dados suficientes (menos que 2)
    }
  }) #Ao final desse if, o dataframe "resultados_shapiro" será composto pelas identificações dos grupos e os valores de p de shapiro-wilk.

  ## Criar uma tabela com os resultados válidos
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
  
  # Identificação de outliers
  nome_da_planilha$outlier <- NA  # Coluna para marcar os outliers
  grupo_dados <- unique (nome_da_planilha$nome_comb)
  
  for (i in grupo_dados){
    # print (i)
    grupo_atual <- nome_da_planilha[nome_da_planilha[["nome_comb"]] == i, resposta]
    # print(grupo_atual)
    if (length(grupo_atual$fluorescencia) > 2) {
      # shapiro_p <- shapiro.test(grupo_atual[[resposta]])
      shapiro_p <- tabela_shapiro[tabela_shapiro$Grupo == i, "p_value"]
      if (!is.na(shapiro_p) && shapiro_p > 0.05) {
        # Método Z-Score para dados normalmente distribuídos
        z_scores <- (grupo_atual$fluorescencia - mean(grupo_atual$fluorescencia)) / sd(grupo_atual$fluorescencia)
        outliers <- abs(z_scores) > 1.96
      } else {
        # Método IQR para dados não normalmente distribuídos
        Q1 <- quantile(grupo_atual$fluorescencia, 0.25)
        Q3 <- quantile(grupo_atual$fluorescencia, 0.75)
        IQR <- Q3 - Q1
        outliers <- (grupo_atual$fluorescencia < (Q1 - 1.5 * IQR)) | (grupo_atual$fluorescencia > (Q3 + 1.5 * IQR))
      }
      nome_da_planilha$outlier[nome_da_planilha[["nome_comb"]] == i] <- outliers #Ao final do for, adiciona uma coluna indicando se aquele valor é um outlier.
    }
    nome_da_planilha2 <- nome_da_planilha %>% filter (!is.na (outlier)) #Seleciona os valores que não são outliers, que são aqueles cuja coluna "outliers" tem valor diferente de "NA"
  }


# }
print("Relação de outliers identificados:\n ")
print(nome_da_planilha2[nome_da_planilha2$outlier == TRUE, ])

nome_da_planilha <- nome_da_planilha %>% dplyr::filter (!grepl("TRUE", outlier))
  
  ########ORGANIZANDO A PLANILHA PARA DEFINIR A ORDEM DE GRUPOS NOS GRÁFICOS############
  
  # nome_da_planilha$nome_comb <- as.factor(nome_da_planilha$nome_comb) #Organização da planilha resultante para gráficos
  # nome_da_planilha$nome_comb <- factor(nome_da_planilha$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "SP100030 (10 uM) + Erteberel (1 nM)", "Tamoxifeno (10 uM) + SP100030 (10 uM) + Erteberel (1 nM)")) #Organização da planilha resultante para gráficos 
  # nome_da_planilha$nome_comb <- factor(nome_da_planilha$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "SP100030 (10 uM) + Erteberel (10 nM)", "Tamoxifeno (10 uM) + SP100030 (10 uM) + Erteberel (10 nM)")) #Organização da planilha resultante para gráficos 
  # nome_da_planilha$nome_comb <- factor(nome_da_planilha$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "Tanshinona IA (8 uM) + Erteberel (1 nM)", "Tamoxifeno (10 uM) + Tanshinona IA (8 uM) + Erteberel (1 nM)")) #Organização da planilha resultante para gráficos 
  # nome_da_planilha$nome_comb <- factor(nome_da_planilha$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "Tanshinona IA (8 uM) + Erteberel (10 nM)", "Tamoxifeno (10 uM) + Tanshinona IA (8 uM) + Erteberel (10 nM)")) #Organização da planilha resultante para gráficos 
  # nome_da_planilha$nome_comb <- factor(nome_da_planilha$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "Tanshinona IIA (8 uM) + Erteberel (1 nM)", "Tamoxifeno (10 uM) + Tanshinona IIA (8 uM) + Erteberel (1 nM)")) #Organização da planilha resultante para gráficos 
  # nome_da_planilha$nome_comb <- factor(nome_da_planilha$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "Tanshinona IIA (8 uM) + Erteberel (10 nM)", "Tamoxifeno (10 uM) + Tanshinona IIA (8 uM) + Erteberel (10 nM)")) #Organização da planilha resultante para gráficos 
  
  ################CONFECÇÃO DO BOXPLOT#####################
  nome_da_planilha$nome_comb <- factor(nome_da_planilha$nome_comb, levels = c ("CT DMSO", nome_ind3, nome_ind1, nome_ind2, nome_dup12,nome_dup13, nome_dup23,nome_comb), ordered = T)
  # nome_da_planilha$funcao <- factor(nome_da_planilha$funcao, levels = c("CT DMSO", "CT ind droga1", "CT ind droga2", "CT ind droga3", "CT dup droga1 droga2", "CT dup droga1 droga3", "CT dup droga2 droga3", "Combinacao"), ordered = T)
  # Boxplot
  cat("\n### Boxplot ###\n")
  grafico_boxplot <- ggplot(nome_da_planilha, aes_string(x = "nome_comb", y = resposta, fill = "nome_comb")) +
    geom_boxplot() +
    labs(title = "Boxplot dos grupos", x = "Tratamento", y = resposta) +
    theme_minimal( #Inicia a parte estética
  
  ) +
  scale_fill_grey(start = 0.4, #Define o padrão de cores de preenchimento dos gráficos
                  end = 1.0, na.value = "red"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none", #Essa linha remove o quadro que informa a legenda, visto que já está presente no eixo x. Ideal para gráficos que serão apresentados individualmente. Para gráficos que serão apresentados em conjunto com outros (exemplo: mesmas drogas, células diferentes), recomendo retirar essa linha e retirar os nomes dos eixos x.
    axis.text.y = element_text(color = 'black'), #Configuração dos títulos do eixo Y.
    axis.title.x = element_text(margin = margin(t = 10)),# Ajusta a posição do título do eixo x   
    axis.text.x = element_text (angle = 45, hjust = 1))
    
    
  ###? ACRESCENTAR UM INTERVALO DE EIXO Y? ###?
  print(grafico_boxplot)
  
  #############=========CONFECÇÃO DO QQPLOT=============################
  
  qqplot_facet <- ggplot(nome_da_planilha, aes(sample = fluorescencia_ajustada)) +
    stat_qq() +
    stat_qq_line(color = "blue") +
    facet_grid(. ~ nome_comb) + # Separar os QQ plots por tratamento
    labs(
      title = "QQ Plots Separados por Tratamento",
      x = "Quantis Teóricos",
      y = "Quantis da Amostra"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 8, face = "bold", angle = 90, hjust = 0.2),
      plot.title = element_text(hjust = 0.5)
    )
  print(qqplot_facet)
  
  ######################CHAVE DE SELEÇÃO DO MODELO ADEQUADO##########################
    # Teste de Levene
  cat("\n### Teste de Levene ###\n")
  levene_result <- leveneTest(as.formula(paste(resposta, "~", "nome_comb")), data = nome_da_planilha)
  print(levene_result)
  
  # Escolha do modelo com base na normalidade e homogeneidade de variância
  compilacao_caracteristicas <- data.frame()
  normalidade_grupo <- data.frame()
  distribuicao_grupo <- data.frame()
  
  for (z in (1:(length(tabela_shapiro$Grupo)))){ #Esse loop é importante para que todos valores de p sejam analisados.
    if (tabela_shapiro$p_value[z] > 0.05){ #Indica que estão sendo selecionados dados paramétricos. O "z" será substituído pelos números de seu vetor, que por sua vez correspondem aos grupos CT DMSO (1), CT Droga1 (2), CT Droga2 (3), Combinação (4
      dado <- "normal" #Se valor de p de shapiro-wilk for superior a 0.05, o grupo é paramétrico.
      if (levene_result$`Pr(>F)`[1] > 0.05) { #Valor de p superior a 0,05 indica que os dados são homogêneos. Dados homogêneos serão avaliados por ANOVA seguido pelo pós teste de Tukey
        distribuicao <- "homogeneo"
      } else { #Aqui inicia a chave de seleção que trata de dados paramétricos com distribuição heterogênea (valor de p de levene inferior a 0.05)
        distribuicao <- "heterogeneo"
      }
    } else { #Se o teste de Shapiro Wilk resultar em p inferior a 0.05 em qualquer um dos grupos, os dados serão considerados não paramétricos.
      dado <- "nao-parametrico"
      distribuicao <- "nao-parametrico"
      
    }
    normalidade_grupo <- c(normalidade_grupo, paste(print (dado))) #Coleta todos dados de normalidade, verificado pelo teste de Shapiro Wilk, gerados durante a condicional.
    distribuicao_grupo <- c(distribuicao_grupo, paste(print (distribuicao))) #Coleta todos dados de distribuição, verificado pelo teste de Levene, gerados durante a condicional.
  }
  compilacao_caracteristicas <- as.data.frame (cbind (tabela_shapiro$Grupo, tabela_shapiro$p_value, normalidade_grupo, distribuicao_grupo)) #Agrega os dados identificadores dos grupos, valores de p de shapiro wilk, informações quanto à normalidade e distribuição dos dados.
  colnames(compilacao_caracteristicas) <- c("grupos", "p de shapiro-wilk", "analise_normalidade", "analise_homogeneidade") #Renomea as colunas deste dataframe.
  
  checagem_naoparametrico <- str_match(compilacao_caracteristicas$analise_homogeneidade, "nao-parametrico")
  checagem_naoparametrico <- checagem_naoparametrico[!is.na(checagem_naoparametrico)]
  checagem_heterogeneo <- str_match(compilacao_caracteristicas$analise_homogeneidade, "heterogeneo")
  checagem_heterogeneo <- checagem_heterogeneo[!is.na(checagem_heterogeneo)]
  
  if (length(checagem_naoparametrico) >= 1){
    teste_e_posteste <- "Kruskal wallis seguido de pós-teste de Dunn com correção de Holm"
    modelo <- kruskal.test(as.formula(paste(resposta, "~", "nome_comb")), data = nome_da_planilha) #Dados não paramétricos deverão usar o teste estatístico de Kruskal Wallis.
    pos_teste <- dunn_test(nome_da_planilha, as.formula(paste(resposta, "~", "nome_comb")), p.adjust.method = "holm") #A análise de dados não paramétricos será seguida do pós-teste de comparações múltiplas de Dunn com correção de Bonferroni
    #compilacao_caracteristicas <- cbind(compilacao_caracteristicas, teste_e_posteste[z]) #ONDE POSICIONAR ESSA LINHA PARA CONSEGUIR CAPTURAR, INDIVIDUALMENTE, A NORMALIDADE DE CADA GRUPO?
  } else if (length(checagem_heterogeneo) >= 1){
    teste_e_posteste <- "Anova com correção de Welch e pós-teste de Games-Howell"
    modelo <- oneway.test(as.formula(paste(resposta, "~", "nome_comb")), data = nome_da_planilha, var.equal = FALSE)
    pos_teste <- nome_da_planilha %>% games_howell_test(as.formula(paste(resposta, "~", "nome_comb")))
    print ("parametrico heterogeneo")
    #compilacao_caracteristicas <- cbind(compilacao_caracteristicas, teste_e_posteste[z]) #ONDE POSICIONAR ESSA LINHA PARA CONSEGUIR CAPTURAR, INDIVIDUALMENTE, A NORMALIDADE DE CADA GRUPO?
  } else if ((length(checagem_naoparametrico) == 0) & (length(checagem_heterogeneo) == 0)){
    teste_e_posteste <- "Anova padrão seguido por pós-teste de Tukey"
    modelo <- aov(as.formula(paste(resposta, "~", "nome_comb")), data = nome_da_planilha) #Fórmula do ANOVA
    pos_teste <- TukeyHSD(modelo)
    print ("parametrico homogeneo")
    #compilacao_caracteristicas <- cbind(compilacao_caracteristicas, teste_e_posteste[z]) #ONDE POSICIONAR ESSA LINHA PARA CONSEGUIR CAPTURAR, INDIVIDUALMENTE, A NORMALIDADE DE CADA GRUPO?
  }
  
  #!Considerar reordenar a sequência dos gráficos#!
  print (teste_e_posteste)
  print (modelo)
  print (pos_teste)
  
  ###########======FORMATANDO RESULTADOS ESTATÍSTICOS PARA ANEXAR EM UM DATAFRAME==============################
  # Converter os resultados para um data.frame e formatar p adj para 4 casas decimais.
  ##AJUSTAR UMA CHAVE DE SELEÇÃO QUE PERMITA TRABALHAR COM OS RESULTADOS NO FORMATO RECEBIDO PELO PÓS-TESTE DE TUKEY, GAMES-HOWELL OU DUNN (NÃO PARAMÉTRICO)
  if (grepl ("Tukey", teste_e_posteste, fixed = T) == T) {
    pos_teste_df <- as.data.frame(pos_teste$nome_comb)#, row.names = id)
    pos_teste_df <- rownames_to_column(pos_teste_df, "Pairs")
  } else {#if (grepl ("Game", teste_e_posteste, fixed = T) == T){
    pos_teste_df <- as.data.frame (pos_teste)
    comparativo <- c(paste(pos_teste_df$group1, "-", pos_teste_df$group2))
    pos_teste_df <- as.data.frame(cbind (comparativo, pos_teste$p.adj))
    colnames(pos_teste_df) <- c("Pairs", "p adj")
  }
  pos_teste_df <- as.data.frame(cbind(pos_teste_df$Pairs, pos_teste_df$`p adj`))
  colnames(pos_teste_df) <- c("Pairs", "p adj")
  
  significancia <- with (pos_teste_df, ifelse (`p adj` <= 0.001, "***", ifelse (`p adj` <= 0.01 & `p adj` > 0.0011, "**", ifelse (`p adj` <= 0.05 & `p adj` > 0.011, "*", NA))))
  pos_teste_df <- cbind (pos_teste_df, significancia)
  
  # Filtrar apenas os pares significativos (p < 0.05)
  pos_teste_df1 <- pos_teste_df %>%  filter(`p adj` < 0.05) #Criando um grupo que tenha somente os dados estatisticamente significativos
  
  # Configurar os pares para geom_signif. É necessário uma condicional porque o objeto originado do pós-teste de Games-Howell possui um espaço antes e depois do hífen.
  if (grepl ("Tukey", teste_e_posteste, fixed = T) == T){
    comparisons <- strsplit(pos_teste_df1$Pairs, "-")
  } else {
    comparisons <- strsplit(pos_teste_df1$Pairs, " - ")
  }
  
  ############===================FAZENDO O GRÁFICO DE BARRAS=====================#############
  # Calculando o valor de início de barras de erros para gráfico de valores absolutos (considerando o dataframe "resumo_grupos", será feita a somatória dos valores das colunas "media" e "desvio_padrao". O maior valor será escolhido, posteriormente será acrescido de 10% e usado como início das anotações)
  id <- as.data.frame(unique(nome_da_planilha$nome_comb)) #criando objeto com os nomes dos grupos, que constarão na identificação das linhas (por algum motivo não aceitou o que já estava pronto no objeto "resumo_grupos")
  somatoria <- as.data.frame(c(resumo_grupos$media + resumo_grupos$desvio_padrao)) # Coluna com os valores das somatórias
  somatoria_acrescida_dezporcento <- c(somatoria$`c(resumo_grupos$media + resumo_grupos$desvio_padrao)` + somatoria$`c(resumo_grupos$media + resumo_grupos$desvio_padrao)`*0.1)
  somatoria_acrescida_dezporcento <- as.data.frame(somatoria_acrescida_dezporcento)
  calculo_posicao_inicio_anotacao <- as.data.frame(cbind(id,resumo_grupos$media, resumo_grupos$desvio_padrao, somatoria, somatoria_acrescida_dezporcento)) #criando o novo df com os nomes dos grupos e os dados relevantes
  colnames (calculo_posicao_inicio_anotacao) <- c("grupos", "media", "desvio_padrao", "somatoria", "somatoria_corrigida") #Renomeando as colunas do dataframe gerado.
  barra_sig <- max(calculo_posicao_inicio_anotacao$somatoria_corrigida)
  correcao_anotacao <- length(comparisons)
  correcao_ajustada <- correcao_anotacao*(barra_sig/4)
  expansao_eixoy_anotacao <- barra_sig + correcao_ajustada
  aux_ticks_eixo_y <- max(calculo_posicao_inicio_anotacao$somatoria)
  rm (id, somatoria, somatoria_acrescida_dezporcento) #Removendo objetos temporários
  
  ##Ajustando os ticks da barra vertical ao valor máximo esperado
  if (aux_ticks_eixo_y <= 30000){
    tique <- 7500
  } else if (aux_ticks_eixo_y > 30000 & aux_ticks_eixo_y <= 40000){
    tique <- 10000
  } else if (aux_ticks_eixo_y > 40000 & aux_ticks_eixo_y <= 50000){
    tique <- 12500
  } else if (aux_ticks_eixo_y > 50000 & aux_ticks_eixo_y <= 60000){
    tique <- 15000
  } else if (aux_ticks_eixo_y > 60000 & aux_ticks_eixo_y <= 70000){
    tique <- 17500
  } else if (aux_ticks_eixo_y > 70000 & aux_ticks_eixo_y <= 80000){
    tique <- 20000
  } else if (aux_ticks_eixo_y > 80000 & aux_ticks_eixo_y <= 90000){
    tique <- 22500
  } else if (aux_ticks_eixo_y > 90000 & aux_ticks_eixo_y <= 100000){
    tique <- 25000
  } else if (aux_ticks_eixo_y > 100000 & aux_ticks_eixo_y <= 110000){
    tique <- 27500
  } else if (aux_ticks_eixo_y > 110000 & aux_ticks_eixo_y <= 120000){
    tique <- 30000
  } else if (aux_ticks_eixo_y > 120000 & aux_ticks_eixo_y <= 130000){
    tique <- 32500
  } else if (aux_ticks_eixo_y > 130000 & aux_ticks_eixo_y <= 140000){
    tique <- 35000
  } else if (aux_ticks_eixo_y > 140000 & aux_ticks_eixo_y <= 150000){
    tique <- 37500
  } else if (aux_ticks_eixo_y > 150000 & aux_ticks_eixo_y <= 160000){
    tique <- 40000
  } else if (aux_ticks_eixo_y > 160000 & aux_ticks_eixo_y <= 170000){
    tique <- 42500
  } else if (aux_ticks_eixo_y > 170000 & aux_ticks_eixo_y <= 180000){
    tique <- 45000
  } else if (aux_ticks_eixo_y > 180000 & aux_ticks_eixo_y <= 190000){
    tique <- 47500
  } else {
    tique <- 50000
}
  #Confecção do gráfico de valor absoluto
  # nome_da_planilha$funcao <- factor(nome_da_planilha$funcao, levels = c("CT DMSO", "CT ind droga1", "CT ind droga2", "CT ind droga3", "CT dup droga1 droga2", "CT dup droga1 droga3", "CT dup droga2 droga3", "Combinacao"), ordered = T)
  grafico_grupos <- ggplot(nome_da_planilha, aes(x = !!sym("nome_comb"), y = !!sym(resposta), fill = nome_comb)) + #Ao adicionar o fill com a referência à coluna "droga2" do dataframe "nome_da_planilha", as barras foram coloridas automaticamente. Verificar as cores presentes no pacote "RColorBrewer" com o comando "colors", escolher 4 cores, criar uma nova coluna e associar cada cor a um grupo do "droga2"
   stat_summary( #Ao anexar a barra de erro na segunda camada, a camada seguinte fará a sobreposição na parte inferior da barra de erro.
     fun.data = mean_sdl, #Linha de desvio padrão para cada barra
     fun.args = list(mult = 1), #? QUAL É O OBJETIVO DESSA LINHA?
     geom = "errorbar",
     width = 0.1,
     color = "black"
   ) +
   scale_fill_grey(start = 0.2,
                   end = 0.8, na.value = "red"
   ) +
   stat_summary(
      fun = mean, #Indica que o valor principal representado será a média
      geom = "col", #Indica que serão usadas barras para representar a média
      width = 0.9, #Largura das barras
   ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none", #Essa linha remove o quadro que informa a legenda, visto que já está presente no eixo x. Ideal para gráficos que serão apresentados individualmente. Para gráficos que serão apresentados em conjunto com outros (exemplo: mesmas drogas, células diferentes), recomendo retirar essa linha e retirar os nomes dos eixos x.
      axis.text.y = element_text(color = 'black'), #Configuração dos títulos do eixo Y.
      axis.title.x = element_text(margin = margin(t = 10))# Ajusta a posição do título do eixo x
      ) +
    labs(x = "Tratamento", y = resposta)
 
 if (length(comparisons) > 0){
   grafico_grupos <- grafico_grupos + 
     geom_signif(
     comparisons = comparisons, # Adicionar os pares
     annotations = pos_teste_df1$significancia, # Adicionar o simbolo de significativo ou o valor de "p", a depender aa referencia
     #annotations = paste('p =', round(post_test_df1$p adj, 3)), # Adiciona o prefixo "p = " aos p-valores ajustados
     y_position = c(max(calculo_posicao_inicio_anotacao$somatoria_corrigida)), #indica a posição para a primeira barra de significância. As demais ficarão acima dela.
     step_increase = 0.15, #Distância entre as diferentes barras de significância
     margin_top = 0.01,
     map_signif_level = T,
     textsize = 4,
     ) 
 } else {
   print ("Não houveram diferenças estatisticamente significativas entre os grupos.")
 }
 grafico_grupos <- grafico_grupos + 
  scale_y_continuous ( #Configura o tamanho do eixo Y.
    limits = c(0, expansao_eixoy_anotacao), # Define os limites reais de todos elementos que serão mostrados no eixo Y.
    breaks = 0:4 * tique, # Define quais pontos do eixo Y serão mostrados
    expand = expansion(mult = c(0, 0)) # Ativar essa linha cria um pequeno espaço entre a linha de 0 ("0") do eixo y e a linha do eixo x. Se remover, o eixo x coincide com o ponto 0 do eixo y.
  )
 # grafico <- grafico +
 # scale_y_cut(breaks=c(1), which=c(1), scales=c(2.00))
 grafico_grupos <- grafico_grupos + 
   annotate("segment", x = 0.5, xend = 0.5, y = 0, yend = barra_sig #O "yend" define até onde irá a linha do eixo y, mas não define o seu tamanho.
   ) + #Essa #Essa parte de modificação de tema do eixo Y *deve* ficar no final, pois ela fará a modificação do tamanho do eixo y, retirando o valor acima do definido como limite superior no "yend" do "annotate". Se ela for movida para um trecho anterior do código, ela será sobreposta por outras camadas e perderá sua função.
   theme(axis.line.y = element_blank(),
         axis.title.x = element_blank(),# Ajusta a posição do título do eixo x
         axis.text.x = element_text(angle = 45, hjust = 1.0),
         axis.title.y = element_text(angle = 90, hjust = 0.15, size = 10))
 
 print(grafico_grupos)

  ##########======GERAÇÃO DE GRÁFICOS DE COLUNA DE PORCENTAGEM RELATIVA AO CT DMSO==============################
 # Calculando o valor de início de barras de erros para gráfico de valores relativos (considerando o dataframe "resumo_porcentagens", será feita a somatória dos valores das colunas "media" e "desvio_padrao". O maior valor será escolhido, posteriormente será acrescido de 10% e usado como início das anotações)
 id_porcent <- as.data.frame(unique(resumo_porcentagens$nome_comb)) #criando objeto com os nomes dos grupos, que constarão na identificação das linhas (por algum motivo não aceitou o que já estava pronto no objeto "resumo_grupos")
 somatoria_porcent <- as.data.frame(c(resumo_porcentagens$media + resumo_porcentagens$desvio_padrao)) # Coluna com os valores das somatórias
 colnames(somatoria_porcent) <- c("media_com_desvpad")
 somatoria_porcent_acrescida_dezporcento <- c(somatoria_porcent$media_com_desvpad * 1.1)
 somatoria_porcent_acrescida_dezporcento <- as.data.frame(somatoria_porcent_acrescida_dezporcento)
 calculo_posicao_inicial_porcent <- as.data.frame(cbind(id_porcent,resumo_porcentagens$media, resumo_porcentagens$desvio_padrao, somatoria_porcent, somatoria_porcent_acrescida_dezporcento)) #criando o novo df com os nomes dos grupos e os dados relevantes
 colnames (calculo_posicao_inicial_porcent) <- c("grupos", "media", "desvio_padrao", "somatoria", "somatoria_corrigida") #Renomeando as colunas do dataframe gerado.
 barra_sig_porcent <- max(calculo_posicao_inicial_porcent$somatoria_corrigida)
 correcao_anotacao_porcent <- length(comparisons)
 correcao_ajustada_porcent <- correcao_anotacao_porcent*(barra_sig_porcent/4)
 expansao_eixoy_anotacao_porcent <- barra_sig_porcent + correcao_ajustada_porcent
 aux_ticks_eixo_y_porcent <- max(calculo_posicao_inicial_porcent$somatoria)
 rm (id_porcent, somatoria_porcent, somatoria_porcent_acrescida_dezporcento,calculo_posicao_inicial_porcent) #Removendo objetos temporários
 
 ##Ajustando os ticks da barra vertical ao valor máximo esperado
 if (aux_ticks_eixo_y_porcent <= 100){
   tique_porcent <- 25
 } else if (aux_ticks_eixo_y_porcent > 100 & aux_ticks_eixo_y_porcent <= 120){
   tique_porcent <- 30
 } else if (aux_ticks_eixo_y_porcent > 120 & aux_ticks_eixo_y_porcent <= 140){
   tique_porcent <- 35
 } else if (aux_ticks_eixo_y_porcent > 140 & aux_ticks_eixo_y_porcent <= 160){
   tique_porcent <- 40
 } else if (aux_ticks_eixo_y_porcent > 160 & aux_ticks_eixo_y_porcent <= 180){
   tique_porcent <- 45
 } else if (aux_ticks_eixo_y_porcent >= 200 & aux_ticks_eixo_y_porcent < 300){
   tique_porcent <- 50
 } else {
   tique_porcent <- 100
 }
 
 ############
 porcentagens_individual$nome_comb <- factor(porcentagens_individual$nome_comb, levels = c ("CT DMSO", nome_ind3, nome_ind1, nome_ind2, nome_dup12,nome_dup13, nome_dup23,nome_comb), ordered = T)
 # porcentagens_individual$nome_comb <- as.factor(porcentagens_individual$nome_comb) #Organização da planilha resultante para gráficos
 # porcentagens_individual$nome_comb <- factor(porcentagens_individual$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "SP100030 (10 uM) + Erteberel (1 nM)", "Tamoxifeno (10 uM) + SP100030 (10 uM) + Erteberel (1 nM)")) #Organização da planilha resultante para gráficos 
 # porcentagens_individual$nome_comb <- factor(porcentagens_individual$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "SP100030 (10 uM) + Erteberel (10 nM)", "Tamoxifeno (10 uM) + SP100030 (10 uM) + Erteberel (10 nM)")) #Organização da planilha resultante para gráficos 
 # porcentagens_individual$nome_comb <- factor(porcentagens_individual$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "Tanshinona IA (8 uM) + Erteberel (1 nM)", "Tamoxifeno (10 uM) + Tanshinona IA (8 uM) + Erteberel (1 nM)")) #Organização da planilha resultante para gráficos 
 # porcentagens_individual$nome_comb <- factor(porcentagens_individual$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "Tanshinona IA (8 uM) + Erteberel (10 nM)", "Tamoxifeno (10 uM) + Tanshinona IA (8 uM) + Erteberel (10 nM)")) #Organização da planilha resultante para gráficos 
 # porcentagens_individual$nome_comb <- factor(porcentagens_individual$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "Tanshinona IIA (8 uM) + Erteberel (1 nM)", "Tamoxifeno (10 uM) + Tanshinona IIA (8 uM) + Erteberel (1 nM)")) #Organização da planilha resultante para gráficos 
 # porcentagens_individual$nome_comb <- factor(porcentagens_individual$nome_comb, levels = c("CT DMSO","Tamoxifeno (10 uM)", "Tanshinona IIA (8 uM) + Erteberel (10 nM)", "Tamoxifeno (10 uM) + Tanshinona IIA (8 uM) + Erteberel (10 nM)")) #Organização da planilha resultante para gráficos 
 
 
 ############CONFECÇÃO GRÁFICO
 grafico_porcentagem <- ggplot(porcentagens_individual, aes(x = nome_comb, y = porcentagem, fill = nome_comb)) + #Ao adicionar o fill com a referência à coluna "droga2" do dataframe "nome_da_planilha", as barras foram coloridas automaticamente. Verificar as cores presentes no pacote "RColorBrewer" com o comando "colors", escolher 4 cores, criar uma nova coluna e associar cada cor a um grupo do "droga2"
   stat_summary( #Ao anexar a barra de erro na segunda camada, a camada seguinte fará a sobreposição na parte inferior da barra de erro.
     fun.data = mean_sdl, #Linha de desvio padrão para cada barra
     fun.args = list(mult = 1), #? QUAL É O OBJETIVO DESSA LINHA?
     geom = "errorbar",
     width = 0.1,
     color = "black"
   ) +
   scale_fill_grey(start = 0.2,
                   end = 0.8, na.value = "red"
   ) +
   stat_summary(
     fun = mean, #Indica que o valor principal representado será a média
     geom = "col", #Indica que serão usadas barras para representar a média
     width = 0.9, #Largura das barras
   ) +
   theme_classic(base_size = 12) +
   theme(
     legend.position = "none", #Essa linha remove o quadro que informa a legenda, visto que já está presente no eixo x. Ideal para gráficos que serão apresentados individualmente. Para gráficos que serão apresentados em conjunto com outros (exemplo: mesmas drogas, células diferentes), recomendo retirar essa linha e retirar os nomes dos eixos x.
     axis.text.y = element_text(color = 'black'), #Configuração dos títulos do eixo Y.
   ) +
   labs(y = paste("Proliferação normalizada (%) \n", nome_da_planilha$tipo_celula[1], sep = ""))
 
 if (length(comparisons) > 0){
   grafico_porcentagem <- grafico_porcentagem + 
     geom_signif(
       comparisons = comparisons, # Adicionar os pares
       annotations = pos_teste_df1$significancia, # Adicionar o simbolo de significativo ou o valor de "p", a depender aa referencia
       y_position = barra_sig_porcent, #indica a posição para a primeira barra de significância. As demais ficarão acima dela.
       step_increase = 0.15, #Distância entre as diferentes barras de significância
       margin_top = 0.01,
       map_signif_level = T,
       textsize = 4,
     ) 
 } else {
   print ("Não houveram diferenças estatisticamente significativas entre os grupos.")
 }
 grafico_porcentagem <- grafico_porcentagem + 
   scale_y_continuous ( #Configura o tamanho do eixo Y.
     limits = c(0, 1000), # Define os limites reais de todos elementos que serão mostrados no eixo Y.
     breaks = 0:4 * 100, # Define quais pontos do eixo Y serão mostrados
     expand = expansion(mult = c(0, 0)) # Ativar essa linha cria um pequeno espaço entre a linha de 0 ("0") do eixo y e a linha do eixo x. Se remover, o eixo x coincide com o ponto 0 do eixo y.
   )
 
 grafico_porcentagem <- grafico_porcentagem + 
   annotate("segment", x = 0.5, xend = 0.5, y = 0, yend = 400 #O "yend" define até onde irá a linha do eixo y, mas não define o seu tamanho.
   ) + #Essa #Essa parte de modificação de tema do eixo Y *deve* ficar no final, pois ela fará a modificação do tamanho do eixo y, retirando o valor acima do definido como limite superior no "yend" do "annotate". Se ela for movida para um trecho anterior do código, ela será sobreposta por outras camadas e perderá sua função.
   theme(axis.line.y = element_blank(),
         axis.title.x = element_blank(),# Ajusta a posição do título do eixo x
         axis.text.x = element_text(angle = 45, hjust = 1.0),
         axis.title.y = element_text(angle = 90, hjust = 0.2, size = 10))
 
 print(grafico_porcentagem)
  
  #########======SALVAMENTO AUTOMÁTICO DE GRÁFICOS E PLANILHAS, EXCLUSIVO PARA EXECUÇÃO EM FORMATO .R, (NÃO USAR COM .RMD)======#########
 #Salvamento automático das listas
 ##Criando data.frame que deverá conter os nomes dos arquivos com resultados originais de cada experimento
 # id_placa <- as.character(nome_da_planilha$id_placa[1])
 # id_placa <- as.data.frame(id_placa)
 # id_placa <- rbind (rep(id_placa), 28)
 # id_placa <- as.data.frame(rep(id_placa, length(pos_teste_df$Pairs)))
 # id_placa <- pivot_longer(id_placa, cols = starts_with("..."), names_to = "id_placa", values_to = "info")
 
 ##Objeto que receberá o menor valor de p de shapiro wilk
 aux_metodo_shapiro <- as.numeric(compilacao_caracteristicas$`p de shapiro-wilk`) #Separando a coluna com valores de p de shapiro wilk em um objeto, e também convertendo o objeto para a classe de números.
 metodo_shapiro <- min(aux_metodo_shapiro) #Selecionando o valor mínimo de p de shapiro. Esse valor ditará se o fluxo de análise será paramétrico (se o menor valor de p for superior a 0.05) ou não paramétrico (se p < 0.05)
 # metodo_shapiro <- as.data.frame(rep(metodo_shapiro, length(pos_teste_df$Pairs)))
 
 vetor_linhagem <- as.character(nome_da_planilha$tipo_celula [1:8])
 
 vetor_grupo <- as.character(resumo_grupos$nome_comb[1:8]) #Extrair o conteúdo dos grupos como caractere para um novo objeto
 # vetor_grupo <- c(vetor_grupo, NA, NA) #Adicionando dois "NA" ao final para ficar com mesmo tamanho do restante do dataframe ao qual se juntará
 
 vetor_n <- resumo_grupos$n[1:8]
 # vetor_n <- c(vetor_n, NA, NA)
 
 vetor_resposta <- resumo_grupos$media [1:8]
 # vetor_resposta <- c(vetor_resposta, NA, NA)
 
 vetor_porcentagem <- resumo_porcentagens$media
 # vetor_porcentagem <- c(vetor_porcentagem, NA, NA)
 
 lista_dados_estatisticos <- as.data.frame(cbind(tituloImagem, vetor_linhagem, metodo_shapiro, levene_result$`Pr(>F)`[1], teste_e_posteste,pos_teste_df$Pairs, pos_teste_df$`p adj`, pos_teste_df$significancia, vetor_grupo, vetor_n, vetor_resposta, vetor_porcentagem))
 colnames(lista_dados_estatisticos) <- c("Objeto analisado", "Linhagem celular", "Menor valor de p de shapiro", "Teste de Levene", "Teste e pós-teste estatísticos usados", "Comparações", "Valor de p das comparações", "Significância", "Grupos Individuais", "N amostral", "Media de resposta dos grupos", "Porcentagem de resposta em relação ao CT DMSO")
 
##Exportando "lista_dados_estatisticos" como um arquivo excel (.xlsx)
 planilha_existente <- createWorkbook() #Cria o objeto da classe necessária para gerar uma planilha excel
 addWorksheet(planilha_existente, tituloImagem) # Adiciona uma aba ao objeto da planilha criada
 writeData(planilha_existente, tituloImagem, lista_dados_estatisticos, colNames = T) #Escreve os dados no arquivo planilha
 saveWorkbook(planilha_existente, file = paste(getwd(),"/relatorios_grupos_selecionados/", "compilado_estatistico_", tituloImagem, ".xlsx", sep=""), overwrite = T) #Salva o arquivo planilha
 
 #Salvamento automático dos gráficos
 # ggsave(paste(getwd(),"/graficos_grupos_selecionados/boxplot_grupos_selecionados",tituloImagem,".jpg", sep=""), plot = grafico_boxplot, width = 10, height = 7, dpi = 500)
 # ggsave(paste(getwd(),"/graficos_grupos_selecionados/qqplot_grupos_selecionados",tituloImagem,".jpg", sep=""), plot = qqplot_facet, width = 10, height = 7, dpi = 500) #Exporta a imagem de gráfico de qqplot
 # ggsave(paste(getwd(),"/graficos_grupos_selecionados/colunasAbsoluto_grupos_selecionados_",tituloImagem,".jpg", sep=""), plot = grafico_grupos, width = 10, height = 7, dpi = 500)
 ggsave(paste(getwd(),"/graficos_grupos_selecionados/", vetor_linhagem[1], "_", "tripla_colunasRelativo_grupos_selecionados_", tituloImagem,".jpg",sep=""), plot = grafico_porcentagem, width = 10, height = 7, dpi = 500)
 # write.csv(nome_da_planilha, paste("./dataframes/tripla_grupos_selecionados_",tituloImagem, ".csv", sep = ""), row.names = F, quote = F)
 save(grafico_grupos, file = paste(getwd(),"/objetos_graficos/", vetor_linhagem[1], "_", "tripla_fluorescencia_grupos_selecionados_", tituloImagem, ".RData", sep = ""))
 save(grafico_porcentagem, file = paste(getwd(),"/objetos_graficos/", vetor_linhagem[1], "_", "tripla_porcentagens_grupos_selecionados_", tituloImagem, ".RData", sep = ""))
}

