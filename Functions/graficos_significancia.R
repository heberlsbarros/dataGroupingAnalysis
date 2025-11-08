graficos_significancia <- function (nome_da_planilha, tituloImagem, resposta, grupo) {
  ######################==========INSTALAÇÃO E CARREGAMENTO DE PACOTES===========######################
  #Instalação e carregamento de pacotes necessários
  ## Listando os pacotes necessários
  pacotes <- c("dplyr", "tibble", "writexl", "readxl", "ggsignif", "ggplot2", "rlang", "rstatix", "Hmisc", "tidyr", "car", "multcomp", "RColorBrewer", "ggbreak", "stringr", "ggpattern")
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
  #Obtendo o nome das drogas 1 e 2 de maneira automática:
  ##Obtendo o nome da droga 1
  nome_droga_1 <- nome_da_planilha %>% filter (droga1 != "CT DMSO" & droga1 != "Nada") #Criando um dataframe no qual foram excluídos os valores "CT DMSO" e "Nada" da coluna "droga1". Dessa forma, ficaram somente os grupos que possuem o nome da droga 1.
  nome_droga_1 <- as.character(unique (nome_droga_1$droga1)) #Obtendo os valores que estão presentes na coluna "droga1", que será somente o nome da droga 1. Convertendo esse output para o formato de caracteres.
  concentracao_droga_1 <- nome_da_planilha %>% filter (droga1 == nome_droga_1)
  concentracao_droga_1 <- as.character(unique (concentracao_droga_1$concentracao_droga1))
  cat(paste("###\nA droga 1 deste grupo é: **", nome_droga_1, "**\n A concentração dessa droga é: **", concentracao_droga_1, "**")) #Confirmação do conteúdo do objeto "nome_droga_1"
  
  ##Obtendo o nome da droga 2
  nome_droga_2 <- nome_da_planilha %>% filter (droga2 != "Nada" & droga2 != "droga2 sozinha" & droga2 != "CT DMSO") #Criando um dataframe no qual foram excluídos os valores "droga2 sozinha" e "Nada" da coluna "droga2". Dessa forma, ficaram somente os grupos que possuem o nome da droga 2.
  nome_droga_2 <- as.character(unique (nome_droga_2$droga2)) #Obtendo os valores que estão presentes na coluna "droga2", que será somente o nome da droga 2. Convertendo esse output para o formato de caracteres.
  concentracao_droga_2 <- nome_da_planilha %>% filter (droga2 == nome_droga_2)
  concentracao_droga_2 <- as.character(unique (concentracao_droga_2$concentracao_droga2))
  cat(paste("###\nA droga 2 deste grupo é: **", nome_droga_2, "**\n A concentração dessa droga é: **", concentracao_droga_2, "**")) #Confirmação do conteúdo do objeto "nome_droga_2"
  
  ##Combinação de ambos nomes
  
  nome_comb1 <- paste(nome_droga_1, "(", concentracao_droga_1, ")", sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  nome_comb2 <- paste(nome_droga_2, "(", concentracao_droga_2, ")", sep = "") #Este vetor será usado posteriormente para construção dos dataframes de análise
  nome_comb <- paste(nome_comb1, "+", nome_comb2, sep = " ") #Este vetor será usado posteriormente para construção dos dataframes de análise
  
  #Isolando os grupos experimentais em dataframes distintos, renomeando da maneira adequada para juntar novamente ao final, na ordem desejada
  ## Grupo CT DMSO:
  ### Trocar a identificação de "Nada" da coluna "droga2" para "CT DMSO"
  grupo_CTDMSO <- nome_da_planilha %>% filter (droga1 == "CT DMSO") #Fazendo um dataframe no qual os valores do grupo CT DMSO são isolados
  levels(grupo_CTDMSO$droga2)[levels(grupo_CTDMSO$droga2)=='Nada']= 'CT DMSO' #Renomeando o identificador "Nada" do grupo CT DMSO para "CT DMSO"
  ## Grupo controle da droga 1:
  grupo_ctdroga1 <- nome_da_planilha %>% filter ((droga1 == nome_droga_1) & (droga2 == "Nada")) #Fazendo um dataframe no qual os valores do grupo CT droga1 são isolados
  levels(grupo_ctdroga1$droga2)[levels(grupo_ctdroga1$droga2)=='Nada']= c(paste(nome_comb1))#, "(", grupo_ctdroga1)) #Renomeando os valores da coluna "droga2" para o mesmo descrito na coluna "droga1" 
  ## Grupo controle da droga 2:
  grupo_ctdroga2 <- nome_da_planilha %>% filter (droga2 == 'droga2 sozinha') #Fazendo um dataframe no qual os valores do grupo CT droga2 são isolados
  levels(grupo_ctdroga2$droga2)[levels(grupo_ctdroga2$droga2)=='droga2 sozinha']= c(paste(nome_comb2)) #
  ## Grupo combinação:
  grupo_comb <- nome_da_planilha %>% filter ((droga1 == nome_droga_1) & (droga2 == nome_droga_2)) #Fazendo um dataframe no qual os valores do grupo combinação são isolados
  levels(grupo_comb$droga2)[levels(grupo_comb$droga2)==nome_droga_2]= nome_comb #A coluna "droga2" terá o nome de ambas drogas.
  
  nome_da_planilha <- rbind(grupo_CTDMSO, grupo_ctdroga1, grupo_ctdroga2, grupo_comb) #Cria data.frame. A ordem que aparecerá no df é: CT DMSO, CT droga1, CT droga2, Combinação
  
  ###Removendo objetos não mais necessários:
  rm (grupo_CTDMSO, grupo_ctdroga1, grupo_ctdroga2, grupo_comb)
  
  ######################==========ANÁLISES DE NORMALIDADE E HETEROGENEIDADE===========######################
  
  #Análises estatísticas necessárias para descoberta do valor de p:
  ##Estatística descritiva básica
  cat("\n### Resumo Descritivo por Grupo ###\n")
  resumo_grupos <- nome_da_planilha %>%
    group_by(!!sym(grupo)) %>%
    summarise(
      n = n(),
      media = mean(!!sym(resposta), na.rm = TRUE),
      desvio_padrao = sd(!!sym(resposta), na.rm = TRUE)
    ) %>%
    ungroup()
  print(resumo_grupos)
  
  ## Teste de Shapiro-Wilk por Grupo: identificará se o grupo tem distribuição paramétrica ou não paramétrica.
  cat("\n### Teste de Normalidade (Shapiro-Wilk) por Grupo ###\n")
  resultados_shapiro <- by(nome_da_planilha[[resposta]], nome_da_planilha[[grupo]], function(x) {
    if (length(x) > 2) { #Verificar se o grupo tem mais de 2 valores.
      return(shapiro.test(x)) #Se passar na condição fixada na linha anterior, aquele grupo receberá o valor de p do teste de shapiro wilk.
    } else {
      return(NULL)  # Retorna NULL se o grupo não tiver dados suficientes (menos que 2)
    }
  }) #Ao final desse if, o dataframe "resultados_shapiro" será composto pelas identificações dos grupos e os valores de p de shapiro-wilk.

  ### Criar uma tabela com os resultados válidos
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
  
  for (grupo_atual in unique(nome_da_planilha[[grupo]])) {
    grupo_dados <- nome_da_planilha[nome_da_planilha[[grupo]] == grupo_atual, resposta]
    
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
      nome_da_planilha$outlier[nome_da_planilha[[grupo]] == grupo_atual] <- outliers #Ao final do for, adiciona uma coluna indicando se aquele valor é um outlier.
    }
  }
  nome_da_planilha2 <- nome_da_planilha %>% filter (is.na(outlier)) #Seleciona os valores que não são outliers, que são aqueles cuja coluna "outliers" tem valor diferente de "NA"
  
  print("Relação de outliers identificados:\n ")
  print(nome_da_planilha2[nome_da_planilha2$outlier == TRUE, ])
  
  ################CONFECÇÃO DO BOXPLOT#####################
  # Boxplot
  cat("\n### Boxplot ###\n")
  grafico_boxplot <- ggplot(nome_da_planilha, aes_string(x = grupo, y = resposta, fill = grupo)) +
    geom_boxplot() +
    labs(title = "Boxplot dos grupos", x = "Tratamento", y = resposta) +
    theme_minimal( #Inicia a parte estética
     ) +
  scale_fill_grey(start = 0.4, #Define o padrão de cores de preenchimento dos gráficos
                  end = 1.0, na.value = "red"
  ) +
    theme_classic(base_size = 12,) +
    theme(
      axis.text.x = element_text(
        color = 'black'),
        # angle = 45, #Ângulo dos títulos dos grupos do eixo x do gráfico
        # vjust = 0.4, # Ajuste vertical do título do eixo x
        # hjust = 0.3), # Ajuste horizontal do título do eixo y
      legend.position = "none", #Essa linha remove o quadro que informa a legenda, visto que já está presente no eixo x. Ideal para gráficos que serão apresentados individualmente. Para gráficos que serão apresentados em conjunto com outros (exemplo: mesmas drogas, células diferentes), recomendo retirar essa linha e retirar os nomes dos eixos x.
      axis.text.y = element_text(color = 'black'), #Configuração dos títulos do eixo Y.
      axis.title.x = element_text(margin = margin(t = 10)))# Ajusta a posição do título do eixo x
  #ACRESCENTAR UM INTERVALO DE EIXO Y?
  print(grafico_boxplot)
  ######################CHAVE DE SELEÇÃO DO MODELO ADEQUADO##########################
  
  # Teste de Levene
  cat("\n### Teste de Levene ###\n")
  levene_result <- leveneTest(as.formula(paste(resposta, "~", grupo)), data = nome_da_planilha)
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
    modelo <- kruskal.test(as.formula(paste(resposta, "~", grupo)), data = nome_da_planilha) #Dados não paramétricos deverão usar o teste estatístico de Kruskal Wallis.
    pos_teste <- dunn_test(nome_da_planilha, as.formula(paste(resposta, "~", grupo)), p.adjust.method = "holm") #A análise de dados não paramétricos será seguida do pós-teste de comparações múltiplas de Dunn com correção de Bonferroni
    #compilacao_caracteristicas <- cbind(compilacao_caracteristicas, teste_e_posteste[z]) #ONDE POSICIONAR ESSA LINHA PARA CONSEGUIR CAPTURAR, INDIVIDUALMENTE, A NORMALIDADE DE CADA GRUPO?
  } else if (length(checagem_heterogeneo) >= 1){
    teste_e_posteste <- "Anova com correção de Welch e pós-teste de Games-Howell"
    modelo <- oneway.test(as.formula(paste(resposta, "~", grupo)), data = nome_da_planilha, var.equal = FALSE)
    pos_teste <- nome_da_planilha %>% games_howell_test(as.formula(paste(resposta, "~", grupo)))
    print ("parametrico heterogeneo")
    #compilacao_caracteristicas <- cbind(compilacao_caracteristicas, teste_e_posteste[z]) #ONDE POSICIONAR ESSA LINHA PARA CONSEGUIR CAPTURAR, INDIVIDUALMENTE, A NORMALIDADE DE CADA GRUPO?
  } else if ((length(checagem_naoparametrico) == 0) & (length(checagem_heterogeneo) == 0)){
    teste_e_posteste <- "Anova padrão seguido por pós-teste de Tukey"
    modelo <- aov(as.formula(paste(resposta, "~", grupo)), data = nome_da_planilha) #Fórmula do ANOVA
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
    pos_teste_df <- as.data.frame(pos_teste$droga2)#, row.names = id)
    pos_teste_df <- rownames_to_column(pos_teste_df, "Pairs")
    #pos_teste_df <- cbind(pos_teste_df$`p adj`)
  } else {#if (grepl ("Game", teste_e_posteste, fixed = T) == T){
    pos_teste_df <- as.data.frame (pos_teste)
    comparativo <- c(paste(pos_teste_df$group1, "-", pos_teste_df$group2))
    #df_nome <- as.data.frame (paste(pos_teste$group1, "x", pos_teste$group2))
    pos_teste_df <- as.data.frame(cbind (comparativo, pos_teste$p.adj))
    colnames(pos_teste_df) <- c("Pairs", "p adj")
  }
  pos_teste_df <- as.data.frame(cbind(pos_teste_df$Pairs, pos_teste_df$`p adj`))
  colnames(pos_teste_df) <- c("Pairs", "p adj")
  
  significancia <- with (pos_teste_df, ifelse (`p adj` <= 0.001, "***", ifelse (`p adj` <= 0.01 & `p adj` > 0.0011, "**", ifelse (`p adj` <= 0.05 & `p adj` > 0.011, "*", NA))))
  pos_teste_df <- cbind (pos_teste_df, significancia)
  
  ###########======GERAÇÃO DE GRÁFICOS ==============################
  # Filtrar apenas os pares significativos (p < 0.05)
  pos_teste_df1 <- pos_teste_df %>%  filter(`p adj` < 0.05) #Criando um grupo que tenha somente os dados estatisticamente significativos
  
  # Configurar os pares para geom_signif. É necessário uma condicional porque o objeto originado do pós-teste de Games-Howell possui um espaço antes e depois do hífen.
  if (grepl ("Tukey", teste_e_posteste, fixed = T) == T){
    comparisons <- strsplit(pos_teste_df1$Pairs, "-")
  } else {
    comparisons <- strsplit(pos_teste_df1$Pairs, " - ")
  }
  
  # Calculando o valor de início de barras de erros (considerando o dataframe "resumo_grupos", será feita a somatória dos valores das colunas "media" e "desvio_padrao". O maior valor será escolhido, posteriormente será acrescido de 10% e usado como início das anotações)
  id <- as.data.frame(unique(nome_da_planilha$droga2)) #criando objeto com os nomes dos grupos, que constarão na identificação das linhas (por algum motivo não aceitou o que já estava pronto no objeto "resumo_grupos")
  somatoria <- as.data.frame(c(resumo_grupos$media + resumo_grupos$desvio_padrao)) # Coluna com os valores das somatórias
  somatoria_acrescida_dezporcento <- c(somatoria$`c(resumo_grupos$media + resumo_grupos$desvio_padrao)` + somatoria$`c(resumo_grupos$media + resumo_grupos$desvio_padrao)`*0.1)
  somatoria_acrescida_dezporcento <- as.data.frame(somatoria_acrescida_dezporcento)
  calculo_posicao_inicio_anotacao <- as.data.frame(cbind(id,resumo_grupos$media, resumo_grupos$desvio_padrao, somatoria, somatoria_acrescida_dezporcento)) #criando o novo df com os nomes dos grupos e os dados relevantes
  colnames (calculo_posicao_inicio_anotacao) <- c("grupos", "media", "desvio_padrao", "somatoria", "somatoria_corrigida") #Renomeando as colunas do dataframe gerado.
  barra_sig <- max(calculo_posicao_inicio_anotacao$somatoria_corrigida)
  correcao_anotacao <- length(comparisons)
  correcao_ajustada <- correcao_anotacao*(barra_sig/4)
  expansao_eixoy_anotacao <- barra_sig + correcao_ajustada
  rm (id, somatoria, somatoria_acrescida_dezporcento) #Removendo objetos temporários
  
  ##### FAZENDO O GRÁFICO
 grafico_grupos <- ggplot(nome_da_planilha, aes(x = !!sym(grupo), y = !!sym(resposta), fill = droga2)) + #Ao adicionar o fill com a referência à coluna "droga2" do dataframe "nome_da_planilha", as barras foram coloridas automaticamente. Verificar as cores presentes no pacote "RColorBrewer" com o comando "colors", escolher 4 cores, criar uma nova coluna e associar cada cor a um grupo do "droga2"
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
 
 #coord_cartesian(clip = "on", ylim = c(0, max(calculo_posicao_inicio_anotacao$somatoria_corrigida))
    #xlim(0,5)
    # scale_x_discrete( #Configura o tamanho do eixo X.
    #   expand = c(0.1, 0.2)
    #) +
    theme_classic(base_size = 12) +
    theme(
      #axis.title.y = element_text(margin = margin (b = 1000)),
      # axis.text.x = element_text(
      #   color = 'black',
      #   angle = 45,
      #   vjust = 0.2,
      #   hjust = 0.3
      #   ),
        #margin = margin(c(5,1,1,1)), # Reduz a margem superior dos rótulos
        #
      #),
      legend.position = "none", #Essa linha remove o quadro que informa a legenda, visto que já está presente no eixo x. Ideal para gráficos que serão apresentados individualmente. Para gráficos que serão apresentados em conjunto com outros (exemplo: mesmas drogas, células diferentes), recomendo retirar essa linha e retirar os nomes dos eixos x.
      #legend.title = element_text(size = 10),
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
    breaks = 0:4 * (barra_sig/4), # Define quais pontos do eixo Y serão mostrados
    expand = expansion(mult = c(0, 0)) # Ativar essa linha cria um pequeno espaço entre a linha de 0 ("0") do eixo y e a linha do eixo x. Se remover, o eixo x coincide com o ponto 0 do eixo y.
  )
 # grafico <- grafico +
 # scale_y_cut(breaks=c(1), which=c(1), scales=c(2.00))
 grafico_grupos <- grafico_grupos + 
   annotate("segment", x = 0.5, xend = 0.5, y = 0, yend = barra_sig #O "yend" define até onde irá a linha do eixo y, mas não define o seu tamanho.
   ) + #Essa #Essa parte de modificação de tema do eixo Y *deve* ficar no final, pois ela fará a modificação do tamanho do eixo y, retirando o valor acima do definido como limite superior no "yend" do "annotate". Se ela for movida para um trecho anterior do código, ela será sobreposta por outras camadas e perderá sua função.
   theme(axis.line.y = element_blank()
   )
  # grafico <- grafico + 
  #   theme(aspect.ratio = 1/1)
   
 
 # grafico_grupos #PARA TESTE. RETIRAR ESSA LINHA QUANDO TERMINAR A FUNÇÃO.
 #return(grafico_grupos)
 
 #########======SALVAMENTO AUTOMÁTICO DE GRÁFICOS, EXCLUSIVO PARA EXECUÇÃO EM FORMATO .R, NÃO .RMD======#########
 ggsave(paste("boxplot_",tituloImagem,".jpg", sep=""), plot = grafico_boxplot, width = 10, height = 7, dpi = 500)
 ggsave(paste("colunas_",tituloImagem,".jpg", sep=""), plot = grafico_grupos, width = 10, height = 7, dpi = 500)
}