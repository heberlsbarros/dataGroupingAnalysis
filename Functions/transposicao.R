transposicao <- function(){
  # SEÇÃO 1: GERAÇÃO DOS ARQUIVOS TRANSPOSTOS
  # Instalando os pacotes necessários:
  
  ## Listando os pacotes necessários
  pacotes <- c("writexl",#permite criar planilhas, formato .xlsx 
               "openxlsx",#permite ler planilhas, formato .xlsx 
               "dplyr",#,#fornece comandos de sintaxe que facilitam a manipulação de objetos, especialmente dataframes
               "tidyverse")#coleção de pacotes que traz ferramentas básicas para outros pacotes, como ggplot2 
  ### Checando se os pacotes estão instalados. Aqueles que não estão, serão automaticamente instalados.
  pacotes_instalados <- pacotes %in% rownames(installed.packages())
  if (any(pacotes_instalados == FALSE)) {
    install.packages(pacotes[!pacotes_instalados])
  }
  ### Carregar pacotes necessários
  lapply(pacotes, library, character.only = T)
  ### Removendo variáveis do processo de instalação e carregamento de pacotes.
  rm (pacotes, pacotes_instalados)
  
  ## Importar arquivos com extensão .xlsx:
  ### Listando automaticamente todos arquivos que sejam da extensão .xlsx da pasta de trabalho.
  path_planilha_original <- paste(getwd(),"/planilha_original/", sep="")
  ls_dir_trabalho <- list.files(path_planilha_original) # Esse comando lista os arquivos presentes no diretório indicado ("dir_trabalho").
  ls_dir_trabalho_xlsx <- grep(".xlsx", ls_dir_trabalho, ignore.case = T, value = T) # Com o output da listagem do conteúdo presente no diretório de trabalho, o comando grep serve para pesquisar o termo ".xlsx" dentro desse output.
  ls_dir_trabalho_xlsx_df <- as.data.frame(ls_dir_trabalho_xlsx) # Esse comando transforma o output da pesquisa da listagem em data.frame.
  numero_de_linhas_arquivo <- row(ls_dir_trabalho_xlsx_df) # O comando row retornará o número de linhas que o data.frame referenciado possui. Esta informação será usada posteriormente para compor o arquivo com os nomes dos arquivos.
  arquivos_df <- cbind(numero_de_linhas_arquivo, ls_dir_trabalho_xlsx_df) # O cbind formará um data.frame com os objetos indicados entre parênteses, cada um em uma coluna.
  colnames(arquivos_df) <- c("contagem_de_abas", "identificacao_de_abas") # O comando colnames permite nomear as colunas do objeto indicado no primeiro parêntese.
  ### Removendo os objetos temporários e retornando à pasta de trabalho anterior
  rm ( ls_dir_trabalho_xlsx, ls_dir_trabalho_xlsx_df, numero_de_linhas_arquivo) # Remoção dos objetos temporários que não serão mais usados.
  
  ## Recebendo todas informações de layout e dados brutos de uma planilha ##
  ### Criando os objetos que armazenarão o conteúdo desejado.
  id_arquivo <- arquivos_df$identificacao_de_abas #Objeto que guarda os nomes dos arquivos .xlsx do diretório alvo.
  objeto_abas <- as.character() #Criando um objeto para auxiliar na estrutura de repetição.
  resultado_df <- tibble() #Criando um dataframe para receber os nomes dos arquivos e das abas.
  ### Estrutura de repetição que criará a planilha 
  for (i in id_arquivo){ # Os elementos de id_arquivo, que contém os nomes das abas, serão acessados.
    objeto_abas <- c(readxl::excel_sheets(paste(path_planilha_original, i, sep=""))) # o comando readxl::excel_sheets será executado para cada elemento de id_arquivo, como definido na primeira linha do for.
    for (y in objeto_abas) { # Uma segunda estrutura de repetição é colocada. Nela, o objeto_abas, que conterá os nomes das abas, será posteriormente combinado com o nome do arquivo correspondente, como explicitado na linha abaixo.
      resultado_df <- c(resultado_df, i, y) # O dataframe resultado_df recebe os valores das combinações entre os nomes dos arquivos (i) e os nomes das abas das planilhas (y)
    }
  }
  ### Removendo os objetos temporários, usados na repetição acima.
  rm (id_arquivo, i, y, objeto_abas)
  #! Os nomes dos arquivos .xlsx e de suas abas estão misturados, ainda que com certa ordem. É necessário separar os nomes dos arquivos excel (que terminam em ".xlsx") dos nomes das abas (que é todo o restante).
  #! Primeiro, é necessário criar os objetos que serão usados somente para a execução da função.
  nomes_arq <- 0 # Esse objeto receberá os nomes dos arquivos de excel presentes na pasta
  pesquisa_arq <- "xlsx$" # Esse é o termo de procura para arquivos xlsx, que é a extensão usual do excel. O cifrão ($) no final indica que esse termo estará no final do nome.
  df_cheio <- data_frame() # Criação do data.frame que receberá todo o conteúdo de arquivos .xlsx e todas as abas desses arquivos.
  #! Agora, é preciso separar os nomes dos arquivos dos nomes das abas. A função grep pode fazer isso.
  nomes_arq <- grep(pesquisa_arq, resultado_df, ignore.case=T, value=T) # Essa é a função de procura. O objeto "nomes_arq" recebe o resultado da função grep, que procura pelo padrão do "pesquisa_arq", dentro da lista "resultado_df", procurando tanto letras maiúsculas quanto minúsculas ("ignore.case=T") e retornando as palavras, ao invés da sua localização ("value=T").
  nomes_aba <- grep(pesquisa_arq, resultado_df, ignore.case=T, value=T, invert = T)# Essa é a função de procura, muito semelhante em sintaxe à executada anteriormente, exceto que agora procuramos por tudo que não termine em ".xlsx", como demonstrado no argumento "invert=T".
  df_cheio <- cbind(nomes_arq, nomes_aba) # Agora é necessário juntar as duas colunas. A primeira é o nome dos arquivos, e a segunda é o nome das abas. Não aplicar nenhuma classificação para que a correspondência não fique errada! A função "cbind", seguida pelos objetos com o conteúdo das colunas, foi usado com sucesso.
  df_cheio <- as.data.frame(df_cheio) # Transformando df_final em dataframe.
  colnames(df_cheio) <- c("nomes_arquivos_xlsx", "respectivas_abas") # Alterando os nomes das colunas.
  ### Rodar separadamente
  rm (pesquisa_arq, nomes_aba, nomes_arq) # Removendo os objetos temporários.
  ### Rodar separadamente
  rm (arquivos_df, resultado_df) # Esses objetos também não são mais necessários.
  
  # Selecionando as abas que possuem o termo pesquisado em seu nome. O termo pesquisado está salvo no objeto "termo_pesquisado" e será usado para encontrar as linhas cujos nomes de abas contenham um padrão referente a dados brutos ("rut").
  
  termo_pesquisado <- "rut" # O termo pesquisado faz parte do nome usual das abas desejadas (no presente caso, é uma parte do termo "DadBrut".
  df_final <- data.frame() # Criando o data.frame que receberá os nomes de arquivos e abas pesquisados.
  df_final <- df_cheio[str_detect(df_cheio$respectivas_abas, termo_pesquisado), ] # Comando para separar as linhas que contém o termo pesquisado no df_cheio (que contém todos nomes de arquivos e suas respectivas abas).
  #Rodar separadamente.
  rm (df_cheio, termo_pesquisado) # Remoção dos objetos temporários que não serão mais utilizados posteriormente.
  
  ## Realizar estrutura de repetição que permita alterar o nome do arquivo e o nome da aba, de maneira correspondente.
  #Incluindo uma coluna com a numeração das linhas no df_final.
  qtdd_termos_df <- length (df_final$nomes_arquivos_xlsx) # Retornará o número de linhas que o data.frame possui.
  range_termos_df <- (1:qtdd_termos_df) # Transforma o número de linhas em um intervalo.
  df_final1 <- cbind(range_termos_df, df_final$nomes_arquivos_xlsx, df_final$respectivas_abas) # Acrescentando uma coluna ao data.frame "df_final", com o número de linhas dele.
  df_final2 <- as.data.frame(df_final1) # Transformando o objeto df_final2 em um data.frame.
  ### Removendo arquivos temporários
  rm (df_final1, range_termos_df) # Removendo objetos já utilizados e sem utilidade daqui em diante.
  ### Renomeando colunas
  df_final <- df_final2 # Passando o nome do df_final2 para df_final.
  colnames(df_final) <- c ("range", "arquivos", "abas") # Transformando os nomes das colunas do df_final.
  ### Removendo arquivos temporários
  rm (df_final2) # Removendo o df_final2.
  
  ## A função split permitirá criar uma lista com 1 data.frame por elemento.
  data2 <- split(df_final, factor(sort(rank(row.names(df_final))%%qtdd_termos_df))) # Gera uma lista composta por todos as linhas do df_final, separadamente e correspondentes.
  #! A sintaxe da função split merece uma explicação mais detalhada: o primeiro elemento é o dataframe original, cujos elementos serão transformados em elementos da lista...?
  ### Ciclo de repetição que importará os dados dos arquivos excel, realizará as transposições e salvará em novos arquivos excel. Para garantir o andamento da repetição, o diretório de trabalho será alterado.
  for (i in data2){ # Esse ciclo de for gerará uma lista que registrará os dados dos layouts e dados brutos diretamente das planilhas. Os dados serão extraídos e alocados em somente uma lista. Posteriormente, será preciso separar esses dados.
    layouts <- readxl::read_excel(paste(path_planilha_original, i$arquivos,sep=""), sheet = i$abas, range = "B22:Y37", col_names = F) # Indicação do arquivo, aba e células que deverão ser incluídas no data.frame.
    layout_combined <- bind_rows(layouts)
    layout_long <- pivot_longer(layout_combined, cols = starts_with("..."), names_to = "col", values_to = "info") # Linha que tira a database do formato "wide" (largo) para o formato "long" (comprido).
    layout_long <- separate(layout_long, info, into = c("tipo_celula", "droga1", "concentracao_droga1", "droga2", "concentracao_droga2", "droga3", "concentracao_droga3", "replicata"), sep = " - ", fill = "right") # Separa cada um dos campos da aba "layout", dando um título à coluna que cada item separado foi colocado.
    dados_brutos <- readxl::read_excel(paste(path_planilha_original, i$arquivos,sep=""), sheet = i$abas, range = "B2:Y17", col_names = F)
    dados_brutos_combined <- bind_rows(dados_brutos)
    dados_brutos_long <- mutate (dados_brutos_combined,(row_num = row_number()))# A combinação dos símbolos "%>%" serve como o pipe do bash. Nesse caso, indica que o objeto "dados_brutos_long" será o resultado da manipulação do "dados_brutos_combined". A manipulação está descrita após os símbolos "%>%".
    dados_brutos_long <- pivot_longer(dados_brutos_long, cols = starts_with("..."), names_to = "col", values_to = "fluorescencia") # Linha que tira a database do formato "wide" (largo) para o formato "long" (comprido).
    dados_final <- cbind(layout_long, dados_brutos_long) #Aqui é a formação do data.frame contendo informações de layout e dados brutos.
    dados_final <- dados_final[complete.cases(dados_final), ] #Essa linha de comando é responsável por tirar as linhas com "N/A" nelas, dispensando a necessidade de fazer isso pelo excel.
    output_planilha <- dplyr::select(dados_final, c(tipo_celula, droga1, concentracao_droga1, droga2, concentracao_droga2, droga3, concentracao_droga3, replicata, fluorescencia))
    acessorio <- paste("./abas_transpostas/",i$abas,".xlsx",sep="")
    planilha_existente <- createWorkbook()
    addWorksheet(planilha_existente, i$abas)
    writeData(planilha_existente, i$abas, output_planilha, colNames = T)
    saveWorkbook(planilha_existente, file = acessorio)
  }
}