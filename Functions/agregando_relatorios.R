agregando_relatorios <- function () {
  ################==============FINALIDADE DESTE SCRIPT: JUNÇÃO DE RELATÓRIOS INDIVIDUAIS EM ARQUIVO ÚNICO=============#########################
  ##########================SEÇÃO 1: INSTALANDO E CARREGANDO PACOTES=========#########################
  ## Listando os pacotes necessários
  pacotes <- c("readxl", "dplyr", "haven", "readr", "foreign", "car", "FSA", "openxlsx", "tidyverse", "stringr")
  
  ## Checando se os pacotes estão instalados. Aqueles que não estão, serão automaticamente instalados.
  pacotes_instalados <- pacotes %in% rownames(installed.packages())
  if (any(pacotes_instalados == FALSE)) {
    install.packages(pacotes[!pacotes_instalados])
  }
  
  ## Carregar pacotes necessários
  lapply(pacotes, library, character.only = T)
  
  ## Removendo variáveis do processo de instalação e carregamento de pacotes.
  rm (pacotes, pacotes_instalados)
  
  ##########================SEÇÃO 2: IMPORTANDO DADOS DE RELATÓRIOS=========#########################
  #Criando a variável com o path da pasta de relatórios:
  ###???
  #Listando conteúdo da pasta de relatórios, path: "./relatorios_todas_combinacoes/"
  pasta_relatorios <- paste(getwd(), "/relatorios_todas_combinacoes/", sep = "") #Cria um objeto que armazena o path da pasta que contém os relatórios individuais. Caso seja necessário ajustar a pasta, alterar o trecho entre aspas.
  lista_relatorios_fullpath <- list.files(pasta_relatorios, "comp", full.names = T) #Cria um objeto que armazena o caminho de cada um dos arquivos que a pasta_relatorios. Nessa busca, já foi aplicada uma filtragem por arquivos que contenham, em seu nome, o padrão especificado entre aspas.
  
  ##Criando uma lista com os dados de todas planilhas de relatórios, cada uma colocada como sublista:
  agregado_relatorios <- list() #Lista que será utilizada a seguir
  agregado_relatorios <- map(.x = lista_relatorios_fullpath, #Gera uma lista com os dados de todos relatórios individuais, sendo cada um dividido como sublistas.
                             .f = ~ {
                               temp_file <- .x
                               map(.x = excel_sheets(.x),
                                   .f = ~ read_excel(path = temp_file,
                                                     sheet = .x))
                             })
  ##Retirando do formato de lista e passando para o formato de dataframe.
  lista_para_df <- data.frame()
  agregado_lista_para_df <- data.frame()
  for (i in 1:length(lista_relatorios_fullpath)){
    lista_para_df <- as.data.frame(agregado_relatorios[[i]][[1]])
    agregado_lista_para_df <- rbind(lista_para_df,agregado_lista_para_df)
  }
  
  ##Salvando um objeto com os dados em formatod de dataframe.
  df_todos_relatorios <- agregado_lista_para_df
  
  ##Exportando "lista_dados_estatisticos" como um arquivo excel (.xlsx)
  planilha_existente <- createWorkbook() #Cria o objeto da classe necessária para gerar uma planilha excel
  addWorksheet(planilha_existente, "agregado_relatorios") # Adiciona uma aba ao objeto da planilha criada
  writeData(planilha_existente, "agregado_relatorios", df_todos_relatorios, colNames = T) #Escreve os dados no arquivo planilha
  saveWorkbook(planilha_existente, file = paste(getwd(),"/relatorios_todas_combinacoes/agregado_de_relatorios.xlsx", sep=""), overwrite = T) #Salva o arquivo planilha
  
  rm (list = ls(all.names = T)) #Limpa a lista de variáveis.
}