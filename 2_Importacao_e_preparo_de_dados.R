# SEÇÃO 1: GERAÇÃO DOS ARQUIVOS TRANSPOSTOS
## Listando os pacotes necessários
pacotes <- c("readxl", "dplyr", "haven", "readr","foreign","car","FSA")

## Checando se os pacotes estão instalados. Aqueles que não estão, serão automaticamente instalados.
pacotes_instalados <- pacotes %in% rownames(installed.packages())
if (any(pacotes_instalados == FALSE)) {
  install.packages(pacotes[!pacotes_instalados])
}

## Carregar pacotes necessários
lapply(pacotes, library, character.only = T)

## Removendo variáveis do processo de instalação e carregamento de pacotes.
rm (pacotes, pacotes_instalados)

######========================================================================================#######

#SEÇÃO 2: CARREGANDO AS FUNÇÕES NECESSÁRIAS.
# Carregando funções necessárias: Essas funções reunem várias ferramentas que serão necessárias na análise estatística dos dados agrupados. 
##NÃO MANUSEAR, exceto se for inserir nova função ou retirar alguma das listadas##
# source('Functions/Funcao show_levels.R')
source("Functions/transposicao.R")#Transpõe e associa os valores de leitura com os rótulos.
source("Functions/agregando_relatorios.R")#Responsável por agregar os valores dos relatórios resultantes
source("Functions/graficos_significancia_tripla.R")#Fará a análise grupo a grupo.
source("Functions/graficos_significancia_tripla_analise_agrupada.R")#Fará a análise de grupos selecionados.
source("Functions/graficos_significancia_tripla_triplasJuntas.R")#Fará a análise somente entre os grupos de tratamento triplo.

######========================================================================================#######
#SEÇÃO 3: TRANSPONDO E ASSOCIANDO OS VALORES DE LEITURA COM AS SUAS IDENTIFICAÇÕES:
###!!NÃO MANUSEAR!!###
transposicao() #Essa função referencia automaticamente a pasta ./abas_transpostas/, trabalhando com os arquivos desta pasta. Ela não necessita de argumentos.

######========================================================================================#######
#SEÇÃO 4: 

# Ler a planilha do Excel (aqui consideramos a primeira aba, mas você pode especificar outra): essa é a forma de input de dados. Aqui deverá ser indicado o caminho para o arquivo que contenha os dados de leitura, já transpostos.
##!NECESSÁRIO MANUSEAR!##
dataset_mcf7 <- read_excel ("./abas_transpostas/brut_MCF7.xlsx") #Alterar o nome do arquivo para o desejado, mantendo a extensão correta.
dataset_t47d <- read_excel ("./abas_transpostas/brut_T47D.xlsx") #Alterar o nome do arquivo para o desejado, mantendo a extensão correta.

###########A PARTIR DESSE PONTO, ALTERAR SOMENTE SE HOUVER MAIS DE UM DATASET A SER ANALISADO, E AS ALTERAÇÕES DEVERÃO SE RESTRINGIR A DUPLICAR O QUE JÁ ESTÁ PRONTO, APENAS ALTERANDO O NOME DO OBJETO (no caso, o objeto é o mesmo que recebeu os dados do excel anteriormente)

# Transformar variáveis de texto em categóricas (factors). Acrescentar uma linha para cada dado que for importado.
##!NECESSÁRIO MANUSEAR!##
dataset_mcf7 = dataset_mcf7 %>% mutate(across(where(is.character),as.factor))
dataset_t47d = dataset_t47d %>% mutate(across(where(is.character),as.factor))

# Verificar os tipos de variáveis para confirmar as mudanças. Acrescentar uma linha para cada dado que for importado.
##!NECESSÁRIO MANUSEAR!##
# str(dataset)

# Mostrar os níveis das variáveis categóricas. Acrescentar uma linha para cada dado que for importado.
##!NECESSÁRIO MANUSEAR!##
# show_levels(dataset)

#########A PARTIR DESSE PONTO, RETIRAR O HASHTAG SOMENTE SE HOUVER DESEJO DE GERAR UM ARQUIVO SPSS DOS DADOS IMPORTADOS##########

# # Definir o caminho do arquivo de saída SPSS
# spss_file <- paste("dataset_composto_por_ambas_placas.sav", sep="")

# # Salvar o DataFrame como um arquivo SPSS
# write_sav(dataset_analise, spss_file)
# 
# # Mensagem de confirmação
# cat("Arquivo SPSS salvo com sucesso em:", spss_file, "\n")
