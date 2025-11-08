# Função para calcular outliers usando o método IQR


# remover_outliers_iqr <- function(subset) {
#   Q1 <- quantile(subset$fluorescencia, 0.25, na.rm = TRUE)
#   Q3 <- quantile(subset$fluorescencia, 0.75, na.rm = TRUE)
#   IQR <- Q3 - Q1
#   lower_bound <- Q1 - 1.5 * IQR
#   upper_bound <- Q3 + 1.5 * IQR
#   
#   # Remover outliers
#   subset_sem_outliers <- subset[subset$fluorescencia >= lower_bound & subset$fluorescencia <= upper_bound, ]
#   
#   # Se nenhum outlier foi removido, usar o conjunto original
#   if (nrow(subset_sem_outliers) == nrow(subset)) {
#     subset_sem_outliers <- subset
#   }
#   
#   return(subset_sem_outliers)
# }

remover_outliers_iqr <- function(subset) {
  Q1 <- quantile(subset$fluorescencia, 0.25, na.rm = TRUE)
  Q3 <- quantile(subset$fluorescencia, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Identificar outliers
  outliers <- subset[subset$fluorescencia < lower_bound | subset$fluorescencia > upper_bound, ]
  
  # Remover outliers
  subset_sem_outliers <- subset[subset$fluorescencia >= lower_bound & subset$fluorescencia <= upper_bound, ]
  
  # Se nenhum outlier foi removido, usar o conjunto original
  if (nrow(subset_sem_outliers) == nrow(subset)) {
    subset_sem_outliers <- subset
    outliers <- data.frame()  # Sem outliers, retorno um data frame vazio
  }
  
  # Retornar tanto os dados sem outliers quanto os outliers
  return(list(sem_outliers = subset_sem_outliers, outliers = outliers))
}



