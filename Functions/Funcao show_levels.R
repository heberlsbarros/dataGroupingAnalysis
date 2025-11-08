# show_levels <- function(df) {
#   cat("Níveis das variáveis categóricas:\n")
#   for (var in names(df)) {
#     if (is.factor(df[[var]])) {
#       cat("Variável:", var, "\n")
#       print(levels(df[[var]]))
#       cat("\n")
#     }
#   }
# }

show_levels <- function(df) {
  cat("Níveis das variáveis categóricas:\n")
  for (var in names(df)) {
    if (is.factor(df[[var]])) {
      df[[var]] <- droplevels(df[[var]]) # Remove os níveis não utilizados
      cat("Variável:", var, "\n")
      print(levels(df[[var]]))
      cat("\n")
    }
  }
}

