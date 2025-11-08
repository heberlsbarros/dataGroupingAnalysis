###Extração automática dos dados dos grupos experimentais###
for (i in agrupamentos$agrupamento){
  print (i)
  filtro <- dataset_analise %>% filter (nome_comb == i)
  assign(i,filtro)
}

rm (filtro, i)
