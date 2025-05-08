###> Função fitoR_camp calcula os parâmetros fitossociologicos para 
###> vegetação campestre.
#> 
####> Como utilizar:
#>  O arquivo de entrada precisa das seguintes colunas com os nomes exatos em minusculo:
#>	- parc (identificação das parcelas, numerico ou letra, tanto faz)
#>	- spp (nome vulgar ou científico do indivíduo)
#>	- cob (cobertura absoluta em % mensurada segundo alguma escala)
#>
####> Argumentos
#> A função pode ser usada da seguinte forma: 
#> fitoR(input_data, area_de_cada_parcela_em_m2, 'nome_do_arquivo'), ou seja,
#> fitoR(x, area)
#>
#> x: um data frame contendo nas colunas as parcelas (parc), espécies (spp) e cobertura absoluta (cob);  
#> area: area das unidades amostrais em metros quadrados; 
#> filename: nome do arquivo de output dos resultados entre aspas simples ou duplas.
#>
#####> Valores 
#> Ao final do processo a função retorna uma lista composta por um resumo dos dados,
#> incluindo: número de unidades amostrais avaliadas, área total amostrada,
#> riqueza, diversidade de Shannon-Wiever (nats - calculada com base na cobertura)
#> e equitabilidade de Pielou.
#> 
#####> Exemplo
#>
#> data_camp <- read.csv(file="example_tab_camp.csv", sep=";", dec=",")
#> fitoR(x=data_camp, area=1)
#> 


fitoR_camp<-function(x, area)
{
  matriz<-table(x$spp,x$parc)
  
  # x[is.na(x)] <- 0
  
  #numero de parcelas
  nparc<-length(levels(as.factor(x$parc)))
  
  #area total amostrada
  area.parc=(area*nparc)
  
  
  #calcula frequencias 
  freq<-(if (length(dim(matriz)) > 1)
  {apply(matriz > 0,1,sum)} else sum(matriz > 0))
  FA<-(freq/nparc)*100
  FR<-(FA/sum(FA))*100
  
  #Cobertura
  
  CA<-tapply(x$cob, x$spp, sum)
  CR<-(CA/sum(CA))*100
  

  #calcula o indice de valor de importancia
  VI<-(CR+FR)/2
  
  #monta a tabela
  fito=data.frame(FA=FA,FR=FR, CA, CR,VI=VI)
  fito$FR<-round(fito$FR,digits=4)
  fito$FA<-round(fito$FA,digits=4)
  fito$CA<-round(fito$CA,digits=4)
  fito$CR<-round(fito$CR,digits=4)
  fito$VI<-round(fito$VI,digits=4)
  fito <- fito[order(VI, decreasing = TRUE),]

  
  #calcula os indices de diversidade
  Pi<-CA/sum(CA)
  Pi<-Pi*log(Pi)
  SW=-sum(Pi)
  S=nrow(fito)
  J=SW/log(S)
  
  #parc resume
  w<-filter(x, !spp %in% c('Rocha', 'Mantilho', 'Morta', 'Areia', 'Solo exposto'))
  
  parc_green_cob<-tapply(w$cob, w$parc, sum)
  
  n_green<-filter(x, spp %in% c('Rocha', 'Mantilho', 'Morta', 'Areia', 'Solo exposto'))
  
  parc_n_green<- tapply(n_green$cob, n_green$parc, sum)
  
  parc_spp<-tapply(w$spp, w$parc, length)
  
  sum_parc=cbind(parc_spp, parc_green_cob)
  
  
  mode_spp <-names(sort(-table(parc_spp)))[1]
  mode_cob <-names(sort(-table(parc_green_cob)))[1]
  mean_spp<-mean(parc_spp)
  mean_cob<-mean(parc_green_cob)
 
  
  resume = cbind.data.frame(
    Parâmetros = c(
      "Numero de parcelas",
      "Area total amostrada",
      'Riqueza',
      'Moda de riqueza por parcela',
      'Média de riqueza por parcela',
      'Média de cobertura verde por parcela',
      "Indice de Shannon-Wiener (H')",
      "Equabilidade de Pielou (J)"
    ),
    Resultado = c(round(nparc, digits = 2), 
                  round(area.parc, digits = 2),
                  S,mode_spp, mean_spp, mean_cob,   
                  round(SW, digits = 2), 
                  round(J, digits = 2))
  )
l <- list(fitossociologia=fito,resumo_ua=sum_parc, resumo_geral=resume)
  return(l)
  
  
  
}

