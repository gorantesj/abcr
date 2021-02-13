calcular_pesos <- function(marco_muestral, id_estratos){
  marco_muestral <- marco_muestral %>% filter(!is.na({{id_estratos}}))
  estratos <- marco_muestral %>%
    count({{id_estratos}}, wt=LISTA_NOMINAL,name = "peso") %>%
    mutate(peso=peso/sum(peso))
  return(estratos)
}

datos_muestra <- function(muestra, id_estratos, candidatos, n_sim=10000){
  estratos_muestra <- muestra %>%
    tidyr::pivot_longer({{candidatos}}, names_to="candidato", values_to="x") %>%
    group_by({{id_estratos}}, candidato) %>%
    mutate(x=x+0.0001) %>%
    summarise(c=n(),
              n=sum(LISTA_NOMINAL),
              x2_n=sum(x^2/LISTA_NOMINAL),
              x=sum(x)) %>%
    mutate(mu=x/n,
           alpha=(c-1)/2,
           beta=(1/2)*(x2_n-(x^2)/n))

  return(estratos_muestra)
}

