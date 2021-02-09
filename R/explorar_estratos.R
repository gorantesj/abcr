distribuicon_peso <- function(marco_muestral, id_estratos){
  # Calcular pesos
  pesos <- calcular_pesos(marco_muestral = marco_muestral,
                          id_estratos = {{id_estratos}})
  # Calcular peso acumulado, porcentaje de estratos
  pesos <- pesos %>%
    arrange(peso) %>%
    mutate(`Peso acumulado`=cumsum(peso),
           `Cuantiles`=row_number()/nrow(pesos))
  pesos <-  bind_rows(tibble(`Peso acumulado`=c(1,0), `Cuantiles`=c(1,0)),
                      pesos)
  # TEMMPORAL
  pesos <- pesos %>% filter(!is.na({{id_estratos}}))
  browser()
  # Letrero
  letrero <- pesos %>% summarise(n=nrow(.), Gini=ineq::Gini(peso))
  letrero <- glue::glue("Número de estratos: {letrero$n}.
  Gini (peso): {round(letrero$Gini,4)}.")
  # Gráfica

  (g <- ggplot(pesos) +
    geom_polygon(aes(x=Cuantiles, y=`Peso acumulado`),
                 color="tomato",
                 fill="steelblue",
                 size=.5,
                 alpha=.5)+
      annotate(x=0,y=1, label=letrero, vjust=1, hjust=0, geom="label")+
    theme_minimal())
  return(g)
}
