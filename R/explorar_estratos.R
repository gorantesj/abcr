#' Title
#'
#' @param marco_muestral
#' @param prefijo
#'
#' @return
#' @export
#'
#' @examples
n_estratos <- function(marco_muestral, prefijo="estratificacion"){
  marco_muestral %>%
    pivot_longer(starts_with(prefijo),
                 names_to="variable",
                 values_to="estrato",
                 names_prefix=glue::glue("{prefijo}_")) %>%
    filter(!is.na(estrato)) %>%
    group_by(variable) %>%
    summarise(n_estrato=n_distinct(estrato)) %>%
    ggplot(aes(x=reorder(variable,n_estrato), y=n_estrato))+
    geom_bar(stat="identity", fill="#2e294e") +
    geom_hline(yintercept=0)+
    labs(x="Variables de estratificación",
         y="Número de estratos",
         title="Número de estratos por criterio de estratificación")+
    coord_flip()+
    theme(panel.grid = element_blank(),
          panel.background = element_blank())
}


#' Title
#'
#' @param marco_muestral
#' @param id_estratos
#'
#' @return
#' @export
#'
#' @examples
distribucion_peso <- function(marco_muestral, id_estratos){
  # TEMPORAL: quitar grupos del marco_muestral
  marco_muestral <- marco_muestral %>% ungroup()
  variables <- names(marco_muestral)[grepl("\\bestratificacion_",x = names(marco_muestral))]
  # Calcular pesos
  calculos <- variables %>% map(~{

    pesos <- calcular_pesos(marco_muestral = marco_muestral,
                            id_estratos = !!sym(.x))
    # Calcular peso acumulado, porcentaje de estratos
    pesos <- pesos %>%
      arrange(peso) %>%
      mutate(`Peso acumulado`=cumsum(peso),
             `Cuantiles`=row_number()/nrow(pesos))
    pesos <-  bind_rows(tibble(`Peso acumulado`=c(1,0), `Cuantiles`=c(1,0)),
                        pesos) %>%
      mutate(estratificacion=.x)

    # Letrero
    letrero <- pesos %>% summarise(n=n(),
                                   Gini=ineq::Gini(peso))
    letrero <- glue::glue("Número de estratos: {letrero$n}.
  Gini (peso): {round(letrero$Gini,4)}.")
    # Gráfica
    letrero <- tibble(letrero=letrero, estratificacion=.x)
    return(list(pesos=pesos, letrero=letrero))
  })
  pesos <- calculos %>% map_df(~.x[[1]]) %>%
    mutate(estratificacion=gsub("estratificacion_", ))
  letrero <- calculos %>% map_df(~.x[[2]])
  (g <- ggplot(pesos) +
    geom_polygon(aes(x=Cuantiles, y=`Peso acumulado`),
                 color="tomato",
                 fill="steelblue",
                 size=.5,
                 alpha=.5)+
      geom_label(data=letrero, aes(x=0,y=1, label=letrero), vjust=1, hjust=0)+
      labs(title = "Curva de Lorenz",
           subtitle = "Pesos por estrato",
           x=stringr::str_wrap("Estratos acumulados de menor a mayor",30))+
      facet_wrap(~estratificacion)+
    theme_minimal())
  return(g)
}


#' Title
#'
#' @param marco_muestral
#' @param id_cae
#' @param frac_m
#' @param estratos
#' @param muestras
#'
#' @return
#' @export
#'
#' @examples
probabilidad_cae <- function(marco_muestral,
                             id_cae,
                             frac_m,
                             estratos,
                             muestras=1000){
  prueba <- expand_grid(i=1:muestras,
                        frac=frac_m,
                        estratos=estratos)

  muestras <- pmap(list(prueba$i,
                        prueba$frac,
                        prueba$estratos),
                   function(first, second, third) {
                     marco_muestral %>%
                       mutate(estratificacion=!!sym(third)) %>%
                       group_by(estratificacion) %>%
                       sample_frac(size=second) %>%
                       mutate(sim=first, frac=second, estratificacion=third)
                   })
  re <- muestras %>% map_df(~.x %>%
                              group_by(estratificacion,
                                       frac,
                                       sim,
                                       {{id_cae}}) %>%
                              summarise(cpc=n()) %>%
                              group_by(cpc,.add = T) %>%
                              summarise(n=n()) %>%
                              mutate(n=n/sum(n))
  )


  re %>%
    ggplot(aes(x=as.character(frac), y=n, color=estratificacion))+
    geom_boxplot()+
    facet_wrap(~cpc,scales = "free")
}


