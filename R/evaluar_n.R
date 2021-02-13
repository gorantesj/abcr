#' Title
#'
#' @param estimaciones
#'
#' @return
#' @export
#'
#' @examples
evaluar_n_cobertura <- function(estimaciones){
  resumen <- estimaciones %>%
    as_tibble() %>%
    group_by(n,candidato) %>%
    summarise(cobertura=sum(contiene)/n())
  letrero <- resumen %>% summarise(cobertura=median(cobertura))
  resumen %>%
    ggplot()+
    geom_hline(yintercept = .95, linetype=2, color="#1b998b")+
    geom_line(aes(group=candidato, x=n,y=cobertura), alpha=.2,color="#f46036")+
    geom_line(data=letrero, aes(x=n,y=cobertura), color="#f46036")+
    geom_point(data=letrero, aes(x=n,y=cobertura), color="#f46036")+
    annotate(x=min(resumen$n),
             y=min(resumen$cobertura),
             geom = "point",
             color="#f46036",
             hjust="inward")+
    annotate(x=min(resumen$n)*1.01,
             y=min(resumen$cobertura),
             label="cobertura mediana",
             color="#2e294e",
             geom = "text", hjust=0)+
    scale_y_continuous(name="Cobertura (%)",labels = scales::percent_format())+
    scale_x_continuous(name="Tamaño de la muestra (n)", breaks = unique(resumen$n)) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.background =element_blank(),
          )
}

#' Title
#'
#' @param estimaciones
#'
#' @return
#' @export
#'
#' @examples
evaluar_n_error <- function(estimaciones, error=0.01){
  g <- estimaciones %>%
    group_by(n,muestra) %>%
    summarise(max_longitud=max(abs(sesgo))) %>%
    ggplot(aes(x=n, y=max_longitud, group=n)) +
    scale_y_continuous(name=expression(paste(D[i]," = max{",hat(p[i])-p[i],"}")),
                       labels = scales::percent_format())+
    scale_x_continuous(name="Tamaño de la muestra (n)", breaks = unique(estimaciones$n))+
    geom_boxplot(color="#2e294e",fill="#2e294e",alpha=.7)+
    geom_hline(yintercept = error, linetype=2, color="#e71d36")+
    # labs(
    #   # title = "Diferencia máxima por muestra entre el estimador puntual y el resultado",
    #      subtitle = glue::glue("{scales::comma(max(evaluacion$muestra))} muestras"))+
    theme(panel.grid.major.x =  element_blank(),
          panel.grid.minor.x =  element_blank(),
          panel.grid.major.y =  element_line(colour = "grey70"),
          axis.line.x = element_line(),
          panel.background = element_blank()
          )


}

#' Title
#'
#' @param estimaciones
#' @param precision
#'
#' @return
#' @export
#'
#' @examples
evaluar_n_precision <- function(estimaciones,precision=0.02){
  estimaciones %>%
    group_by(n,muestra) %>%
    summarise(max_longitud=max(longitud_intervalo)) %>%
    ggplot(aes(x=n, y=max_longitud, group=n)) +
    geom_hline(yintercept = precision, linetype=2, color="#e71d36")+
    scale_y_continuous(name=expression(paste("Max{",q[i]-q[i],"}")),
                       labels = scales::percent_format())+
    scale_x_continuous(name="Tamaño de la muestra (n)", breaks = unique(estimaciones$n))+
    # labs(
    #   title = "Longitud máxima por muestra del intervalo de estimación",
    #      subtitle = glue::glue("{scales::comma(max(evaluacion$muestra))} muestras"))+
    geom_boxplot(color="#2e294e",fill="#2e294e",alpha=.7)+
    theme(panel.grid.major.x =  element_blank(),
          panel.grid.minor.x =  element_blank(),
          panel.grid.major.y =  element_line(colour = "grey70"),
          axis.line.x = element_line(),
          panel.background = element_blank()
    )
}


