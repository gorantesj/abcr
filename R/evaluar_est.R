#' Title
#'
#' @param estimaciones
#'
#' @return
#' @export
#'
#' @examples
evaluar_est_cobertura <- function(estimaciones){
  resumen2 <- estimaciones %>%
    as_tibble() %>%
    group_by(variable,candidato) %>%
    summarise(cobertura=sum(contiene)/n()) %>%
    mutate(variable=gsub(pattern = "`",replacement = "",x = variable) %>%
             gsub(pattern = "~",replacement = "",x = .) %>%
             gsub(pattern = "estratificacion_",replacement = "",x = .))
  letrero <- resumen2 %>% summarise(cobertura=median(cobertura))
  resumen2 %>%
    ggplot()+
    geom_hline(yintercept = .95, linetype=2, color="#1b998b")+
    geom_line(aes(group=candidato, x=variable,y=cobertura),
              alpha=.2,
              color="#f46036")+
    geom_line(data=letrero, aes(x=variable,y=cobertura, group=1), color="#f46036")+
    geom_point(data=letrero, aes(x=variable,y=cobertura), color="#f46036")+
    scale_y_continuous(name="Cobertura (%)",
                       labels = scales::percent_format(),
                       limits = c(min(c(min(resumen2$cobertura,.7))),1)
                       )+
    theme(panel.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank())

}

#' Title
#'
#' @param estimaciones
#' @param error
#'
#' @return
#' @export
#'
#' @examples
evaluar_est_error <- function(estimaciones, error=0.01){
  estimaciones %>%
    group_by(variable,muestra) %>%
    summarise(sesgo=max(abs(sesgo))) %>%
    mutate(variable=gsub(pattern = "`",replacement = "",x = variable) %>%
             gsub(pattern = "~",replacement = "",x = .) %>%
             gsub(pattern = "estratificacion_",replacement = "",x = .)) %>%
    ggplot(aes(x=variable, y=sesgo, group=variable)) +
    scale_y_continuous(name=expression(paste(D[i]," = max{",hat(p[i])-p[i],"}")),
                       labels = scales::percent_format())+
    geom_boxplot(color="#2e294e",fill="#2e294e",alpha=.7)+
    geom_hline(yintercept = error, linetype=2, color="#e71d36")+
    labs(title = "Diferencia máxima por muestra entre el estimador puntual y el resultado",
         subtitle = glue::glue("{scales::comma(max(estimaciones$muestra))} muestras"),
         caption = glue::glue("n={round(mean(estimaciones$n))}"))+
    theme(panel.grid.major.x =  element_blank(),
          panel.grid.minor.x =  element_blank(),
          panel.grid.major.y =  element_line(),
          panel.grid.minor.y =  element_line(),
          panel.background= element_blank(),
          axis.line.x = element_line()
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
evaluar_est_precision <- function(estimaciones, precision=0.02){
  estimaciones %>%
    group_by(variable,muestra) %>%
    summarise(longitud_maxima=max(longitud_intervalo)) %>%
    mutate(variable=gsub(pattern = "`",replacement = "",x = variable) %>%
             gsub(pattern = "~",replacement = "",x = .) %>%
             gsub(pattern = "estratificacion_",replacement = "",x = .)) %>%
    ggplot(aes(x=variable, y=longitud_maxima, group=variable)) +
    scale_y_continuous(name=expression(paste(L[i],"=max{",q[.975i]-q[.025i],"}")),
                       labels = scales::percent_format())+
    geom_boxplot(color="#2e294e",fill="#2e294e",alpha=.7)+
    geom_hline(yintercept = precision, linetype=2, color="#e71d36")+
    labs(
      # title = "Longitud dele intervalo de estimación máxima por muestra",
      #    subtitle = glue::glue("{scales::comma(max(estimaciones$muestra))} muestras"),
         caption = glue::glue("n={round(mean(estimaciones$n))}"))+
    theme(panel.grid.major.x =  element_blank(),
          panel.grid.minor.x =  element_blank(),
          panel.grid.major.y =  element_line(),
          panel.grid.minor.y =  element_line(),
          panel.background= element_blank(),
          axis.line.x = element_line()
    )
}

