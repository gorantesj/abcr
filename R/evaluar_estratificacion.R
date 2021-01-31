#' Title
#'
#' @param marco_muestral
#' @param n_muestras
#' @param id_estratos
#' @param candidatos
#' @param fracc_muestreo
#'
#' @return
#' @export
#'
#' @examples
evaluar_estratificacion <- function(marco_muestral, n_muestras=100,
                                    id_estratos, candidatos,
                                    fracc_muestreo=0.05){
  muestras <-map(1:n_muestras,
                 ~marco_muestral %>%
                   group_by({{id_estratos}}) %>%
                   sample_frac(size=fracc_muestreo) %>%
                   mutate(variable=rlang::as_name(quote(id_estratos)),
                          muestra=.x))
  reales <- marco_muestral %>%
    summarise(across({{candidatos}},
                     ~sum(.x))) %>%
    pivot_longer(everything(),
                 names_to="candidato",
                 values_to="valor") %>%
    mutate(valor=valor/sum(valor))
  estimaciones <- map2_df(.x=muestras,
                          .y=1:length(muestras),
                          ~.x %>% ajustar_modelo(id_estratos = {{id_estratos}},
                                                 criterio_ce = "2018",
                                                 candidatos = {{candidatos}},
                                                 marco_muestral = marco_muestral) %>%
                            pluck("nacional") %>%
                            mutate(variable=rlang::as_name(quote(id_estratos)),
                                   muestra=.y,
                                   n=nrow(.x),
                                   longitud_intervalo=ic_975-ic_025))
  estimaciones <- estimaciones %>%
    full_join(.,reales) %>%
    mutate(contiene=(round(valor,4)>=round(ic_025,4) &
                       round(valor,4)<=round(ic_975,4)),
           sesgo=est_puntual-valor
    )
  return(estimaciones)

}

#' Title
#'
#' @return
#' @export
#'
#' @examples
evaluar_estratificacion_cobertura <- function(estimaciones){
  resumen <- estimaciones %>%
    group_by(candidato, contiene) %>%
    summarise(n=n(), valor=mean(valor)) %>%
    mutate(n=n/sum(n))
  cobertura_mediana <- resumen %>%
    filter(contiene) %>%
    ungroup() %>%
    summarise(n=median(n)) %>%
    pull(n)

  (g1 <- resumen %>%
      ggplot(aes(x=forcats::fct_reorder(candidato,valor))) +
      geom_bar(aes(y=n,fill=as.factor(contiene)),
               stat = "identity") +
      geom_text(data = resumen %>% filter(contiene),
                aes(y=n, label=scales::percent(n,accuracy = 1)), hjust=1, color="white")+
      scale_x_discrete(name="Candidato")+
      scale_fill_manual(values = c("TRUE"="#1b998b","FALSE"="#e71d36"),
                        name="Contiene al valor",
                        labels=c("Sí","No"))+
      scale_y_continuous(labels = scales::percent_format(), name="Cobertura")+
      expand_limits(x=c(0, n_distinct(resumen$candidato)+1))+
      annotate(x=n_distinct(resumen$candidato)+.4,
               y=cobertura_mediana,
               geom="point",
               shape=25, fill="black")+
      annotate(x=n_distinct(resumen$candidato)+.5,
               y=cobertura_mediana,
               geom="text",
               label=glue::glue("cobertura mediana {scales::percent(cobertura_mediana)}"),
               vjust=0, hjust=1)+
      labs(title = "Cobertura",
           subtitle = glue::glue("{scales::comma(max(evaluacion$muestra))} muestras"),
           caption = glue::glue("n={mean(estimaciones$n)}")) +
      coord_flip()+
      theme(panel.grid = element_blank(),
            axis.line.x = element_line())
  )
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
evaluar_estratificacion_error <- function(estimaciones, error=0.01){
  error_pct <- round(100*error,digits = 0)
  letrero <- estimaciones %>%
    group_by(candidato, s=abs(sesgo)>error) %>%
    summarise(n=n(),o=max(abs(sesgo))) %>%
    mutate(n=n/sum(n)) %>%
    ungroup() %>%
    complete(candidato, s=c(T,F), fill = list(n=0,o=0)) %>%
    group_by(candidato) %>%
    mutate(o=max(o)) %>%
    filter(s)
  aux <- letrero %>%
    filter(s) %>%
    ggplot(aes(x=forcats::fct_reorder(candidato,abs(o),max),
               y=1,
               label=scales::percent(n,1),
               color=n)) +
    scale_color_gradient2(low = "#1b998b",
                          high = "#e71d36",mid = "#f46036",
                          midpoint = 0.05,
                          guide = NULL, limits=c(0,.1)) +
    scale_y_continuous(name=glue::glue("|D|>{error_pct}%"))+
    geom_text(hjust=0) +
    theme_void()+
    theme(axis.title.x = element_text(),
          axis.line.x =   element_line(),
          panel.background = element_rect(fill="#F2F2F8",color = NULL, size=0))+
    coord_flip()

  g <- evaluacion %>%
    ggplot(aes(x=forcats::fct_reorder(candidato,.fun = max, abs(sesgo)),
               y=sesgo)) +
    geom_boxplot(color="#2e294e", fill="#2e294e", alpha=.7) +
    geom_hline(yintercept = error, linetype=2, color="#e71d36")+
    geom_hline(yintercept = -error, linetype=2, color="#e71d36") +
    scale_x_discrete(name="Candidato")+
    scale_y_continuous(labels=scales::percent_format(),
                       name = expression(paste("D=",hat(p)-p)))+
    coord_flip() +
    theme_minimal()+
    # labs(title = "Diferencia entre el estimador puntual y el resultado",
    #      subtitle = glue::glue("{scales::comma(max(evaluacion$muestra))} muestras"),
    #      caption = glue::glue("n={mean(estimaciones$n)}"))+
    theme(axis.line.x = element_line(),
          panel.grid=   element_blank())

  g + aux +plot_layout(widths = c(.8,.2))
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
evaluar_estratificacion_precision <- function(estimaciones, precision=0.02){
  prec <- round(100*precision)
  letrero <- estimaciones %>%
    group_by(candidato, s=longitud_intervalo>precision) %>%
    summarise(n=n(),o=max(longitud_intervalo)) %>%
    mutate(n=n/sum(n)) %>%
    ungroup() %>%
    complete(candidato,  s=c(T,F), fill = list(n=0,o=0)) %>%
    group_by(candidato) %>%
    mutate(o=max(o)) %>%
    filter(s)

  aux <- letrero %>%
    filter(s) %>%
    ggplot(aes(x=forcats::fct_reorder(candidato,o,.fun = max),
               y=1,
               label=scales::percent(n,1),
               color=n)) +
    scale_color_gradient2(low = "#1b998b",
                          high = "#e71d36",mid = "#f46036",
                          midpoint = 0.05,
                          guide = NULL, limits=c(0,1)) +
    scale_y_continuous(name=glue::glue("L>{prec}%"))+
    geom_text(hjust=0) +
    theme_void()+
    theme(axis.title.x = element_text(),
          axis.line.x =   element_line(),
          panel.background = element_rect(fill="#F2F2F8",color = NULL, size=0))+
    coord_flip()
  g <- evaluacion %>%
      ggplot(aes(x=forcats::fct_reorder(candidato,longitud_intervalo,.fun = max),
                 y=longitud_intervalo)) +
      geom_boxplot(color="#2e294e", fill="#2e294e", alpha=.7) +
      coord_flip() +
      geom_hline(yintercept = precision, linetype=2)+
      theme_minimal()+
      scale_x_discrete(name="candidato")+
      scale_y_continuous(name=expression(paste("L=",q[0.975]-q[0.025])),
                         labels=scales::percent_format())+
      # labs(title="Longitud del intervalo",
      #      caption = glue::glue("n={mean(estimaciones$n)}"))+
      theme(axis.line.x = element_line(),
            panel.grid=   element_blank())

  g + aux +plot_layout(widths = c(.8,.2))
}
