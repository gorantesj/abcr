#' Title
#'
#' @param resultados
#' @param candidatos
#'
#' @return
#' @export
#'
#' @examples
graficar_estimaciones_ev <-function(resultados,
                                    candidatos,
                                    estimacion="puntual",
                                    participacion=F){
  if(!is.null(resultados$salidas.estimaciones)){
    if(estimacion=="puntual"){
      if(participacion){
        g <- resultados$salidas.estimaciones %>%
          select(PART, LMU) %>%
          tidyr::pivot_longer(PART,
                              names_to = "candidato",
                              values_to = "estimaciones")

      }
      else{
        g <- resultados$salidas.estimaciones %>%
          select(-PART) %>%
          tidyr::pivot_longer({{candidatos}},
                              names_to = "candidato",
                              values_to = "estimaciones")

      }
      g <- g %>% filter(LMU==1) %>%
        ggplot() +
        geom_bar(aes(x=reorder(candidato, -estimaciones),
                     y=estimaciones), stat="identity") +
        labs(x="Candidato",
             y="Voto porcentual (%)",
             title = "Estimación puntual")


    }
    if(estimacion=="intervalos"){
      if(participacion){
        g <- resultados$salidas.estimaciones %>%
          select(PART, LMU) %>%
          tidyr::pivot_longer(PART,
                              names_to = "candidato",
                              values_to = "estimaciones")

      }
      else{
        g <- resultados$salidas.estimaciones %>%
          select(-PART) %>%
          tidyr::pivot_longer({{candidatos}},
                              names_to = "candidato",
                              values_to = "estimaciones")

      }
      g <- g %>%
        tidyr::pivot_wider(names_from = LMU, values_from = estimaciones) %>%
        ggplot(aes(x=reorder(candidato,-`2`))) +
        geom_errorbar(aes(ymin=`0`, ymax=`2`)) +
        # geom_point(aes(y=`1`)) +
        labs(x="Candidato", y="Voto porcentual (%)",
             title = "Intervalos de estimación")

    }
    g <- g + scale_y_continuous()
  }
  else{
    g <- NULL
  }
  return(g)
}

#' Title
#'
#' @param resultados
#'
#' @return
#' @export
#'
#' @examples
graficar_distribucion_ev <- function(resultados, candidato,
                                     por_estrato=F){
  if(!is.null(resultados$estimaciones$lambda)){
    cand <- rlang::expr_text(substitute(candidato))
    minimo <- min(c(resultados$remesas$remesa %>%
                      mutate(theta={{candidato}}/LISTA_NOMINAL) %>% pull(theta),
                    resultados$estimaciones$lambda %>%
                      filter(candidato == cand ) %>%
                      pull(theta)))
    maximo <- max(c(resultados$remesas$remesa %>%
                      mutate(theta={{candidato}}/LISTA_NOMINAL) %>% pull(theta),
                    resultados$estimaciones$lambda %>%
                      filter(candidato == cand )
                    %>%pull(theta)))
    g <- resultados$estimaciones$lambda %>%
      filter(candidato == cand ) %>%
      ggplot(aes(x=theta, color=candidato))+
      geom_density()+
      scale_x_continuous(limits = c(minimo, maximo))
    if(por_estrato){

      estratos <-  resultados$estimaciones$resumen_estratos %>%
        filter(candidato == cand ) %>%
        mutate(cota_inf=purrr::map_dbl(theta, ~quantile(.x, probs=c(0.025))),
               cota_sup=purrr::map_dbl(theta, ~quantile(.x, probs=c(0.975))))
      minimo <- min(c(minimo, estratos %>% pull(cota_inf)))
      maximo <- max(c(minimo, estratos %>% pull(cota_sup)))
      g1 <- estratos %>%
        ggplot()+
        geom_linerange(aes(x=ID_ESTRATO_L,
                           ymin=cota_inf,
                           ymax=cota_sup,
                           # size=peso,
                           color=as.factor(ID_ESTRATO_L))) +
        geom_point(data =resultados$remesas$remesa,
                   aes(x=ID_ESTRATO_L,
                       y={{candidato}}/LISTA_NOMINAL,
                   ), size=.1)+
        scale_y_continuous(limits = c(minimo,maximo))+
        guides(color=F)+
        coord_flip()
      g <- (g/(g1 +theme_void()))+
        plot_layout(heights = c(5,5))

    }
  }
  else{
    g <- NULL
  }
  return(g)
}

#' Title
#'
#' @param resultados
#'
#' @return
#' @export
#'
#' @examples
graficar_probabilidad_ev <- function(resultados){
  if(!is.null(resultados$estimaciones$lambda)){
    base <- resultados$estimaciones$lambda %>%
      group_by(i) %>%
      summarise(ganador=unique(candidato)[which.max(lambda)]) %>%
      group_by(ganador) %>%
      summarise(prob=n()) %>%
      mutate(prob=prob/sum(prob),
             linea=row_number()
             )
    base %>%
      ggplot(aes(x=linea, y=prob)) +
      geom_bar(aes(y=1),stat="identity", fill="beige")+
      geom_bar(aes(fill=ganador),stat="identity")+
      coord_polar(theta = "y") +
      scale_y_continuous(limits=c(0,1))+
      # scale_x_continuous(limits=c(min(base$linea)-max(base$linea)/2,
      #                             max(base$linea)))+
      theme_void()
  }
}

#' Title
#'
#' @param resultados
#' @param candidatos
#'
#' @return
#' @export
#'
#' @examples
graficar_lambdabi_ev <- function(resultados, candidatos){
  if(!is.null(resultados$estimaciones$lambda)){
    if(length(candidatos)==2){
      resultados$estimaciones$lambdas %>%
        filter(candidato %in% candidatos) %>%
        tidyr::pivot_wider(-theta,names_from = candidato, values_from = lambda) %>%
        ggplot(aes(x=!!sym(candidatos[[1]]),
                   y=!!sym(candidatos[[2]]),
                   color=!!sym(candidatos[[2]])>!!sym(candidatos[[1]]))) +
        geom_point() +
        theme(aspect.ratio = 1) +
        geom_abline(slope=1, intercept = 0) +
        scale_x_continuous(limits = c(0,1))+
        scale_y_continuous(limits = c(0,1))
    }
    else{
      ggplot() +
        geom_text(aes(x=1, y=1, label="Elija dos candidatos"))

    }

  }

}

#' Title
#'
#' @param carpeta
#' @param candidatos
#'
#' @return
#' @export
#'
#' @examples
graficar_estimaciones_tiempo_ev <- function(carpeta, candidatos){
  archivos <- list.files(path = carpeta,
                         pattern = ".rds",
                         full.names = T,
                         recursive = F,
                         all.files = F)
  estimaciones <- purrr::map_df(archivos,
                                ~{
                                  estimaciones <- .x %>%
                                    readRDS() %>%
                                    purrr::pluck("salidas.estimaciones")
                                  if(!is.null(estimaciones)){
                                    estimaciones <- estimaciones %>%
                                      mutate(remesa=stringr::str_extract(.x, "[^/]*$") %>%
                                               stringr::str_remove(".rds") %>%
                                               as.numeric())

                                  }
                                })

  estimaciones <- estimaciones %>%
    tidyr::pivot_longer(cols = {{candidatos}},
                        names_to = "candidato",
                        values_to = "estimaciones") %>%
    tidyr::pivot_wider(names_from = "LMU", values_from = "estimaciones")
  if(nrow(estimaciones)>0){
    estimaciones %>%
      ggplot(aes(x=remesa,
                 color=candidato))+
      geom_ribbon(aes(fill=candidato,
                      ymin=`0`,
                      ymax=`2`), alpha=.3)+
      geom_line(aes(y=`1`))+
      geom_point(aes(y=`1`))
  }

}

#' Title
#'
#' @param resultados
#'
#' @return
#' @export
#'
#' @examples
graficar_mensajes_ev <- function(resultados){
  g <- glue::glue("{resultados$remesas$info$mensaje}\n")
  if(!is.null(resultados$estimaciones$info))g <- glue::glue("{g}\n{resultados$estimaciones$info}")
  if(!is.null({resultados$salidas.mensaje}))g <- glue::glue("{g}\n{resultados$salidas.mensaje}")
  return(g)

}

#' Title
#'
#' @param resultados
#'
#' @return
#' @export
#'
#' @examples
graficar_arribo_remesas_ev <- function(resultados){
  g <- resultados$remesas$remesa %>% select(DIA, HORA, MINUTOS) %>%
    mutate(fecha=lubridate::ymd_hm(glue::glue("2021-06-{DIA} {HORA}:{MINUTOS}")),
           fecha=lubridate::floor_date(fecha, unit = "10 minutes")) %>%
    count(fecha) %>%
    mutate(n=cumsum(n)/sum(n)) %>%
    ggplot(aes(x=fecha, y=n))+ geom_point() +geom_smooth()


}
