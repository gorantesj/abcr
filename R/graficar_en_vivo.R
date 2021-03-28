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
                                    estimacion="puntual"){
  if(!is.null(resultados$salidas.estimaciones)){
    if(estimacion=="puntual"){
      g <- resultados$salidas.estimaciones %>%
        tidyr::pivot_longer({{candidatos}},
                            names_to = "candidato",
                            values_to = "estimaciones") %>%
        filter(LMU==1) %>%
        ggplot() +
        geom_bar(aes(x=reorder(candidato, -estimaciones),
                     y=estimaciones), stat="identity") +
        labs(x="Candidato", y="Voto porcentual (%)")

    }
    if(estimacion=="intervalos"){
      g <- resultados$salidas.estimaciones %>%
        tidyr::pivot_longer({{candidatos}},
                            names_to = "candidato",
                            values_to = "estimaciones") %>%
        tidyr::pivot_wider(names_from = LMU, values_from = estimaciones) %>%
        ggplot(aes(x=reorder(candidato,-`2`))) +
        geom_linerange(aes(ymin=`0`, ymax=`2`)) +
        geom_point(aes(y=`1`)) +
        labs(x="Candidato", y="Voto porcentual (%)")

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
graficar_distribucion_ev <- function(resultados, candidato,
                                     por_estrato=F,
                                     por_casilla=F){
  if(!is.null(resultados$estimaciones$lambda)){
    cand <- rlang::expr_text(substitute(candidato))
    g <- resultados$estimaciones$lambda %>%
      filter(candidato == cand ) %>%
      ggplot(aes(x=theta, color=candidato))+
      geom_density()
    if(por_estrato){

      g1 <-  resultados$estimaciones$resumen_estratos %>%
        filter(candidato == cand ) %>%
        mutate(cota_inf=purrr::map_dbl(theta, ~quantile(.x, probs=c(0.025))),
               cota_sup=purrr::map_dbl(theta, ~quantile(.x, probs=c(0.975)))) %>%
        ggplot()+
        geom_linerange(aes(x=ID_ESTRATO_L,
                           ymin=cota_inf,
                           ymax=cota_sup,
                           size=peso,
                           color=as.factor(ID_ESTRATO_L))) +
        guides(color=F)+
        coord_flip()
      g <- (g/(g1 +theme_void()))+
        plot_layout(heights = c(7,3))

    }
    if(por_casilla){
      g2 <- resultados$remesas$remesa %>%
        ggplot(aes(x={{candidato}}/LISTA_NOMINAL,ymin=0, ymax=1))+
        geom_linerange()

      (g/(g2 +theme_void()))+
        plot_layout(heights = c(9,1)) &
        scale_x_continuous(limits = c(min(resultados$remesas$remesa %>%
                                            mutate(lambda={{candidato}}/LISTA_NOMINAL) %>%
                                            pull(lambda)),
                                      max(resultados$remesas$remesa %>%
                                            mutate(lambda={{candidato}}/LISTA_NOMINAL) %>%
                                            pull(lambda))))
    }
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
graficar_probabilidad_ev <- function(resultados){
  if(!is.null(resultados$estimaciones$lambda)){
    b$estimaciones$lambda %>%
      group_by(i) %>%
      summarise(ganador=unique(candidato)[which.max(lambda)]) %>%
      group_by(ganador) %>%
      summarise(prob=n()) %>%
      mutate(prob=prob/sum(prob)) %>%
      ggplot(aes(x=ganador)) +
      geom_linerange(aes(ymin=0, ymax=prob))+
      coord_polar(theta = "y") +
      scale_x_continuous(limits = c(0.5,1.5))
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
      ~.x %>%
        readRDS() %>%
        purrr::pluck("salidas.estimaciones") %>%
        mutate(remesa=stringr::str_extract(.x, "[^/]*$") %>%
                 stringr::str_remove(".rds") %>%
                 as.numeric())
        )
  estimaciones %>%
    tidyr::pivot_longer(cols = all_of(candidatos),
                        names_to = "candidatos",
                        values_to = "estimaciones") %>%
    tidyr::pivot_wider(names_from = "LMU", values_from = "estimaciones")
  if(nrow(estimaciones)>0){

    ggplot()+
      geom_ribbon(aes(x=remesa, ymin=))
  }

}

# mostrat_texto_ev <- function(resultad)
