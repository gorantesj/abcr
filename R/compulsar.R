#' Title
#'
#' @param info
#' @param estimaciones_compulsadas
#' @param carpeta_remesas
#' @param carpeta_salida
#'
#' @return
#' @export
#'
#' @examples
compulsar <- function(info,
                      estado,
                      raiz,
                      carpeta_interna,
                      carpeta_remesas,
                      carpeta_salida,
                      estimaciones_compulsadas,
                      tiempo_max,
                      intevalo){
  # Información
  info <- info %>%
    filter(nombre_estado==estado)
  res <- NULL
  responsables <- info %>%
    select(starts_with("responsable")) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to ="responsable",
                        names_prefix = "responsable_",
                        values_to = "auxiliar",
                        values_drop_na = T) %>%
    pull(responsable)
  carpeta <- paste(raiz,
                   carpeta_interna,
                   "estimaciones",
                   info$abreviatura, sep="/")
  carpeta_remesas <-  paste(raiz,
                            carpeta_remesas,
                            info$abreviatura, sep="/")
  carpeta_salida <- paste(raiz,
                          carpeta_salida,
                          paste0("compulsado_",
                                 stringr::str_sub(info$abreviatura,1,3)), sep="/")
  res$tiempo_max <- tiempo_max
  res$intervalo <- intervalo
  # Leer estimaciones
  nombre_estimaciones <- map(responsables,
                             ~list.files(path = carpeta,
                                         pattern = .x,
                                         all.files = F,
                                         full.names = T,
                                         recursive = F,
                                         ignore.case = F)
  )
  # Alguna vacía
  if(sum(map_dbl(nombre_estimaciones, is_empty))>0){
    res$compulsado <- NULL
    res$mensaje <- "No hay estimaciones de alguno de los responsables en la carpeta correspondiente"
  }
  # Últimas estimaciones
  else{
    ultimas_estimaciones <-map(nombre_estimaciones,~rev(sort(.x))[[1]])
    # Ya habían sido analizadas?
    analizadas <- sum(map_dbl(ultimas_estimaciones,
                              ~(.x %in% estimaciones_compulsadas)))
    # No han sido analizadas
    if(analizadas==0){
      # Son iguales?
      unicos <- ultimas_estimaciones %>%
        map_chr(~.x %>% stringr::str_sub(-12, -5)) %>%
        n_distinct()
      if(unicos==1){
        # Compulsar-Union
        res$compulsado <- unir_estimaciones(ultimas_estimaciones)
      }
      else{
        # Se venció el tiempo
        if(Sys.time()>=tiempo_max){
          # Compulsar-Union
          res$compulsado <- unir_estimaciones(ultimas_estimaciones)

        }
        # No se ha vencido el tiempo
        else{
          res$compulsado <- NULL
          res$mensaje <- "Las estimaciones más recientes no son de las mismas remesas."
        }
      }
    }
    # 1 remesa analizada
    if(analizadas==1){
      # Se venció el tiempo
      if(Sys.time()>=tiempo_max){
        # Compulsar-Union
        res$compulsado <- unir_estimaciones(ultimas_estimaciones)

      }
      # No se ha vencido el tiempo
      else{
        res$compulsado <- NULL
        res$mensaje <- "No hay estimaciones nuevas de alguno de los responsables."
      }
    }
    # Más de 1 remesa ya analizada
    if(analizadas>1){
      res$compulsado <- NULL
      res$mensaje <- "No hay estimaciones nuevas de más de uno de los responsables."
    }
    if(!is.null(res$compulsado)) {
      res$tiempo_max <- res$tiempo_max+res$intervalo
      res$estimaciones_compulsadas <- c(estimaciones_compulsadas,
                                        reduce(ultimas_estimaciones,c)) %>%
        unique()
      info_remesas <- read_delim(file = list.files(carpeta_remesas,
                                                   pattern =  paste0(
                                                     "REMESAS02",
                                                     unique(res$compulsado$EN),
                                                     unique(res$compulsado$R)),
                                                   all.files = F,
                                                   full.names = T),
                                 delim="|",skip = 1)
      casillas_recibidas <- nrow(info_remesas)
      estratos_recibidos <- info_remesas %>%
        pull(ID_ESTRATO_L) %>% n_distinct()
      res$compulsado <- res$compulsado %>% mutate(ESTRATOS=info$m,
                                                  EST_REC=estratos_recibidos,
                                                  TOT_CAS=info$n,
                                                  CAS_REC=casillas_recibidas,
                                                  PORCENTAJE=round(100*CAS_REC/TOT_CAS,1))
      readr::write_excel_csv(x = res$compulsado,
                             file=paste0(carpeta_salida,
                                         "/compulsado",
                                         unique(res$compulsado$EN),
                                         unique(res$compulsado$R),
                                         ".csv"))
      # Escribir copia
      readr::write_excel_csv(x = res$compulsado,
                             file=paste0(carpeta,
                                         "/compulsado",
                                         unique(res$compulsado$EN),
                                         unique(res$compulsado$R),
                                         ".csv"))
      res$mensaje <- "Se realizó el compulsado con éxito"
    }
    return(res)
  }
  return(res)
}

#' Title
#'
#' @param archivos
#'
#' @return
#' @export
#'
#' @examples
unir_estimaciones <- function(archivos){
  compulsado <-  map(archivos, ~read_csv(.x) %>%
                       mutate(EN=as.character(EN),
                              R=as.character(R))) %>%
    reduce(bind_rows) %>%
    group_by(EN, LMU) %>%
    summarise(EQ="compulsado",
              R=max(R),
              across(where(is.numeric),
                     ~case_when(LMU==0~min(.x),
                                LMU==1~mean(.x),
                                LMU==2~max(.x)))) %>%
    distinct() %>%
    ungroup() %>%
    relocate(LMU, .after = last_col()) %>%
    relocate(EQ, .before = 1)
}


