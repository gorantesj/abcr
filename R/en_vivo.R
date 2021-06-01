#' Title
#'
#' @param carpeta_remesas
#' @param carpeta_INE
#' @param capeta_interna
#' @param resultados
#'
#' @return
#' @export
#'
#' @examples
correr_conteo_rapido <- function(carpeta_remesas,
                                 carpeta_INE,
                                 carpeta_interna,
                                 estado,
                                 info_eleccion,
                                 info_candidatos,
                                 marco_muestral,
                                 n_sim=10000,
                                 tipo_casilla=TIPO_CASILLA,
                                 resultados=NULL){
  info_eleccion <- info_eleccion %>% filter(nombre_estado==estado)
  marco_muestral <- marco_muestral %>%
    filter(ID_ESTADO==unique(info_eleccion$id_estado))
  info_candidatos <- info_candidatos %>%
    filter(ID_ESTADO==unique(info_eleccion$id_estado))
  candidatos <- c("CNR", "NULOS", info_candidatos$CANDIDATO)
  res <- NULL
  carpeta_remesas <-  paste(raiz,
                            carpeta_remesas,
                            info_eleccion$abreviatura,
                            sep="/")
  carpeta_INE <- paste(raiz,
                       carpeta_INE,
                       stringr::str_sub(info_eleccion$abreviatura,1,3),
                       sep = "/")
  carpeta_interna <- paste(raiz,
                           carpeta_interna,
                           sep = "/")
  # Info elección requiere: candidatos, identificador casilla,
  if(is.null(resultados)){
    especiales <- marco_muestral %>%
      count(especial=(TIPO_CASILLA=="S"))
    n_especiales <- especiales %>% filter(especial) %>% pull(n)
    nn_especiales <- especiales %>% filter(!especial) %>% pull(n)

    marco_muestral <- marco_muestral %>%
      mutate(LISTA_NOMINAL:=if_else(TIPO_CASILLA=="S", 750,
                                    as.numeric(LISTA_NOMINAL)),
             LISTA_NOMINAL:=LISTA_NOMINAL-
               750*(n_especiales/nn_especiales))
    resultados$marco_muestral <- marco_muestral
    # Calcular pesos
    resultados$pesos <- calcular_pesos(marco_muestral = marco_muestral,
                                       id_estrato = ID_ESTRATO_L,
                                       lista_nominal=LISTA_NOMINAL)
    resultados$contador <- 0
  }

  # Leer remesas ------------------------------------------------------------
  res <- leer_remesas(carpeta_remesas = carpeta_remesas,resultados = resultados)
  # Producir estimaciones ---------------------------------------------------
  if(res$remesas$info$nueva_remesa) {
    res <- producir_estimaciones(remesas = res,
                                 id_estrato=ID_ESTRATO_L,
                                 lista_nominal = LISTA_NOMINAL,
                                 candidatos=candidatos,
                                 resultados = resultados,
                                 n_sim=n_sim)


    # Salidas -----------------------------------------------------------------
    carpeta_interna_auxiliar <- paste(carpeta_interna,
                                      "estimaciones",
                                      info_eleccion$abreviatura,
                                      sep="/")

    res <- construir_salidas(res,
                             carpeta_unicom = carpeta_INE,
                             carpeta_interna=carpeta_interna_auxiliar)

  }
  # Constuir resultados
  saveRDS(object = res,
          file = paste(carpeta_interna,
                       "mendoza",
                       info_eleccion$abreviatura,
                       paste(res$contador,"rds", sep = "."),
                       sep="/"))
  return(res)
}


leer_remesas <- function(carpeta_remesas, resultados=NULL){
  info <- NULL
  # Lista de archivos.
  nombre_archivos <- list.files(carpeta_remesas,
                                pattern="REMESAS",
                                full.names = T,
                                all.files = F,
                                recursive = F)
  if(length(nombre_archivos)==0){
    info$nueva_remesa <- F
    info$mensaje <- "La carpeta de origen está vacía"
    remesa_nombre <- NULL
    remesa <- NULL
  }
  else{
    # Información de archivos
    archivos <- file.info(nombre_archivos,extra_cols = F)
    # Última remesa. Qué pasa si hay dos archivos creados al mismo tiempo?
    archivos <- archivos  %>%
      as_tibble(rownames = "nombre") %>%
      arrange(desc(nombre)) %>%
      slice(1)
    remesa_nombre <- archivos$nombre
    # Checar que la última remesa no haya sido analizada
    remesa_analizada <- remesa_nombre %in% resultados$remesas$remesas_analizadas
    if(remesa_analizada){
      info$nueva_remesa <- F
      info$mensaje <- "La última remesa ya ha sido analizada"
      remesa <- resultados$remesas$remesa
    }
    else{
      # Leer remesa
      remesa <- readr::read_delim(remesa_nombre,
                                  delim = "|",
                                  skip = 1)
      # TEMPORAL
      remesa_nombre <- remesa_nombre %>%
        stringr::str_extract(., "[^/]*$") %>%
        stringr::str_remove(".txt")
      if(nrow(remesa)==0){
        info$nueva_remesa <- F
        info$mensaje <- "Remesa aún sin información"
      }
      else{
        # Falta checar nombres de variable
        # Tipo de datos
        info$nueva_remesa <- T
        info$mensaje <- "Nueva remesa recibida"

      }
    }

  }

  # Contruir resultados de la lectura
  res <- list(remesas=list(
    remesa=remesa,
    info=info,
    remesas_analizadas=c(resultados$remesas$remesas_analizadas,
                         remesa_nombre)
  ),
  contador = resultados$contador+1,
  pesos=resultados$pesos,
  marco_muestral=resultados$marco_muestral)
  return(res)
}

producir_estimaciones <- function(remesas,
                                  id_estrato,
                                  lista_nominal,
                                  candidatos,
                                  n_sim=10000,
                                  resultados=NULL){

  estimaciones <- NULL
  # Ajustar la lista nominal de las remesas. PROVISIONAL!!!!!!
  # remesas$remesas$remesa <- remesas$remesas$remesa %>%
  #   mutate(CLAVE_CASILLA=paste(ID_ESTADO,
  #                   SECCION,
  #                   ID_CASILLA,
  #                   TIPO_CASILLA,
  #                   EXT_CONTIGUA, sep="-"))
  # Recordar borrar estrato y sustituir por ID_ESTRATO_L
  # Checar que coincidan todas
  # NOTA ENORME EN COLIMA NO COINCIDEN ID_ESTRATO_L con marco muestral
  remesas$remesas$remesa <- remesas$remesas$remesa %>%
    mutate(SECCION=as.numeric(SECCION)) %>%
    select(-LISTA_NOMINAL, -ID_ESTRATO_L) %>%
    inner_join(remesas$marco_muestral %>%
                 select(ID_ESTADO,
                        SECCION,
                        ID_CASILLA,
                        TIPO_CASILLA,
                        EXT_CONTIGUA,
                        LISTA_NOMINAL,
                        ID_ESTRATO_L))
  # Resumen estratos
  # Falta parametrizar el id_estrato. Nesting.
  resumen_estratos <- datos_muestra(remesas$remesas$remesa,
                                    id_estrato = ID_ESTRATO_L,
                                    candidatos = candidatos)
  resumen_estratos <- full_join(resultados$pesos,resumen_estratos) %>%
    complete(nesting(ID_ESTRATO_L,peso), candidato,
             fill = list(c_i=0,n=0,x2_n=0,x=0,mu=0,alpha=NA,beta=NA)) %>%
    filter(!is.na(candidato))
  # Primera vez
  if(is.null(resultados$estimaciones$resumen_estratos)){
    # Construir resumen inicial
    estimaciones$resumen_estratos <- resumen_estratos
    # Simular parámetros gamma y theta
    estimaciones$resumen_estratos <- estimaciones$resumen_estratos %>%
      estimar_theta_gamma(n_sim = n_sim,n_candidatos=length(candidatos), part_historica=.45)
    estimaciones$info <- "La nueva remesa contiene nueva información."

  }
  else{

    interseccion <- dplyr::semi_join(resultados$estimaciones$resumen_estratos,
                                     resumen_estratos,
                                     by=c("ID_ESTRATO_L", "candidato", "c_i"))
    # Checar que todas las casillas de la nueva remesa pertenezcan al marco muestral
    # Cambiar remesas$remesa
    diferencia <- dplyr::anti_join(resumen_estratos,
                                   resultados$estimaciones$resumen_estratos,
                                   by=c("ID_ESTRATO_L", "candidato", "c_i"))
    if(nrow(diferencia)==0){
      estimaciones$resumen_estratos <- resultados$estimaciones$resumen_estratos
      estimaciones$info <- "La nueva remesa no contiene nueva información."
    }
    else{

      diferencia <- diferencia %>%
        estimar_theta_gamma(n_sim = n_sim,
                            n_candidatos=length(candidatos),
                            part_historica=.45)
      estimaciones$resumen_estratos <- bind_rows(interseccion ,
                                                 diferencia)
      estimaciones$info <- "La nueva remesa contiene nueva información."
    }
  }
  # Existe información en resumen_estratos
  if(!is.null(estimaciones$resumen_estratos)){
    estimaciones$lambdas <-  estimaciones$resumen_estratos %>%
      select(ID_ESTRATO_L, candidato, peso, theta) %>%
      tidyr::unnest(theta) %>%
      # Agrupa por estrato y candidato
      group_by(ID_ESTRATO_L, candidato) %>%
      # Ponderar theta por peso del estrato
      mutate(i=row_number(),theta=theta*peso) %>%
      # Sumar todos los estratos por simulación para obtener theta nacional
      group_by(i,candidato) %>%
      summarise(theta=sum(theta)) %>%
      # Dividir
      mutate(PC=sum(theta),lambda=theta/PC)
    # Simular Participación
    estimaciones$participacion <-  estimaciones$lambdas %>%
      group_by(i) %>%
      summarise(PC=mean(PC))

    estimaciones$lambdas <- estimaciones$lambdas %>% select(-PC)
  }
  # Regresar remesa, lambdas, thetas, participación
  res <- c(
    remesas,
    estimaciones=list(estimaciones)
  )
  return(res)
}

construir_salidas <- function(resultados,
                              equipo="mendoza",
                              carpeta_unicom,
                              carpeta_interna
){
  # Remesa
  entidad <- last(resultados$remesas$remesas_analizadas) %>%
    stringr::str_remove("REMESAS") %>%
    stringr::str_sub(3,4)
  remesa <- last(resultados$remesas$remesas_analizadas) %>%
    stringr::str_remove("REMESAS") %>%
    stringr::str_sub(5)
  # Candidatos
  estimaciones_unicom <- resultados$estimaciones$lambdas %>%
    group_by(candidato) %>%
    summarise(estimaciones=round(100*quantile(lambda,probs=c(.025,.5, .975)),1),
              LMU=0:2) %>%
    tidyr::pivot_wider(names_from=candidato, values_from=estimaciones,
                       names_prefix = "cand") %>%
    mutate(EQ=equipo,EN=entidad, R=remesa) %>%
    select(EQ, EN,R, starts_with("cand"), LMU, -candCNR, -candNULOS) %>%
    rename_with(.cols = starts_with("cand"),
                ~ stringr::str_replace(string = .,
                                       pattern = "cand",replacement = ""))
  # Participación
  estimaciones_unicom_part <- resultados$estimaciones$participacion %>%
    summarise(PART=round(100*quantile(PC,probs=c(.025,.5, .975)),1),
              LMU=0:2)

  # Pegar participacion y candidatos
  estimaciones_unicom <- bind_cols(estimaciones_unicom %>% select(-LMU),
                                   estimaciones_unicom_part)

  # Escribir en la carpeta de salida
  readr::write_excel_csv(estimaciones_unicom,
                         file = paste(
                           paste(carpeta_unicom,
                                 paste0("mendoza",
                                        entidad,
                                        remesa),
                                 sep="/"), "csv", sep = "."))
  # Escribir copia en la carpeta de salida interna
  readr::write_excel_csv(estimaciones_unicom,
                         file = paste(carpeta_interna,
                                      paste0("mendoza",
                                             entidad,
                                             remesa,
                                             ".csv"),
                                      sep="/"))
  mensaje <- "Se escribió exitosamente los resultados"
  res <- c(resultados,
           salidas=list(estimaciones=estimaciones_unicom,
                        mensaje=mensaje))
  return(res)

}
