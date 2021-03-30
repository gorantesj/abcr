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
                                 info_eleccion,
                                 candidatos,
                                 otros,
                                 marco_muestral,
                                 id_casilla,
                                 id_estrato,
                                 lista_nominal,
                                 n_sim=10000,
                                 tipo_casilla=TIPO_CASILLA,
                                 resultados=NULL){
  res <- NULL
  # Info elección requiere: candidatos, identificador casilla,
  if(is.null(resultados)){
    # ajustar marco muestral
    # ajustar_marco_muestral
    especiales <- marco_muestral %>%
      count(especial=({{tipo_casilla}}=="S"))
    n_especiales <- especiales %>% filter(especial) %>% pull(n)
    nn_especiales <- especiales %>% filter(!especial) %>% pull(n)

    marco_muestral <- marco_muestral %>%
      mutate({{lista_nominal}}:=if_else({{tipo_casilla}}=="S", 750,as.numeric({{lista_nominal}})),
             {{lista_nominal}}:={{lista_nominal}}-750*(n_especiales/nn_especiales))
    resultados$marco_muestral <- marco_muestral
    # Calcular pesos
    resultados$pesos <- calcular_pesos(marco_muestral = marco_muestral,
                                       id_estrato = {{id_estrato}},
                                       lista_nominal={{lista_nominal}})
    resultados$contador <- 0
  }

  # Leer remesas ------------------------------------------------------------
  res <- leer_remesas(carpeta_remesas = carpeta_remesas,resultados = resultados)

  # Producir estimaciones ---------------------------------------------------
  if(res$remesas$info$nueva_remesa) {
    res <- producir_estimaciones(remesas = res,
                                 id_estrato={{id_estrato}},
                                 lista_nominal = {{lista_nominal}},
                                 candidatos={{candidatos}},
                                 resultados = resultados,
                                 n_sim=n_sim)


    # Salidas -----------------------------------------------------------------
    res <- construir_salidas(res,carpeta_unicom = carpeta_INE)

  }
  # Constuir resultados
  saveRDS(object = res,
          file = paste(carpeta_interna,
                       paste(res$contador,"rds", sep = "."),
                       sep="/"))
  return(res)
}


leer_remesas <- function(carpeta_remesas, resultados=NULL){
  info <- NULL
  # Lista de archivos.
  nombre_archivos <- list.files(carpeta_remesas,
                                full.names = T,
                                all.files = F,
                                recursive = F,
                                pattern = ".txt")
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
      arrange(desc(ctime)) %>%
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
  remesas$remesas$remesa <- remesas$remesas$remesa %>%
    mutate(LISTA_NOMINAL=if_else(LISTA_NOMINAL==0,
                                 450,
                                 as.numeric(LISTA_NOMINAL)))
  # Resumen estratos
  # Falta parametrizar el id_estrato. Nesting.
  resumen_estratos <- datos_muestra(remesas$remesas$remesa,
                                    id_estrato = {{id_estrato}},
                                    candidatos = {{candidatos}})
  resumen_estratos <- full_join(resultados$pesos,resumen_estratos) %>%
    complete(nesting(ID_ESTRATO_L,peso), candidato,
             fill = list(c=0,n=0,x2_n=0,x=0,mu=0,alpha=NA,beta=NA)) %>%
    filter(!is.na(candidato))
  # Primera vez
  if(is.null(resultados$estimaciones$resumen_estratos)){
    # Construir resumen inicial
    estimaciones$resumen_estratos <- resumen_estratos
    # Simular parámetros gamma y theta
    estimaciones$resumen_estratos <- estimaciones$resumen_estratos %>%
      estimar_theta_gamma(n_sim = n_sim)
    estimaciones$info <- "La nueva remesa contiene nueva información."

  }
  else{

    interseccion <- dplyr::semi_join(resultados$estimaciones$resumen_estratos,
                                     resumen_estratos,
                                     by=c("ID_ESTRATO_L", "candidato", "c"))
    # Checar que todas las casillas de la nueva remesa pertenezcan al marco muestral
    # Cambiar remesas$remesa
    diferencia <- dplyr::anti_join(resumen_estratos,
                                   resultados$estimaciones$resumen_estratos,
                                   by=c("ID_ESTRATO_L", "candidato", "c"))
    if(nrow(diferencia)==0){
      estimaciones$resumen_estratos <- resultados$estimaciones$resumen_estratos
      estimaciones$info <- "La nueva remesa no contiene nueva información."
    }
    else{

      diferencia <- diferencia %>%
        estimar_theta_gamma(n_sim = n_sim)
      estimaciones$resumen_estratos <- bind_rows(interseccion ,
                                                 diferencia)
      estimaciones$info <- "La nueva remesa contiene nueva información."
    }
  }
  # Existe información en resumen_estratos
  if(!is.null(estimaciones$resumen_estratos)){

    estimaciones$lambdas <-  estimaciones$resumen_estratos %>%
      select({{id_estrato}}, candidato, peso, theta) %>%
      tidyr::unnest(theta) %>%
      # Agrupa por estrato y candidato
      group_by({{id_estrato}}, candidato) %>%
      # Ponderar theta por peso del estrato
      mutate(i=row_number(),theta=theta*peso) %>%
      # Sumar todos los estratos por simulación para obtener theta nacional
      group_by(i,candidato) %>%
      summarise(theta=sum(theta)) %>%
      # Dividir
      mutate(PC=sum(theta),lambda=theta/PC)
  }
  # Regresar remesa, lambdas, thetas, participación
  res <- c(
    remesas,
    estimaciones=list(estimaciones)
  )
  return(res)
}

construir_salidas <- function(resultados,
                              equipo="equipo2",
                              carpeta_unicom
){

  estimaciones_unicom <- resultados$estimaciones$lambdas %>%
    group_by(candidato) %>%
    summarise(estimaciones=quantile(lambda,probs=c(.025,.5, .975)), LMU=0:2) %>%
    tidyr::pivot_wider(names_from=candidato, values_from=estimaciones,
                       names_prefix = "cand") %>%
    mutate(EQ=stringr::str_to_upper(equipo),EN=0) %>%
    select(EQ, EN, starts_with("cand"), LMU) %>%
    rename_with(.cols = starts_with("cand"),
                ~ stringr::str_replace(string = .,
                                       pattern = "cand",replacement = ""))

  readr::write_excel_csv(estimaciones_unicom,
                         file = paste(paste(carpeta_unicom,
                                            last(resultados$remesas$remesas_analizadas),
                                            sep="/"), "csv", sep = "."))
  mensaje <- "Se escribió exitosamente los resultados"
  res <- c(resultados,
           salidas=list(estimaciones=estimaciones_unicom,
                        mensaje=mensaje))
  return(res)

}
