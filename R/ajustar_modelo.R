#' Estima el porcentaje efectivo de voto para cada candidato
#'
#' @param muestra Base de datos de muestra. Debe contener las columnas con resultados por candidato, el identificador del estrato y la lista nominal.
#' @param id_estrato El nombre de la variable dentro de la base de datos que identifica a los estratos.
#' @param marco_muestral Base de datos que contiene el marco muestral. Debe contener un identificar de estrato y la lista nominal.
#' @param candidatos Un vector con los nombres de las variables de los candidatos cuya votación se desea estimar.
#' @param criterio_ce \bold{Default:} "2018".
#' @param n_sim El número de simulaciones para los parámetros.
#'
#' @description Implenta el modelo Mendoza, Nieto-Barajas (2016) para estimar el porcentaje de voto efectivo para cada candidato. Esta implementación incorpora las adaptaciones que se hicieron para la elección presidencial de 2018 documentadas en Orantes-Jordan (2019).
#' @seealso \href{https://www.sciencedirect.com/science/article/abs/pii/S0261379415300305}{Mendoza, Nieto-Barajas (2017)}
#' @return
#' @export
#' @importFrom purrr map2 map2 pmap
#' @import dplyr
#' @examples
ajustar_modelo <- function(muestra,
                           id_estrato,
                           marco_muestral,
                           candidatos,
                           criterio_ce,
                           nombre_estratos="",
                           nombre_eleccion="",
                           fuente="",
                           n_sim=10000,
                           regresar_sim=F){
  info <- info_estimacion(muestra = muestra,
                          id_estrato = {{id_estrato}} ,
                          marco_muestral =marco_muestral ,
                          candidatos=candidatos,
                          criterio_ce=criterio_ce,
                          nombre_estratos=nombre_estratos,
                          nombre_eleccion=nombre_eleccion,
                          fuente=fuente)
  # Ajustar casillas especiales
  bases_datos <- ajustar_casillas_especiales(muestra = info$muestra,
                                             marco_muestral = info$marco_muestral,
                                             criterio = criterio_ce)
  # Calcular pesos
  pesos <- calcular_pesos(marco_muestral = bases_datos$marco_muestral,
                          id_estrato = {{id_estrato}},
                          lista_nominal=LISTA_NOMINAL)
  # Datos de la muestra
  estratos_muestra <- datos_muestra(bases_datos$muestra,
                                    id_estrato = {{id_estrato}},
                                    candidatos = {{candidatos}})
  # Remplaza NA por 0. Preguntar.
  # Razones detectadas de NA:
  # 1. Falta de estrato en muestra
  estratos <- full_join(pesos,estratos_muestra) %>%
    mutate(across(everything(), ~tidyr::replace_na(.x, 0)))
  # Simular Gamma y Theta. Utiliza uniforme. Necesita ser Beta.
  # Revisar!!!!!!!
  estratos <- estimar_theta_gamma(estratos,
                                  n_sim=n_sim,
                                  n_candidatos=length(candidatos),
                                  part_historica=.5)

  # Simular lambdas
  simulaciones <- estratos %>%
    select({{id_estrato}}, candidato, peso, theta) %>%
    tidyr::unnest(theta) %>%
    group_by({{id_estrato}}, candidato) %>%
    # Ponderar theta por peso del estrato
    mutate(i=row_number(),theta=theta*peso) %>%
    # Sumar todos los estratos por simulación
    group_by(candidato, i) %>%
    summarise(theta=sum(theta)) %>%
    # Relativizar
    group_by(i) %>%
    mutate(PC=sum(theta),lambda=theta/PC)

  nacional <- simulaciones %>%
    # Resumir. Si queremos las simulaciones?
    group_by(candidato) %>%
    summarise(ic_025=quantile(lambda, probs = c(0.025),na.rm = F),
              ic_975=quantile(lambda, probs = c(0.975),na.rm = F),
              est_puntual=mean(lambda),
              `ic_025(PC)`=quantile(PC, probs = c(0.025),na.rm = F),
              `ic_975(PC)`=quantile(PC, probs = c(0.975),na.rm = F),
              `est_puntual(PC)`=mean(PC)
              )
  res <- list(nacional=nacional, estratos=estratos, info=info)
  if(regresar_sim) res <- append(res, simulaciones=simulaciones)

  return(res)
}



info_estimacion <- function(muestra,
                            marco_muestral,
                            id_estrato,
                            candidatos,
                            criterio_ce="2018",
                            nombre_estratos="",
                            nombre_eleccion="",
                            fuente=""){

  # Quitar grupos si los tiene
  muestra <- muestra %>% ungroup()
  marco_muestral <- marco_muestral %>% ungroup()

  # Contruir mensaje
  mensaje <- glue::glue("Se eliminaron {sum(is.na(muestra %>% pull({{id_estrato}})))} casillas de la muestra con valor NA en la variable de estratificación.
                        Se eliminaron {sum(is.na(marco_muestral %>% pull({{id_estrato}})))} casillas de la muestra con valor NA en la variable de estratificación.")

  # Eliminar tanto del marco muestral como de la muestra NA en variable de estratificación
  muestra <- muestra %>% filter(!is.na({{id_estrato}}))
  marco_muestral <- marco_muestral %>% filter(!is.na({{id_estrato}}))

  # Declarar elementos de la info
  info <- NULL
  info$muestra <- muestra
  info$marco_muestral <- marco_muestral
  info$id_estrato <- rlang::expr_text(rlang::expr(id_estrato))
  info$candidatos <- rlang::expr_text(rlang::expr(candidatos))
  info$criterio_ce <- criterio_ce
  info$nombre_estratos <- nombre_estratos
  info$nombre_eleccion <- nombre_eleccion
  info$fuente <- fuente

  return(info)
}

estimar_theta_gamma <- function(resumen, n_sim, n_candidatos, part_historica){
  resumen <- resumen %>%
    mutate(gamma=if_else(c_i>1,
                         map2(.x = alpha, .y=beta,
                              ~rgamma(n = n_sim, shape = .x,rate = .y)),
                         list(NA)),
           theta=if_else(c_i>1,
                         pmap(list(x=mu, y=gamma, z=n),
                              .f =  function(x, y, z){
                                truncnorm::rtruncnorm(n=n_sim,
                                                      mean = x,
                                                      sd = (y*z)^(-1/2),
                                                      a=0,
                                                      b=1)
                              } ),
                         map(c_i, ~{
                           b<-n_candidatos*.1/(part_historica)-.1
                           rbeta(shape1 = .1, shape2 = b, n_sim)
                         })))
  return(resumen)

}


