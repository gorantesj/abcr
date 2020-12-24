#' Estima el porcentaje efectivo de voto para cada candidato
#'
#' @param muestra Base de datos de muestra. Debe contener las columnas con resultados por candidato, el identificador del estrato y la lista nominal.
#' @param id_estratos El nombre de la variable dentro de la base de datos que identifica a los estratos.
#' @param marco_muestral Base de datos que contiene el marco muestral. Debe contener un identificar de estrato y la lista nominal.
#' @param candidatos Un vector con los nombres de las variables de los candidatos cuya votación se desea estimar.
#' @param n_sim El número de simulaciones para los parámetros.
#' @description Implenta el modelo Mendoza, Nieto-Barajas (2016) para estimar el porcentaje de voto efectivo para cada candidato. Esta implementación incorpora las adaptaciones que se hicieron para la elección presidencial de 2018 documentadas en Orantes-Jordan (2019).
#' @seealso \url{https://www.sciencedirect.com/science/article/abs/pii/S0261379415300305}
#' @return
#' @export
#' @importFrom purrr map2 map2 pmap
#' @import dplyr
#' @examples
ajustar_modelo <- function(muestra,id_estratos , marco_muestral, candidatos,
                           n_sim=10000){
  # Ajustar casillas especiales
  bases_datos <- ajustar_casillas_especiales(muestra = muestra,
                                             marco_muestral = marco_muestral,
                                             criterio = "2018")
  # Calcular pesos
  pesos <- calcular_pesos(marco_muestral = bases_datos$marco_muestral,
                          id_estratos = {{id_estratos}})
  # Datos de la muestra
  estratos_muestra <- datos_muestra(bases_datos$muestra,
                                    id_estratos = {{id_estratos}},
                                    candidatos = {{candidatos}},
                                    n_sim = n_sim)
  # Simular parámetros
  pesos <- tidyr::expand(pesos, tidyr::nesting(!!ensym(id_estratos), peso),
                  candidato=unique(estratos_muestra$candidato))
  estratos <- full_join(pesos,estratos_muestra) %>%
    mutate(across(everything(), ~tidyr::replace_na(.x, 0)))
  estratos <- estratos %>%
    mutate(gamma=if_else(c>1,
                         map2(.x = alpha, .y=beta,
                              ~rgamma(n = n_sim, shape = .x,rate = .y)),
                         list(NA)),
           theta=if_else(c>1,
                         pmap(list(x=mu, y=gamma, z=n),.f =  function(x, y, z){
                           truncnorm::rtruncnorm(n=n_sim,
                                      mean = x,
                                      sd = (y*z)^(-1/2),
                                      a=0,
                                      b=1)
                         } ),
                         list(runif(n=10000))))
  # Simular lambdas
  nacional <- estratos %>%
    select(ESTRATO, candidato, peso, theta) %>%
    tidyr::unnest(theta) %>%
    group_by(ESTRATO ,candidato) %>%
    mutate(i=row_number(),theta=theta*peso) %>%
    group_by(candidato, i) %>%
    summarise(lambda=sum(theta))%>%
    group_by(i) %>%
    mutate(lambda=lambda/sum(lambda)) %>%
    group_by(candidato) %>%
    summarise(quantile(lambda, probs = c(0.025,0.975),na.rm=T))
  return(list(nacional=nacional, estratos=estratos))
}
