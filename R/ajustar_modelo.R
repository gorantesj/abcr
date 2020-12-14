#' Ajusta el modelo Mendoza, Nieto-Barajas (2016)
#'
#' @return
#' @export
#'
#' @examples
ajustar_modelo <- function(id_estratos ,muestra, marco_muestral, candidatos,
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
  pesos <- expand(pesos, nesting(!!ensym(id_estratos), peso),
                  candidato=unique(estratos_muestra$candidato))
  estratos <- full_join(pesos,estratos_muestra) %>%
    mutate(across(everything(), ~replace_na(.x, 0)))
  estratos <- estratos %>%
    mutate(gamma=if_else(c>1,
                         map2(.x = alpha, .y=beta,
                              ~rgamma(n = n_sim, shape = .x,rate = .y)),
                         list(NA)),
           theta=if_else(c>1,
                         pmap(list(x=mu, y=gamma, z=n),.f =  function(x, y, z){
                           rtruncnorm(n=n_sim,
                                      mean = x,
                                      sd = (y*z)^(-1/2),
                                      a=0,
                                      b=1)
                         } ),
                         list(runif(n=10000))))
  # Simular lambdas
  nacional <- estratos %>%
    select(ESTRATO, candidato, peso, theta) %>%
    unnest(theta) %>%
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
