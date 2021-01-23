#' Ajusta el marco muestral y la muestra de acuerdo al criterio que se utilice
#'
#' @param muestra La muestra con la que se realiza las estimaciones. Debe contener la columna LISTA_NOMINAL.
#' @param marco_muestral El marco muestral del que se extrae la muestra. Debe contener la columnas ID_ESTADO, ID_DISTRITO, SECCION, TIPO_CASILLA, EXT_CONTIGUA, ID_CASILLA, y LISTA_NOMINA. Las casillas especiales deben tener la letra 'S'.
#' @param criterio Criterio para ajustar la lista nominal de las casillas especiales, puesto que por su naturaleza no cuentan con lista nominal. Las opciones disponibles son '2018'.
#' *2018*: Agregar la lista nominal máxima a las casillas especiales (750). Posteriormente, restar a todas las casillas de manera uniforme el incremento en la lista nominal.
#'
#'
#' @return
#' @export
#' @import dplyr
#' @examples
ajustar_casillas_especiales <- function(muestra, marco_muestral, criterio){
  if(criterio=="2018"){
    especiales <- marco_muestral %>% count(especial=TIPO_CASILLA=="S")
    n_especiales <- especiales %>% filter(especial) %>% pull(n)
    nn_especiales <- especiales %>% filter(!especial) %>% pull(n)
    marco_muestral <- marco_muestral %>%
      mutate(LISTA_NOMINAL=if_else(TIPO_CASILLA=="S", 750,LISTA_NOMINAL),
             LISTA_NOMINAL=LISTA_NOMINAL-750*(n_especiales/nn_especiales))
             muestra <- muestra %>%
               select(-LISTA_NOMINAL) %>%
               inner_join(marco_muestral)

  }
  return(list(muestra=muestra,marco_muestral=marco_muestral))
}
