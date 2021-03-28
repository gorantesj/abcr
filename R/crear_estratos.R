#' Title
#'
#' @param condiciones
#' @param m_min
#'
#' @return
#' @export
#'
#' @examples
crear_estratos <- function(bd, nombre,condiciones, m_min){
  n_cond <- length(condiciones)
  # Agregar un estrato mayor a todos
  bd <- bd %>% mutate("e_{0}":=1)
  if(n_cond>1){
    for(i in n_cond:1){
      # Crear identificacodres por estrato considerando NA a los NA

      bd <-  bd %>%
        rowwise()%>%
        mutate(bandera=sum(is.na(c_across(condiciones[1:i])))) %>%
        group_by(!!!rlang::parse_exprs(condiciones[1:i])) %>%
        mutate("e_{i}":=if_else(bandera>0,NA_integer_,cur_group_id()),
               "t_{i}":=n()<100) %>%
        ungroup()

    }
    a %>% group_by(e_1) %>%
      mutate(e_1=case_when(sum(t_2)>0~min(e_2),
                           sum(t_2)==0~e_2))

  }
  else{

  }
  return(bd)
}

#' Title
#'
#' @param condiciones
#' @param m_min
#'
#' @return
#' @export
#'
#' @examples
crear_estratos1 <- function(bd, nombre,condicion, m_min, prefijo="estratificacion"){
  bd <- bd %>%
    group_by({{condicion}}) %>%
    mutate("{prefijo}_{nombre}":=if_else(is.na({{condicion}}),
                                       NA_integer_,
                                       cur_group_id())) %>%
    ungroup()
  return(bd)
}

crear_estratos2 <- function(bd, nombre,
                            condicion1,
                            condicion2,
                            m_min, prefijo="estratificacion"){
  bd <- bd %>%
    group_by({{condicion1}}, {{condicion2}}) %>%
    mutate("{prefijo}_{nombre}":=if_else(is.na({{condicion1}})|is.na({{condicion2}}),
                                  NA_integer_,
                                  cur_group_id()),
           aux=n()<m_min)
  bd <- bd %>%
    group_by({{condicion1}}) %>%
    mutate("{prefijo}_{nombre}":=case_when(sum(aux)>0~min(!!sym(glue::glue("{prefijo}_{nombre}"))),
                                        sum(aux)==0~!!sym(glue::glue("{prefijo}_{nombre}")))) %>%
    ungroup() %>%
    select(-aux)
  return(bd)
}
