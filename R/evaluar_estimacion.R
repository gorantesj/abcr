#' Title
#'
#' @param resultado
#' @param marco_muestral
#'
#' @return
#' @export
#'
#' @examples
evaluar_estimacion <- function(resultado, marco_muestral){
  cand <- resultado %>% pull(candidato) %>% unique()
  reales <- marco_muestral %>%
    summarise(across(all_of(cand),~sum(.x))) %>%
    pivot_longer(everything(), names_to="candidato", values_to="valor") %>%
    mutate(valor=valor/sum(valor))
  completa <- full_join(resultado, reales) %>%
    mutate(contenido=(round(valor,4)>=round(ic_025,4) &
                        round(valor)<=round(ic_975,4)))
  letrero <- completa %>%
    filter(!contenido) %>%
    mutate(referencia=if_else(valor>ic_975, ic_975, ic_025),
           diferencia=valor-referencia,
           referencia=case_when(referencia<max(ic_975)*.5 ~ ic_975,
                                referencia>max(ic_975)*.9 ~ ic_025))
  res <- completa %>%
    ggplot()+
    geom_linerange(aes(x=forcats::fct_reorder(candidato,ic_975),
                       ymin=ic_025,
                       ymax=ic_975), color="#f46036")+
    geom_point(aes(x=candidato, y=valor, color=as.factor(contenido))) +
    scale_x_discrete(name="Candidatos")+
    scale_y_continuous(labels = scales::percent_format(), name = "Votación (%)") +
    scale_color_manual(values = c("TRUE"="#1b998b","FALSE"="#e71d36"),
                       name="Contiene al valor",
                       labels=c("Sí","No"))+
    annotate(x=letrero$candidato,
             y=letrero$valor,
             label=round(letrero$diferencia,4),
             vjust=-0.5,
             nudge_y=.1,
             geom="text") +
    labs(title = "Estimación por intervalos y resultado de la votación",
         subtitle = "Intervalos al 95% de credibilidad") +
    theme_light()+
    coord_flip()
  return(res)
}
