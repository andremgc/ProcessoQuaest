trata_entrada <- function (entrada,
                           reduz_rendaf = T,
                           reduz_esc = T,
                           reduz_aval = T,
                           categoriza_idade = T
){
    
    
    saida <- entrada %>%
        # Transformando variaveis chr em fct
        mutate_if(is.character,as.factor) %>%
        
        # Ordenando voto de acordo com a frequencia
        mutate(voto1 = voto1 %>% fct_infreq %>%
                   # Colocando "Ninguém/Branco/Nulo" e "NS/NR" no final
                   fct_relevel("Ninguém/Branco/Nulo","NS/NR", after = Inf)
        ) %>%
        
        # Renomeando e reordenando fatores de renda fixa
        mutate(rendaf = rendaf %>% fct_relabel(function(x)
            # Obtem conteúdo entre parênteses
            x %>%  str_extract("\\(.*\\)$") %>%
                # Remove caracteres especiais
                str_remove_all("[^[:alnum:] ]") %>%
                # Remove barra de espaço no inicio
                str_remove("^ ") %>% 
                # Coloca um barra de espaço entre número e SM
                str_replace("([0-9])S", "\\1 S") %>%
                # Remove a palaVra SM
                str_remove_all(" SM") %>%
                # Ajuste para último level
                str_replace("^de 20$", "mais de 20"))
            
            # Reordendnando fatores
            %>% fct_relevel(
                "de 10 até 15", "de 15 até 20", after = 5)
        ) 
    
    # Se solicitado, reduz o número de fatores de renda fixa, agrupando as 
    # categorias com mais de 10 salários mínimos.
    
    if(reduz_rendaf){
        saida <- saida %>% 
            mutate(rendaf = rendaf %>% fct_collapse(
                "mais de 10" = c("de 10 até 15", 
                                 "de 15 até 20", 
                                 "mais de 20")
            )
            ) 
    }
    
    
    # Se solicitado, reduz o número de fatores de escolaridade, considerando se
    # completou ensino fundamental, médio e superior.
    # Se não solicitado, apenas reordena os fatores.
    
    if(reduz_esc){
        saida <- saida %>%
            mutate(esc = esc %>% fct_collapse(
                "Não completou EF" = c(
                    "Sem instrução e menos de 1 ano de estudo",
                    "Ensino fundamental incompleto"),
                "Ensino Fundamental" = c(
                    "Ensino fundamental completo",
                    "Ensino médio incompleto"),
                "Ensino Médio" = c(
                    "Ensino médio completo",
                    "Ensino superior incompleto") ,
                "Ensino Superior" =  "Ensino superior completo") %>%
                    
                    fct_relevel("Não completou EF")
            )
    } else {
        saida <- saida %>% 
            mutate(esc = esc %>% fct_relevel(
                "Sem instrução e menos de 1 ano de estudo",
                "Ensino fundamental incompleto",
                "Ensino fundamental completo",
                "Ensino médio incompleto",
                "Ensino médio completo",
                "Ensino superior incompleto",
                "Ensino superior completo" 
            ))
    }
    
    # Se solicitado, reduz o número de categorias de avaliação do governo em 
    # boa/ótima, regular e ruim/péssima.
    # Se não solicitado, apenas reordena os fatores.    
    
    if(reduz_aval){
        saida <- saida %>%
            # Ordenando avaliação do governo
            mutate(aval_gov = aval_gov %>% fct_collapse(
                "Boa/Ótima" = c("Boa", "Ótima"),
                "Regular" = c("Regular positiva", "Regular negativa"),
                "Ruim/Péssima" = c("Ruim", "Péssima")
            ) %>% fct_relevel(c("Boa/Ótima","Regular","Ruim/Péssima")))
    } else {
        saida <- saida %>%
            mutate(aval_gov = aval_gov %>% fct_relevel(
                "Péssima",
                "Ruim" ,
                "Regular negativa",
                "Regular positiva",
                "Boa",
                "Ótima" ,
                "NS/NR"
            ))
    }
    
    
    
    # Se solicitado, transforma idade em fatores de acordo com critério utilizado
    # pelo Datafolha
    
    if(categoriza_idade){
        saida <- saida %>% 
            mutate(idade = idade %>% cut(breaks = c(0, 24, 34, 44, 59, 200)) %>%
                       fct_recode("de 16 a 24" = "(0,24]",
                                  "de 25 a 34" = "(24,34]",
                                  "de 35 a 44" = "(34,44]",
                                  "de 45 a 59" = "(44,59]",
                                  "mais de 60" = "(59,200]") 
            ) 
    }
}