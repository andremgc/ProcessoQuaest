tabela_contingencia <-function(variavel,
                               df = survey,
                               porcentagem = T,
                               dimensao = NULL,
                               margens = F,
                               varref = "voto1"){
    
    if(!is.character(variavel)) 
        stop("A entrada 'variavel' deve ser do tipo character")
    if(!(porcentagem %in% c(0,1,2) )) 
        stop("A entrada 'porcentagem' deve ser 0, 1 ou 2")
    if(sum (variavel %in% names(df) ) < length(variavel) )
        stop("A entrada 'variavel' deve conter apenas nomes de coluna de 'df'")
    if(anyDuplicated(variavel))
        stop("A entrada 'variavel' não deve conter valores repetidos")
    
    
    f <- as.formula(paste(paste0("~",varref,"+"),
                          paste(variavel, collapse="+")))
    
    tabela <-  xtabs(f,df) 
    
    if (porcentagem){
        tabela <- tabela %>% prop.table(dimensao) %>% `*`(100) %>% round(1) 
    }
    
    if (margens){
        tabela <- tabela %>% addmargins()  
    }
    
    return(tabela)
}
