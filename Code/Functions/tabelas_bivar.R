tabelas_bivar <-function(df, 
                         porcentagem = T,
                         dimensao = NULL,
                         margens = F,
                         varref = "voto1",
                         ignorar = c("voto1","voto1_red","voto1_red2")){
    
    df <- df[,sapply(df, is.factor)]
    for (v in names(df)){
        if (!(v %in% ignorar)){
            print(tabela_contingencia(v, df, porcentagem, dimensao,
                                      margens, varref))
        }
    }
    
}
