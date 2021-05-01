#### Carregando pacotes, dados e funcoes ####
rm(list=ls())

library(openxlsx) # Abrir arquivo de entrada
library(dplyr) # Manipulação de dataframes
library(forcats) # Manipulação de fatores
library(stringr) # Manipulação de chars
library(ggplot2) # Criação de gráficos

source('./Code/Functions/trata_entrada.R', encoding = 'UTF-8')
source('./Code/Functions/tabelas_bivar.R')
source('./Code/Functions/tabela_contingencia.R', encoding = 'UTF-8')

entrada <- read.xlsx("./Data/bd_surveyquaest.xlsx",sheet=1)

#### Tratamento de dados #####

survey <- trata_entrada(entrada)

# Manualmente cria coluna:
# voto1_red, que considera candidatos com menos de 1.0% dos votos como outros.

survey <- survey %>%
    mutate(voto1_red = voto1 %>% fct_collapse(
        Outros = c("Candidato 7", "Candidato 12", "Candidato 14",
                   "Candidato 11", "Candidato 13"))
    ) 

##### Grafico 2 ####

# Definindo cor do fundo
cor="#EEEEFF"


# Gerando dados para o gráfico:
# obtendo porcentagem de cada candidato para cada aval.

df_plot2 <- survey %>% 
    group_by(voto1_red,aval_gov) %>% summarise(n = n()) %>% ungroup() %>%
    group_by(aval_gov) %>%
    mutate(pct = n/sum(n)*100)%>%
    mutate(voto1_red = voto1_red %>%
               fct_recode("Ninguém/\nBranco/Nulo" = "Ninguém/Branco/Nulo")) %>%
    filter(aval_gov != "NS/NR") %>%
    mutate(aval_gov = aval_gov %>%
               fct_recode(
                   # paste0("Boa/Ótima\n",a["Boa/Ótima"],"%") ="Boa/Ótima",
                   # paste0("Regular\n",a["Regular"],"%") ="Regular",
                   "Ruim/Péssima\n11,3 %"="Ruim/Péssima"
               )
    )

# Obtendo a porcentagem das respostas de cada categoria de avaliacao

pct_aval <- table(survey$aval_gov) %>% prop.table() %>% `*`(100)


# Gerando gráfico 2

p <- ggplot(aes(fill=fct_rev(voto1_red), x = fct_rev(aval_gov), y = pct),
            data = df_plot2) +
    
    geom_bar(stat = "identity",position = "dodge")+ 
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(df_plot2$pct)+3),
                       breaks = seq(0,70,10), minor_breaks = seq(0,75,5)) +
    
    scale_x_discrete(labels = c(
        paste0("Ruim/Péssima\n(",pct_aval["Ruim/Péssima"],"% do total)"),
        paste0("Regular\n(",pct_aval["Regular"],"% do total)"),
        paste0("Boa/Ótima\n(",pct_aval["Boa/Ótima"],"% do total)")))+
    
    scale_fill_manual(values = 
                          rev(c(RColorBrewer::brewer.pal(8,"Set1")[1:5],
                                "#CCCC00",
                                RColorBrewer::brewer.pal(8,"Set1")[7:8],
                                "#772211", "#156426", "#AAAAAA", "#BBBBBB"))) +
    
    ggtitle("Pesquisa eleitoral:\nIntenção de voto por avaliação do governo") + 
    ylab("%")+
    
    theme_classic()+
    guides(fill = guide_legend(reverse = TRUE))+
    theme(axis.ticks=element_blank(),
          axis.text.y = element_text(vjust = 0.3),
          panel.grid.major.x =  element_line(colour = "#CCCCCC"),
          panel.grid.minor.x =  element_line(colour = "#E4E4E4"),
          axis.title.y = element_blank(),
          plot.background = element_rect(fill = cor, colour = cor),
          panel.background = element_rect(fill = cor, colour = cor),
          legend.background = element_rect(fill = cor, colour = cor),
          legend.title = element_blank())+ 
    coord_flip()

ggsave("./Results/Plots/Grafico2.png", p, width = 16, height = 12, units = "cm")
