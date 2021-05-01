#### Carregando pacotes, dados e funcoes ########
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

#### Tratamento de dados ####

survey <- trata_entrada(entrada)

##### Gráfico 1 ####

# Definindo cor do fundo
cor="#EEEEFF"

# Gerando dados para o gráfico: 
# obtendo porcentagem de cada voto e colocando quebra de linha em "Ninguém/..."

df_plot1 <- survey %>% 
    group_by(voto1) %>% summarise(n = n()) %>% ungroup()%>%
    mutate(pct = n/sum(n)*100)%>%
    mutate(voto1 = voto1 %>%
               fct_recode("Ninguém/\nBranco/Nulo" = "Ninguém/Branco/Nulo"))

# Gerando gráfico 1

p <- ggplot(aes(x = fct_rev(voto1), y = pct), data = df_plot1) +
    
    geom_bar(stat = "identity", aes(fill=voto1))+
    
    geom_text(aes(label = pct %>% format(nsmall=1) %>% paste0("%"),
                  y=pct +0.5, hjust=0))+
    
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(df_plot1$pct)+7)
                       ) +
    
    scale_fill_manual(values = c(rep("#222277", 14), "#AAAAAA", "#AAAAAA"))+
    
    ggtitle("Pesquisa eleitoral: Intenção de voto") + 
    
    theme_classic()+
    
    theme(
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_text(vjust = 0.4),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none',
        plot.background = element_rect(fill = cor, colour = cor),
        panel.background = element_rect(fill = cor, colour = cor)
    ) + 
    coord_flip()

ggsave("./Results/Plots/Grafico1.png", p, width = 14.8, height = 11.1, units = "cm")

