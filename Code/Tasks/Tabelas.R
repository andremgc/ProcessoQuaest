#### Carregando pacotes, dados e funcoes ########
rm(list=ls())

library(openxlsx) # Abrir arquivo de entrada
library(dplyr) # Manipulação de dataframes
library(forcats) # Manipulação de fatores
library(stringr) # Manipulação de chars

source('./Code/Functions/trata_entrada.R', encoding = 'UTF-8')
source('./Code/Functions/tabelas_bivar.R')
source('./Code/Functions/tabela_contingencia.R', encoding = 'UTF-8')

entrada <- read.xlsx("./Data/bd_surveyquaest.xlsx",sheet=1)

#### Tratamento de dados ####

survey <- trata_entrada(entrada)

#### Tabelas de Contingência #####

# As mulheres e os homens estão votando no mesmo candidato ?
pergunta = tabela_contingencia("sexo", dimensao = 2)

# Parece haver uma maior preferência feminina ao candidato 2
# em relação aos homens, enquanto os homens parecem apresentar
# maior tendência a votar no candidato 1

# Exemplo simples bivariada com dados absolutos
sexo = tabela_contingencia("sexo", porcentagem = F)

# Exemplo simples bivariada com dados em porcento
aval_gov = tabela_contingencia("aval_gov")

# Exemplo bivariada com dados em porcento e margens
esc = tabela_contingencia("esc", margens = T)

# Exemplo bivariada com dados em porcento em relação ao voto
# Significado: Quantos porcento dos que disseram votar em um candidato 
# estão em cada categoria de idade
idade = tabela_contingencia("idade", dimensao = 1)

# Exemplo bivariada com dados em porcento em relação à outra variável
# Significado: Quantos porcento dos que tem certa renda dizem votar
# em cada candidato
rendaf = tabela_contingencia("rendaf", dimensao = 2)

# Exemplo multivariada simples com dados em porcento e margens
rendaf_sexo =  tabela_contingencia(c("rendaf","sexo"), margens = T)


# Exemplo multivariada simples com dados em porcento para uma dada
# intenção de voto e um dado sexo.
idade_sexo = tabela_contingencia(c("idade", "sexo"), dimensao = c(1,3))




write.csv2(pergunta, "./Results/Tables/pergunta.csv")
write.csv2(sexo, "./Results/Tables/sexo.csv")
write.csv2(aval_gov, "./Results/Tables/aval_gov.csv")
write.csv2(esc, "./Results/Tables/esc.csv")
write.csv2(idade, "./Results/Tables/idade.csv")
write.csv2(rendaf, "./Results/Tables/rendaf.csv")
write.csv2(rendaf_sexo, "./Results/Tables/rendaf_sexo.csv")
write.csv2(idade_sexo, "./Results/Tables/idade_sexo.csv")


print(pergunta)
print(aval_gov)
print(esc)
print(idade)
print(rendaf)
print(rendaf_sexo)
print(idade_sexo)

