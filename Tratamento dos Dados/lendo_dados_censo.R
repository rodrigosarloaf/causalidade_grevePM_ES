# Vamos ler os dados de Indicadores Municipais do Censo 2010 e criar um ".rds"
# no final para nossa análise posterior - uma grande tabela com todos os indicadores
# por município

# Os dados foram baixados pelo caminho:
# https://downloads.ibge.gov.br/downloads_estatisticas.htm#  >
    # Censos >
        # Censo_Demografico_2010>
            # indicadores_sociais_municipais >
                # Brasil.zip

# Aqui, os dados vem em ".xls" ao nível município
# Serão usados para construção do Controle Sintético

# Esses dados estão em "Tratamento dos Dados/Dados RAR Indicadores Sociais Municipios Censo 2010"

# Abri manualmente e salvei cada uma das planilhas em 
# "Tratamento dos Dados/Dados XLS Indicadores Sociais Municipios Censo 2010"

# Chamando pacotes
library(tidyverse)
library(magrittr)
library(readr)
library(readxl)

# Tabela 1 ----
tab1 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab1.xls", 
                   col_names = FALSE, col_types = c("numeric", 
                   "text", "blank", "numeric", "numeric", 
                   "text", "numeric", "numeric", "blank"), 
                   skip = 8)

# Trocando nomes das colunas
colnames(tab1)<-c("Cod_Municipio", "UF", "Populacao_Total", "Perc_Urbana", "Perc_Rural",
                  "Perc_Homem", "Perc_Mulher")

# Lá embaixo tem fonte... Vamos verificar e retirar
tail(tab1)
tab1<-tab1[-c(5573, 5574), ]

# Checando coisas como "Até 5.000..." - tem algumas divisórias desnecessárias
head(tab1)
tab1<-tab1[-1, ]

which(is.na(tab1$Cod_Municipio))
View(tab1[which(is.na(tab1$Cod_Municipio)),]) # Vendo no Excel, parece que esse
    # padrão de não preencher UF está associado às divisórias de tamanhos
    # de municípios

tab1 %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                   UF = as.character(UF))
    # Agora parece que já temos o número de municípios correto - só falta 
    # verificar os erros como: porque temos colunas além de UF como character

str(tab1) # Rural não deveria ser character
# Alguns lugares que deveriam ser "0" estão com "-" no lugar

View(tab1[which(str_detect(tab1$Perc_Rural, "-")),]) 

tab1$Perc_Rural[tab1$Perc_Rural == "-"]<-0

tab1$Perc_Rural<-as.numeric(tab1$Perc_Rural)

# Checando se finalizamos
anyNA(tab1) # OK

# Tabela 2 ----
# 2A
tab2_total <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab2.xls", 
            sheet = "tab2A", col_names = FALSE, col_types = c("numeric", 
            "text", "blank", "numeric", "numeric", 
            "numeric", "numeric", "numeric", 
            "numeric", "numeric"), skip = 10)

# Checando a estrutura
str(tab2_total) # todos os que deviam ser numéricos estão ok - não vamos ter que
        # corrigir isso

# Mudando nomes das colunas
head(tab2_total)
colnames(tab2_total)<-c("Cod_Municipio", "UF", "Populacao_Residente_Total",
                        "Perc_0e5anos_Total", "Perc_6e14anos_Total",
                        "Perc_15e24anos_Total", "Perc_25e39anos_Total",
                        "Perc_40e59anos_Total", "Perc_60e+anos_Total")

# Removendo divisórias de tamanho dos municípios
which(is.na(tab2_total$Cod_Municipio))
View(tab2_total[which(is.na(tab2_total$Cod_Municipio)),])

tab2_total %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                         UF = as.character(UF))

# 2B
tab2_urbana <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab2.xls", 
    sheet = "tab2B", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "numeric", "numeric", "numeric", 
    "numeric", "numeric"), skip = 10)

str(tab2_urbana)

head(tab2_urbana)
colnames(tab2_urbana)<-c("Cod_Municipio", "UF", "Populacao_Residente_Urbana",
                         "Perc_0e5anos_Urbana", "Perc_6e14anos_Urbana",
                         "Perc_15e24anos_Urbana", "Perc_25e39anos_Urbana",
                         "Perc_40e59anos_Urbana", "Perc_60e+anos_Urbana")

which(is.na(tab2_urbana$Cod_Municipio))
View(tab2_urbana[which(is.na(tab2_urbana$Cod_Municipio)),])
tab2_urbana %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                          UF = as.character(UF))

# 2C
tab2_rural <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab2.xls", 
        sheet = "tab2C", col_names = FALSE, col_types = c("numeric", 
        "text", "blank", "text", "text", 
        "text", "text", "text", "text", "text"), 
        skip = 10)

colnames(tab2_rural)<-c("Cod_Municipio", "UF", "Populacao_Residente_Rural",
                         "Perc_0e5anos_Rural", "Perc_6e14anos_Rural",
                         "Perc_15e24anos_Rural", "Perc_25e39anos_Rural",
                         "Perc_40e59anos_Rural", "Perc_60e+anos_Rural")

str(tab2_rural) # Os zeros devem estar com traços de novo

View(tab2_rural[which(str_detect(tab2_rural$Perc_6e14anos_Rural, "-")),]) 
# Substituindo por zeros
for(i in 3:length(colnames(tab2_rural))){
    vec_modificar<-as_vector(tab2_rural[,colnames(tab2_rural)[i]])
        names(vec_modificar)<-NULL
    tab2_rural[,colnames(tab2_rural)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                      "-", "0"))
}

str(tab2_rural)

which(is.na(tab2_rural$Cod_Municipio))
View(tab2_rural[which(is.na(tab2_rural$Cod_Municipio)),])

tab2_rural %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                         UF = as.character(UF))

# Tabela 3 ----
# 3A
tab3_total <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab3.xls", 
    sheet = "tab3A", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "text", "text", "numeric", "numeric", 
    "numeric", "numeric", "numeric", 
    "numeric"), skip = 11)

str(tab3_total) # temos colunas com problemas

colnames(tab3_total)<-c("Cod_Municipio", "UF",
                        "Total_Analfab_15-_Total", "Perc_Analfab_15-_Total",
                        "Total_Analfab_15a24_Total", "Perc_Analfab_15a24_Total",
                        "Total_Analfab_25a39_Total", "Perc_Analfab_25a39_Total",
                        "Total_Analfab_40a59_Total", "Perc_Analfab_40a59_Total",
                        "Total_Analfab_60+_Total", "Perc_Analfab_60+_Total")

View(tab3_total[which(str_detect(tab3_total$Total_Analfab_15a24_Total, "-")),]) 

# Substituindo por zeros
for(i in 3:length(colnames(tab3_total))){
    vec_modificar<-as_vector(tab3_total[,colnames(tab3_total)[i]])
    names(vec_modificar)<-NULL
    tab3_total[,colnames(tab3_total)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                                     "-", "0"))
}

str(tab3_total)

tab3_total %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                         UF = as.character(UF))

# 3B
tab3_branca <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab3.xls", 
    sheet = "tab3B", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "text", "text", "text", "text", "numeric", 
    "numeric", "numeric", "numeric"), 
    skip = 11)

str(tab3_branca)

colnames(tab3_branca)<-c("Cod_Municipio", "UF",
                        "Total_Analfab_15-_Branca", "Perc_Analfab_15-_Branca",
                        "Total_Analfab_15a24_Branca", "Perc_Analfab_15a24_Branca",
                        "Total_Analfab_25a39_Branca", "Perc_Analfab_25a39_Branca",
                        "Total_Analfab_40a59_Branca", "Perc_Analfab_40a59_Branca",
                        "Total_Analfab_60+_Branca", "Perc_Analfab_60+_Branca")

str(tab3_branca)

View(tab3_branca[which(str_detect(tab3_branca$Total_Analfab_15a24_Branca, "-")),]) 

# Substituindo por zeros
for(i in 3:length(colnames(tab3_branca))){
    vec_modificar<-as_vector(tab3_branca[,colnames(tab3_branca)[i]])
    names(vec_modificar)<-NULL
    tab3_branca[,colnames(tab3_branca)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                                     "-", "0"))
}


tab3_branca %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                          UF = as.character(UF))

# 3C
tab3_preta <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab3.xls", 
    sheet = "tab3C", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "text", 
    "text", "text", "text", "text", "text", 
    "text", "text", "text"), skip = 11)

str(tab3_preta)

colnames(tab3_preta)<-c("Cod_Municipio", "UF",
                         "Total_Analfab_15-_Preta", "Perc_Analfab_15-_Preta",
                         "Total_Analfab_15a24_Preta", "Perc_Analfab_15a24_Preta",
                         "Total_Analfab_25a39_Preta", "Perc_Analfab_25a39_Preta",
                         "Total_Analfab_40a59_Preta", "Perc_Analfab_40a59_Preta",
                         "Total_Analfab_60+_Preta", "Perc_Analfab_60+_Preta")

View(tab3_preta[which(str_detect(tab3_preta$Total_Analfab_15a24_Preta, "-")),]) 

# Substituindo por zeros
for(i in 3:length(colnames(tab3_preta))){
    vec_modificar<-as_vector(tab3_preta[,colnames(tab3_preta)[i]])
    names(vec_modificar)<-NULL
    tab3_preta[,colnames(tab3_preta)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                                     "-", "0"))
}

tab3_preta %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                         UF = as.character(UF))

# 3D
tab3_parda <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab3.xls", 
    sheet = "tab3D", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "text", 
    "text", "text", "text", "text", "text", 
    "text", "text", "text"), skip = 11)

str(tab3_parda)

colnames(tab3_parda)<-c("Cod_Municipio", "UF",
                        "Total_Analfab_15-_Parda", "Perc_Analfab_15-_Parda",
                        "Total_Analfab_15a24_Parda", "Perc_Analfab_15a24_Parda",
                        "Total_Analfab_25a39_Parda", "Perc_Analfab_25a39_Parda",
                        "Total_Analfab_40a59_Parda", "Perc_Analfab_40a59_Parda",
                        "Total_Analfab_60+_Parda", "Perc_Analfab_60+_Parda")

# Substituindo por zeros
for(i in 3:length(colnames(tab3_parda))){
    vec_modificar<-as_vector(tab3_parda[,colnames(tab3_parda)[i]])
    names(vec_modificar)<-NULL
    tab3_parda[,colnames(tab3_parda)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                                     "-", "0"))
}

tab3_parda %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                         UF = as.character(UF))

# 3E
tab3_amarela <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab3.xls", 
                         sheet = "tab3E", col_names = FALSE, col_types = c("numeric", 
                                                                           "text", "blank", "text", "text", 
                                                                           "text", "text", "text", "text", "text", 
                                                                           "text", "text", "text"), skip = 11)

str(tab3_amarela)

colnames(tab3_amarela)<-c("Cod_Municipio", "UF",
                        "Total_Analfab_15-_Amarela", "Perc_Analfab_15-_Amarela",
                        "Total_Analfab_15a24_Amarela", "Perc_Analfab_15a24_Amarela",
                        "Total_Analfab_25a39_Amarela", "Perc_Analfab_25a39_Amarela",
                        "Total_Analfab_40a59_Amarela", "Perc_Analfab_40a59_Amarela",
                        "Total_Analfab_60+_Amarela", "Perc_Analfab_60+_Amarela")

# Substituindo por zeros
for(i in 3:length(colnames(tab3_amarela))){
    vec_modificar<-as_vector(tab3_amarela[,colnames(tab3_amarela)[i]])
    names(vec_modificar)<-NULL
    tab3_amarela[,colnames(tab3_amarela)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                                     "-", "0"))
}

tab3_amarela %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                           UF = as.character(UF))

# 3F
tab3_indigena <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab3.xls", 
                         sheet = "tab3F", col_names = FALSE, col_types = c("numeric", 
                                                                           "text", "blank", "text", "text", 
                                                                           "text", "text", "text", "text", "text", 
                                                                           "text", "text", "text"), skip = 11)

str(tab3_indigena)

colnames(tab3_indigena)<-c("Cod_Municipio", "UF",
                        "Total_Analfab_15-_Indigena", "Perc_Analfab_15-_Indigena",
                        "Total_Analfab_15a24_Indigena", "Perc_Analfab_15a24_Indigena",
                        "Total_Analfab_25a39_Indigena", "Perc_Analfab_25a39_Indigena",
                        "Total_Analfab_40a59_Indigena", "Perc_Analfab_40a59_Indigena",
                        "Total_Analfab_60+_Indigena", "Perc_Analfab_60+_Indigena")

# Substituindo por zeros
for(i in 3:length(colnames(tab3_indigena))){
    vec_modificar<-as_vector(tab3_indigena[,colnames(tab3_indigena)[i]])
    names(vec_modificar)<-NULL
    tab3_indigena[,colnames(tab3_indigena)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                                     "-", "0"))
}

tab3_indigena %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                            UF = as.character(UF))

# Tabela 4 ----
# Por algum motivo, o "readr" não conseguia abrir esse. Tive que criar um novo arquivo e
# colar na mão. O original ainda está no ZIP

tab4 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab4.xlsx", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "text", "numeric", "numeric", "text", 
    "numeric", "numeric", "text"), skip = 9)

str(tab4)

colnames(tab4)<-c("Cod_Municipio", "UF", 
                  "Total_Domicilios", "Perc_Domicilios_1Responsavel", "Perc_Domicilios_maisDe1_Responsavel",
                  "Total_Domicilios_ResposavelHomem", "Perc_Domicilios_1Responsavel_Homem", "Perc_Domicilios_maisDe1_Responsavel_Homem",
                  "Total_Domicilios_ResposavelMulher", "Perc_Domicilios_1Responsavel_Mulher", "Perc_Domicilios_maisDe1_Responsavel_Mulher")

# Substituindo por zeros
for(i in 3:length(colnames(tab4))){
    vec_modificar<-as_vector(tab4[,colnames(tab4)[i]])
    names(vec_modificar)<-NULL
    tab4[,colnames(tab4)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                                           "-", "0"))
}

tab4 %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                   UF = as.character(UF))

# Tabela 5 ----
tab5 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab5.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "text", "numeric", "numeric", "text", 
    "numeric", "numeric", "text", "numeric", 
    "numeric", "text"), skip = 11)

str(tab5)

colnames(tab5)<-c("Cod_Municipio", "UF",
                  "Total_Domicilios", "Perc_Domicilios_1Responsavel", "Perc_Domicilios_maisDe1_Responsavel",
                  "Total_Domicilios_ResponsavelEConjuje", "Perc_Domicilios_ResponsavelEConjuje_1Responsavel", 
                  "Perc_Domicilios_ResponsavelEConjuje_maisde1Responsavel",
                  "Total_Domicilios_ResponsavelConjuje_maisFilhosEParentes", "Perc_Domicilios_ResponsavelEConjuje_1Responsavel_maisFilhosEParentes", 
                  "Perc_Domicilios_ResponsavelEConjuje_maisde1Responsavel_maisFilhosEParentes",
                  "Total_Domicilios_Outros", "Perc_Domicilios_1Responsavel_Outros", 
                  "Perc_Domicilios_maisde1Responsavel_Outros")

# Substituindo por zeros
for(i in 3:length(colnames(tab5))){
    vec_modificar<-as_vector(tab5[,colnames(tab5)[i]])
    names(vec_modificar)<-NULL
    tab5[,colnames(tab5)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                         "-", "0"))
}

tab5 %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                   UF = as.character(UF))

# Tabela 6 ----
# 6A
tab6A <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab6.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "numeric", "numeric", "numeric", 
    "numeric", "numeric"), skip = 9)

str(tab6A)

colnames(tab6A)<-c("Cod_Municipio", "UF",
                   "Total_Domicilios", "Perc_Unipessoais_Nucleares",
                   "Total_Domicilios_Nucleares", "Perc_Domicilios_Nucleares_CalsalSemFilhos",
                   "Perc_Domicilios_Nucleares_CasalComFilhos", "Perc_Domicilios_Nucleares_HomemComFilhos",
                   "Perc_Domicilios_Nucleares_MulherComFilhos")

tab6A %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# 6B
tab6B <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab6.xls", 
    sheet = "tab6B", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "numeric", "text", "numeric", "numeric"), 
    skip = 9)

str(tab6B)

colnames(tab6B)<-c("Cod_Municipio", "UF",
                   "Total_Domicilios_Estendida",
                   "Perc_Domicilios_Estendida_CalsalSemFilhosMaisParentes",
                   "Perc_Domicilios_Estendida_CasalComFilhosMaisParentes",
                   "Perc_Domicilios_Estendida_HomemComFilhosMaisParentes",
                   "Perc_Domicilios_Estendida_MulherComFilhosMaisParentes",
                   "Perc_Domicilios_Estendida_Outros")

# Substituindo por zeros
for(i in 3:length(colnames(tab6B))){
    vec_modificar<-as_vector(tab6B[,colnames(tab6B)[i]])
    names(vec_modificar)<-NULL
    tab6B[,colnames(tab6B)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                         "-", "0"))
}

tab6B %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# 6C
tab6C <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab6.xls", 
    sheet = "tab6C", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "text", 
    "text", "text", "text", "text"), 
    skip = 9)

str(tab6C)

colnames(tab6C)<-c("Cod_Municipio", "UF",
                   "Total_Domicilios_Composta",
                   "Perc_Domicilios_Composta_CalsalSemFilhosMaisOutros",
                   "Perc_Domicilios_Composta_CasalComFilhosMaisOutros",
                   "Perc_Domicilios_Composta_HomemComFilhosMaisOutros",
                   "Perc_Domicilios_Composta_MulherComFilhosMaisOutros",
                   "Perc_Domicilios_Composta_OutrosTipos")

# Substituindo por zeros
for(i in 3:length(colnames(tab6C))){
    vec_modificar<-as_vector(tab6C[,colnames(tab6C)[i]])
    names(vec_modificar)<-NULL
    tab6C[,colnames(tab6C)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab6C %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# Tabela 7 ----
# 7A
tab7A <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab7.xls", 
    sheet = "tab7A", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "text", 
    "numeric", "text"), skip = 11)

str(tab7A)

colnames(tab7A)<-c("Cod_Municipio", "UF",
                   "Total_Domicílios", 
                   "Perc_Saneamento_Adequado_Total", "Perc_Saneamento_SemiAdequado_Total",
                   "Perc_Saneamento_Inadequado_Total")

# Substituindo por zeros
for(i in 3:length(colnames(tab7A))){
    vec_modificar<-as_vector(tab7A[,colnames(tab7A)[i]])
    names(vec_modificar)<-NULL
    tab7A[,colnames(tab7A)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab7A %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# 7B
tab7B <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab7.xls", 
    sheet = "tab7B", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "text", 
    "text", "text"), skip = 11)

str(tab7B)

colnames(tab7B)<-c("Cod_Municipio", "UF",
                   "Total_Domicílios_Urbanos", 
                   "Perc_Saneamento_Adequado_Urbanos", "Perc_Saneamento_SemiAdequado_Urbanos",
                   "Perc_Saneamento_Inadequado_Urbanos")

# Substituindo por zeros
for(i in 3:length(colnames(tab7B))){
    vec_modificar<-as_vector(tab7B[,colnames(tab7B)[i]])
    names(vec_modificar)<-NULL
    tab7B[,colnames(tab7B)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab7B %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# 7C
tab7C <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab7.xls", 
    sheet = "tab7C", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "text", 
    "text", "text"),
    skip = 11)

str(tab7C)

colnames(tab7C)<-c("Cod_Municipio", "UF",
                   "Total_Domicílios_Rural", 
                   "Perc_Saneamento_Adequado_Rural", "Perc_Saneamento_SemiAdequado_Rural",
                   "Perc_Saneamento_Inadequado_Rural")

# Substituindo por zeros
for(i in 3:length(colnames(tab7C))){
    vec_modificar<-as_vector(tab7C[,colnames(tab7C)[i]])
    names(vec_modificar)<-NULL
    tab7C[,colnames(tab7C)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab7C %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# Tabela 8 ----
# 8A
tab8A <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab8.xls", 
    sheet = "tab8A", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "numeric", "numeric"), skip = 11)

str(tab8A)

colnames(tab8A)<-c("Cod_Municipio", "UF", "Rendimento_Mensal_Medio$_Total",
                   "Rendimento_Mensal_1Quartil$_Total", "Rendimento_Mensal_2Quartil$_Total",
                   "Rendimento_Mensal_3Quartil$_Total")

tab8A %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# 8B
tab8B <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab8.xls", 
    sheet = "tab8B", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "numeric", "numeric"), skip = 11)

str(tab8B)

colnames(tab8B)<-c("Cod_Municipio", "UF", "Rendimento_Mensal_Medio$_Urbana",
                   "Rendimento_Mensal_1Quartil$_Urbana", "Rendimento_Mensal_2Quartil$_Urbana",
                   "Rendimento_Mensal_3Quartil$_Urbana")

tab8B %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# 8C
tab8C <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab8.xls", 
    sheet = "tab8C", col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "text", 
    "text", "text"), skip = 11)

str(tab8C)

colnames(tab8C)<-c("Cod_Municipio", "UF", "Rendimento_Mensal_Medio$_Rural",
                   "Rendimento_Mensal_1Quartil$_Rural", "Rendimento_Mensal_2Quartil$_Rural",
                   "Rendimento_Mensal_3Quartil$_Rural")

# Substituindo por zeros
for(i in 3:length(colnames(tab8C))){
    vec_modificar<-as_vector(tab8C[,colnames(tab8C)[i]])
    names(vec_modificar)<-NULL
    tab8C[,colnames(tab8C)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab8C %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# Tabela 9 ----
tab9 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab9.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "numeric", 
    "numeric", "numeric", "blank", "blank"), 
    skip = 10)

str(tab9)

colnames(tab9)<-c("Cod_Municipio", "UF",
                  "Rendimento_Medio_Homem", "Rendimento_Medio_Mulher",
                  "Rendimento_Mediano_Homem", "Rendimento_Mediano_Mulher")

tab9 %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                   UF = as.character(UF))

# Tabela 10 ----
tab10 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab10.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "numeric", "text", 
    "numeric", "text", "text"), skip = 8)

str(tab10)

colnames(tab10)<-c("Cod_Municipio", "UF",
                   "Rendimento_Medio_Branca", "Rendimento_Medio_Preta",
                   "Rendimento_Medio_Parda", "Rendimento_Medio_Amarelo",
                   "Rendimento_Medio_Indigena")
# Substituindo por zeros
for(i in 3:length(colnames(tab10))){
    vec_modificar<-as_vector(tab10[,colnames(tab10)[i]])
    names(vec_modificar)<-NULL
    tab10[,colnames(tab10)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab10 %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# Tabela 11 ----
tab11 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab11.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "numeric", 
    "text", "text", "text"), skip = 8)

str(tab11)

colnames(tab11)<-c("Cod_Municipio", "UF",
                   "Razao_Medias_Rendimento_BrancaPreta", "Razao_Medias_Rendimento_BrancaParda",
                   "Razao_Medias_Rendimento_BrancaAmarela", "Razao_Medias_Rendimento_BrancaIndigena",
                   "Razao_Medias_Rendimento_PretaParda")

# Substituindo por zeros
for(i in 3:length(colnames(tab11))){
    vec_modificar<-as_vector(tab11[,colnames(tab11)[i]])
    names(vec_modificar)<-NULL
    tab11[,colnames(tab11)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab11 %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# Tabela 12 ----
# 12A
tab12A <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab12.xls", 
    sheet = "tab12A", col_names = FALSE, 
    col_types = c("numeric", "text", "blank", 
    "numeric", "text", "numeric", "numeric", 
    "blank"), skip = 10)

str(tab12A)

colnames(tab12A)<-c("Cod_Municipio", "UF",
                    "Populacao_Total", "Perc_PessoasEmDomicilios_ate70reais_PerCapita_Total",
                    "Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Total", 
                    "Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Total")

# Substituindo por zeros
for(i in 3:length(colnames(tab12A))){
    vec_modificar<-as_vector(tab12A[,colnames(tab12A)[i]])
    names(vec_modificar)<-NULL
    tab12A[,colnames(tab12A)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab12A %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                     UF = as.character(UF))

# 12B
tab12B <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab12.xls", 
    sheet = "tab12B", col_names = FALSE, 
    col_types = c("numeric", "text", "blank", 
    "numeric", "text", "text", "numeric", 
    "blank"), skip = 10)

str(tab12B)

colnames(tab12B)<-c("Cod_Municipio", "UF",
                    "Populacao_Urbana", "Perc_PessoasEmDomicilios_ate70reais_PerCapita_Urbana",
                    "Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Urbana", 
                    "Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Urbana")

# Substituindo por zeros
for(i in 3:length(colnames(tab12B))){
    vec_modificar<-as_vector(tab12B[,colnames(tab12B)[i]])
    names(vec_modificar)<-NULL
    tab12B[,colnames(tab12B)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                             "-", "0"))
}

tab12B %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                     UF = as.character(UF))

# 12C
tab12C <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab12.xls", 
    sheet = "tab12C", col_names = FALSE, 
    col_types = c("numeric", "text", "blank", 
    "text", "text", "text", "text", "blank"), 
    skip = 10)

str(tab12C)

colnames(tab12C)<-c("Cod_Municipio", "UF",
                    "Populacao_Rural", "Perc_PessoasEmDomicilios_ate70reais_PerCapita_Rural",
                    "Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Rural", 
                    "Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Rural")

# Substituindo por zeros
for(i in 3:length(colnames(tab12C))){
    vec_modificar<-as_vector(tab12C[,colnames(tab12C)[i]])
    names(vec_modificar)<-NULL
    tab12C[,colnames(tab12C)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                             "-", "0"))
}

tab12C %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                     UF = as.character(UF))

# Tabela 13 ----
tab13 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab13.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "text", 
    "text", "text", "blank"), skip = 10)

str(tab13)

colnames(tab13)<-c("Cod_Municipio", "UF",
                    "Populacao_Domicilio_SaneamentoInadequado_Total", 
                    "Perc_PessoasEmDomicilios_SaneamentoInadequado_ate70reais_PerCapita",
                    "Perc_PessoasEmDomicilios_SaneamentoInadequado_ate1/4SalMin_PerCapita", 
                    "Perc_PessoasEmDomicilios_SaneamentoInadequado_ate1/2SalMin_PerCapita")

# Substituindo por zeros
for(i in 3:length(colnames(tab13))){
    vec_modificar<-as_vector(tab13[,colnames(tab13)[i]])
    names(vec_modificar)<-NULL
    tab13[,colnames(tab13)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                             "-", "0"))
}

tab13 %<>% filter(!is.na(Cod_Municipio)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                                    UF = as.character(UF))

# Tabela 14 ----
tab14 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab14.xls", 
    col_names = FALSE, col_types = c("text", 
    "text", "blank", "text", "numeric", 
    "text", "text", "text", "numeric", 
    "text", "numeric"), skip = 10)

str(tab14)

colnames(tab14)<-c("Cod_Municipio", "UF",
                   "Taxa_Analfabetismo_+15anos_2000", "Taxa_Analfabetismo_+15anos_2010",
                   "Taxa_Analfabetismo_15a24anos_2000", "Taxa_Analfabetismo_15a24anos_2010",
                   "Taxa_Analfabetismo_25a59anos_2000", "Taxa_Analfabetismo_25a59anos_2010",
                   "Taxa_Analfabetismo_60+anos_2000", "Taxa_Analfabetismo_60+anos_2010")

# Substituindo por zeros
for(i in 3:length(colnames(tab14))){
    vec_modificar<-as_vector(tab14[,colnames(tab14)[i]])
    names(vec_modificar)<-NULL
    tab14[,colnames(tab14)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab14 %<>% filter(!is.na(UF)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                         UF = as.character(UF))

# Tabela 15 ----
tab15 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab15.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "text", 
    "text", "numeric", "text", "text"), 
    skip = 10)

str(tab15)

colnames(tab15)<-c("Cod_Municipio", "UF",
                   "Perc_Domicilios_Saneamento_Adequado_2000", "Perc_Domicilios_Saneamento_Adequado_2010",
                   "Perc_Domicilios_Saneamento_SemiAdequado_2000", "Perc_Domicilios_Saneamento_SemiAdequado_2010",
                   "Perc_Domicilios_Saneamento_Inadequado_2000", "Perc_Domicilios_Saneamento_Inadequado_2010")

# Substituindo por zeros
for(i in 3:length(colnames(tab15))){
    vec_modificar<-as_vector(tab15[,colnames(tab15)[i]])
    names(vec_modificar)<-NULL
    tab15[,colnames(tab15)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab15 %<>% filter(!is.na(UF)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                         UF = as.character(UF))

# Tabela 16 ----
tab16 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab16.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "numeric", 
    "text", "numeric", "text", "numeric"), 
    skip = 10)

str(tab16)

colnames(tab16)<-c("Cod_Municipio", "UF",
                   "Perc_Criancas0a5anos_Responsavel_Analfabeto_2000", "Perc_Criancas0a5anos_Responsavel_Analfabeto_2010",
                   "Perc_Criancas0a5anos_SaneamentoInadequado_2000", "Perc_Criancas0a5anos_SaneamentoInadequado_2010",
                   "Perc_Criancas0a5anos_SanIna_E_RespAnalf_2000", "Perc_Criancas0a5anos_SanIna_E_RespAnalf_2010")

# Substituindo por zeros
for(i in 3:length(colnames(tab16))){
    vec_modificar<-as_vector(tab16[,colnames(tab16)[i]])
    names(vec_modificar)<-NULL
    tab16[,colnames(tab16)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           "-", "0"))
}

tab16 %<>% filter(!is.na(UF)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                         UF = as.character(UF))

# Tabela 17 ----
tab17 <- read_excel("Tratamento dos Dados/Dados XLS Indicadores Sociais Municipais Censo 2010/tab17.xls", 
    col_names = FALSE, col_types = c("numeric", 
    "text", "blank", "text", "text"), 
    skip = 9)

str(tab17)

colnames(tab17)<-c("Cod_Municipio", "UF",
                   "Total_Pessoas10anos_naoLeemNemEscrevem",
                   "Perc_Pessoas10anos_naoLeemNemEscrevem")

# Substituindo por zeros
for(i in 3:length(colnames(tab17))){
    vec_modificar<-as_vector(tab17[,colnames(tab17)[i]])
    names(vec_modificar)<-NULL
    tab17[,colnames(tab17)[i]]<-as.numeric(str_replace_all(vec_modificar,
                                                           ".", "0"))
}

tab17 %<>% filter(!is.na(UF)) %>% mutate(Cod_Municipio = as.character(Cod_Municipio),
                                         UF = as.character(UF))


# Juntando, salvando e finalizando
    # Vou salvar numa lista para a nossa análise exploratória posteriormente
    # Vou salvar todas as tabelas sem pensar no que entra ou não. Isso vai ficar
    # Para uma análise já do efeito da Greve
lista_tabelas_censo<-list()
    nomes_tabs<-ls(pattern = "tab")
        nomes_tabs<-nomes_tabs[-1]
        
    for(i in 1:length(nomes_tabs)){
        lista_tabelas_censo[[nomes_tabs[i]]]<-get(nomes_tabs[i])
        }    
        
    saveRDS(lista_tabelas_censo, file = "Tratamento dos Dados/lista_tabelas_censo.rds")

    # Limpando meu environment
    rm(list = ls())
    