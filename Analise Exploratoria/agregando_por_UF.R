# Agregando os dados para o nível Estado

# Carregando Pacotes
library(tidyverse)
library(magrittr)
library(lubridate)

# Importando dados
df_homicidios<-readRDS("Tratamento dos Dados/dataframe_homicidios_datasus.rds")
lista_censo<-readRDS("Tratamento dos Dados/lista_tabelas_censo.rds")

# Homicídios ----

df_UF<-df_homicidios %>%
    group_by(Data, UF) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ungroup()

rm(df_homicidios)

saveRDS(df_UF, "Analise Exploratoria/dataframe_homicidios_UF.rds")

rm(df_UF)

# Indicadores ----

# Vamos criar ao nível UF antes
lista_censo_UF<-list()

# 1
lista_censo_UF[["tab1"]]<-lista_censo$tab1 %>%
    group_by(UF) %>%
    summarise(Perc_Urbana = weighted.mean(Perc_Urbana, Populacao_Total),
              Perc_Rural = weighted.mean(Perc_Rural, Populacao_Total),
              Perc_Homem = weighted.mean(Perc_Homem, Populacao_Total),
              Perc_Mulher = weighted.mean(Perc_Mulher, Populacao_Total),
              Populacao_Total = sum(Populacao_Total))

# 2
# _total
head(lista_censo$tab2_total)
colnames(lista_censo$tab2_total)

lista_censo_UF[["tab2_total"]]<-lista_censo$tab2_total %>%
    group_by(UF) %>%
    summarise(Perc_0e5anos_Total = weighted.mean(Perc_0e5anos_Total, Populacao_Residente_Total),
              Perc_6e14anos_Total = weighted.mean(Perc_6e14anos_Total, Populacao_Residente_Total),
              Perc_15e24anos_Total = weighted.mean(Perc_15e24anos_Total, Populacao_Residente_Total),
              Perc_25e39anos_Total = weighted.mean(Perc_25e39anos_Total, Populacao_Residente_Total),
              Perc_40e59anos_Total = weighted.mean(Perc_40e59anos_Total, Populacao_Residente_Total),
              `Perc_60e+anos_Total` = weighted.mean(`Perc_60e+anos_Total`, Populacao_Residente_Total),
              Populacao_Residente_Total = sum(Populacao_Residente_Total))

# _urbana
head(lista_censo$tab2_urbana)
colnames(lista_censo$tab2_urbana)

lista_censo_UF[["tab2_urbana"]]<-lista_censo$tab2_urbana %>%
    group_by(UF) %>%
    summarise(Perc_0e5anos_Urbana = weighted.mean(Perc_0e5anos_Urbana, Populacao_Residente_Urbana),
              Perc_6e14anos_Urbana = weighted.mean(Perc_6e14anos_Urbana, Populacao_Residente_Urbana),
              Perc_15e24anos_Urbana = weighted.mean(Perc_15e24anos_Urbana, Populacao_Residente_Urbana),
              Perc_25e39anos_Urbana = weighted.mean(Perc_25e39anos_Urbana, Populacao_Residente_Urbana),
              Perc_40e59anos_Urbana = weighted.mean(Perc_40e59anos_Urbana, Populacao_Residente_Urbana),
              `Perc_60e+anos_Urbana` = weighted.mean(`Perc_60e+anos_Urbana`, Populacao_Residente_Urbana),
              Populacao_Residente_Urbana = sum(Populacao_Residente_Urbana))

# _rural
head(lista_censo$tab2_rural)
colnames(lista_censo$tab2_rural)

lista_censo_UF[["tab2_rural"]]<-lista_censo$tab2_rural %>%
    group_by(UF) %>%
    summarise(Perc_0e5anos_Rural = weighted.mean(Perc_0e5anos_Rural, Populacao_Residente_Rural),
              Perc_6e14anos_Rural = weighted.mean(Perc_6e14anos_Rural, Populacao_Residente_Rural),
              Perc_15e24anos_Rural = weighted.mean(Perc_15e24anos_Rural, Populacao_Residente_Rural),
              Perc_25e39anos_Rural = weighted.mean(Perc_25e39anos_Rural, Populacao_Residente_Rural),
              Perc_40e59anos_Rural = weighted.mean(Perc_40e59anos_Rural, Populacao_Residente_Rural),
              `Perc_60e+anos_Rural` = weighted.mean(`Perc_60e+anos_Rural`, Populacao_Residente_Rural),
              Populacao_Residente_Rural = sum(Populacao_Residente_Rural))

# 3
# _total
head(lista_censo$tab3_total)
colnames(lista_censo$tab3_total)

lista_censo_UF[["tab3_total"]]<-lista_censo$tab3_total %>%
        group_by(UF) %>%
        summarise(`Perc_Analfab_15-_Total` = weighted.mean(`Perc_Analfab_15-_Total`, `Total_Analfab_15-_Total`),   
                  `Total_Analfab_15-_Total` = sum(`Total_Analfab_15-_Total`),
                  `Perc_Analfab_15a24_Total` = weighted.mean(`Perc_Analfab_15a24_Total`, `Total_Analfab_15a24_Total`),  
                  `Total_Analfab_15a24_Total` = sum(`Total_Analfab_15a24_Total`), 
                  `Perc_Analfab_25a39_Total` = weighted.mean(`Perc_Analfab_25a39_Total`, `Total_Analfab_25a39_Total`),
                  `Total_Analfab_25a39_Total` = sum(`Total_Analfab_25a39_Total`), 
                  `Perc_Analfab_40a59_Total` = weighted.mean(`Perc_Analfab_40a59_Total`, `Total_Analfab_40a59_Total`), 
                  `Total_Analfab_40a59_Total` = sum(`Total_Analfab_40a59_Total`), 
                  `Perc_Analfab_60+_Total` = weighted.mean(`Perc_Analfab_60+_Total`, `Total_Analfab_60+_Total`),
                  `Total_Analfab_60+_Total` = sum(`Total_Analfab_60+_Total`))

# _branca
head(lista_censo$tab3_branca)
colnames(lista_censo$tab3_branca)

lista_censo_UF[["tab3_branca"]]<-lista_censo$tab3_branca %>%
    group_by(UF) %>%
    summarise(`Perc_Analfab_15-_Branca` = weighted.mean(`Perc_Analfab_15-_Branca`, `Total_Analfab_15-_Branca`),   
              `Total_Analfab_15-_Branca` = sum(`Total_Analfab_15-_Branca`),
              `Perc_Analfab_15a24_Branca` = weighted.mean(`Perc_Analfab_15a24_Branca`, `Total_Analfab_15a24_Branca`),  
              `Total_Analfab_15a24_Branca` = sum(`Total_Analfab_15a24_Branca`), 
              `Perc_Analfab_25a39_Branca` = weighted.mean(`Perc_Analfab_25a39_Branca`, `Total_Analfab_25a39_Branca`),
              `Total_Analfab_25a39_Branca` = sum(`Total_Analfab_25a39_Branca`), 
              `Perc_Analfab_40a59_Branca` = weighted.mean(`Perc_Analfab_40a59_Branca`, `Total_Analfab_40a59_Branca`), 
              `Total_Analfab_40a59_Branca` = sum(`Total_Analfab_40a59_Branca`), 
              `Perc_Analfab_60+_Branca` = weighted.mean(`Perc_Analfab_60+_Branca`, `Total_Analfab_60+_Branca`),
              `Total_Analfab_60+_Branca` = sum(`Total_Analfab_60+_Branca`))

# _preta
head(lista_censo$tab3_preta)
colnames(lista_censo$tab3_preta)

lista_censo_UF[["tab3_preta"]]<-lista_censo$tab3_preta %>%
    group_by(UF) %>%
    summarise(`Perc_Analfab_15-_Preta` = weighted.mean(`Perc_Analfab_15-_Preta`, `Total_Analfab_15-_Preta`),   
              `Total_Analfab_15-_Preta` = sum(`Total_Analfab_15-_Preta`),
              `Perc_Analfab_15a24_Preta` = weighted.mean(`Perc_Analfab_15a24_Preta`, `Total_Analfab_15a24_Preta`),  
              `Total_Analfab_15a24_Preta` = sum(`Total_Analfab_15a24_Preta`), 
              `Perc_Analfab_25a39_Preta` = weighted.mean(`Perc_Analfab_25a39_Preta`, `Total_Analfab_25a39_Preta`),
              `Total_Analfab_25a39_Preta` = sum(`Total_Analfab_25a39_Preta`), 
              `Perc_Analfab_40a59_Preta` = weighted.mean(`Perc_Analfab_40a59_Preta`, `Total_Analfab_40a59_Preta`), 
              `Total_Analfab_40a59_Preta` = sum(`Total_Analfab_40a59_Preta`), 
              `Perc_Analfab_60+_Preta` = weighted.mean(`Perc_Analfab_60+_Preta`, `Total_Analfab_60+_Preta`),
              `Total_Analfab_60+_Preta` = sum(`Total_Analfab_60+_Preta`))

# _parda
head(lista_censo$tab3_parda)
colnames(lista_censo$tab3_parda)

lista_censo_UF[["tab3_parda"]]<-lista_censo$tab3_parda %>%
    group_by(UF) %>%
    summarise(`Perc_Analfab_15-_Parda` = weighted.mean(`Perc_Analfab_15-_Parda`, `Total_Analfab_15-_Parda`),   
              `Total_Analfab_15-_Parda` = sum(`Total_Analfab_15-_Parda`),
              `Perc_Analfab_15a24_Parda` = weighted.mean(`Perc_Analfab_15a24_Parda`, `Total_Analfab_15a24_Parda`),  
              `Total_Analfab_15a24_Parda` = sum(`Total_Analfab_15a24_Parda`), 
              `Perc_Analfab_25a39_Parda` = weighted.mean(`Perc_Analfab_25a39_Parda`, `Total_Analfab_25a39_Parda`),
              `Total_Analfab_25a39_Parda` = sum(`Total_Analfab_25a39_Parda`), 
              `Perc_Analfab_40a59_Parda` = weighted.mean(`Perc_Analfab_40a59_Parda`, `Total_Analfab_40a59_Parda`), 
              `Total_Analfab_40a59_Parda` = sum(`Total_Analfab_40a59_Parda`), 
              `Perc_Analfab_60+_Parda` = weighted.mean(`Perc_Analfab_60+_Parda`, `Total_Analfab_60+_Parda`),
              `Total_Analfab_60+_Parda` = sum(`Total_Analfab_60+_Parda`))

# _amarela
head(lista_censo$tab3_amarela)
colnames(lista_censo$tab3_amarela)

lista_censo_UF[["tab3_amarela"]]<-lista_censo$tab3_amarela %>%
    group_by(UF) %>%
    summarise(`Perc_Analfab_15-_Amarela` = weighted.mean(`Perc_Analfab_15-_Amarela`, `Total_Analfab_15-_Amarela`),   
              `Total_Analfab_15-_Amarela` = sum(`Total_Analfab_15-_Amarela`),
              `Perc_Analfab_15a24_Amarela` = weighted.mean(`Perc_Analfab_15a24_Amarela`, `Total_Analfab_15a24_Amarela`),  
              `Total_Analfab_15a24_Amarela` = sum(`Total_Analfab_15a24_Amarela`), 
              `Perc_Analfab_25a39_Amarela` = weighted.mean(`Perc_Analfab_25a39_Amarela`, `Total_Analfab_25a39_Amarela`),
              `Total_Analfab_25a39_Amarela` = sum(`Total_Analfab_25a39_Amarela`), 
              `Perc_Analfab_40a59_Amarela` = weighted.mean(`Perc_Analfab_40a59_Amarela`, `Total_Analfab_40a59_Amarela`), 
              `Total_Analfab_40a59_Amarela` = sum(`Total_Analfab_40a59_Amarela`), 
              `Perc_Analfab_60+_Amarela` = weighted.mean(`Perc_Analfab_60+_Amarela`, `Total_Analfab_60+_Amarela`),
              `Total_Analfab_60+_Amarela` = sum(`Total_Analfab_60+_Amarela`))

# _indigena
head(lista_censo$tab3_indigena)
colnames(lista_censo$tab3_indigena)

lista_censo_UF[["tab3_indigena"]]<-lista_censo$tab3_indigena %>%
    group_by(UF) %>%
    summarise(`Perc_Analfab_15-_Indigena` = weighted.mean(`Perc_Analfab_15-_Indigena`, `Total_Analfab_15-_Indigena`),   
              `Total_Analfab_15-_Indigena` = sum(`Total_Analfab_15-_Indigena`),
              `Perc_Analfab_15a24_Indigena` = weighted.mean(`Perc_Analfab_15a24_Indigena`, `Total_Analfab_15a24_Indigena`),  
              `Total_Analfab_15a24_Indigena` = sum(`Total_Analfab_15a24_Indigena`), 
              `Perc_Analfab_25a39_Indigena` = weighted.mean(`Perc_Analfab_25a39_Indigena`, `Total_Analfab_25a39_Indigena`),
              `Total_Analfab_25a39_Indigena` = sum(`Total_Analfab_25a39_Indigena`), 
              `Perc_Analfab_40a59_Indigena` = weighted.mean(`Perc_Analfab_40a59_Indigena`, `Total_Analfab_40a59_Indigena`), 
              `Total_Analfab_40a59_Indigena` = sum(`Total_Analfab_40a59_Indigena`), 
              `Perc_Analfab_60+_Indigena` = weighted.mean(`Perc_Analfab_60+_Indigena`, `Total_Analfab_60+_Indigena`),
              `Total_Analfab_60+_Indigena` = sum(`Total_Analfab_60+_Indigena`))

# 4
head(lista_censo$tab4)
colnames(lista_censo$tab4)

lista_censo_UF[["tab4"]]<-lista_censo$tab4 %>%
    group_by(UF) %>%
    summarise(`Perc_Domicilios_1Responsavel` = weighted.mean(`Perc_Domicilios_1Responsavel`, `Total_Domicilios`),              
              `Perc_Domicilios_maisDe1_Responsavel` = weighted.mean(`Perc_Domicilios_maisDe1_Responsavel`, `Total_Domicilios`),
              `Total_Domicilios` = sum(`Total_Domicilios`),
              `Perc_Domicilios_1Responsavel_Homem` = weighted.mean(`Perc_Domicilios_1Responsavel_Homem`, `Total_Domicilios_ResposavelHomem`),
              `Perc_Domicilios_maisDe1_Responsavel_Homem` = weighted.mean(`Perc_Domicilios_maisDe1_Responsavel_Homem`, `Total_Domicilios_ResposavelHomem`), 
              `Total_Domicilios_ResposavelHomem` = sum(`Total_Domicilios_ResposavelHomem`),          
              `Perc_Domicilios_1Responsavel_Mulher` = weighted.mean(`Perc_Domicilios_1Responsavel_Mulher`, `Total_Domicilios_ResposavelMulher`),       
              `Perc_Domicilios_maisDe1_Responsavel_Mulher` = weighted.mean(`Perc_Domicilios_maisDe1_Responsavel_Mulher`, `Total_Domicilios_ResposavelMulher`),
              `Total_Domicilios_ResposavelMulher` = sum(`Total_Domicilios_ResposavelMulher`))

# 5
head(lista_censo$tab5)
colnames(lista_censo$tab5)

lista_censo_UF[["tab5"]]<-lista_censo$tab5 %>%
    group_by(UF) %>%
    summarise(Perc_Domicilios_1Responsavel = weighted.mean(Perc_Domicilios_1Responsavel, Total_Domicilios),                                              
              Perc_Domicilios_maisDe1_Responsavel = weighted.mean(Perc_Domicilios_maisDe1_Responsavel, Total_Domicilios),                                       
              Total_Domicilios = sum(Total_Domicilios),                                                          
              Perc_Domicilios_ResponsavelEConjuje_1Responsavel = weighted.mean(Perc_Domicilios_ResponsavelEConjuje_1Responsavel, Total_Domicilios_ResponsavelEConjuje),                          
              Perc_Domicilios_ResponsavelEConjuje_maisde1Responsavel = weighted.mean(Perc_Domicilios_ResponsavelEConjuje_maisde1Responsavel, Total_Domicilios_ResponsavelEConjuje),                    
              Total_Domicilios_ResponsavelEConjuje = sum(Total_Domicilios_ResponsavelEConjuje),                                      
              Perc_Domicilios_ResponsavelEConjuje_1Responsavel_maisFilhosEParentes = weighted.mean(Perc_Domicilios_ResponsavelEConjuje_1Responsavel_maisFilhosEParentes, Total_Domicilios_ResponsavelConjuje_maisFilhosEParentes),
              Perc_Domicilios_ResponsavelEConjuje_maisde1Responsavel_maisFilhosEParentes = weighted.mean(Perc_Domicilios_ResponsavelEConjuje_maisde1Responsavel_maisFilhosEParentes, Total_Domicilios_ResponsavelConjuje_maisFilhosEParentes),
              Total_Domicilios_ResponsavelConjuje_maisFilhosEParentes = sum(Total_Domicilios_ResponsavelConjuje_maisFilhosEParentes),                   
              Perc_Domicilios_1Responsavel_Outros = weighted.mean(Perc_Domicilios_1Responsavel_Outros, Total_Domicilios_Outros),
              Perc_Domicilios_maisde1Responsavel_Outros = weighted.mean(Perc_Domicilios_maisde1Responsavel_Outros, Total_Domicilios_Outros),
              Total_Domicilios_Outros = sum(Total_Domicilios_Outros))

# 6
# A
head(lista_censo$tab6A)
colnames(lista_censo$tab6A)

lista_censo_UF[["tab6A"]]<-lista_censo$tab6A %>%
    group_by(UF) %>%
    summarise(Total_Domicilios = sum(Total_Domicilios),
              Perc_Unipessoais_Nucleares = weighted.mean(Perc_Unipessoais_Nucleares, Total_Domicilios_Nucleares),              
              Perc_Domicilios_Nucleares_CalsalSemFilhos = weighted.mean(Perc_Domicilios_Nucleares_CalsalSemFilhos, Total_Domicilios_Nucleares),
              Perc_Domicilios_Nucleares_CasalComFilhos = weighted.mean(Perc_Domicilios_Nucleares_CasalComFilhos, Total_Domicilios_Nucleares),  
              Perc_Domicilios_Nucleares_HomemComFilhos = weighted.mean(Perc_Domicilios_Nucleares_HomemComFilhos, Total_Domicilios_Nucleares), 
              Perc_Domicilios_Nucleares_MulherComFilhos = weighted.mean(Perc_Domicilios_Nucleares_MulherComFilhos, Total_Domicilios_Nucleares),
              Total_Domicilios_Nucleares = sum(Total_Domicilios_Nucleares))

# B
head(lista_censo$tab6B)
colnames(lista_censo$tab6B)

lista_censo_UF[["tab6B"]]<-lista_censo$tab6B %>%
    group_by(UF) %>%
    summarise(Perc_Domicilios_Estendida_CalsalSemFilhosMaisParentes = weighted.mean(Perc_Domicilios_Estendida_CalsalSemFilhosMaisParentes, Total_Domicilios_Estendida),
              Perc_Domicilios_Estendida_CasalComFilhosMaisParentes = weighted.mean(Perc_Domicilios_Estendida_CasalComFilhosMaisParentes, Total_Domicilios_Estendida),
              Perc_Domicilios_Estendida_HomemComFilhosMaisParentes = weighted.mean(Perc_Domicilios_Estendida_HomemComFilhosMaisParentes, Total_Domicilios_Estendida),
              Perc_Domicilios_Estendida_MulherComFilhosMaisParentes = weighted.mean(Perc_Domicilios_Estendida_MulherComFilhosMaisParentes, Total_Domicilios_Estendida),
              Perc_Domicilios_Estendida_Outros = weighted.mean(Perc_Domicilios_Estendida_Outros, Total_Domicilios_Estendida),
              Total_Domicilios_Estendida = sum(Total_Domicilios_Estendida))

# C
head(lista_censo$tab6C)
colnames(lista_censo$tab6C)

lista_censo_UF[["tab6C"]]<-lista_censo$tab6C %>%
    group_by(UF) %>%
    summarise(Perc_Domicilios_Composta_CalsalSemFilhosMaisOutros = weighted.mean(Perc_Domicilios_Composta_CalsalSemFilhosMaisOutros, Total_Domicilios_Composta),
              Perc_Domicilios_Composta_CasalComFilhosMaisOutros = weighted.mean(Perc_Domicilios_Composta_CasalComFilhosMaisOutros, Total_Domicilios_Composta),  
              Perc_Domicilios_Composta_HomemComFilhosMaisOutros = weighted.mean(Perc_Domicilios_Composta_HomemComFilhosMaisOutros, Total_Domicilios_Composta), 
              Perc_Domicilios_Composta_MulherComFilhosMaisOutros = weighted.mean(Perc_Domicilios_Composta_MulherComFilhosMaisOutros, Total_Domicilios_Composta), 
              Perc_Domicilios_Composta_OutrosTipos = weighted.mean(Perc_Domicilios_Composta_OutrosTipos, Total_Domicilios_Composta),
              Total_Domicilios_Composta = sum(Total_Domicilios_Composta))

# 7
# A
head(lista_censo$tab7A)
colnames(lista_censo$tab7A)

lista_censo_UF[["tab7A"]]<-lista_censo$tab7A %>%
group_by(UF) %>%
    summarise(Perc_Saneamento_Adequado_Total = weighted.mean(Perc_Saneamento_Adequado_Total, Total_Domicílios),
              Perc_Saneamento_SemiAdequado_Total = weighted.mean(Perc_Saneamento_SemiAdequado_Total, Total_Domicílios),
              Perc_Saneamento_Inadequado_Total = weighted.mean(Perc_Saneamento_Inadequado_Total, Total_Domicílios),
              Total_Domicilios = sum(Total_Domicílios))

# B
head(lista_censo$tab7B)
colnames(lista_censo$tab7B)

lista_censo_UF[["tab7B"]]<-lista_censo$tab7B %>%
    group_by(UF) %>%
    summarise(Perc_Saneamento_Adequado_Urbanos = weighted.mean(Perc_Saneamento_Adequado_Urbanos, Total_Domicílios_Urbanos),
              Perc_Saneamento_SemiAdequado_Urbanos = weighted.mean(Perc_Saneamento_SemiAdequado_Urbanos, Total_Domicílios_Urbanos),
              Perc_Saneamento_Inadequado_Urbanos = weighted.mean(Perc_Saneamento_Inadequado_Urbanos, Total_Domicílios_Urbanos),
              Total_Domicilios_Urbanos = sum(Total_Domicílios_Urbanos))

# C
head(lista_censo$tab7C)
colnames(lista_censo$tab7C)

lista_censo_UF[["tab7C"]]<-lista_censo$tab7C %>%
    group_by(UF) %>%
    summarise(Perc_Saneamento_Adequado_Rural = weighted.mean(Perc_Saneamento_Adequado_Rural, Total_Domicílios_Rural),
              Perc_Saneamento_SemiAdequado_Rural = weighted.mean(Perc_Saneamento_SemiAdequado_Rural, Total_Domicílios_Rural),
              Perc_Saneamento_Inadequado_Rural = weighted.mean(Perc_Saneamento_Inadequado_Rural, Total_Domicílios_Rural),
              Total_Domicilios_Rural = sum(Total_Domicílios_Rural))

# 8
# A
head(lista_censo$tab8A)
colnames(lista_censo$tab8A)

lista_censo_UF[["tab8A"]]<-lista_censo$tab8A %>% 
    mutate(Populacao_Municipio = lista_censo$tab1$Populacao_Total) %>%
    group_by(UF) %>%
    summarise(`Rendimento_Mensal_Medio$_Total` = weighted.mean(`Rendimento_Mensal_Medio$_Total`, Populacao_Municipio),
              `Rendimento_Mensal_1Quartil$_Total` = weighted.mean(`Rendimento_Mensal_1Quartil$_Total`, Populacao_Municipio),
              `Rendimento_Mensal_2Quartil$_Total` = weighted.mean(`Rendimento_Mensal_2Quartil$_Total`, Populacao_Municipio),
              `Rendimento_Mensal_3Quartil$_Total`= weighted.mean(`Rendimento_Mensal_3Quartil$_Total`, Populacao_Municipio))
    
# B
head(lista_censo$tab8B)
colnames(lista_censo$tab8B)

lista_censo_UF[["tab8B"]]<-lista_censo$tab8B %>% 
    mutate(Populacao_Municipio = lista_censo$tab1$Populacao_Total) %>%
    group_by(UF) %>%
    summarise(`Rendimento_Mensal_Medio$_Urbana` = weighted.mean(`Rendimento_Mensal_Medio$_Urbana`, Populacao_Municipio),
              `Rendimento_Mensal_1Quartil$_Urbana` = weighted.mean(`Rendimento_Mensal_1Quartil$_Urbana`, Populacao_Municipio),
              `Rendimento_Mensal_2Quartil$_Urbana` = weighted.mean(`Rendimento_Mensal_2Quartil$_Urbana`, Populacao_Municipio),
              `Rendimento_Mensal_3Quartil$_Urbana`= weighted.mean(`Rendimento_Mensal_3Quartil$_Urbana`, Populacao_Municipio))

# C
head(lista_censo$tab8C)
colnames(lista_censo$tab8C)

lista_censo_UF[["tab8C"]]<-lista_censo$tab8C %>% 
    mutate(Populacao_Municipio = lista_censo$tab1$Populacao_Total) %>%
    group_by(UF) %>%
    summarise(`Rendimento_Mensal_Medio$_Rural` = weighted.mean(`Rendimento_Mensal_Medio$_Rural`, Populacao_Municipio),
              `Rendimento_Mensal_1Quartil$_Rural` = weighted.mean(`Rendimento_Mensal_1Quartil$_Rural`, Populacao_Municipio),
              `Rendimento_Mensal_2Quartil$_Rural` = weighted.mean(`Rendimento_Mensal_2Quartil$_Rural`, Populacao_Municipio),
              `Rendimento_Mensal_3Quartil$_Rural`= weighted.mean(`Rendimento_Mensal_3Quartil$_Rural`, Populacao_Municipio))

# 9
head(lista_censo$tab9)
colnames(lista_censo$tab9)

lista_censo_UF[["tab9"]]<-lista_censo$tab9 %>% 
    mutate(Populacao_Homem = lista_censo$tab1$Perc_Homem*lista_censo$tab1$Populacao_Total,
           Populacao_Mulher = lista_censo$tab1$Perc_Mulher*lista_censo$tab1$Populacao_Total) %>%
    group_by(UF) %>%
    summarise(Rendimento_Medio_Homem = weighted.mean(Rendimento_Medio_Homem, Populacao_Homem),
              Rendimento_Medio_Mulher = weighted.mean(Rendimento_Medio_Mulher, Populacao_Mulher),  
              Rendimento_Mediano_Homem = weighted.mean(Rendimento_Mediano_Homem, Populacao_Homem),
              Rendimento_Mediano_Mulher = weighted.mean(Rendimento_Mediano_Mulher, Populacao_Mulher))

# 10
head(lista_censo$tab10)
colnames(lista_censo$tab10)

# Deu problema aqui nessa agregação: nessa agregação está
# implícito que assumi mesma proporção de pop de cada cor no mesmo
# estado - não tem a info de percentual de cada cor na pop do município

lista_censo_UF[["tab10"]]<-lista_censo$tab10 %>%
    mutate(Populacao = lista_censo$tab1$Populacao_Total) %>%
    group_by(UF) %>%
    summarise(Rendimento_Medio_Branca = weighted.mean(Rendimento_Medio_Branca, Populacao),
              Rendimento_Medio_Preta = weighted.mean(Rendimento_Medio_Preta, Populacao),  
              Rendimento_Medio_Parda = weighted.mean(Rendimento_Medio_Parda, Populacao),
              Rendimento_Medio_Amarelo = weighted.mean(Rendimento_Medio_Amarelo, Populacao),
              Rendimento_Medio_Indigena = weighted.mean(Rendimento_Medio_Indigena, Populacao))

# 11
head(lista_censo$tab11)
colnames(lista_censo$tab11)

# Usa a tabela acima, portanto leva em conta a mesma hipótese implícita
lista_censo_UF[["tab11"]]<-lista_censo_UF$tab10 %>%
    mutate(Razao_Medias_Rendimento_BrancaPreta = (Rendimento_Medio_Branca/Rendimento_Medio_Preta),   
           Razao_Medias_Rendimento_BrancaParda = (Rendimento_Medio_Branca/Rendimento_Medio_Parda),
           Razao_Medias_Rendimento_BrancaAmarela = (Rendimento_Medio_Branca/Rendimento_Medio_Amarelo),
           Razao_Medias_Rendimento_BrancaIndigena = (Rendimento_Medio_Branca/Rendimento_Medio_Indigena),
           Razao_Medias_Rendimento_PretaParda = (Rendimento_Medio_Preta/Rendimento_Medio_Parda)) %>%
    select(-Rendimento_Medio_Branca, -Rendimento_Medio_Preta,
           -Rendimento_Medio_Parda, -Rendimento_Medio_Amarelo,
           -Rendimento_Medio_Indigena)

# 12
# A
head(lista_censo$tab12A)
colnames(lista_censo$tab12A)

lista_censo_UF[["tab12A"]]<-lista_censo$tab12A %>%
    group_by(UF) %>%
    summarise(`Perc_PessoasEmDomicilios_ate70reais_PerCapita_Total` = weighted.mean(`Perc_PessoasEmDomicilios_ate70reais_PerCapita_Total`, Populacao_Total),  
              `Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Total` = weighted.mean(`Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Total`, Populacao_Total),
              `Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Total` = weighted.mean(`Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Total`, Populacao_Total),
              Populacao_Total = sum(Populacao_Total))

# B
head(lista_censo$tab12B)
colnames(lista_censo$tab12B)

lista_censo_UF[["tab12B"]]<-lista_censo$tab12B %>%
    group_by(UF) %>%
    summarise(`Perc_PessoasEmDomicilios_ate70reais_PerCapita_Urbana` = weighted.mean(`Perc_PessoasEmDomicilios_ate70reais_PerCapita_Urbana`, Populacao_Urbana),  
              `Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Urbana` = weighted.mean(`Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Urbana`, Populacao_Urbana),
              `Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Urbana` = weighted.mean(`Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Urbana`, Populacao_Urbana),
              Populacao_Urbana = sum(Populacao_Urbana))

# C
head(lista_censo$tab12C)
colnames(lista_censo$tab12C)

lista_censo_UF[["tab12C"]]<-lista_censo$tab12C %>%
    group_by(UF) %>%
    summarise(`Perc_PessoasEmDomicilios_ate70reais_PerCapita_Rural` = weighted.mean(`Perc_PessoasEmDomicilios_ate70reais_PerCapita_Rural`, Populacao_Rural),  
              `Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Rural` = weighted.mean(`Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Rural`, Populacao_Rural),
              `Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Rural` = weighted.mean(`Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Rural`, Populacao_Rural),
              Populacao_Rural = sum(Populacao_Rural))

# 13
head(lista_censo$tab13)
colnames(lista_censo$tab13)

lista_censo_UF[["tab13"]]<-lista_censo$tab13 %>%
    group_by(UF) %>%
    summarise(`Perc_PessoasEmDomicilios_SaneamentoInadequado_ate70reais_PerCapita` = weighted.mean(`Perc_PessoasEmDomicilios_SaneamentoInadequado_ate70reais_PerCapita`, Populacao_Domicilio_SaneamentoInadequado_Total),  
              `Perc_PessoasEmDomicilios_SaneamentoInadequado_ate1/4SalMin_PerCapita` = weighted.mean(`Perc_PessoasEmDomicilios_SaneamentoInadequado_ate1/4SalMin_PerCapita`, Populacao_Domicilio_SaneamentoInadequado_Total),
              `Perc_PessoasEmDomicilios_SaneamentoInadequado_ate1/2SalMin_PerCapita` = weighted.mean(`Perc_PessoasEmDomicilios_SaneamentoInadequado_ate1/2SalMin_PerCapita`, Populacao_Domicilio_SaneamentoInadequado_Total),
              Populacao_Domicilio_SaneamentoInadequado_Total = sum(Populacao_Domicilio_SaneamentoInadequado_Total))

# 14
# Vai ser redundante com a "tab3_total" já que não temos as infos para
# agregar a de 2000

# 15
# Vai ser redundante com a "tab7A" já que não temos as infos para
# agregar a de 2000


# 16
# Não tem como agregar esse

# 17
head(lista_censo$tab17)
colnames(lista_censo$tab17)

lista_censo_UF[["tab17"]]<-lista_censo$tab17 %>%
    group_by(UF) %>%
    summarise(Perc_Pessoas10anos_naoLeemNemEscrevem = weighted.mean(Perc_Pessoas10anos_naoLeemNemEscrevem, Total_Pessoas10anos_naoLeemNemEscrevem),
              Total_Pessoas10anos_naoLeemNemEscrevem = sum(Total_Pessoas10anos_naoLeemNemEscrevem))

# Salvando
saveRDS(lista_censo_UF, "Analise Exploratoria/lista_censo_por_UF.rds")

