# Neste script, vamos importar e tratar dados de população ao nível UF
    # Agora que vamos trabalhar em definitivo por UF, vamos importar esses
    # dados para definir qual a métrica que vamos trabalhar - medir o efeito
    # da Greve em número absoluto ou em taxa (por 100 mil habitantes)

# Carregando Pacotes
library(tidyverse)
library(magrittr)
library(lubridate)

#
homicidios_UF<-readRDS("Analise Exploratoria/dataframe_homicidios_UF.rds")

# Lendo os dados de População
    # Vamos importar dados de projeções de População ao nível UF
    # elaborados pelo IBGE.
    # Esses dados são projeções de pop. feitas em 2018 para os anos 2010-2060.
    # Vamos usar somente os dados de 2017 - ano da Greve

# Os dados foram baixados pelo caminho:
    # https://downloads.ibge.gov.br/downloads_estatisticas.htm#  >
        # Projecao_da_Populacao >
            # Projecao_da_Populacao_2018 >
                # projecoes_2018_populacao_2010_2060.xls

estados<-unique(homicidios_UF$UF)

# Vamos ler em loop - vamos usar os "estados" acima para ler as abas de
# pop de cada um deles. Precisamos da pop. para o estado inteiro - > linha
# 52. Vamos puxar linhas 49, 51 E 52 para selecionar o ano de 2017 depois e também para
# conferirmos o loop

lista_import<-list()

for(i  in 1:length(estados)){
lista_import[[estados[i]]]<- read_excel("Tratamento dos Dados/Dados Pop XLS/projecoes_2018_populacao_2010_2060.xls", 
              sheet = estados[i], skip = 49, n_max = 2)

}

for(i in 1:length(lista_import)){
        cat("\n", names(lista_import)[i], "\n")
        print(lista_import[[i]]$...9)}
# Parece que funcionou

# Montando tabela
df_pops<-cbind.data.frame(UF = NA, Populacao_proj_2017 = NA)

for (i in 1:length(lista_import)) {
    df_pops<-rbind.data.frame(df_pops,
                cbind.data.frame(UF = names(lista_import)[i], 
                                 Populacao_proj_2017 = lista_import[[i]]$...9[2]))
}

df_pops<-df_pops[-1,]

# Salvando
saveRDS(df_pops, "Tratamento dos Dados/data_frame_projecao_Pop_UF.rds")

# Limpando
rm(list = ls())
   