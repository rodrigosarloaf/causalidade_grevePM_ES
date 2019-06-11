# Objetivos
    # Tentar descrever o ES antes, durante e depois da Greve em relação à série de homicídios
    # Comparar com os outros estados pré - ter uma ideia de se teremos um bom
    # contrafactural

# Primeiro Dia de Greve: 04/02/2017
# Último Dia de Greve: 24/02/2017 

# Carregando Pacotes
library(tidyverse)
library(magrittr)
library(lubridate)

# Séries Temporais de Homicídios ----

# Importando dados
df_homicidios<-readRDS("Tratamento dos Dados/dataframe_homicidios_datasus.rds")

# Primeiro, exibindo série de tempo do ES
    # Muito complexo analisar todas as séries temporais separadamente
df_ES<-df_homicidios %>% filter(UF == "ES")

df_ES$Ano<-year(df_ES$Data)
df_ES$Mes<-month(df_ES$Data)


# Exibindo ES
df_ES %>% filter(Data >= ymd("2016-01-01")) %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios no ES", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")
    
# Zoom nas Datas - Essa coisa de Zoom vai ficar legal com o HTMLWidgets
df_ES %>% filter(Data >= ymd("2016-07-01") & Data <= ymd("2017-07-01")) %>%
    group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios no ES", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Vitória
plt_vitoria<-df_ES %>% filter(Data >= ymd("2016-01-01"), Municipio == "VITORIA") %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios em Vitória", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Serra
plt_serra<-df_ES %>% filter(Data >= ymd("2016-01-01"), Municipio == "SERRA") %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios na Serra", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Viana
plt_viana<-df_ES %>% filter(Data >= ymd("2016-01-01"), Municipio == "VIANA") %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios em Viana", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Guarapari
plt_guarapari<-df_ES %>% filter(Data >= ymd("2016-01-01"), Municipio == "GUARAPARI") %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios em Guarapari", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Vila Velha
plt_vilavelha<-df_ES %>% filter(Data >= ymd("2016-01-01"), Municipio == "VILA VELHA") %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios em Vila Velha", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Fundão
plt_fundao<-df_ES %>% filter(Data >= ymd("2016-01-01"), Municipio == "FUNDAO") %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios em Fundão", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Cariacica
plt_cariacica<-df_ES %>% filter(Data >= ymd("2016-01-01"), Municipio == "CARIACICA") %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios em Cariacica", x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Realmente não me parece fazer sentido analisar ao nível de município
# Mesmo na Grande Vitória temos muitos zeros e contagens baixas.
# Imagine em Divino São Lourenço?
plt_divino<-df_ES %>% filter(Data >= ymd("2016-01-01"),
                 Municipio == "DIVINO DE SAO LOURENCO") %>% group_by(Data) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ggplot(aes(x = Data)) +
    geom_line(aes(y = Homicidios)) + theme_minimal() +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(title = "Série de Homicídios em Divino de São Lourenço",
         x = "Tempo", y = "Número de Homicídios",
         subtitle = "Linhas tracejadas indicam período de greve") +
    scale_x_date(date_breaks = "2 months")

# Homicídios ao nível de UF
df_UF<-df_homicidios %>%
    group_by(Data, UF) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    ungroup()

rm(df_homicidios)
rm(df_ES)

# Plotando ES contra os outros estados
estados<-unique(df_UF$UF)
    estados<-estados[-which(estados == "ES")]

df_ES<-df_UF %>% filter(Data >= ymd("2016-07-01") &
                        Data <= ymd("2017-07-01"),
                        UF == "ES")
    
lista_graf_ES_vs_Estados<-list()    

for(i in 1:length(estados)){
    df_aux<-df_UF %>% filter(Data >= ymd("2016-07-01") &
        Data <= ymd("2017-07-01"), UF == estados[i])
    
    lista_graf_ES_vs_Estados[[estados[i]]]<-ggplot(data = df_ES, aes(x = Data)) +
        geom_line(aes(y = Homicidios), col = "black") +
        theme_minimal() +
        geom_line(data = df_aux, aes(y = Homicidios), col = "red", alpha = 0.7) +
        geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
        geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
        labs(x = "Datas", y = "Número de Homicídios",
             title = paste0("Série de Homicídios ES vs ", estados[i]),
             caption = "Linhas tracejadas indicam período de greve",
             subtitle = paste0("ES em preto \n", estados[i], " em vermelho"))
}
    
# Vendo ow gráficos
for (i in 1:length(lista_graf_ES_vs_Estados)) {
    print(lista_graf_ES_vs_Estados[[i]])
    
    Sys.sleep(7)
}
# De fato ninguém mais foi "tratado" nesse período

# Agregando gráficos por região
source("Analise Exploratoria/multiplot.R")

# SE
multiplot(lista_graf_ES_vs_Estados$SP, lista_graf_ES_vs_Estados$RJ,
          lista_graf_ES_vs_Estados$MG, cols = 1)
# CO
multiplot(lista_graf_ES_vs_Estados$GO, lista_graf_ES_vs_Estados$MT,
          cols = 1)
multiplot(lista_graf_ES_vs_Estados$MS, lista_graf_ES_vs_Estados$DF,
          cols = 1)
# NE
multiplot(lista_graf_ES_vs_Estados$AL, lista_graf_ES_vs_Estados$BA,
          lista_graf_ES_vs_Estados$CE, cols = 1)
multiplot(lista_graf_ES_vs_Estados$MA, lista_graf_ES_vs_Estados$PB,
          lista_graf_ES_vs_Estados$PE, cols = 1)
multiplot(lista_graf_ES_vs_Estados$PI, lista_graf_ES_vs_Estados$RN,
          lista_graf_ES_vs_Estados$SE, cols = 1)
# N
multiplot(lista_graf_ES_vs_Estados$AC, lista_graf_ES_vs_Estados$AP,
          lista_graf_ES_vs_Estados$AM, cols = 1)
multiplot(lista_graf_ES_vs_Estados$PA, lista_graf_ES_vs_Estados$RO,
         cols = 1)
multiplot(lista_graf_ES_vs_Estados$RR, lista_graf_ES_vs_Estados$TO,
          cols = 1)
# S
multiplot(lista_graf_ES_vs_Estados$PR, lista_graf_ES_vs_Estados$RS,
          lista_graf_ES_vs_Estados$SC, cols = 1)

rm(df_aux)
rm(df_ES)
rm(df_UF)

# Censo ----

# Importando dados
lista_censo_UF<-readRDS("Tratamento dos Dados/lista_censo_UF.rds")

lista_histogram_indicadores<-list()


for(i in 1:length(lista_censo_UF)){
    lista_censo_UF[[i]]$Label<-ifelse(lista_censo_UF[[i]]$UF == "Espírito Santo", "ES", "Outros Estados")
        
    for (j in 2:(ncol(lista_censo_UF[[i]])-1)){
    df_aux<-cbind.data.frame(lista_censo_UF[[i]][,j], lista_censo_UF[[i]]$Label)
        colnames(df_aux)<-c("Dados", "Label")
        
    lista_histogram_indicadores[[paste0(names(lista_censo_UF)[i],
                                        "_", colnames(lista_censo_UF[[i]])[j])]]<-df_aux %>%
        ggplot(aes(x = Dados, fill = Label)) +
            geom_histogram(bins = 30) +
            labs(x = colnames(lista_censo_UF[[i]])[j], title = "Histograma dos Indicadores nos Estados")
        
    }
}

# Exibindo
for (i in 1:length(lista_histogram_indicadores)){
    print(lista_histogram_indicadores[[i]])
    
    Sys.sleep(2)
}

# Alguns aleatórios
for (i in sample(1:length(lista_histogram_indicadores), 15)){
    print(lista_histogram_indicadores[[i]])
    
    Sys.sleep(5)
}

# Salvando ----

saveRDS(lista_graf_ES_vs_Estados, "Analise Exploratoria/lista_graf_ES_vs_Estados.rds")
saveRDS(lista_histogram_indicadores, "Analise Exploratoria/lista_histogram_indicadores.rds")

lista_series_GV_e_DivinoSL<-list()
    lista_series_GV_e_DivinoSL[["Divino_de_Sao_Lourenco"]]<-plt_divino
    lista_series_GV_e_DivinoSL[["Cariacica"]]<-plt_cariacica
    lista_series_GV_e_DivinoSL[["Fundao"]]<-plt_fundao
    lista_series_GV_e_DivinoSL[["Guarapari"]]<-plt_guarapari
    lista_series_GV_e_DivinoSL[["Serra"]]<-plt_serra
    lista_series_GV_e_DivinoSL[["Viana"]]<-plt_viana
    lista_series_GV_e_DivinoSL[["Vila_Velha"]]<-plt_vilavelha
    lista_series_GV_e_DivinoSL[["Vitoria"]]<-plt_vitoria

saveRDS(lista_series_GV_e_DivinoSL, "Analise Exploratoria/lista_graf_municES.rds")    

    # Limpando
    rm(list = ls())
    