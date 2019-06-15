# Nesse script, vamos fazer o mesmo do "selec_janela_fit.R", mas para dados
# em taxa de homicídio (por 100 mil habitantes). Como os denominadores variam,
# podemos ter um fit melhor. Isso será especialmente útili no Controle
# Sintético, onde nós temos um erro com média diferente de zero

# Queremos saber qual melhor fit para as taxas

# Carregando Pacotes
library(tidyverse)
library(magrittr)
library(lubridate)
library(ArCo)
library(HDeconometrics)
library(randomForest)
library(Synth)

# Carregando dados
df_homicidios_UF<-readRDS("Analise Exploratoria/dataframe_homicidios_UF.rds")
df_pop<-readRDS("Tratamento dos Dados/data_frame_projecao_Pop_UF.rds")
    # Lemrbando: dados de população sãoo projeções do IBGE para o ano de 2017

df_pop$Populacao2017_CentenasDeMilhares<-df_pop$Populacao_proj_2017/100000
df_pop %<>% select(-Populacao_proj_2017)

# Juntando
df_homicidios_UF %<>% left_join(df_pop, by = "UF")
rm(df_pop)

df_homicidios_UF$HomicioDiaPor100mil<-(df_homicidios_UF$Homicidios/df_homicidios_UF$Populacao2017_CentenasDeMilhares)

df_homicidios_UF %<>% select(-Homicidios, -Populacao2017_CentenasDeMilhares)

# Fazendo como no outro script
lista_dfs<-split.data.frame(df_homicidios_UF, df_homicidios_UF$UF)

df_homicidios<-as.data.frame(matrix(data = NA, nrow = nrow(lista_dfs$AC), ncol = length(lista_dfs)))

for (i in 1:length(lista_dfs)) {
    df_homicidios[,i]<-lista_dfs[[i]]$HomicioDiaPor100mil
}

colnames(df_homicidios)<-names(lista_dfs)
df_homicidios$Data<-lista_dfs$AC$Data

# Vamos testar nos primeiros 28 dias de todos os meses de 2016:
# 29 dias facilita para escrever o loop já que todos os meses tem pelo menos
# 29 dias - 2016 foi ano bissexto;
# ano de 2016 porque é o ano mais perto da greve (fev/2017);
# vamos fitar novamente o método a cada nova previsão pseudo-oos.

# Janelas serão de: (1 até 3 anos, aprox.)
# 360, 450, 540, 630, 720, 810, 900, 990, 1080 (dias)

tamanho_janelas<-c(360L, 450L, 540L, 630L, 720L, 810L, 900L, 990L, 1080L)
ultimo_dia_in<-c("2015-12-31", "2016-01-31", "2016-02-29", "2016-03-31", 
                 "2016-04-30", "2016-05-31", "2016-06-30", "2016-07-31", 
                 "2016-08-31", "2016-09-30", "2016-10-31", "2016-11-30")

# Vou retirar o Amazonas - revolta em presídio causou pico de mortes:
plot.ts(tail(df_homicidios$AM, 1000L))
df_homicidios$Data[which.max(df_homicidios$AM)]
df_homicidios %<>% select(-AM)

# Retirando RN também - tem alguma coisa estranha nessa série,
# um dia que tem 25 registros
plot.ts(tail(df_homicidios$RN, 1000L))
df_homicidios$Data[which.max(df_homicidios$RN)]
df_homicidios %<>% select(-RN)

# Vamos fazer aqui vários plots mas em taxa
estados<-colnames(df_homicidios)
    estados<-estados[-(which(estados %in% c("ES", "Data")))]

df_ES<-df_homicidios %>% select(ES, Data) %>% filter(Data >= ymd("2016-07-01") &
                                                         Data <= ymd("2017-07-01"))
# Para salvarmos
lista_graf<-list()

for(i in estados){
df_ajuda<-df_homicidios %>% select(i, Data) %>% filter(Data >= ymd("2016-07-01") &
                                                     Data <= ymd("2017-07-01"))
colnames(df_ajuda)<-c("Num", "Data")

plt<-ggplot(data = df_ES, aes(x = as.Date(Data))) +
    geom_line(aes(y = ES), col = "black") +
    theme_minimal() +
    geom_line(data = df_ajuda, aes(y = Num), col = "red", alpha = 0.7) +
    geom_vline(xintercept = ymd("2017-02-04"), linetype = "dashed", col = "blue") +
    geom_vline(xintercept = ymd("2017-02-24"), linetype = "dashed", col = "blue") +
    labs(x = "Datas", y = "Número de Homicídios",
         title = paste0("Série de Homicídios ES vs ", i),
         caption = "Linhas tracejadas indicam período de greve",
         subtitle = paste0("ES em preto \n", i, " em vermelho"))

lista_graf[[paste0("ES_vs_", i, "_Taxa")]]<-plt

print(plt)

    Sys.sleep(3)
}

# Salvando
saveRDS(lista_graf, "Modelagem/lista_graficos_TaxaHomicidios.rds")
rm(lista_graf)
rm(df_ES)
rm(df_ajuda)
rm(plt)

# Continuando

# Para o ArCo: ----
# Vou usar a estimação pelo ic.glmnet, que foi para qual método a
# teoria de inferência foi desenvolvida, além de randomForest

lista_outputs_arco_ic<-list()
lista_outputs_arco_rf<-list()

df_deltas_ic<-cbind.data.frame(tamanho_fit = NA, ult_in = NA, LB = NA, Delta = NA, UB = NA)
df_deltas_rf<-cbind.data.frame(tamanho_fit = NA, ult_in = NA, LB = NA, Delta = NA, UB = NA)

# seed
set.seed(10)

for(i in 1:length(tamanho_janelas)){
    for(j in 1:length(ultimo_dia_in)){
        
        # Montando df necessário
        df_auxiliar<-df_homicidios %>% filter(Data >= ymd(ultimo_dia_in[j])-tamanho_janelas[i] &
                                                  Data <= ymd(ultimo_dia_in[j])+29)
        
        # selecionando t0
        selec_posicao<-which(df_auxiliar$Data == (ymd(ultimo_dia_in[j])+1))
        
        # Finalizando df
        df_auxiliar %<>% select(-Data)
        
        # Fitando
        modelo_ic<-fitArCo(data = list(df_auxiliar), fn = ic.glmnet, p.fn = predict,
                           treated.unit = which(colnames(df_auxiliar) == "ES"), 
                           t0 = selec_posicao)
        
        modelo_rf<-fitArCo(data = list(df_auxiliar), fn = randomForest, p.fn = predict,
                           treated.unit = which(colnames(df_auxiliar) == "ES"), 
                           t0 = selec_posicao)
        
        # Salvando
        lista_outputs_arco_ic[[paste0("tamanho_janela_", tamanho_janelas[i])]][[paste0("ultimo_in_", ultimo_dia_in[j])]]<-modelo_ic
        lista_outputs_arco_rf[[paste0("tamanho_janela_", tamanho_janelas[i])]][[paste0("ultimo_in_", ultimo_dia_in[j])]]<-modelo_rf
        
        df_deltas_ic<-rbind.data.frame(df_deltas_ic,
                                       cbind.data.frame(tamanho_fit = tamanho_janelas[i], ult_in = ultimo_dia_in[j],
                                                        LB = modelo_ic$delta[1], Delta = modelo_ic$delta[2], 
                                                        UB = modelo_ic$delta[3]))
        df_deltas_rf<-rbind.data.frame(df_deltas_rf,
                                       cbind.data.frame(tamanho_fit = tamanho_janelas[i], ult_in = ultimo_dia_in[j],
                                                        LB = modelo_rf$delta[1], Delta = modelo_rf$delta[2], 
                                                        UB = modelo_rf$delta[3]))
        
        # Print
        print(paste("Janela de ", tamanho_janelas[i], "; Ult_Dia =", ultimo_dia_in[j], sep = " "))
        
    }
}

# Excluindo a primeira linha
df_deltas_ic<-df_deltas_ic[-1,]
df_deltas_rf<-df_deltas_rf[-1,]

# Vamos checar as métricas de cada um
metricas_ic<-cbind.data.frame(tamanho_fit = NA, delta_medio = NA, 
                              tamanho_interv_conf = NA)
metricas_rf<-cbind.data.frame(tamanho_fit = NA, delta_medio = NA, 
                              tamanho_interv_conf = NA)

for(i in 1:9){
    
    df_aux_ic<-split.data.frame(df_deltas_ic, df_deltas_ic$tamanho_fit)[i][[1]]
    df_aux_rf<-split.data.frame(df_deltas_rf, df_deltas_rf$tamanho_fit)[i][[1]]
    
    metricas_ic<-rbind.data.frame(metricas_ic,
                                  cbind.data.frame(tamanho_fit = tamanho_janelas[i], delta_medio = mean(df_aux_ic$Delta), 
                                                   tamanho_interv_conf = sum(abs(range(c(df_aux_ic$LB, df_aux_ic$UB))))
                                  )
    )
    
    metricas_rf<-rbind.data.frame(metricas_rf,
                                  cbind.data.frame(tamanho_fit = tamanho_janelas[i], delta_medio = mean(df_aux_rf$Delta), 
                                                   tamanho_interv_conf = sum(abs(range(c(df_aux_rf$LB, df_aux_rf$UB))))
                                  )
    )
    
}

metricas_ic<-metricas_ic[-1,]
metricas_rf<-metricas_rf[-1,]

which.min(abs(metricas_ic$delta_medio))
which.min(abs(metricas_rf$delta_medio))

which.min(abs(metricas_ic$tamanho_interv_conf))
which.min(abs(metricas_rf$tamanho_interv_conf))

# Mesmos resultados do outro script - vamos ficar com a 1

# Plotando
plot(lista_outputs_arco_ic$tamanho_janela_360$`ultimo_in_2016-05-31`, display.fitted = T)
plot(lista_outputs_arco_rf$tamanho_janela_360$`ultimo_in_2016-05-31`, display.fitted = T)

# Novamente também o RF capta bem melhor a variação - vamos ficar com esse também em taxa
# para a apresentação

lista_selec_ArCo<-list(lista_outputs_arco_ic$tamanho_janela_360,
                       lista_outputs_arco_rf$tamanho_janela_360)

saveRDS(lista_selec_ArCo, "Modelagem/lista_modelos_selec_ArCo_emTaxa.rds")

# Para o Controle Sintético: ----
dados_censo<-readRDS("Tratamento dos Dados/lista_censo_UF.rds")

# Tratando dados
# Vou ter que selecionar o que e relevante para a análise
tabela_censo<-data_frame(UF = unique(dados_censo$tb1$UF))

#
head(dados_censo$tb1)
tabela_censo %<>% left_join(dados_censo$tb1, by = "UF")

#
head(dados_censo$tb2)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb2, by = "UF")

#
head(dados_censo$tb3)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb3, by = "UF")

#
head(dados_censo$tb4)
colnames(dados_censo$tb4)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb4, by = "UF")

#
head(dados_censo$tb5)
colnames(dados_censo$tb5)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb5, by = "UF")

#
head(dados_censo$tb6)
colnames(dados_censo$tb6)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb6, by = "UF")

#
head(dados_censo$tb7)
colnames(dados_censo$tb7)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb7, by = "UF")

#
head(dados_censo$tb8)
colnames(dados_censo$tb8)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb8, by = "UF")

#
head(dados_censo$tb9)
colnames(dados_censo$tb9)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb9, by = "UF")

#
head(dados_censo$tb10)
colnames(dados_censo$tb10)
head(tabela_censo)
tabela_censo %<>% left_join(dados_censo$tb10, by = "UF")

# Finalizado

# Preparação dos dados:
# Para usar o "dataprep()" precisamos colocar esses dados do censo junto
# com a série original
which(is.na(tabela_censo))
head(df_homicidios_UF)

df_homicidios_UF %<>% filter(UF != "AM" & UF != "RN") # Já falamos antes o porquê

# Modificando Tabela Censo:
# UF tem que ser em sigla
tabela_censo$UF[tabela_censo$UF == "Acre"]<-"AC"
tabela_censo$UF[tabela_censo$UF == "Alagoas"]<-"AL"
tabela_censo$UF[tabela_censo$UF == "Amapá"]<-"AP"
tabela_censo$UF[tabela_censo$UF == "Amazonas"]<-"AM"
tabela_censo$UF[tabela_censo$UF == "Bahia"]<-"BA"
tabela_censo$UF[tabela_censo$UF == "Ceará"]<-"CE"
tabela_censo$UF[tabela_censo$UF == "Distrito Federal"]<-"DF"
tabela_censo$UF[tabela_censo$UF == "Espírito Santo"]<-"ES"
tabela_censo$UF[tabela_censo$UF == "Goiás"]<-"GO"
tabela_censo$UF[tabela_censo$UF == "Maranhão"]<-"MA"
tabela_censo$UF[tabela_censo$UF == "Mato Grosso"]<-"MT"
tabela_censo$UF[tabela_censo$UF == "Mato Grosso do Sul"]<-"MS"
tabela_censo$UF[tabela_censo$UF == "Minas Gerais"]<-"MG"
tabela_censo$UF[tabela_censo$UF == "Pará"]<-"PA"
tabela_censo$UF[tabela_censo$UF == "Paraíba"]<-"PB"
tabela_censo$UF[tabela_censo$UF == "Paraná"]<-"PR"
tabela_censo$UF[tabela_censo$UF == "Pernambuco"]<-"PE"
tabela_censo$UF[tabela_censo$UF == "Piauí"]<-"PI"
tabela_censo$UF[tabela_censo$UF == "Rio de Janeiro"]<-"RJ"
tabela_censo$UF[tabela_censo$UF == "Rio Grande do Norte"]<-"RN"
tabela_censo$UF[tabela_censo$UF == "Rio Grande do Sul"]<-"RS"
tabela_censo$UF[tabela_censo$UF == "Rondônia"]<-"RO"
tabela_censo$UF[tabela_censo$UF == "Roraima"]<-"RR"
tabela_censo$UF[tabela_censo$UF == "Santa Catarina"]<-"SC"
tabela_censo$UF[tabela_censo$UF == "São Paulo"]<-"SP"
tabela_censo$UF[tabela_censo$UF == "Sergipe"]<-"SE"
tabela_censo$UF[tabela_censo$UF == "Tocantins"]<-"TO"

df_homicidios_UF %<>% left_join(tabela_censo, by = "UF")    

# Função precisa de nomes e números para identificar as unidades
identificadores<-cbind.data.frame(UF = unique(df_homicidios_UF$UF), identif = 1:25)
df_homicidios_UF %<>% left_join(identificadores, by = "UF")
df_homicidios_UF %<>% select(-ends_with(".y")) # Alguns dados se repentem entre
# tabelas
df_homicidios_UF$identif<-as.numeric(df_homicidios_UF$identif)

which(is.na(df_homicidios_UF)) # Ok, nenhum NA

# Pelo que entendi, essa função não lida muito bem com objetos de Data
# vamos ter que ficar voltando aqui para consultar
identif_datas<-cbind.data.frame(Data = seq.Date(from = ymd("2012-01-01"), to = ymd("2017-12-31"), by = 1),
                                identifica_data = 1:length(seq.Date(from = ymd("2012-01-01"), to = ymd("2017-12-31"), by = 1)))
identif_datas %<>% filter(Data <= "2017-02-24")

df_homicidios_UF %<>% left_join(identif_datas, by = "Data")
df_homicidios_UF %<>% select(-Data)
class(df_homicidios_UF)
df_homicidios_UF<-as.data.frame(df_homicidios_UF)

# Não vamos ter que fazer como no ArCo com períodos pseudo-oos de tratamento
# A função objetivo que queremos otimizar só depende dos observáveis.
# Como os observáveis não mudam (são fixos nos valores observados no censo de 
# 2010), só vamos ter uma resposta para o controle sintético
# Por isso vou usar de input todos os dados disponíveis

# Checando periodo da otimização - ano de 2016 (como os dados não mudam, 
# isso é indiferente):
identif_datas$identifica_data[identif_datas$Data == "2016-01-01"]
# Primeiro dia da otimização
identif_datas$identifica_data[identif_datas$Data == "2016-12-31"]
# Primeiro dia da otimização
identif_datas$identifica_data[identif_datas$Data == (ymd("2016-01-01")-361)]


dados_synth<-dataprep(foo = df_homicidios_UF,
                      predictors = colnames(df_homicidios_UF)[-c(1, 2, 45, 46)],
                      predictors.op = "mean", time.predictors.prior = 1:1461,
                      dependent = "HomicioDiaPor100mil", unit.variable = "identif",
                      unit.names.variable = "UF", time.variable = "identifica_data",
                      treatment.identifier = 7, controls.identifier = c(1:6, 8:25),
                      time.optimize.ssr = 1462:1827, # ano de 2016 para otimizar
                      time.plot = 1462:1827)

modelo_synth<-synth(data.prep.obj = dados_synth, method = "BFGS")

gaps.plot(modelo_synth, dados_synth)
# Muito melhor!

lista_synth<-list(dados_synth, modelo_synth)
saveRDS(lista_synth, "Modelagem/lista_synth_emTaxa.rds")    






