# Neste scrit vou definir as janelas de fit para nossa análise do período da greve
# O problema das janelas é que não parece ser razoável a série ser estável - ter um
# processo gerador fixo - para todos os anos que tenho disponíveis.

# Meu critério será o seguite: vou escolher a janela que "erra menos" em períodos
# falsos de tratamento - a diferença deveria ser ZERO

# Queremos evitar overfitting

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

# Tratando ----
lista_dfs<-split.data.frame(df_homicidios_UF, df_homicidios_UF$UF)
    
df_homicidios<-as.data.frame(matrix(data = NA, nrow = nrow(lista_dfs$AC), ncol = length(lista_dfs)))
    
for (i in 1:length(lista_dfs)) {
df_homicidios[,i]<-lista_dfs[[i]]$Homicidios
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

# Ambos escolheram o de 360 como melhor média e divergem no tamanho do
# intervalo de confiança

# Plotando
plot(lista_outputs_arco_ic$tamanho_janela_360$`ultimo_in_2016-05-31`, display.fitted = T)
plot(lista_outputs_arco_rf$tamanho_janela_360$`ultimo_in_2016-05-31`, display.fitted = T)

# Bem, parece que o random forest fica melhor já que acompanha mais a série in-sample
lista_selec_ArCo<-list(lista_outputs_arco_ic$tamanho_janela_360,
                       lista_outputs_arco_rf$tamanho_janela_360)

saveRDS(lista_selec_ArCo, "Modelagem/lista_modelos_selec_ArCo.rds")

# Para o Controle Sintético: ----
dados_censo<-readRDS("Analise Exploratoria/lista_censo_por_UF.rds")

# Tratando dados
    # Vou ter que selecionar o que e relevante para a análise
tabela_censo<-data_frame(UF = unique(dados_censo$tab1$UF))

    #
    head(dados_censo$tab1) # Vamos colocar
    tabela_censo %<>% left_join(dados_censo$tab1, by = "UF")

    #
    head(dados_censo$tab2_total) # Vamos colocar só a total
    head(tabela_censo)
    tabela_censo %<>% left_join(dados_censo$tab2_total, by = "UF")
    
    #
    head(dados_censo$tab3_total) # Vamos colocar só a total
    head(tabela_censo)
    tabela_censo %<>% left_join(dados_censo$tab3_total, by = "UF")
    
    #
    head(dados_censo$tab4) # Vamos colocar
    colnames(dados_censo$tab4)
    head(tabela_censo)
    tabela_censo %<>% left_join(dados_censo$tab4, by = "UF")

    #
    head(dados_censo$tab5) # Vamos colocar
    colnames(dados_censo$tab5)
    head(tabela_censo)
    tabela_censo %<>% left_join(dados_censo$tab5, by = "UF")
    
    #
    head(dados_censo$tab6A) # Vamos pular as 6

    #
    head(dados_censo$tab7A) # Vamos colocar só essa da 7
    colnames(dados_censo$tab7A)
    head(tabela_censo)
    tabela_censo %<>% left_join(dados_censo$tab7A, by = "UF")

    #
    head(dados_censo$tab8A) # Vamos colocar só essa da 8
    colnames(dados_censo$tab8A)
    tabela_censo %<>% left_join(dados_censo$tab8A, by = "UF")
    
    #
    head(dados_censo$tab9) # Vamos colocar
    colnames(dados_censo$tab9)
    tabela_censo %<>% left_join(dados_censo$tab9, by = "UF")
    
    #
    head(dados_censo$tab10) # Vamos colocar
    colnames(dados_censo$tab10)
    tabela_censo %<>% left_join(dados_censo$tab10, by = "UF")

    #
    head(dados_censo$tab11) # Vamos colocar
    colnames(dados_censo$tab11)
    tabela_censo %<>% left_join(dados_censo$tab11, by = "UF")

    #
    head(dados_censo$tab12A) # Vamos colocar só essa da 12
    colnames(dados_censo$tab12A)
    tabela_censo %<>% left_join(dados_censo$tab12A, by = "UF")
    
    #
    head(dados_censo$tab13) # Vamos colocar
    colnames(dados_censo$tab13)
    tabela_censo %<>% left_join(dados_censo$tab13, by = "UF")
    
    # Finalizado
    
# Preparação dos dados:
    # Para usar o "dataprep()" precisamos colocar esses dados do censo junto
    # com a série original
    which(is.na(tabela_censo), arr.ind = T)
        # Vou ter que retirar essa tabela já que vai nos dar um painel desbalanceado!
        colnames(tabela_censo)[68]
        colnames(dados_censo$tab12A)
        tabela_censo %<>% select(-`Perc_PessoasEmDomicilios_ate70reais_PerCapita_Total`,
                                 -`Perc_PessoasEmDomicilios_ate1/4SalMin_PerCapita_Total`,
                                 -`Perc_PessoasEmDomicilios_ate1/2SalMin_PerCapita_Total`)
        
    head(df_homicidios_UF)
    df_homicidios_UF %<>% filter(UF != "AM") # Já falamos antes o porquê
    df_homicidios_UF %<>% left_join(tabela_censo, by = "UF")    
    
    # Função precisa de nomes e números para identificar as unidades
    identificadores<-cbind.data.frame(UF = unique(df_homicidios_UF$UF), identif = 1:26)
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
                        predictors = colnames(df_homicidios_UF)[-c(1,2, 69, 70)],
                        predictors.op = "mean", time.predictors.prior = 1:1461,
                        dependent = "Homicidios", unit.variable = "identif",
                        unit.names.variable = "UF", time.variable = "identifica_data",
                        treatment.identifier = 7, controls.identifier = c(1:6, 8:26),
                        time.optimize.ssr = 1462:1827, # ano de 2016 para otimizar
                        time.plot = 1462:1827)
        
    modelo_synth<-synth(data.prep.obj = dados_synth, method = "BFGS")
    
    # Salvando
    lista_synth<-list(dados_synth, modelo_synth)
    saveRDS(lista_synth, "Modelagem/lista_synth.rds")    
    