# Análise do Efeito da Greve #

# Conforme escrito em outros scripts, aqui vamos implementar somente os seguintes modelos:
    # - ArCo via Random Forest tanto em nível quanto em taxa
    # - Controle Sintético tanto em nível quanto em taxa, 
        # notando que a versão em taxa parece mais plausível

# Lembrando: Período da Greve (04/02/2017 - 24/02/2017)

# Carregando Pacotes
library(tidyverse)
library(magrittr)
library(lubridate)
library(ArCo)
library(HDeconometrics)
library(randomForest)
library(Synth)

# Dados - Importando e Tratando ----
df_homicidios_UF<-readRDS("Analise Exploratoria/dataframe_homicidios_UF.rds")
# População
df_pop<-readRDS("Tratamento dos Dados/data_frame_projecao_Pop_UF.rds")
# Lemrbando: dados de população sãoo projeções do IBGE para o ano de 2017

df_pop$Populacao2017_CentenasDeMilhares<-df_pop$Populacao_proj_2017/100000
df_pop %<>% select(-Populacao_proj_2017)
# Juntando
df_homicidios_UF %<>% left_join(df_pop, by = "UF")
rm(df_pop)

df_homicidios_UF$HomicioDiaPor100mil<-(df_homicidios_UF$Homicidios/df_homicidios_UF$Populacao2017_CentenasDeMilhares)
df_homicidios_UF %<>% select(-Populacao2017_CentenasDeMilhares)
# Censo
dados_censo<-readRDS("Tratamento dos Dados/lista_censo_UF.rds")
tabela_censo<-data_frame(UF = unique(dados_censo$tb1$UF))

tabela_censo %<>% left_join(dados_censo$tb1, by = "UF") %>%
    left_join(dados_censo$tb2, by = "UF") %>% left_join(dados_censo$tb3, by = "UF") %>%
    left_join(dados_censo$tb4, by = "UF") %>% left_join(dados_censo$tb5, by = "UF") %>%
    left_join(dados_censo$tb6, by = "UF")  %>% left_join(dados_censo$tb7, by = "UF") %>%
    left_join(dados_censo$tb8, by = "UF") %>% left_join(dados_censo$tb9, by = "UF") %>%
    left_join(dados_censo$tb10, by = "UF")

# Excluindo séries com outliers
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

rm(tabela_censo)
rm(dados_censo)

# Para cada método teremos outros tratamentos específicos
# Por hora, paramos com o tratamento dos dados

# Não vamos analisar efeitos permanentes da greve pois parece que não exite
# Pelo menos não é claro pela inspeção visual - não vou investir tempo nisso
plot.ts(pull(df_homicidios_UF %>% filter(UF == "ES") %>% select(Homicidios)))

# ArCo ----

# Ficou definido que teremos estimação via RF e janela in-sample de 360 dias
# Finalizando tratamento do ArCo
df_ARCO_homicidios_UF<-df_homicidios_UF %>%
    select(Data, UF, starts_with("Homi"))

lista_dfs<-split.data.frame(df_ARCO_homicidios_UF, df_ARCO_homicidios_UF$UF)

df_ARCO_nivel<-as.data.frame(matrix(data = NA, nrow = nrow(lista_dfs$AC), ncol = length(lista_dfs)))
df_ARCO_taxa<-as.data.frame(matrix(data = NA, nrow = nrow(lista_dfs$AC), ncol = length(lista_dfs)))


for (i in 1:length(lista_dfs)) {
    df_ARCO_nivel[,i]<-lista_dfs[[i]]$Homicidios
    df_ARCO_taxa[,i]<-lista_dfs[[i]]$HomicioDiaPor100mil
}

colnames(df_ARCO_nivel)<-names(lista_dfs)
colnames(df_ARCO_taxa)<-names(lista_dfs)

df_ARCO_nivel$Data<-lista_dfs$AC$Data
df_ARCO_taxa$Data<-lista_dfs$AC$Data

rm(lista_dfs)
rm(df_ARCO_homicidios_UF)

# Rodando modelos
# Nivel - Só durante a greve
df_ARCO_nivel %<>% filter(Data >= (ymd("2017-02-04")-361) &
                             Data <= ymd("2017-02-24"))
which(df_ARCO_nivel$Data == ymd("2017-02-04")) # 362

# Guardando as Datas
datas_selec<-df_ARCO_nivel$Data

df_ARCO_nivel %<>% select(-Data)

nivel_ARCO<-fitArCo(data = list(df_ARCO_nivel), fn = randomForest, p.fn = predict,
                   treated.unit = which(colnames(df_ARCO_nivel) == "ES"), 
                   t0 = 362, boot.cf = T)

plot(nivel_ARCO, display.fitted = T)
plot(nivel_ARCO, display.fitted = T, confidence.bands = T)

nivel_ARCO$delta # efeito 5.97 a mais na média por dia de greve
# como a greve tem 21 dias, 5.975*21 = 125.475

nivel_ARCO$p.value

nivel_ARCO$boot.cf

# Taxa - Só durante a greve. Pensar em efeitos que duram mais depois
df_ARCO_taxa %<>% filter(Data >= (ymd("2017-02-04")-361) &
                              Data <= ymd("2017-02-24"))
which(df_ARCO_taxa$Data == ymd("2017-02-04")) # 362

df_ARCO_taxa %<>% select(-Data)

taxa_ARCO<-fitArCo(data = list(df_ARCO_taxa), fn = randomForest, p.fn = predict,
                    treated.unit = which(colnames(df_ARCO_taxa) == "ES"), 
                    t0 = 362, boot.cf = T)

plot(taxa_ARCO, display.fitted = T)
plot(taxa_ARCO, display.fitted = T, confidence.bands = T)

taxa_ARCO$delta # efeito 0.151 a mais em média na taxa de homicídios
# por dia de greve como a greve tem 21 dias, 0.151*21 = 3.171 por 
# cem mil habitantes - tem que pensar um pouco mais no que isso quer dizer
# Lembrando: a pop da tabela no ES era 39.25341 centenas de milhares

taxa_ARCO$p.value

taxa_ARCO$boot.cf

# Os dois são muito parecidos
plot(taxa_ARCO, display.fitted = T)
plot(nivel_ARCO, display.fitted = T)

# Salvando o modelo ArCo e dados para posterior 
# análise/preparação da apresentação

lista_arco_final<-list(Datas = datas_selec, modelo_nivel = nivel_ARCO,
                       modelo_taxa = taxa_ARCO, df_nivel = df_ARCO_nivel,
                       df_taxa = df_ARCO_taxa)

saveRDS(lista_arco_final, "Modelagem/lista_final_ArCo.rds")
rm(df_ARCO_nivel)
rm(df_ARCO_taxa)
rm(lista_arco_final)
rm(nivel_ARCO)
rm(taxa_ARCO)
rm(datas_selec)

# Controle Sintético ----
head(df_homicidios_UF)

# Função precisa de nomes e números para identificar as unidades
identificadores<-cbind.data.frame(UF = unique(df_homicidios_UF$UF), identif = 1:25)
df_homicidios_UF %<>% left_join(identificadores, by = "UF")
df_homicidios_UF$identif<-as.numeric(df_homicidios_UF$identif)

which(is.na(df_homicidios_UF)) # Ok, nenhum NA

# Pelo que entendi, essa função não lida muito bem com objetos de Data
# vamos ter que ficar voltando aqui para consultar
identif_datas<-cbind.data.frame(Data = seq.Date(from = ymd("2012-01-01"), to = ymd("2017-02-24"), by = 1),
                                identifica_data = 1:length(seq.Date(from = ymd("2012-01-01"), to = ymd("2017-02-24"), by = 1)))

df_homicidios_UF_ContSint<-df_homicidios_UF %>%
    filter(Data<="2017-02-24")

df_homicidios_UF_ContSint %<>% left_join(identif_datas, by = "Data")
    anyNA(df_homicidios_UF_ContSint)

datas_selec<-df_homicidios_UF_ContSint$Data
    
df_homicidios_UF_ContSint %<>% select(-Data)
df_homicidios_UF_ContSint<-as.data.frame(df_homicidios_UF_ContSint)

# Checando periodo da otimização - ano de 2016 (como os dados não mudam, 
# isso é indiferente):
identif_datas$identifica_data[identif_datas$Data == "2016-01-01"]
# Primeiro dia da otimização
identif_datas$identifica_data[identif_datas$Data == "2016-12-31"]
# Primeiro dia da otimização
identif_datas$identifica_data[identif_datas$Data == (ymd("2016-01-01")-361)]

# Nível
dados_synth_nivel<-dataprep(foo = df_homicidios_UF_ContSint,
                      predictors = colnames(df_homicidios_UF_ContSint)[-c(1, 2, 3, 46, 47)],
                      predictors.op = "mean", time.predictors.prior = 1:1461,
                      dependent = "Homicidios", unit.variable = "identif",
                      unit.names.variable = "UF", time.variable = "identifica_data",
                      treatment.identifier = 7, controls.identifier = c(1:6, 8:25),
                      time.optimize.ssr = 1462:1827, # ano de 2016 para otimizar
                      time.plot = 1462:1882)

modelo_synth_nivel<-synth(data.prep.obj = dados_synth_nivel, method = "BFGS")

tabelas_nivel<-synth.tab(dataprep.res = dados_synth_nivel, 
                         synth.res = modelo_synth_nivel)
tabelas_nivel$tab.pred # Resultados fitados para cada observável
tabelas_nivel$tab.w # Peso de cada UF
# Essas são as mais interessantes

path.plot(dataprep.res = dados_synth_nivel, 
          synth.res = modelo_synth_nivel)
# Controle parece deslocado para cima pré tratamento
# Mesmo assim parece ter efeito

gaps.plot(dataprep.res = dados_synth_nivel, 
          synth.res = modelo_synth_nivel)
# Erro negativo pré tratamento - mesmo assim temos efeito

# Taxa
dados_synth_taxa<-dataprep(foo = df_homicidios_UF_ContSint,
                            predictors = colnames(df_homicidios_UF_ContSint)[-c(1, 2, 3, 46, 47)],
                            predictors.op = "mean", time.predictors.prior = 1:1461,
                            dependent = "HomicioDiaPor100mil", unit.variable = "identif",
                            unit.names.variable = "UF", time.variable = "identifica_data",
                            treatment.identifier = 7, controls.identifier = c(1:6, 8:25),
                            time.optimize.ssr = 1462:1827, # ano de 2016 para otimizar
                            time.plot = 1462:1882)

modelo_synth_taxa<-synth(data.prep.obj = dados_synth_taxa, method = "BFGS")

tabelas_taxa<-synth.tab(dataprep.res = dados_synth_taxa, 
                         synth.res = modelo_synth_taxa)
tabelas_taxa$tab.pred # Resultados fitados para cada observável
tabelas_taxa$tab.w # Peso de cada UF
# Essas são as mais interessantes

path.plot(dataprep.res = dados_synth_taxa, 
          synth.res = modelo_synth_taxa)
# Bem melhor que antes. Agora não parece mais deslocado. Também tem efeito

gaps.plot(dataprep.res = dados_synth_taxa, 
          synth.res = modelo_synth_taxa)
# Também é melhor. Erros de fato parecem ter média zero

# Obtendo resultados para cada tipo:
    identif_datas$identifica_data[identif_datas$Data == "2017-02-04"]
    identif_datas$identifica_data[identif_datas$Data == "2017-02-24"]
    
difs_nivel<-dados_synth_nivel$Y1plot - (dados_synth_nivel$Y0plot %*%
                                            modelo_synth_nivel$solution.w)

last(cumsum(tail(difs_nivel, 21L))) # Deu = 89.25435
# Mas vimos que estava esquisito

difs_taxa<-dados_synth_taxa$Y1plot - (dados_synth_taxa$Y0plot %*%
                                            modelo_synth_taxa$solution.w)

last(cumsum(tail(difs_taxa, 21L))) # Deu = 3.091 - parecido com o ArCo
# Vimos que esta melhor no gráfico. Fazendo como no ArCo (pensar melhor
# na interpretação depois):
# 3.091*39.25341 = 121.3323

# Salvando
lista_synth_final<-list(Datas = datas_selec,
                        data.prep_nivel = dados_synth_nivel, modelo_nivel = modelo_synth_nivel,
                        data.prep_taxa = dados_synth_taxa, modelo_taxa = modelo_synth_taxa,
                        df_usado_prep = df_homicidios_UF_ContSint)

saveRDS(lista_synth_final, "Modelagem/lista_final_Synth.rds")

# Vou precisar voltar a esse script para checar como pegar/usar os dados
# do controle sintético

