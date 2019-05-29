# Neste script vou testar raízes unitárias - pelo menos o método
# do ArCo já não temos certeza se funciona...

# Carregando Pacotes
library(tidyverse)
library(magrittr)
library(lubridate)
library(urca)

# Carregando dados
df_homicidios_UF<-readRDS("Analise Exploratoria/dataframe_homicidios_UF.rds")
    lista_dfs<-split.data.frame(df_homicidios_UF, df_homicidios_UF$UF)

# Testes ADF e visualização dos dados

    # AC
    plot.ts(tail(lista_dfs$AC$Homicidios, 1000L))
        # Parece ser "tipo 1"
    summary(ur.df(as.ts(tail(lista_dfs$AC$Homicidios, 1000L)),
                  type = "none", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # AL
    plot.ts(tail(lista_dfs$AL$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$AL$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # AM
    plot.ts(tail(lista_dfs$AM$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$AM$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # AP
    plot.ts(tail(lista_dfs$AP$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$AP$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # BA
    plot.ts(tail(lista_dfs$BA$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$BA$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # CE
    plot.ts(tail(lista_dfs$CE$Homicidios, 1000L))
        # Parece "tipo 3"
    summary(ur.df(as.ts(tail(lista_dfs$CE$Homicidios, 1000L)),
                  type = "trend", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # DF    
    plot.ts(tail(lista_dfs$DF$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$DF$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # ES
    plot.ts(tail(lista_dfs$ES$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$ES$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # GO
    plot.ts(tail(lista_dfs$GO$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$GO$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # MA
    plot.ts(tail(lista_dfs$MA$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$MA$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # MG
    plot.ts(tail(lista_dfs$MG$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$MG$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # MS
    plot.ts(tail(lista_dfs$MS$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$MS$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # MT
    plot.ts(tail(lista_dfs$MT$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$MT$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # PA
    plot.ts(tail(lista_dfs$PA$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$PA$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # PB
    plot.ts(tail(lista_dfs$PB$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$PB$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # PE
    plot.ts(tail(lista_dfs$PE$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$PE$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # PI
    plot.ts(tail(lista_dfs$PI$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$PI$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # PR
    plot.ts(tail(lista_dfs$PR$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$PR$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # RJ
    plot.ts(tail(lista_dfs$RJ$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$RJ$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # RN
    plot.ts(tail(lista_dfs$RN$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$RN$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # RO
    plot.ts(tail(lista_dfs$RO$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$RO$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # RR
    plot.ts(tail(lista_dfs$RR$Homicidios, 1000L))
        # Parece "tipo 1"
    summary(ur.df(as.ts(tail(lista_dfs$RR$Homicidios, 1000L)),
                  type = "none", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # RS
    plot.ts(tail(lista_dfs$RS$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$RS$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # SC
    plot.ts(tail(lista_dfs$SC$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$SC$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # SE
    plot.ts(tail(lista_dfs$SE$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$SE$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # SP
    plot.ts(tail(lista_dfs$SP$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$SP$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
    # TO
    plot.ts(tail(lista_dfs$TO$Homicidios, 1000L))
        # Parece "tipo 2"
    summary(ur.df(as.ts(tail(lista_dfs$TO$Homicidios, 1000L)),
                  type = "drift", selectlags = "AIC"))
    # Rejeitamos H0 - Não tem RU
    
# Parece tudo OK para trabalharmos com os métodos pensados!    
