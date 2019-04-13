# Começando a leitura dos dados diretamente em DBF e
# criando arquivos em CSV

# Chamando pacotes
library(read.dbc) # Para ler os arquivos em DBC
library(tidyverse)
library(magrittr)
library(readr)

# Os dados diretamente do Download estão em "Tratamento dos Dados/DATASUS Arquivos RAR"
# Precisei extrair para "Tratamento dos Dados/Dados de DO DBC" para ler

nomes_estados<-c("AC", "AL", "AP", "AM", "BA", "CE",
                 "DF", "ES", "GO", "MA", "MT", "MS",
                 "MG", "PA", "PB", "PR", "PE", "PI",
                 "RJ", "RN", "RS", "RO", "RR", "SC",
                 "SP", "SE", "TO")

# Vamos criar DFs únicos com dados desde 2012 até 2017 para cada estado
DOs_2017<-paste0("DOP", nomes_estados, "2017.dbc")

for(i in 1:length(nomes_estados)){
    df<-read.dbc(paste0("Tratamento dos Dados/Dados de DO DBC/", DOs_2017[i]))
    
    nomes_colunas<-colnames(df)
    
    for (j in 2012:2016) {
        df_aux<-read.dbc(paste0("Tratamento dos Dados/Dados de DO DBC/DO", nomes_estados[i], j, ".dbc"))
        
        # Primeiro vamos ter que corrigir pelo fato de o tamanho dos DFs
        # mudar com o tempo
        if(length(colnames(df_aux)) != length(nomes_colunas)){
            novas_colunas<-setdiff(nomes_colunas, colnames(df_aux)) # Checando faltantes
            
            complementar_df<-as.data.frame(matrix(data = NA, nrow = nrow(df_aux), 
                                                  ncol = length(novas_colunas))) # Criando dados vazios
            colnames(complementar_df)<-novas_colunas # Dando nomes das colunas
            
            df_aux<-cbind.data.frame(df_aux, complementar_df) # Completando dados
            
        }
        
        df_aux<-df_aux[,colnames(df)]# Colocando colunas na mesma ordem 
        
        df<-rbind.data.frame(df, df_aux)
        
    }
    
    write.csv(df, file = paste0("Tratamento dos Dados/Dados de DO CSV/DOs_", nomes_estados[i],"12a17.csv"))
    
}

# Agora, vamos separar somente os homicídios - com o que vamos trabalhar!
# (Local e data)
# Consultar "Tratamento do Dados/Dados Extras SUS/Estrutura_SIM_para_CD.pdf" caso precise
# ver outras informações

df_obitos<-cbind.data.frame(NUMERODO = NA, DTOBITO = NA, CODMUNOCOR = NA, CIRCOBITO = NA,
                            CAUSABAS = NA, CAUSABAS_O = NA)

csvs<-list.files("Tratamento dos Dados/Dados de DO CSV")
    csvs<-paste0("Tratamento dos Dados/Dados de DO CSV/", csvs)    

# Vamos pegar só essas informações de cada CSV criado
for (i in 1:length(csvs)) {
    df_pre<- readr::read_csv(csvs[i], 
                            col_types = cols(NUMERODO = col_character(),
                                             DTOBITO = col_character(),
                                             CODMUNOCOR = col_character(),
                                             CIRCOBITO = col_character(),
                                             CAUSABAS = col_character(),
                                             CAUSABAS_O = col_character(),
                                             
                                             X1 = col_skip(), CODINST = col_skip(), NUMERODV = col_skip(), ORIGEM = col_skip(),
                                             TIPOBITO = col_skip(), HORAOBITO = col_skip(), NUMSUS = col_skip(), NATURAL = col_skip(),
                                             CODMUNNATU = col_skip(), DTNASC = col_skip(), IDADE = col_skip(), SEXO = col_skip(),
                                             RACACOR = col_skip(), ESTCIV = col_skip(), ESC = col_skip(), ESC2010 = col_skip(),
                                             SERIESCFAL = col_skip(), OCUP = col_skip(),CODMUNRES = col_skip(), LOCOCOR = col_skip(),
                                             CODESTAB = col_skip(),ESTABDESCR = col_skip(), IDADEMAE = col_skip(), ESCMAE = col_skip(),
                                             ESCMAE2010 = col_skip(), SERIESCMAE = col_skip(), OCUPMAE = col_skip(), QTDFILVIVO = col_skip(),
                                             QTDFILMORT = col_skip(), GRAVIDEZ = col_skip(), SEMAGESTAC = col_skip(), GESTACAO = col_skip(),
                                             PARTO = col_skip(), OBITOPARTO = col_skip(), PESO = col_skip(), NUMERODN = col_skip(),
                                             TPMORTEOCO = col_skip(), OBITOGRAV = col_skip(), OBITOPUERP = col_skip(), ASSISTMED = col_skip(),
                                             EXAME = col_skip(), CIRURGIA = col_skip(), NECROPSIA = col_skip(), LINHAA = col_skip(),
                                             LINHAB = col_skip(), LINHAC = col_skip(), LINHAD = col_skip(), LINHAII = col_skip(),
                                             CB_PRE = col_skip(), CRM = col_skip(), COMUNSVOIM = col_skip(), DTATESTADO = col_skip(),
                                             ACIDTRAB = col_skip(), FONTE = col_skip(), NUMEROLOTE = col_skip(), TPPOS = col_skip(),
                                             DTINVESTIG = col_skip(), DTCADASTRO = col_skip(), ATESTANTE = col_skip(), STCODIFICA = col_skip(),
                                             CODIFICADO = col_skip(), VERSAOSIST = col_skip(), VERSAOSCB = col_skip(), FONTEINV = col_skip(),
                                             DTRECEBIM = col_skip(), ATESTADO = col_skip(), DTRECORIGA = col_skip(), CAUSAMAT = col_skip(),
                                             ESCMAEAGR1 = col_skip(), ESCFALAGR1 = col_skip(), STDOEPIDEM = col_skip(), STDONOVA = col_skip(),
                                             DIFDATA = col_skip(), NUDIASOBCO = col_skip(), NUDIASOBIN = col_skip(), DTCADINV = col_skip(),
                                             TPOBITOCOR = col_skip(), DTCONINV = col_skip(), FONTES = col_skip(), TPRESGINFO = col_skip(),
                                             TPNIVELINV = col_skip(), NUDIASINF = col_skip(), DTCADINF = col_skip(), MORTEPARTO = col_skip(),
                                             DTCONCASO = col_skip(), FONTESINF = col_skip(), ALTCAUSA = col_skip()))
    
    
    
    df_pre<-df_pre[,colnames(df_obitos)]
    
    df_obitos<-rbind.data.frame(df_obitos, df_pre)
    
    print(i)
}
        rm(df_pre)
        rm(csvs)
    df_obitos<-df_obitos[-1,]

        
# Mexendo nas colunas selecionadas
summary(df_obitos)
    # Note que não temos NAs em CAUSABAS nem em CAUSABAS_O (Causa do Obito na CID-10)
    # enquanto CIRCOBITO (classificação como homicídio, suicídio... tem mais de 6.5 mi
    # vazias em 7.5 mi).
    # Vamos ter que classificar via CID na mão
    mean(is.na(df_obitos$CIRCOBITO))
    # Essa coluna de fato tem que estar vazia. Ver documentação
    

df_obitos$DTOBITO<-as.Date.character(paste(str_sub(df_obitos$DTOBITO, start = 5L, end = 8L),
                                           str_sub(df_obitos$DTOBITO, start = 3L, end = 4L),
                                           str_sub(df_obitos$DTOBITO, start = 1L, end = 2L),sep = "-"))
    colnames(df_obitos)<-c("Num_DO", "Data", "Cod_Municipio", "Indica_Tipo_Obito",
                           "Causa_basica", "Causa_basica_original")

df_obitos$Cod_Municipio<-as.character(df_obitos$Cod_Municipio)
    
# Nos arquivos em DBF dos "Tratamento dos Dados/ Dados Extras SUS", tive que usar o TABWIN para transformar em CSV
# TABWIN está nos "Tratamento dos Dados/ DATASUS Arquivos RAR"
tb_mun <- readr::read_csv("Tratamento dos Dados/Dados Extras SUS/CADMUN.csv", 
                     col_types = cols(AGLCOD = col_skip(), ALTITUDE = col_skip(), AMAZONIA = col_skip(), 
                     ANOEXT = col_character(), ANOINST = col_character(),  AREA = col_skip(), CAPITAL = col_skip(), 
                     CSAUDCOD = col_skip(), FRONTEIRA = col_skip(),  LATITUDE = col_skip(), LONGITUDE = col_skip(), 
                     MESOCOD = col_skip(), MICROCOD = col_skip(),  MSAUDCOD = col_skip(), MUNCOD = col_character(), 
                     MUNCODDV = col_character(), MUNNOME = col_character(),  MUNNOMEX = col_character(), MUNSIAFI = col_skip(), 
                     MUNSINON = col_skip(), MUNSINONDV = col_skip(),  MUNSINP = col_skip(), OBSERV = col_skip(), 
                     RMETRCOD = col_skip(), RSAUDCOD = col_skip(),  SITUACAO = col_skip(), SUCESSOR = col_skip(), 
                     UFCOD = col_character()))

    # Checando no View(tb_mun) identificamos que todos os municípios foram INSTAURADOS
    # ATÉ 2005 e EXTINTOS ATÉ 2005 - não vamos ter problemas na nossa base.
    # Vou tirar essas colunas
    tb_mun %<>% select(-ANOINST, -ANOEXT, -MUNCODDV, -MUNNOME)
    colnames(tb_mun)<-c("Cod_Municipio", "Municipio", "Cod_UF")
        
# Vamos importar os códigos de UF e dar match
tb_uf<- read_csv("Tratamento dos Dados/Dados Extras SUS/TABUF.csv", 
        col_types = cols(CODIGO = col_character(), 
        DESCRICAO = col_skip(), SIGLA_UF = col_character()))
    colnames(tb_uf)<-c("UF", "Cod_UF")

tabela_cidades<-left_join(tb_mun, tb_uf, by = "Cod_UF")
    
    rm(tb_mun)
    rm(tb_uf)

# Juntando com os óbitos
df_obitos %<>% left_join(tabela_cidades, by = "Cod_Municipio")

    View(df_obitos[which(is.na(df_obitos$Municipio)),])
    which(is.na(df_obitos$Municipio)) # São 528 óbitos sem município

# Substituindo o Label de Indica_Tipo_Obito - Ver em 
# "Tratamento dos Dados/Dados Extras SUS/Estrutura_SIM_para_CD.pdf"
df_obitos$Indica_Tipo_Obito[df_obitos$Indica_Tipo_Obito == "9"]<-"Ignorado"
df_obitos$Indica_Tipo_Obito[df_obitos$Indica_Tipo_Obito == "1"]<-"Acidente"
df_obitos$Indica_Tipo_Obito[df_obitos$Indica_Tipo_Obito == "2"]<-"Suicídio"
df_obitos$Indica_Tipo_Obito[df_obitos$Indica_Tipo_Obito == "3"]<-"Homicídio"
df_obitos$Indica_Tipo_Obito[df_obitos$Indica_Tipo_Obito == "4"]<-"Outros"

# Alguns checks
    df_obitos %>%
        dplyr::filter(!is.na(Indica_Tipo_Obito)) %>%
        dplyr::group_by(Indica_Tipo_Obito) %>%
        dplyr::summarise(Numero = n())
    # Tem muitos homicídios, o que me parece
    # OK para nossa fase de preencher por Naive-Bayes
    
# Importando a CID-10
causas_agrupadas <- readr::read_csv("Tratamento dos Dados/Dados Extras SUS/CIDCAP10.csv")
causas_completas <- readr::read_csv("Tratamento dos Dados/Dados Extras SUS/CID10.csv")

# Vamos trabalhar com os homicídios somente ou completando os
# ignorados
df_homicidios_oficial<- df_obitos %>%
    dplyr::filter(Indica_Tipo_Obito == "Homicídio") %>%
    group_by(Data, Municipio, Cod_Municipio, UF, Cod_UF) %>%
    summarise(Homicidios = n()) %>% ungroup() %>%
    dplyr::filter(!is.na(Municipio))

    # Completando dias com Zeros
    completa_zeros_oficial<-expand.grid(Data = seq.Date(from = as.Date("2012-01-01"),
                                                        to = as.Date("2017-12-31"),
                                                        by = 1),
                                        Cod_Municipio = unique(tabela_cidades$Cod_Municipio))
    completa_zeros_oficial$Homicidios<-0
    completa_zeros_oficial %<>% left_join(tabela_cidades, by = "Cod_Municipio") # Preenchendo informações   
    completa_zeros_oficial<-completa_zeros_oficial[,colnames(df_homicidios_oficial)] # Reordenando as colunas
    completa_zeros_oficial %<>% dplyr::filter(!is.na(UF))
    
    # Tentando via anti_join
    completa_zeros_oficial %<>% anti_join(df_homicidios_oficial,
                                          by = c("Data", "Cod_Municipio"))

        rm(causas_agrupadas)
        rm(causas_completas)
    
    # Juntando e finalizando
    df_homicidios_oficial<-rbind.data.frame(df_homicidios_oficial,
                                            completa_zeros_oficial)
    
    df_homicidios_oficial %<>% arrange(Data, Municipio)
    
        # "rm" em todas as outras coisas para
        rm(df_obitos)
        rm(tabela_cidades)
        rm(completa_zeros_oficial)
        
    write.csv2(df_homicidios_oficial, "Tratamento dos Dados/dataframe_homicidios_datasus.csv")
    
    # Limpando meu environment
    rm(list = ls())
    