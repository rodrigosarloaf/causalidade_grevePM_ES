### Baixando dados do Censo 2010 por Estado - IBGE ###

# Baixando resultados relevantes do IBGE já por UF
# Veja https://cran.r-project.org/web/packages/sidrar/vignettes/Introduction_to_sidrar.html
# para entender o pacote usado para baixar os dados
# Veja https://sidra.ibge.gov.br/pesquisa/censo-demografico/demografico-2010/amostra-resultados-gerais
# para as tabelas

# Pacotes
library(sidrar)
library(tidyverse)
library(magrittr)


# Baixando todas as tabelas para depois tratar

numeros_tabelas<-c(1495, 1617, 2065, 2249, 2250, 3424, 2426,
                   1547, 1549, 1551, 1554, 1611, 1497, 1505,
                   1535, 1611, 1615, 3422, 1539, 1541, 1542, 
                   1543, 1545, 1572, 1573, 1575, 1577, 1579, 
                   1581, 1615, 2030, 2033, 2034, 3422)
    # Tem algumas que se repetem em diferentes categorias:
    numeros_tabelas<-unique(numeros_tabelas)


for (i in 1:length(numeros_tabelas)) {
    assign(paste0("tb_", numeros_tabelas[i]),
           get_sidra(x = numeros_tabelas[i], geo = "State", format = 2))
    
    print(i)
}
    
# Tratamento
# Vamos salvar depois numa lista para mexermos posteriormente:
    lista_dados_censoUF<-list()
# 1
numeros_tabelas[1]

head(tb_1495)
View(tb_1495)
# Pessoas com deficiência - vou pular
rm(tb_1495)

# 2
numeros_tabelas[2]

head(tb_1617)
View(tb_1617)
# Daqui me parece que só uma das variáveis já serviria -
# vou pegar número de comodos
tb_1617 %<>% select(`Unidade da Federação`, `Variável`,
                    `Número de cômodos`, Valor)

colnames(tb_1617)<-c("UF", "Variavel", "Num_comodos", "Valor")

unique(tb_1617$Variavel) # Vou pegar pelo número de moradores

tb_1617 %<>% filter(Variavel == "Moradores em domicílios particulares permanentes",
                    !is.na(Valor)) %>%
    select(-Variavel)

# Estava separado por tipo de parede - vamos pegar só o total sem cada tipo de
# parede
tb_1617 %<>% group_by(UF, Num_comodos) %>%
    arrange(desc(Valor), .by_group =  T)

tb_1617<-unique.data.frame(tb_1617)

tb_1617 %<>% group_by(UF, Num_comodos) %>%
    summarise(Valor = first(Valor)) %>%
    ungroup()

# Salvando
lista_dados_censoUF[["tb1"]]<-tb_1617
rm(tb_1617)

# 3
numeros_tabelas[3]

head(tb_2065)
View(tb_2065)

unique(tb_2065$`Existência de água canalizada e forma de abastecimento`) 
# vamos pegar só tinham ou não em (%)

colnames(tb_2065)<-c("UF", "Ano", "Variavel", "Exist_Agua_Canali",
                     "Unidade_Medida", "Valor")

tb_2065 %<>% select(UF, Exist_Agua_Canali, Valor)

unique(tb_2065$Exist_Agua_Canali)
tb_2065$Exist_Agua_Canali[tb_2065$Exist_Agua_Canali == "Tinham"]<-"Sim"
tb_2065$Exist_Agua_Canali[tb_2065$Exist_Agua_Canali == "Não tinham água canalizada"]<-"Nao"

tb_2065 %<>% filter(Exist_Agua_Canali == "Total" |
                    Exist_Agua_Canali == "Sim" |
                    Exist_Agua_Canali == "Nao")

tb_2065_2<-data.frame(UF = unique(tb_2065$UF), stringsAsFactors = F)
    tb_2065_2$Prop_Domici_Com_AguaCanali<-(tb_2065$Valor[tb_2065$Exist_Agua_Canali == "Sim"]/tb_2065$Valor[tb_2065$Exist_Agua_Canali == "Total"])
    tb_2065_2$Prop_Domici_Sem_AguaCanali<-(tb_2065$Valor[tb_2065$Exist_Agua_Canali == "Nao"]/tb_2065$Valor[tb_2065$Exist_Agua_Canali == "Total"])
# Salvando
lista_dados_censoUF[["tb2"]]<-tb_2065_2    
rm(tb_2065)
rm(tb_2065_2)

# 4
numeros_tabelas[4]

head(tb_2249)
View(tb_2249)
# Vou pular essa - teremos variável de renda mais para frente
rm(tb_2249)

# 5
numeros_tabelas[5]

head(tb_2250)
View(tb_2250)
# Vou pular pelo mesmo motivo acima
rm(tb_2250)

# 6
numeros_tabelas[6]

head(tb_3424)
View(tb_3424)
# renda mensal - vamos ficar com essa e trabalhar em (%)
colnames(tb_3424)<-c("UF", "Ano", "Variavel", "Faixa_Rend_Domic",
                     "Unidade", "Valor")

tb_3424 %<>% select(UF, Faixa_Rend_Domic, Valor)

tb_3424$Faixa_Rend_Domic[tb_3424$Faixa_Rend_Domic == "Até 1/4 de salário mínimo"]<-"ate_UmQuarto_SM"
tb_3424$Faixa_Rend_Domic[tb_3424$Faixa_Rend_Domic == "Mais de 1/4 a 1/2 salário mínimo"]<-"entre_UmQuarto_e_Meio_SM"
tb_3424$Faixa_Rend_Domic[tb_3424$Faixa_Rend_Domic == "Mais de 1/2 a 1 salário mínimo"]<-"entre_Meio_e_1_SM"
tb_3424$Faixa_Rend_Domic[tb_3424$Faixa_Rend_Domic == "Mais de 1 a 2 salários mínimos"]<-"entre_1e2_SM"
tb_3424$Faixa_Rend_Domic[tb_3424$Faixa_Rend_Domic == "Mais de 2 a 3 salários mínimos"]<-"entre_2e3_SM"
tb_3424$Faixa_Rend_Domic[tb_3424$Faixa_Rend_Domic == "Mais de 3 a 5 salários mínimos"]<-"entre_3e5_SM"
tb_3424$Faixa_Rend_Domic[tb_3424$Faixa_Rend_Domic == "Mais de 5 salários mínimos"]<-"mais_5_SM"
tb_3424$Faixa_Rend_Domic[tb_3424$Faixa_Rend_Domic == "Sem rendimento"]<-"Sem_rendimento"

tb_3424_2<-data.frame(UF = unique(tb_3424$UF), stringsAsFactors = F)
    tb_3424_2$Prop_ate_UmQuarto_SM<-(tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "ate_UmQuarto_SM"]/tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Total"])
    tb_3424_2$Prop_entre_UmQuarto_e_Meio_SM<-(tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "entre_UmQuarto_e_Meio_SM"]/tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Total"])
    tb_3424_2$Prop_entre_Meio_e_1SM<-(tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "entre_Meio_e_1_SM"]/tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Total"])
    tb_3424_2$Prop_entre_1e2_SM<-(tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "entre_1e2_SM"]/tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Total"])
    tb_3424_2$Prop_entre_2e3_SM<-(tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "entre_2e3_SM"]/tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Total"])
    tb_3424_2$Prop_entre_3e5_SM<-(tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "entre_3e5_SM"]/tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Total"])
    tb_3424_2$Prop_mais_5_SM<-(tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "mais_5_SM"]/tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Total"])
    tb_3424_2$Prop_Sem_Rend<-(tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Sem_rendimento"]/tb_3424$Valor[tb_3424$Faixa_Rend_Domic == "Total"])
    
# Salvando
lista_dados_censoUF[["tb3"]]<-tb_3424_2
rm(tb_3424)
rm(tb_3424_2)

# 7
numeros_tabelas[7]

head(tb_2426)
View(tb_2426)

unique(tb_2426$`Situação do domicílio`)
colnames(tb_2426)<-c("UF", "Ano", "Var", "Tipo_Domi", "Unidade", "Valor")

tb_2426$Var[str_detect(tb_2426$Var, "médio")]<-"medio"

tb_2426 %<>% filter(Tipo_Domi == "Urbana" |
                    Tipo_Domi == "Rural",
                    Unidade == "Reais", 
                    Var == "medio") %>%
    select(-Ano, -Unidade, -Var)

colnames(tb_2426)<-c("UF", "Tipo_Domicilio", "Valor_Renda_Media")

# Salvando
lista_dados_censoUF[["tb4"]]<-tb_2426
rm(tb_2426)

# 8
numeros_tabelas[8]
# Vou ficar com outra de Escolaridade
head(tb_1547)
View(tb_1547)

rm(tb_1547)

# 9
numeros_tabelas[9]

# Vamos ficar só com pop separada por idade
head(tb_1549)
View(tb_1549)

unique(tb_1549$Variável)

colnames(tb_1549)<-c("UF", "Ano", "Var", "Faixa_Idade", "Medida", "Val")

tb_1549 %<>% filter(Var == "População residente") %>%
    select(-Ano, -Var, -Medida)

lista_dados_censoUF[["tb5"]]<-tb_1549
rm(tb_1549)

# 10
numeros_tabelas[10]
# Vou ficar com outra de Escolaridade
head(tb_1551)
View(tb_1551)

rm(tb_1551)

# 11
numeros_tabelas[11]

head(tb_1554)
View(tb_1554)
# Vamos ficar em (%)

colnames(tb_1554)<-c("UF", "Ano", "Var", "Instrucao", "Medida", "Val")

tb_1554 %<>% select(-Ano, -Var, -Medida)

unique(tb_1554$Instrucao)
tb_1554$Instrucao[tb_1554$Instrucao == "Sem instrução e fundamental incompleto"]<-"Sem_ou_Fundamental_Incompleto"
tb_1554$Instrucao[tb_1554$Instrucao == "Fundamental completo e médio incompleto"]<-"Fundamental_Completo"
tb_1554$Instrucao[tb_1554$Instrucao == "Médio completo e superior incompleto"]<-"Medio_Completo"
tb_1554$Instrucao[tb_1554$Instrucao == "Superior completo"]<-"Superior_Completo"
tb_1554$Instrucao[tb_1554$Instrucao == "Não determinado"]<-"Indeterminado"

tb_1554_2<-data.frame(UF = unique(tb_1554$UF), stringsAsFactors = F)
    tb_1554_2$Prop_Sem_ou_Fundam_Incomp<-(tb_1554$Val[tb_1554$Instrucao == "Sem_ou_Fundamental_Incompleto"]/tb_1554$Val[tb_1554$Instrucao == "Total"])
    tb_1554_2$Prop_Fundam_Comp<-(tb_1554$Val[tb_1554$Instrucao == "Fundamental_Completo"]/tb_1554$Val[tb_1554$Instrucao == "Total"])
    tb_1554_2$Prop_Medio_Comp<-(tb_1554$Val[tb_1554$Instrucao == "Medio_Completo"]/tb_1554$Val[tb_1554$Instrucao == "Total"])
    tb_1554_2$Prop_Superior_Comp<-(tb_1554$Val[tb_1554$Instrucao == "Superior_Completo"]/tb_1554$Val[tb_1554$Instrucao == "Total"])
    tb_1554_2$Prop_Indeterminado<-(tb_1554$Val[tb_1554$Instrucao == "Indeterminado"]/tb_1554$Val[tb_1554$Instrucao == "Total"])

# Salvando
lista_dados_censoUF[["tb6"]]<-tb_1554_2
rm(tb_1554_2)
rm(tb_1554)

# 12
numeros_tabelas[12]

# Vou pular
head(tb_1611)
View(tb_1611)

rm(tb_1611)

# 13
numeros_tabelas[13]

head(tb_1497)
View(tb_1497)
# Vou usar em (%)

colnames(tb_1497)<-c("UF", "Ano", "Var", "Nacionalidade", "Medida", "Val")
unique(tb_1497$Nacionalidade)
unique(tb_1497$Var)

tb_1497 %<>% select(-Ano, -Var, -Medida)

tb_1497 %<>% filter(Nacionalidade != "Brasileira")

tb_1497$Nacionalidade[tb_1497$Nacionalidade == "Brasileira - nata"]<-"Brasileiro_Nato"
tb_1497$Nacionalidade[tb_1497$Nacionalidade == "Brasileira - por naturalização"]<-"Brasileiro_Nacion"

tb_1497_2<-data.frame(UF = unique(tb_1497$UF), stringsAsFactors = F)

    tb_1497_2$Prop_Brasileiros_Natos<-(tb_1497$Val[tb_1497$Nacionalidade == "Brasileiro_Nato"]/tb_1497$Val[tb_1497$Nacionalidade == "Total"])
    tb_1497_2$Prop_Brasileiros_Nacion<-(tb_1497$Val[tb_1497$Nacionalidade == "Brasileiro_Nacion"]/tb_1497$Val[tb_1497$Nacionalidade == "Total"])
    tb_1497_2$Prop_Estrangeiros<-(tb_1497$Val[tb_1497$Nacionalidade == "Estrangeira"]/tb_1497$Val[tb_1497$Nacionalidade == "Total"])

#
lista_dados_censoUF[["tb7"]]<-tb_1497_2
rm(tb_1497)
rm(tb_1497_2)

# 14
numeros_tabelas[14]

head(tb_1505)
View(tb_1505)

colnames(tb_1505)<-c("UF", "Ano", "Var", "Natural_Municipio", "Medida", "Val")

unique(tb_1505$Var)

tb_1505 %<>% select(-Ano, -Var, -Medida)

unique(tb_1505$Natural_Municipio)

tb_1505$Natural_Municipio<-ifelse(tb_1505$Natural_Municipio == "Naturais do município", "Sim",
                                  ifelse(tb_1505$Natural_Municipio == "Total", "Total",
                                         "Nao"))

auxiliar<-tb_1505 %>% filter(Natural_Municipio != "Nao")
    auxiliar$Val[auxiliar$Natural_Municipio == "Sim"]<- (-auxiliar$Val[auxiliar$Natural_Municipio == "Sim"])
    auxiliar %<>% group_by(UF) %>%
            summarise(Val = sum(Val))
auxiliar$Natural_Municipio = "Nao"

auxiliar<-auxiliar[ ,c(1,3,2)]

tb_1505 %<>% filter(Natural_Municipio != "Nao")

tb_1505<-rbind(tb_1505, auxiliar)

tb_1505 %<>% arrange(UF)

#
lista_dados_censoUF[["tb8"]]<-tb_1505
rm(auxiliar)
rm(tb_1505)

# 15
numeros_tabelas[15]

head(tb_1535)
View(tb_1535)
# Pular
rm(tb_1535)

# 16
numeros_tabelas[16]

head(tb_1615)
View(tb_1615)
# Pular
rm(tb_1615)

# 17
numeros_tabelas[17]

head(tb_3422)
View(tb_3422)
# Pular
rm(tb_3422)

# 18
numeros_tabelas[18]

head(tb_1539)
View(tb_1539)
#
colnames(tb_1539)<-c("UF", "Ano", "Var", "Estado_Conjulgal", "Medida", "Val")

tb_1539 %<>% select(-Ano, -Var, -Medida)

unique(tb_1539$Estado_Conjulgal)

tb_1539$Estado_Conjulgal[tb_1539$Estado_Conjulgal == "Viviam em união"]<-"uniao"
tb_1539$Estado_Conjulgal<-ifelse(tb_1539$Estado_Conjulgal != "Total" & tb_1539$Estado_Conjulgal != "uniao",
                                 "sem_uniao", tb_1539$Estado_Conjulgal)


auxiliar<-tb_1539 %>% filter(Estado_Conjulgal != "sem_uniao")
tb_1539 %<>% filter(Estado_Conjulgal != "sem_uniao")
auxiliar$Val[auxiliar$Estado_Conjulgal == "uniao"]<-(-auxiliar$Val[auxiliar$Estado_Conjulgal == "uniao"])
auxiliar %<>% group_by(UF) %>%
    summarise(Val = sum(Val))

auxiliar$Estado_Conjulgal<-"sem_uniao"
auxiliar<-auxiliar[, c(1,3,2)]

tb_1539<-rbind.data.frame(tb_1539, auxiliar)
rm(auxiliar)

tb_1539_2<-data.frame(UF = unique(tb_1539$UF), stringsAsFactors = F)
    tb_1539_2$Prop_uniao<-(tb_1539$Val[tb_1539$Estado_Conjulgal == "uniao"]/tb_1539$Val[tb_1539$Estado_Conjulgal == "Total"])
    tb_1539_2$Prop_sem_uniao<-(1-tb_1539_2$Prop_uniao)
#
lista_dados_censoUF[["tb9"]]<-tb_1539_2
rm(tb_1539)
rm(tb_1539_2)    
    
# Corrigir tb8 - era para estar em prop
    tb8<-lista_dados_censoUF[["tb8"]]
    aux<-data.frame(UF = unique(tb8$UF), stringsAsFactors = F)
    aux$Prop_Natural_Municipio<-(tb8$Val[tb8$Natural_Municipio == "Sim"]/tb8$Val[tb8$Natural_Municipio == "Total"])
    aux$Prop_Natural_Fora_Muni<-(1-aux$Prop_Natural_Municipio)
    #
    lista_dados_censoUF[["tb8"]]<-aux
    rm(tb8)    
    rm(aux)    

# 19
numeros_tabelas[19]
# Já tem parecido - vou pular
head(tb_1541)
View(tb_1541)
    
rm(tb_1541)

# 20    
numeros_tabelas[20]
# Pular
head(tb_1542)
View(tb_1542)

rm(tb_1542)

# 21
numeros_tabelas[21]
# Pular
head(tb_1543)
View(tb_1543)

rm(tb_1543)

# 22
numeros_tabelas[22]
# Natalidade e natimortos - acho que não precisa
# infos um pouco dispersas parece
# Pular
head(tb_1545)
View(tb_1545)

rm(tb_1545)

# 23
numeros_tabelas[23]

head(tb_1572)
View(tb_1572)
# Desemprego neste ano de 2010 não parece servir em 2017...
# As de distribuição de renda só que fazem sentido
# Pular
rm(tb_1572)

# 24
numeros_tabelas[24]
# Vou pular também - mesma justificativa acima
head(tb_1573)
View(tb_1573)

rm(tb_1573)

# 25
numeros_tabelas[25]
# Vou pular também - mesma justificativa acima
head(tb_1575)
View(tb_1575)

rm(tb_1575)

# 26
numeros_tabelas[26]
# Vou pular também - mesma justificativa acima
head(tb_1577)
View(tb_1577)

rm(tb_1577)

# 27
numeros_tabelas[27]
# Vou pular também - mesma justificativa acima
head(tb_1579)
View(tb_1579)

rm(tb_1579)

# 28
numeros_tabelas[28]
# Já temos isso por domicílio
head(tb_1581)
View(tb_1581)

rm(tb_1581)

# 29
numeros_tabelas[29]
# Já temos isso por domicílio
head(tb_2030)
View(tb_2030)

rm(tb_2030)

# 30
numeros_tabelas[30]
# Vamos pular e usar a de baixo - parecem iguais
head(tb_2033)
View(tb_2033)

rm(tb_2033)

# 31
numeros_tabelas[31]

head(tb_2034)
View(tb_2034)

tb_2034 %<>% filter(`Unidade de Medida` == "Reais")

colnames(tb_2034)<-c("UF", "Ano", "Var", "Sexo", "Medida", "Val")

unique(tb_2034$Var)

# Vamos ficar só com o médio
tb_2034$Var[str_detect(tb_2034$Var, "médio")]<-"Medio"

tb_2034 %<>% filter(Var == "Medio") %>%
    select(-Ano, -Var, -Medida)

tb_2034_2<-data.frame(UF = unique(tb_2034$UF), stringsAsFactors = F)

    tb_2034_2$Prop_Rend_Mulher_emRel_Medio<-(tb_2034$Val[tb_2034$Sexo == "Mulheres"]/tb_2034$Val[tb_2034$Sexo == "Total"])
    tb_2034_2$Prop_Rend_Homem_emRel_Medio<-(tb_2034$Val[tb_2034$Sexo == "Homens"]/tb_2034$Val[tb_2034$Sexo == "Total"])

#
lista_dados_censoUF[["tb10"]]<-tb_2034_2
rm(tb_2034)
rm(tb_2034_2)



# Checando as que vamos salvar e finalizando

# 1

aux<-lista_dados_censoUF$tb1 %>%
    spread(key = Num_comodos, value = Valor)

colnames(aux)<-c("UF", "Prop_1comodos", "Prop_2comodos", "Prop_3comodos",
                 "Prop_4comodos", "Prop_5comodos", "Prop_6comodos",
                 "Prop_7comodos", "Prop_8ouMais_comodos", "Total")

aux$Prop_1comodos<- aux$Prop_1comodos/aux$Total
aux$Prop_2comodos<- aux$Prop_2comodos/aux$Total
aux$Prop_3comodos<- aux$Prop_3comodos/aux$Total
aux$Prop_4comodos<- aux$Prop_4comodos/aux$Total
aux$Prop_5comodos<- aux$Prop_5comodos/aux$Total
aux$Prop_6comodos<- aux$Prop_6comodos/aux$Total
aux$Prop_7comodos<- aux$Prop_7comodos/aux$Total
aux$Prop_8ouMais_comodos<- aux$Prop_8ouMais_comodos/aux$Total

aux %<>% select(-Total)

lista_dados_censoUF[["tb1"]]<-aux

rm(aux)

# 2
lista_dados_censoUF$tb2

# 3
lista_dados_censoUF$tb3

# 4
lista_dados_censoUF$tb4

aux<-lista_dados_censoUF$tb4 %>%
    spread(key = Tipo_Domicilio, value = Valor_Renda_Media)

colnames(aux)<-c("UF", "Renda_Media_Domi_Rural", "Renda_Media_Domi_Urbano")

lista_dados_censoUF[["tb4"]]<-aux

rm(aux)

# 5
lista_dados_censoUF$tb5

aux<-lista_dados_censoUF$tb5 %>%
    spread(key = Faixa_Idade, value = Val)

colnames(aux)<-c("UF", "Prop_0a3anos", "Prop_15a17anos", "Prop_18a19anos", "Prop_20a24anos",
                 "Prop_25mais_anos", "Prop_4a5anos", "Prop_6anos", "Prop_7a14anos", "Total")

aux$Prop_0a3anos<-aux$Prop_0a3anos/aux$Total
aux$Prop_15a17anos<-aux$Prop_15a17anos/aux$Total
aux$Prop_18a19anos<-aux$Prop_18a19anos/aux$Total
aux$Prop_20a24anos<-aux$Prop_20a24anos/aux$Total
aux$Prop_25mais_anos<-aux$Prop_25mais_anos/aux$Total
aux$Prop_4a5anos<-aux$Prop_4a5anos/aux$Total
aux$Prop_6anos<-aux$Prop_6anos/aux$Total
aux$Prop_7a14anos<-aux$Prop_7a14anos/aux$Total

aux %<>% select(-Total)

lista_dados_censoUF[["tb5"]]<-aux

rm(aux)

# 6
lista_dados_censoUF$tb6

# 7
lista_dados_censoUF$tb7

# 8
lista_dados_censoUF$tb8

# 9
lista_dados_censoUF$tb9

# 10
lista_dados_censoUF$tb10

# Salvando e Finalizando
saveRDS(lista_dados_censoUF, "Tratamento dos Dados/lista_censo_UF.rds")

rm(list = ls())




