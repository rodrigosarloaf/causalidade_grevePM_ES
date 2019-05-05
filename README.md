# Efeito da Greve da PM Sobre os Homicídios no ES

Repositório para trabalho de Introdução à Ciência de Dados (INF2420) - PUC-Rio.
O objetivo é chegar a uma estimativa de quantos homicídios são atribuíveis à
Greve da PM no ES (04/02/2017 - 24/02/2017).

---

## Dados
Os dados não estão disponíveis nesse repositório (nem brutos nem tratados), mas 
cada script contém instruções do caminho para conseguí-los e os montar os
dados tratados.


Todos os dados utilizados aqui são abertos.

---

## Organização


Cada script traz informações mais detalhadas sobre os dados e escolhas acerca do
seu tratamento. Tentei torná-los autoexplicativos.

### Tratamento dos Dados/
Nesta pasta, trato os dados brutos. A ideia inicial era trabalhar ao nível de municípios,
porém, após a etapa de visualização dos dados, optei por trabalhar no nível UF - esses dados
são agregados na outra pasta.


#### lendo_dados_censo.R
Neste script, importo todos os indicadores sociais municipais do censo de 2010. Devido à falta
de padronização, optei por fazer uma importação separada para cada "sheet" do Excel.

#### lendo_dados_dbf.R
Aqui, crio séries temporais de números de homicídios ao nível município/dia a partir
dos dados do SIM-DataSUS


Note que, na data de download, os dados de 2017 ainda estavam como "preliminares".
Assim que os dados forem tratados como definitivos, irei alterar esses arquivos.

### Analise Exploratoria/

Nesta pasta, tento entender um pouco melhor como o ES é em relação aos outros
Estados e de que forma isso vai impactar possíveis métodos para estimação
do Efeito da Greve.

#### multiplot.R
Função para plotar mais de um gráfico gerado pelo ggplot2 por vez.


Disponível em: <http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/>

#### visualizando_dados_tratados.R
Aqui, gero gráficos de homicídios e dos indicadores sociais usados.


Durante essa análise, optei por agregar os dados por UF - próxima descrição - e 
voltei para essa análise.


#### agregando_por_UF.R
Neste script, agrego os dados de homicídios e indicadores sociais por UF.


Sobre a agregação de indicadores sociais por UF: existem dados já disponíveis do
próprio IBGE neste nível, mas não para todos os indicadores e alguns estão organizados
de uma forma um pouco diferente. Exemplo: dados somam 100% em cada classificação
de tipo de domicílio (único responável, mais de um responsável...) e não entre 
todos os tipos possíveis de domicílios, como optei por organizar.

#### adciona_dados_popUF.R
Aqui, adiciono dados de projeções de população para o ano de 2017 (ano da Greve)
elaborados peo IBGE em 2018. A ideia é de uma possível análise posterior do efeito
sobre a taxa de homicídios.

---

## Sugestões
Quaisquer sugestões, seja sobre a utilização do GitHub, organização das pastas
ou sobre os scripts do R serão muito bem-vindos.


Essa é a minha primeira experiência com Git e compartilhamento dos códigos.


