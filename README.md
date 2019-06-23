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
porém, após a etapa de visualização dos dados, optei por trabalhar no nível UF - os dados de
homicídios são agregados na outra pasta durante a visualização.


#### lendo_dados_censo_Municipios.R
Neste script, importo todos os indicadores sociais municipais do censo de 2010. Devido à falta
de padronização, optei por fazer uma importação separada para cada "sheet" do Excel.
Devido à opção por trabalhar com dados ao nível de UF, abandonamos totalmente o uso dos indicadores ao
nível municipal, optamos por baixar os dados estaduais do IBGE ou invés de agregar os municipais.


#### lendo_dados_dbf.R
Aqui, crio séries temporais de números de homicídios ao nível município/dia a partir
dos dados do SIM-DataSUS, posteriormente agregados.


#### adiciona_dados_popUF.R
Aqui, adiciono dados de projeções de população para o ano de 2017 (ano da Greve)
elaborados peo IBGE em 2018. A ideia é de uma possível análise posterior do efeito
sobre a taxa de homicídios.


#### baixando_tratando_dados_censoUF.R
Uso a API do IBGE para requisitar os indicadores sociais ao nível de UF. Optei
por mudar para esse formato para evitar possíveis erros cometidos por mim, ou escolhas
ruins na forma de agregar.
Neste script também faço o tratamento para o formato que optei por usar no Controle 
Sintético.


### Analise Exploratoria/

Nesta pasta, tento entender um pouco melhor como o ES é em relação aos outros
Estados e de que forma isso vai impactar possíveis métodos para estimação
do Efeito da Greve.

#### multiplot.R
Função para plotar mais de um gráfico gerado pelo ggplot2 por vez.


Disponível em: <http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/>

#### visualizando_dados_tratados.R
Aqui, gero gráficos de homicídios e dos indicadores sociais usados.


Durante essa análise, optei por agregar os dados por UF e 
voltei para essa análise. Também detectamos aqui alguumas séries (do controle) que
devem ser retiradas por conter outliers - AM e RN, especificamente.


#### testes_estac.R
Neste arquivo, realizo o Testes ADF para raiz unitária - precisamos determinar
se alguma série contém raiz unitária, o que (em geral) é um problema para os estimadores.


#### agregando_por_UF_ANTIGO.R
Neste script, agrego os dados de homicídios e indicadores sociais por UF.


Sobre a agregação de indicadores sociais por UF: existem dados já disponíveis do
próprio IBGE neste nível, mas não para todos os indicadores e alguns estão organizados
de uma forma um pouco diferente. Exemplo: dados somam 100% em cada classificação
de tipo de domicílio (único responável, mais de um responsável...) e não entre 
todos os tipos possíveis de domicílios, como optei por organizar.
Posteriormente, optei por mudar isso. Vou trabalhar com dados do próprio IBGE 
explicados na pasta anterior.


### Modelagem/
Aqui, já começo a modelagem do contrafactual para o período da greve.


#### selec_janela_fit.R
Para cada método (ArCo via Lasso ou via RadomForest e Controle Sintético) estimo os modelos
de contrafactual. No caso do Lasso, para selecionar qual período de fit in-sample que
mais se aproximam de um efeito estimado igual a Zero para o ano de 2016 (sem tratamento).
Para o caso do controle sintético, como os dados usados no fit não variam, isso já é o
próprio modelo de contrafactual.


#### selec_janela_fit_emTaxa.R
Mesma ideia do explicado acima, mas com valores em taxa de homicídios (por 100 mil
habitantes).

#### modelagem_greve.R
Rodo e salvo os modelos "escolhidos" nos scripts acima.


### Apresentacao/
Nesta pasta, uso os dados gerados em outros arquivos e uso como referência os pdfs presentes para preparar a apresentação final do trabalho e seus resultados.

#### apresentacao.Rmd
Arquivo que gera o beamer da apresentação.

---

## Sugestões
Quaisquer sugestões, seja sobre a utilização do GitHub, organização das pastas
ou sobre os scripts do R, serão muito bem-vindos.


Essa é a minha primeira experiência com Git e compartilhamento dos códigos.


