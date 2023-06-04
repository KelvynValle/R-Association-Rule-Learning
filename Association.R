#instalar e carregar o pacote arules
#install.packages("arules")
library(arules)
#instalar e carregar arulesViz
#install.packages("arulesViz")
library(arulesViz)
#instalar e carregar tidyverse
#install.packages("tidyverse")
library(tidyverse)
#instalar e carregar Readxl
install.packages("readxl")
library(readxl)
#instalar e caregar o knitr
install.packages("knitr")
library(knitr)
#Carregar o ggplot2 pois ele já vem em tidyverse
library(ggplot2)
#instalar e caregar o lubridate
#install.packages("lubridate")
library(lubridate)
#instalar e carregar plyr
install.packages("plyr")
library(plyr)
library(dplyr)
# UFA!!! Quantos pacotes e bibliotecas!! Mas o R é assim mesmo!!

#Ler os dados do Excel para o R dataframe
library(readxl)
retail <- read_excel('C:/CODE/R-Association-Rule-Learning/Online Retail.xlsx')
view(retail)

#complete.cases(data) retornará um vetor lógico que indica quais linhas não têm valores
#ausentes. Em seguida, use o vetor para obter apenas as linhas que estão completas
#usando o comando a seguir:
retail <- retail[complete.cases(retail), ]
#mutate esta função é do pacote dplyr. Ela é usado para editar ou adicionar novas
#colunas para dataframes. Aqui a coluna description está sendo convertida para a coluna
#de fator. as.factor converte a coluna para fator coluna. %>% é um operador com o qual
#você pode canalizar valores para outra função ou expressão
retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))
#Converte dados de caracteres para data. Converte InvoiceDate como data na nova variável
retail$Date <- as.Date(retail$InvoiceDate)
#Extrai a hora de InvoiceDate e armazena em outra variável
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
#Converte e edita InvoiceNo em numérico
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
#NAs introduzido por coerção
#Acrescenta as novas colunas TransTime e InvoiceNo no dataframe retail
cbind(retail,TransTime)
cbind(retail,InvoiceNo)
# Finalmente, dê uma olhada agora nos seus dados
glimpse(retail)


library(plyr)
# ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description, collapse = ","))
# A função R paste() concatena vetores para caracteres e resultados separados usando
# collapse=[any optional charcater string ]. Aqui ', ' é usado.
transactionData



#set coluna InvoiceNo do dataframe transactionData
transactionData$InvoiceNo <- NULL
#set coluna Date de dataframe transactionData
transactionData$Date <- NULL
#Renomeie a coluna para itens
colnames(transactionData) <- c("items")
#Mostre agora novamente o dataframe transactionData
transactionData
View(transactionData)



write.csv(transactionData,"C:/CODE/R-Association-Rule-Learning/market_basket_transactions.csv", quote = FALSE,
          row.names = FALSE)
#transactionData: Dados a serem gravados
#"C:/dados/market_basket.csv": localização e nome do arquivo a ser gravado
#quote: Se TRUE ele irá delimitar a coluna de caractere ou fator com aspas duplas.
#Se False nada será feito
#row.names: um valor lógico indicando se os nomes de linha de x devem ser gravados junto
#com x, ou um vetor de caracteres de nomes de linha a serem gravados.



library(arules)
tr <- read.transactions('C:/CODE/R-Association-Rule-Learning/market_basket_transactions.csv', format = 'basket',
                        sep=',')
#sep diz como os itens são separados. Neste caso, você separou usando vírgulas ','



tr

summary(tr)


# Criar um gráfico de frequência de item para os 20 itens principais
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'),
                  main="Frequencia Absoluta de Itens")


itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Frequenci
a Relativa de Itens")


# Min suporte como 0.001, confiança como 0,8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))

summary(association.rules)
inspect(association.rules[1:10])

shorter.association.rules <- apriori(tr, parameter = list(supp=0.001,
                                                          conf=0.8,maxlen=3))
inspect(shorter.association.rules[1:10])

subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1)
# Obtém um subconjunto de regras em um vetor
length(subset.rules) #> 3913
subset.association.rules. <- association.rules[-subset.rules]


metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance
                                   = list(default="lhs",rhs="METAL"))

inspect(head(metal.association.rules))


# Filtrar regras com confiança maior do que 0,4 ou 40%
library(arulesviz)
subRules<-association.rules[quality(association.rules)$confidence>0.4]
plot(subRules)

plot(subRules,method="two-key plot")

plotly_arules(subRules)


top10subRules <- head(subRules, n = 10, by = "confidence")

plot(top10subRules, method = "graph", engine = "htmlwidget")


saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")


# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")
