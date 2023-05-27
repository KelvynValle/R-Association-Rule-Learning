# Criar uma Lista de Cestas
market_basket <- 
  list( 
    c("maçã", "cerveja", "arroz", "carne"),
    c("maçã", "cerveja", "arroz"),
    c("maçã", "cerveja"), 
    c("maçã", "pêra"),
    c("leite", "cerveja", "arroz", "carne"), 
    c("leite", "cerveja", "arroz"), 
    c("leite", "cerveja"),
    c("leite", "pêra")
  )
# Nomes das Transações (T1 a T8)
names(market_basket) <- paste("T", c(1:8), sep = "")

install.packages("arules")
install.packages("arulesViz")

library(arules)
trans <- as(market_basket, "transactions")

dim(trans)

itemLabels(trans)

summary(trans)

image(trans)

itemFrequencyPlot(trans, topN=10, cex.names=1)

rules <- apriori(trans, 
                 parameter = list(supp=0.3, conf=0.5, 
                                  maxlen=10, 
                                  target= "rules"))

summary(rules)

inspect(rules)


#Min Support 0.3, confidence as 0.5.
rules <- apriori(trans, 
                 parameter = list(supp=0.3, conf=0.5, 
                                  maxlen=10, 
                                  minlen=2,
                                  target= "rules"))

cerveja_rules_rhs <- apriori(trans, 
                             parameter = list(supp=0.3, conf=0.5, 
                                              maxlen=10, 
                                              minlen=2),
                             appearance = list(default="lhs", 
                                               rhs="cerveja"))
inspect(cerveja_rules_rhs)

cerveja_rules_lhs <- apriori(trans, 
                             parameter = list(supp=0.3, conf=0.5, 
                                              maxlen=10, 
                                              minlen=2),
                             appearance = list(lhs="cerveja", 
                                               default="rhs"))
inspect(cerveja_rules_lhs)

library(arulesViz)
plot(rules)

plot(rules, measure="confidence")

plot(rules, method = "two-key plot")

plot(rules, engine = "plotly")

subrules <- head(rules, n = 10, by = "confidence")
plot(subrules, method = "graph", engine = "htmlwidget")

plot(subrules, method="paracoord")
