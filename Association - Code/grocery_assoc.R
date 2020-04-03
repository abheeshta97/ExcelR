library(arules)
library(arulesViz)
library(RColorBrewer)

grocery <- read.transactions(file.choose(), format = "basket")
inspect(grocery[1:15])

itemFrequencyPlot(grocery, topN = 12, col = brewer.pal(n = 12, name = "Set2"))

grocery.rules1 <- apriori(grocery, 
                          parameter = list(support = 0.0015, confidence = 0.1, minlen = 3))
grocery.rules1
inspect(sort(grocery.rules1, by = "confidence")[1:15])
inspect(sort(grocery.rules1, by = "support")[1:15])
inspect(sort(grocery.rules1, by = "lift")[1:15])
plot(grocery.rules1, method = "graph")
plot(grocery.rules1, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.6, jitter = 0.4)

grocery.rules2 <- apriori(grocery,
                          parameter = list(support = 0.0015, confidence = 0.3, minlen = 3))
grocery.rules2
inspect(sort(grocery.rules2, by = "confidence")[1:15])
inspect(sort(grocery.rules2, by = "support")[1:15])
inspect(sort(grocery.rules2, by = "lift")[1:15])
plot(grocery.rules2, method = "graph")
?plot
plot(grocery.rules2, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.6, jitter = 0.6)

grocery.rules3 <- apriori(grocery,
                          parameter = list(support = 0.002, confidence = 0.3, minlen = 3))
grocery.rules3
inspect(sort(grocery.rules3, by = "confidence")[1:15])
inspect(sort(grocery.rules3, by = "support")[1:15])
inspect(sort(grocery.rules3, by = "lift")[1:15])
plot(grocery.rules3, method = "graph")
plot(grocery.rules3, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.6, jitter = 0.6)

grocery.rules4 <- apriori(grocery,
                          parameter = list(support = 0.003, confidence = 0.5, minlen = 3))
grocery.rules4
inspect(sort(grocery.rules4, by = "confidence"))
inspect(sort(grocery.rules4, by = "support"))
inspect(sort(grocery.rules4, by = "lift"))
plot(grocery.rules4, method = "graph")
plot(grocery.rules4, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.6, jitter = 0.6)
