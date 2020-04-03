library(arules)
library(arulesViz)
library(RColorBrewer)

books <- read.csv(file.choose()) #book.csv
class(books)
View(books)

books.trans <- as(as.matrix(books), "transactions")
itemFrequencyPlot(books.trans, col = brewer.pal(n = 11, name = "Purples") )


books.rules1 <- apriori(as.matrix(books), 
                 parameter = list(support = 0.1, confidence = 0.5, minlen = 3))

inspect(books.rules1)
inspect(sort(books.rules1, by = "confidence"))
inspect(sort(books.rules1, by = "support"))
inspect(sort(books.rules1, by = "lift"))

plot(books.rules1, method = "graph")
plot(books.rules1, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.9)

books.rules2 <- apriori(as.matrix(books), 
                        parameter = list(support = 0.15, confidence = 0.5, minlen = 2))
inspect(books.rules2)
inspect(sort(books.rules2, by = "confidence"))
inspect(sort(books.rules2, by = "support"))
inspect(sort(books.rules2, by = "lift"))

plot(books.rules2, method = "graph")
plot(books.rules2, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.9)

books.rules3 <- apriori(as.matrix(books),
                        parameter = list(support = 0.1, confidence = 0.7, minlen = 3 ))

inspect(sort(books.rules3, by = "confidence"))
inspect(sort(books.rules3, by = "support"))
inspect(sort(books.rules3, by = "lift"))
plot(books.rules3, method = "graph")
plot(books.rules3, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Set1")), cex = 0.9)
