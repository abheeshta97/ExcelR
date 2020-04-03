library(arules)
library(arulesViz)
library(RColorBrewer)

my_movies <- read.csv(file.choose())
View(my_movies)

movies <- my_movies[-c(1:5)]
View(movies)

movies.trans <- as(as.matrix(movies), "transactions")

itemFrequencyPlot(movies.trans, col = brewer.pal(n = 9, name = "Dark2"))

movies.rules1 <- apriori(as.matrix(movies), 
                        parameter = list(support = 0.1, confidence = 0.5, minlen = 2))
inspect(sort(movies.rules1, by = "confidence"))
inspect(sort(movies.rules1, by = "support"))
inspect(sort(movies.rules1, by = "lift"))
plot(movies.rules1, method = "graph", control = list(max = 105))
plot(movies.rules1, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.9, jitter = 0)

movies.rules2 <- apriori(as.matrix(movies), 
                         parameter = list(support = 0.1, confidence = 0.5, minlen = 3))
inspect(sort(movies.rules2, by = "confidence"))
inspect(sort(movies.rules2, by = "support"))
inspect(sort(movies.rules2, by = "lift"))
plot(movies.rules2, method = "graph")
plot(movies.rules2, method = "graph")
plot(movies.rules2, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.9, jitter = 1)

movies.rules3 <- apriori(as.matrix(movies),
                        parameter = list(support = 0.2, confidence = 0.6, minlen = 2))
inspect(sort(movies.rules3, by = "confidence"))
inspect(sort(movies.rules3, by = "support"))
inspect(sort(movies.rules3, by = "lift"))
plot(movies.rules3, method = "graph")
plot(movies.rules3, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.9, jitter = 1)

movies.rules4 <- apriori(as.matrix(movies),
                         parameter = list(support = 0.2, confidence = 0.6, minlen = 3))
inspect(sort(movies.rules4, by = "confidence"))
inspect(sort(movies.rules4, by = "support"))
inspect(sort(movies.rules4, by = "lift"))
plot(movies.rules4, method = "graph")
plot(movies.rules4, method = "scatter", control = list(col = brewer.pal(n = 7, name = "Dark2")), cex = 0.9, jitter = 1)
