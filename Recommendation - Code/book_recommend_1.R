#Book Recommendation - Pradeep Abheeshta


library(caTools, recommenderlab)

bookDF <- read.csv(file.choose())
dim(bookDF)
anyNA(bookDF)
bookDF <- bookDF[-c(1)]
head(bookDF)

bookDF$Book.Title <- as.factor(bookDF$Book.Title)
bookDF$Book.Rating <- as.factor(bookDF$Book.Rating)
bookDF$User.ID <- as.factor(bookDF$User.ID)

unique(bookDF$Book.Rating)

book_matrix <- as(bookDF, "realRatingMatrix")

dim(book_matrix@data)

ggplot(bookDF, aes(x=factor(Book.Rating), fill = Book.Rating))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  ggtitle("Book Ratings")
table(bookDF$Book.Rating)

user_count <- data.frame(head(sort(table(bookDF$User.ID), decreasing = TRUE), 10))
View(user_count)

names(user_count)[1] <- "User_ID"

ggplot(user_count, aes(x=factor(User_ID), y = Freq, fill = User_ID))+
  geom_bar(stat="identity", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  ggtitle("Top 10 Users by Frequency")

train <- sample(x = c(T, F), size = nrow(book_matrix), replace = T, prob = c(0.8, 0.2)) 
book_train <- book_matrix[train, ] 
book_train
book_test <- book_matrix[!train, ]


#Popularity based
book_model_1 <- Recommender(book_train, method = "POPULAR")
#Predictions for 2 users
book_rec_1 <- predict(book_model_1, book_test[100:102], n = 5)
as(book_rec_1, "list")

book_model_2 <- Recommender(book_train, method ="UBCF")
#pREDICTIONS FOR 2 USERS
book_rec_2 <- predict(book_model_2, book_matrix[100:102], n = 5)
as(book_rec_2, "list")

