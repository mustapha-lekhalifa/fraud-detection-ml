data <- read.csv("Processed_data_FD.csv")

View(data)


x <- data[, names(data) != "Fraudulent.Transaction..Yes.No."]

y <-  data$Fraudulent.Transaction..Yes.No.


dim(x)
length(y)


set.seed(123)

indice_fraude <- which(y==1)
indice_non_fraude <- which(y==0)

indice_train_fraude <- sample(indice_fraude , size = floor(0.8 * length(indice_fraude)))

indice_train_non_fraude <- sample(indice_non_fraude , size = floor(0.8 * length(indice_non_fraude)))

indice_train <- sample(c(indice_train_fraude , indice_train_non_fraude))


indice_test <- setdiff(seq_len(nrow(x)), indice_train)


x_train <- x[indice_train, ]
y_train <- y[indice_train]

x_test <- x[indice_test, ]
y_test <- y[indice_test]

dim(x_train)
length(y_train)


dim(x_test)
length(y_test)                        



prop.table(table(y_train))*100
prop.table(table(y_test))*100