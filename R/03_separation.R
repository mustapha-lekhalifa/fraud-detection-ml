data <- read.csv("Processed_data_FD.csv")

View(data)


x <- data[, names(data) != "Fraudulent.Transaction..Yes.No."]

y <-  data$Fraudulent.Transaction..Yes.No.


dim(x)
length(y)
