setwd("~/M2/projet_machine/data/raw")
data <- read.csv("Processed_data_FD.csv")


dim(data)

str(data)

sum(is.na(data))

sum(duplicated(data))


table(data$Fraudulent.Transaction..Yes.No.)

prop.table(table(data$Fraudulent.Transaction..Yes.No.))*100


names(data)[names(data)=="Fraudulent.Transaction..Yes.No."]


x <- data[, names(data) != "Fraudulent.Transaction..Yes.No."]

y <- data$Fraudulent.Transaction..Yes.No.

dim(x)
dim(data)

length(y)


unique(y)

"Fraudulent.Transaction..Yes.No." %in% names(x)
