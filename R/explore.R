library(data.table)

train <- fread('input/train.csv', sep=",", na.strings="", nrows = 1000)
stores <- fread('input/stores.csv', sep=",", na.strings = "")
transactions <- fread('input/transactions.csv', sep=",", na.strings = "")
