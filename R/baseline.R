library(data.table)
library(dplyr)

source('R/explore.R')

transform <- function(data) {

  all_combinations <- expand.grid(
    unique(data$date),
    unique(data$store_nbr),
    unique(data$item_nbr)
  ) %>%
    as.data.table()
  
  colnames(all_combinations) <- c("date", "store_nbr", "item_nbr")
  not_in_data <- all_combinations %>%
    anti_join(data, by = c("date", "store_nbr", "item_nbr"))
  
  data <- rbindlist(list(data, not_in_data), use.names=TRUE, fill=TRUE) %>%
    mutate(
      unit_sales = log1p(ifelse(unit_sales > 0, unit_sales, 0)),
      onpromotion = as.numeric(onpromotion),
      date = as.Date(date)
    ) %>%
    as.data.table()
  
  data[is.na(unit_sales), unit_sales := 0]
  data[is.na(onpromotion), onpromotion := 0]

}

submit <- function(data) {
  
  test <- fread('input/test.csv', sep=",", na.strings = "")
  test <- test %>%
    mutate(
      onpromotion = as.numeric(onpromotion),
      date = as.Date(date)
    ) %>%
    left_join(data, by=c("item_nbr","store_nbr","onpromotion")) %>% 
    select(id, unit_sales) %>%
    as.data.table()

  test[is.na(unit_sales), unit_sales := 0]
  test %>% write.table('output/submission.csv', sep=",", dec=".", quote=FALSE, row.names=FALSE)
}

train_transformed <- transform(train)

train_transformed <- train_transformed %>%
  group_by(item_nbr, store_nbr, onpromotion) %>%
  summarise(
    unit_sales = expm1(mean(unit_sales))
  )

submit(train_transformed)
