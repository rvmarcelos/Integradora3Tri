## Analise da Base

library(dplyr)


## TO DOs -----

# Mudar colunas "review_creation_date" e "review_answer_timestamp" para formato date time




## Base de dados-----
#Lendo

sellers <- read.csv('olist_sellers_dataset.csv')

product_name <- read.csv('product_category_name_translation.csv')

product <- read.csv('olist_products_dataset.csv')

reviews <- read.csv('olist_order_reviews_dataset.csv')

orders <- read.csv('olist_orders_dataset.csv')

payments <- read.csv('olist_order_payments_dataset.csv')

item <- read.csv('olist_order_items_dataset.csv')

geolocalization <- read.csv('olist_geolocation_dataset.csv')

customers <- read.csv('olist_customers_dataset.csv')

# Tentativa de fazer rápido, olhar depois
# head(sellers)
#
# file_names_wd <- list.files( pattern = "*.csv", full.names = T)
#
# df <- file_names_wd %>%
#   purrr::map(~read.csv(.))

#join
df_olist <- orders %>%
  dplyr::left_join(item, by = "order_id") %>%
  dplyr::full_join(payments, by = "order_id") %>%
  dplyr::full_join(reviews, by = "order_id") %>%
  dplyr::full_join(product, by = "product_id") %>%
  dplyr::full_join(customers, by = "customer_id") %>%
  dplyr::full_join(sellers, by = "seller_id") %>%
  glimpse

## Primeira visão da tabela

df_olist %>% select(ends_with("id")) %>%  glimpse()

colunas_chr <- df_olist %>% select(where(is.character)) %>% glimpse()

df_olist %>% select(where(is.integer)) %>% glimpse()
#olhar colunas order id
