## Analise da Base

library(dplyr)
library(lubridate)
library(recipes)
library(ggplot2)
library(purrr)
library(tidyr)
library(corrplot)

## TO DOs -----

# contar / tratar NAs nas colunas de tempo

## Import e Join Base de dados-----
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
  dplyr::full_join(sellers, by = "seller_id")

## Primeira visão da tabela-----

df_olist %>% select(ends_with("id")) %>%  glimpse()

df_olist %>% select(where(is.character)) %>% glimpse()

df_olist %>% select(where(is.integer)) %>% glimpse()

## Limpeza da tabela-----

# Tirando as colunas de identificação uma vez que foram juntadas e renomeando colunas order item id para number_order:

df_no_id <- df_olist %>% select(!ends_with("id")) %>% mutate(number_order = df_olist$order_item_id)

# formato date:

df_date <- df_no_id %>%
  mutate(day_purchase = as_date(df_olist$order_purchase_timestamp)) %>%
  mutate(day_aprove = as_date(df_olist$order_approved_at)) %>%
  mutate(day_delivered_carrier = as_date(df_olist$order_delivered_carrier_date)) %>%
  mutate(day_delivered_customer = as_date(df_olist$order_delivered_customer_date)) %>%
  mutate(day_estimated = as_date(df_olist$order_estimated_delivery_date)) %>%
  mutate(day_shipping_limit = as_date(df_olist$shipping_limit_date)) %>%
  mutate(day_review_creation = as_date(df_olist$review_creation_date)) %>%
  mutate(day_answer_review = as_date(df_olist$review_answer_timestamp)) %>%
  select(!ends_with("date")) %>%
  select(!ends_with("timestamp")) %>%
  select(!order_approved_at)

## Estimativa de dias de atraso e tempo decorrico

df_delay <- df_date %>% mutate(delay_time = df_date$day_estimated - df_date$day_delivered_customer) %>%
  mutate(delivery_time = df_date$day_delivered_customer -  df_date$day_purchase)

## Criar variável de alaviação alta ou baixa (high e low) para visualizar correlação de notas altas e outras variáveis

df_review <- df_delay %>% mutate(review_high = ifelse(df_olist$review_score>=4, "high", "low"))

## Criar variável tamanho do produto pois é mais relevante q ter três medidas (comprimento, altura e largura)

df_dim <- df_review %>% select(!ends_with("cm")) %>%
  mutate(product_dimensions_cm = df_olist$product_height_cm * df_olist$product_length_cm * df_olist$product_width_cm)

## Substituir NA values por moda

df_NA_chr <- df_dim %>% mutate(across(where(is.character),~replace_na(.x, max(.x , na.rm = TRUE))))

df_NA_numeric <- df_NA_chr %>% mutate(across(where(is.numeric),~replace_na(.x, mean(.x , na.rm = TRUE))))

## Criar variável estado do vendedor é o mesmo do comprador

df_ae <- df_NA_numeric %>% mutate(same_estate_cs = ifelse(df_olist$customer_state == df_olist$seller_state, 1, 0))
## Analise Exploratória -----
# Variaveis categorica:

df_ae %>% count(delivery_time) %>% arrange(desc(n))

col_cat <- df_ae %>%
  select(where(is.character)) %>%
  select(!starts_with("review")) %>%
  select(!ends_with("city")) %>%
  colnames()

df_ae %>%
  ggplot(aes(payment_type, fill = review_high)) +
  geom_bar(position = 'dodge')

fazer_graf_cat <- function(x){
  df_ae %>%
    ggplot(aes(.data[[x]], fill = review_high)) +
    geom_bar(position = 'dodge')

}
fazer_list_cat <- function(x){
  df_ae %>% count(.data[[x]]) %>% arrange(desc(n)) %>% head()
}

graficos_cat <- map(col_cat, fazer_graf_cat)

summarise_cat <- map(col_cat,fazer_list_cat )

# patchwork::wrap_plots(graficos_cat) obs. pode ser util

#variaveis continuas:

col_con <- df_ae %>% select(where(is.numeric)) %>% colnames()# tirar colunas q n fazem sentido, tipo review score e lenght name


df_ae %>%
  ggplot(aes(delay_time, colours = review_high)) +
  geom_freqpoly()




fazer_graf_con <- function(x){
  df_ae %>%
    ggplot(aes(.data[[x]], fill = review_high)) +
    geom_histogram()

}
graficos_con <- map(col_con, fazer_graf_con)

summarise_cat <- map(col_con,fazer_list_cat )

## Fazer uma matrix de correlação ("this is fine :,)")

correlacao <- cor(df_ae %>% select(where(is.numeric)))

corrplot(correlacao, method = 'color', order = 'alphabet')

df_ae %>% filter(!is.na(review_score)) %>% count(review_score)

df_ae %>% count(review_score)

teste <- replace(NA, df_ae$review_score, mean(df_ae$review_score, na.rm = TRUE))
df_semna <- replace

## Processamento de dados  -----

