## Analise da Base

library(dplyr)
library(lubridate)
library(recipes)
library(ggplot2)
library(purrr)
library(tidyr)
library(corrplot)

## TO DOs -----

# Analise Kmeans ok

# Melhorar matrix correlaÃ§Ã£o...

# Fazer Boxplots e outros grÃ¡ficos pertinentes...


# Tratar outliers

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

br <- sf::read_sf("C:/Users/Marcelo/Desktop/insper/geodata-br-master/geojson/geojs-100-mun.json")

# Tentativa de fazer rÃ¡pido, olhar depois
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

## Primeira visÃ£o da tabela-----

df_olist %>% select(ends_with("id")) %>%  glimpse()

df_olist %>% select(where(is.character)) %>% glimpse()

df_olist %>% select(where(is.integer)) %>% glimpse()

## Limpeza da tabela-----

# Tirando as colunas de identificaÃ§Ã£o uma vez que foram juntadas e renomeando colunas order item id para number_order:

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


## Substituir NA values por moda e mÃ©dia

df_NA_chr <- df_date %>%
  mutate(across(where(is.character) & !starts_with("review"),~replace_na(.x, max(.x , na.rm = TRUE))))

df_NA_numeric <- df_NA_chr %>% mutate(across(where(is.numeric),~replace_na(.x, mean(.x , na.rm = TRUE))))


## Estimativa de dias de atraso e tempo decorrico

df_delay <- df_NA_numeric %>% mutate(delay_time = as.numeric( day_delivered_customer - day_estimated )) %>%
  mutate(delivery_time = as.numeric(df_NA_numeric$day_delivered_customer -  df_NA_numeric$day_purchase))

## Criar variÃ¡vel de alaviaÃ§Ã£o alta ou baixa (high e low) para visualizar correlaÃ§Ã£o de notas altas e outras variÃ¡veis

df_review <- df_delay %>% mutate(review_high = ifelse(df_delay$review_score>=4, "high", "low"))

## Criar variÃ¡vel tamanho do produto pois Ã© mais relevante q ter trÃªs medidas (comprimento, altura e largura)

df_dim <- df_review %>% select(!ends_with("cm")) %>%
  mutate(product_dimensions_cm = df_review$product_height_cm * df_review$product_length_cm * df_review$product_width_cm)

## Criar variÃ¡vel estado do vendedor Ã© o mesmo do comprador

df_ae <- df_dim %>% mutate(same_estate_cs = ifelse(df_dim$customer_state == df_dim$seller_state, 1, 0))

## Dummys

Receita <- recipe(review_high ~ ., data = df_ae) %>%
  step_select(-starts_with("review_comment"), -ends_with("city")) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal())


preparado <- prep(Receita, df_ae)
df_dummy <- bake(preparado, new_data = NULL)

## GeoLocalizacao:


# Guardei isso pq mostra raciocínio de como resolver
# left_join(x = df_ae, y = geolocalization %>% distinct_all(), 
#           by = c('seller_zip_code_prefix' = 'geolocation_zip_code_prefix', 
#                  'seller_city' = "geolocation_city",
#                  "seller_state" = "geolocation_state")) %>% glimpse()


df_mega_foda <- df_ae %>%
  left_join(geolocalization %>% group_by(geolocation_zip_code_prefix) %>% 
              summarise(geo_lat = max(geolocation_lat),
                        geo_lng = max(geolocation_lng)),
            by = c('seller_zip_code_prefix' = 'geolocation_zip_code_prefix')) %>%
  mutate(seller_lat = geo_lat,
         seller_lgn = geo_lng) %>% 
  select(!starts_with('geo')) %>% 
  left_join(geolocalization %>% group_by(geolocation_zip_code_prefix) %>% 
              summarise(geo_lat = max(geolocation_lat),
                        geo_lng = max(geolocation_lng)),
            by = c('customer_zip_code_prefix' = 'geolocation_zip_code_prefix')) %>%
  mutate(customer_lat = geo_lat,
         customer_lgn = geo_lng) %>% 
  select(!starts_with('geo'))



 # VisualizaÃ§Ã£o pÃ³s tratamento

#df_NA_chr %>% glimpse()

df_ae %>% skimr::skim()

## Analise ExploratÃ³ria -----
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

## Fazer uma matrix de correlaÃ§Ã£o ("this is fine :,)")----

correlacao <- cor(df_ae %>% select(where(is.numeric)))

corrplot(correlacao, method = 'color', order = 'alphabet')

df_ae %>% filter(!is.na(review_score)) %>% count(review_score)

df_ae %>% count(review_score)

# teste <- replace(NA, df_ae$review_score, mean(df_ae$review_score, na.rm = TRUE))
# df_semna <- replace

## Analise dos Sellers e Customers  -----

#Sellers:

df_ae %>% drop_na()%>%
  group_by(seller_zip_code_prefix) %>%
  summarise(Media_atraso = mean(delay_time),
            ticket_medio = mean(price),
            vendas_total = sum(payment_value),
            estado = max(seller_state)) %>%
  arrange(desc(vendas_total)) 




df_ae %>% count(seller_state) %>%
  arrange(desc(n)) %>% head()

df_ae %>% count(product_category_name) %>% 
  arrange(desc(n)) %>% head()

## Plot em mapa: ----

df_mega_foda %>% 
  ggplot(aes(payment_type))+
  geom_bar()

df_mega_foda %>% 
  ggplot()+
  geom_bar(aes(payment_type))


df_mega_foda %>% 
  ggplot()+
  geom_point(aes(seller_lgn, seller_lat))



gg_pontos_com_mapa <- br %>%
  ggplot() +
  geom_sf(colour = "black", size = .1) +
  geom_point(
    aes(seller_lgn, seller_lat),
    data = df_mega_foda,
    alpha = .7
  )


gg_pontos_com_mapa




## Kmeans ----

df_dummy %>% skimr::skim()
df_ae  %>% skimr::skim()

analisek <- kmeans(df_dummy %>% drop_na() %>% select(!starts_with("day")), centers = 4)

df_cluster <- df_ae %>% drop_na() %>% mutate(cluster = analisek$cluster)

df_cluster %>% group_by(cluster) %>% 
  summarise(delay = mean(delay_time),
            ticket_medio = mean(payment_value),
            review = mean(review_score),
            mesmo_UF = mean(same_estate_cs))
