## Analise da Base



library(dplyr)
library(lubridate)
library(recipes)
library(ggplot2)
library(purrr)
library(tidyr)
library(corrplot)
library(ggridges)
library(hrbrthemes)
library(viridis)
library(geosphere)


## TO DOs -----

# Olhar a recorrência dos usuários

# Olhar relação com qualidade da photo do produto e descrição produto (nesta, olhar um tamanho ótimo de descrição, nem grande nem pequeno)

# Mexer nos slides


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

#tomar cuidado, revisar sum
item <- read.csv('olist_order_items_dataset.csv') %>% 
  group_by(order_id) %>% 
  summarise(across(c(product_id,seller_id, shipping_limit_date   ), first), 
            price = sum(price),
            freight_value = sum(freight_value ))

geolocalization <- read.csv('olist_geolocation_dataset.csv')

customers <- read.csv('olist_customers_dataset.csv')

br <- sf::read_sf("C:/Users/USER/Desktop/Projetos_GitHub/geodata-br-master/geojson/geojs-100-mun.json")

sp <- sf::read_sf("C:/Users/USER/Desktop/Projetos_GitHub/geodata-br-master/geojson/geojs-35-mun.json")

# Tentativa de fazer rÃ¡pido, olhar depois
# head(sellers)
#
# file_names_wd <- list.files( pattern = "*.csv", full.names = T)
#
# df <- file_names_wd %>%
#   purrr::map(~read.csv(.))

#join
df_olist <- orders %>%
  left_join(item, by = "order_id") %>%
  full_join(payments, by = "order_id") %>%
  full_join(reviews, by = "order_id") %>%
  full_join(product, by = "product_id") %>%
  full_join(customers, by = "customer_id") %>%
  full_join(sellers, by = "seller_id")%>%
  # Juntando com tabela geolocalizacao, culpa do Julio
  left_join(geolocalization %>% group_by(geolocation_zip_code_prefix) %>% 
              summarise(geo_lat = max(geolocation_lat),
                        geo_lng = max(geolocation_lng)),
            by = c('seller_zip_code_prefix' = 'geolocation_zip_code_prefix')) %>%
  mutate(seller_lat = geo_lat,
         seller_lng = geo_lng) %>% 
  select(!starts_with('geo')) %>% 
  left_join(geolocalization %>% group_by(geolocation_zip_code_prefix) %>% 
              summarise(geo_lat = max(geolocation_lat),
                        geo_lng = max(geolocation_lng)),
            by = c('customer_zip_code_prefix' = 'geolocation_zip_code_prefix')) %>%
  mutate(customer_lat = geo_lat,
         customer_lng = geo_lng) %>% 
  select(!starts_with('geo'))

## Feature engineering-----

df_ae <- df_olist %>%
  # formato date:
  mutate(day_purchase = as_date(order_purchase_timestamp)) %>%
  mutate(day_aprove = as_date(order_approved_at)) %>%
  mutate(day_delivered_carrier = as_date(order_delivered_carrier_date)) %>%
  mutate(day_delivered_customer = as_date(order_delivered_customer_date)) %>%
  mutate(day_estimated = as_date(order_estimated_delivery_date)) %>%
  mutate(day_shipping_limit = as_date(shipping_limit_date)) %>%
  mutate(day_review_creation = as_date(review_creation_date)) %>%
  mutate(day_answer_review = as_date(review_answer_timestamp)) %>%
  select(!ends_with("date")) %>%
  select(!ends_with("timestamp")) %>%
  select(!order_approved_at)%>%
  #Tratando NA com mode e media
  mutate(across(where(is.character) & !starts_with("review"),~replace_na(.x, max(.x , na.rm = TRUE))))%>% 
  mutate(across(where(is.numeric),~replace_na(.x, mean(.x , na.rm = TRUE))))%>% 
  # Distancia Seller e Customer
  mutate(distance = sqrt((customer_lat - seller_lat)^2+(customer_lng - seller_lng)^2))%>% 
  #mesmo estado
  mutate(same_estate_cs = ifelse(customer_state == seller_state, 1, 0))%>%
  # Dias de atraso
  mutate(delay_total_time = as.numeric( day_delivered_customer - day_estimated )) %>%
  # Tempo decorrido total
  mutate(delivery_time = as.numeric(day_delivered_customer - day_purchase)) %>% 
  # Tempo entre seller e carrier
  mutate(seller_to_carier_time = as.numeric(day_delivered_carrier - day_aprove)) %>% 
  #Dias de atraso pela espectativa do cliente (problema com fim de semana)
  mutate(delay_expectation_time = as.numeric(day_delivered_customer - (day_purchase + 10))) %>% 
  #Dias de atraso pela espectativa do cliente 2 (usando distância) (problema com fim de semana)
  mutate(delay_expectation_time_2 = as.numeric(day_delivered_customer - (day_purchase + 12 - 7*same_estate_cs))) %>% 
  #Variavel tempo de resposta
  mutate(answer_review_time = as.numeric(day_answer_review - day_review_creation)) %>% 
  # Variavel de visualizacao review high or low
  mutate(review_high = ifelse(review_score>=4, "high", "low"))%>% 
  #Criar variavel dimensao
  mutate(product_dimensions_cm3 = product_height_cm * product_length_cm * product_width_cm) %>% 
  select(!ends_with("cm"))%>%
  #Tratando NA de categorica com valor mais frequente
  mutate(across(where(is.character) & !starts_with("review"),~replace_na(.x, max(.x , na.rm = TRUE)))) %>% 
  # Tratando NA de continuas com media
  mutate(across(where(is.numeric),~replace_na(.x, mean(.x , na.rm = TRUE)))) %>% 
  #Levando a review_score para ultima coluna
  relocate(review_score, .after = last_col())

# Distancia dois
mutate(distance =  distm(c(customer_lng, customer_lat), c(seller_lng, seller_lat), fun = distHaversine)) %>%
  glimpse()

lat_lng <- distm(c(df_ae$customer_lng, df_ae$customer_lat), c(df_ae$seller_lng, df_ae$seller_lat), fun = distHaversine)  
  
## Substituir NA values por moda e mÃ©dia

# df_NA_chr <- df_date %>%
#   #Tratando NA de categorica com valor mais frequente
#   mutate(across(where(is.character) & !starts_with("review"),~replace_na(.x, max(.x , na.rm = TRUE))))
# 
# df_NA_numeric <- df_NA_chr %>% 
#   mutate(across(where(is.numeric),~replace_na(.x, mean(.x , na.rm = TRUE))))


## Estimativa de dias de atraso e tempo decorrico

# df_delay <- df_ae %>%
  # Dias de atraso
  # mutate(delay_total_time = as.numeric( day_delivered_customer - day_estimated )) %>%
  # # Tempo decorrido total
  # mutate(delivery_time = as.numeric(day_delivered_customer - day_purchase)) %>% 
  # # Tempo entre seller e carrier
  # mutate(seller_to_carier_time = as.numeric(day_delivered_carrier - day_aprove)) %>% 
  #Dias de atraso pela espectativa do cliente (problema com fim de semana)
  # mutate(delay_expectation_time = as.numeric(day_delivered_customer - (day_purchase + 10))) %>% 
  # #Dias de atraso pela espectativa do cliente 2 (usando distância) (problema com fim de semana)
  # mutate(delay_expectation_time_2 = as.numeric(day_delivered_customer - (day_purchase + 12 - 7*same_estate_cs))) %>% 
  # glimpse()

## Criar variÃ¡vel de alaviaÃ§Ã£o alta ou baixa (high e low) para visualizar correlaÃ§Ã£o de notas altas e outras variÃ¡veis

# df_review <- df_delay %>% 
#   # Variavel de visualizacao review high or low
#   mutate(review_high = ifelse(df_delay$review_score>=4, "high", "low"))

## Criar variÃ¡vel tamanho do produto pois Ã© mais relevante q ter trÃªs medidas (comprimento, altura e largura)

# df_dim <- df_review %>% 
#   #Criar variavel dimensao
#   mutate(product_dimensions_cm3 = df_review$product_height_cm * df_review$product_length_cm * df_review$product_width_cm) %>% 
#   select(!ends_with("cm"))

## Criar variÃ¡vel estado do vendedor Ã© o mesmo do comprador

# df_mesmo_UF <- df_dim %>% 
#   #mesmo estado
#   mutate(same_estate_cs = ifelse(df_dim$customer_state == df_dim$seller_state, 1, 0))

## Criar variavél atrasou ou não
# 
# df_ae <- df_mesmo_UF %>% mutate(delay_occurrence = ifelse(delay_time >=0,1,0))


## GeoLocalizacao:


# Guardei isso pq mostra raciocínio de como resolver
# left_join(x = df_ae, y = geolocalization %>% distinct_all(), 
#           by = c('seller_zip_code_prefix' = 'geolocation_zip_code_prefix', 
#                  'seller_city' = "geolocation_city",
#                  "seller_state" = "geolocation_state")) %>% glimpse()


# df_lat_lng <- df_ae %>%
#   left_join(geolocalization %>% group_by(geolocation_zip_code_prefix) %>% 
#               summarise(geo_lat = max(geolocation_lat),
#                         geo_lng = max(geolocation_lng)),
#             by = c('seller_zip_code_prefix' = 'geolocation_zip_code_prefix')) %>%
#   mutate(seller_lat = geo_lat,
#          seller_lng = geo_lng) %>% 
#   select(!starts_with('geo')) %>% 
#   left_join(geolocalization %>% group_by(geolocation_zip_code_prefix) %>% 
#               summarise(geo_lat = max(geolocation_lat),
#                         geo_lng = max(geolocation_lng)),
#             by = c('customer_zip_code_prefix' = 'geolocation_zip_code_prefix')) %>%
#   mutate(customer_lat = geo_lat,
#          customer_lng = geo_lng) %>% 
#   select(!starts_with('geo'))

## Distancia Seller e Customer 

# df_map_plot <- df_lat_lng%>% 
#   mutate(distance = sqrt((customer_lat - seller_lat)^2+(customer_lng - seller_lng)^2)) 

## Recipes: Dummy, normalização -----


Receita <- recipe(review_high ~ ., data = df_ae) %>%
  step_select(-ends_with("id"),
              -payment_type,
              -review_comment_title,
              -review_comment_message, 
              #-product_category_name,
              - customer_city,
              - customer_state,
              - seller_city,
              - seller_state,
              - starts_with('day')) %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())


preparado <- prep(Receita, df_ae)
df_receita <- bake(preparado, new_data = NULL)




## Analise Exploratoria Grafico -----
# Variaveis categorica:

df_ae %>% count(delivery_time) %>% arrange(desc(n))

col_cat <- df_ae %>%
  select(where(is.character)) %>%
  select(!starts_with("review")) %>%
  select(!ends_with("city")) %>%
  colnames()

df_ae %>%
  ggplot(aes(payment_type, fill = review_high)) +
  geom_bar(position = 'fill')

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

df_ae %>% 
  ggplot(aes(factor(review_score), delay_time))+
  geom_boxplot()


df_ae %>% ggplot(aes(number_order))+
  geom_histogram()

fazer_graf_con <- function(x){
  df_ae %>%
    ggplot(aes(.data[[x]], fill = review_high)) +
    geom_histogram()
  
}
graficos_con <- map(col_con, fazer_graf_con)

summarise_cat <- map(col_con,fazer_list_cat )

## Fazer uma matrix de correlacao ("this is fine :,)")----

correlacao <- cor(df_ae %>% select(where(is.numeric)))

corrplot(correlacao, method = 'color', order = 'alphabet')

cor_review <- data.frame(n = c(correlacao)) %>%
  slice_tail(n = 26) %>%
  mutate(variaveis = df_ae %>%select(where(is.numeric)) %>%colnames())

cor_review %>% arrange(desc(n))

df_ae$product_name_lenght

df_ae %>% filter(!is.na(review_score)) %>% count(review_score)

df_ae %>% count(answer_review_time) %>% arrange() %>% head()

df_ae %>% count(delivery_time) %>% arrange()

# teste <- replace(NA, df_ae$review_score, mean(df_ae$review_score, na.rm = TRUE))
# df_semna <- replace

## Analise dos Sellers e Customers  -----

#Sellers:

df_ae %>% drop_na()%>%
  group_by(seller_zip_code_prefix) %>%
  summarise(Media_atraso = mean(delay_time),
            ticket_medio = mean(price),
            vendas_total = sum(payment_value),
            estado = max(seller_state),
            review = mean(review_score),
            produto_mais_vendido = max(product_category_name)) %>%
  arrange(desc(vendas_total))

mean(df_ae$review_score)

df_ae %>% count(seller_state) %>%
  arrange(desc(n)) %>% head()

df_ae %>% count(product_category_name) %>% 
  arrange(desc(n)) %>% head()

# Customer:

df_ae %>% count(customer_zip_code_prefix) %>% arrange(desc(n)) %>% head()

## Plot em mapa: ----



mapa_import <- br %>%
  ggplot() +
  geom_sf(colour = "black", size = .1) +
  geom_point(
    aes(x = customer_lgn,
        y = customer_lat,
        colour = factor(review_high)),
    data = df_map_plot %>% filter(delay_occurrence == 1) %>% filter(customer_lat<=20),
    alpha = .7,
    size = 1
  )

mapa_import

mapa_sp_seller <- sp %>%
  ggplot() +
  geom_sf(colour = "black", size = .1) +
  geom_point(
    aes(seller_lgn, seller_lat),
    colour = "green",
    data = df_map_plot %>% filter(seller_state == "SP"),
    alpha = .7
  )

mapa_sp_seller




mapa_br_customer <- br %>%
  ggplot() +
  geom_sf(colour = "black", size = .1) +
  geom_point(
    aes(customer_lgn, customer_lat),
    colour = "red",
    data = df_map_plot %>% filter(customer_lat<=20),
    alpha = .7
  )

mapa_sp_customer <- sp %>%
  ggplot() +
  geom_sf(colour = "black", size = .1) +
  geom_point(
    aes(customer_lgn, customer_lat),
    colour = "red",
    data = df_map_plot %>% filter(customer_state == "SP"),
    alpha = .7
  )
df_map_plot %>% count(seller_lat) %>% arrange(desc(n))


mapa_br_customer
mapa_sp_customer


## Kmeans ----

df_receita %>% skimr::skim()
df_ae  %>% skimr::skim()

analisek <- kmeans(df_receita  %>% select(!review_high), centers = 4)

df_cluster <- df_ae %>% mutate(cluster = analisek$cluster)

df_cluster %>% glimpse()

df_cluster %>% group_by(cluster) %>% 
  summarise(delay = mean(delay_time),
            ticket_medio = mean(payment_value),
            review = mean(review_score),
            mesmo_UF = mean(same_estate_cs))






# Atraso e Review------

df_ae %>% filter(!between(review_score,4.01,4.9)) %>% 
  ggplot(aes(x = delay_expectation_time_2,
             y = factor(review_score), 
             fill = as.factor(review_score))) +
  geom_density_ridges(alpha=0.6,
                      quantile_lines = TRUE, quantiles = 2) + 
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(0,50))+
  #scale_y_discrete(limits = c(1,5))+
  labs(x = "Atraso do Produto",
       y = "Review",
       
       caption = "Em dias")+
  theme_classic()+
  theme(legend.position = "none")+
  geom_vline(xintercept = mean(df_ae$delay_expectation_time_2),
             color = "#595959",
             size = 1,
             linetype = 2)+
  scale_fill_brewer(palette = 'RdGy')+
  annotate(geom = "text", x = 9, y = 6.7, label = "média atraso geral")+
  annotate(geom = "text", x = 13, y = .9, label = "mediana atraso grupo")
  #scale_fill_manual()
  

# Descrição / transparencia do produto e review-----

df_ae %>% filter(!between(review_score,4.01,4.9)) %>% 
  filter(product_category_name %in% descrição_grande) %>%
  ggplot(aes(x = product_description_lenght,
             y = factor(review_score), 
             fill = as.factor(review_score))) +
  geom_density_ridges(alpha=0.6, quantile_lines = TRUE, quantiles = 2) + 
  theme(legend.position = "none")+
  #scale_x_continuous(limits = c(0,4000))+
  #scale_y_discrete(limits = c(1,5))+
  labs(x = "Tamanho da descrição",
       y = "Review",
       title = "Análise do review pela descrição",
       subtitle = "Sem conclusão",
       caption = "Em número de palavras")+
  theme_classic()+
  theme(legend.position = "none")+
  geom_vline(xintercept = mean(df_ae$product_description_lenght),color = "gray",size = 1,linetype = 2)+
  scale_fill_brewer(palette = 'RdGy')+
  #annotate(geom = "text", x = 1250, y = 6.5, label = "média atraso geral", color = "gray")+
  annotate(geom = "text", x = 300, y = .9, label = "mediana atraso grupo") 


#procura de filtro por categoria

descrição_grande <- df_ae %>%
  filter(review_score == 1) %>% 
  group_by(product_category_name) %>% 
  summarise(
            #nota = mean(review_score),
            descricao_media = mean(product_description_lenght),
            descricao_var = sd(product_description_lenght),
            receita = sum(payment_value)) %>% 
  arrange(desc(descricao_media)) %>% 
  pull(product_category_name) %>% 
  head(10)




# filtro de produto de informatica

df_ae %>% filter(!between(review_score,4.01,4.9)) %>% filter(product_category_name == "informatica_acessorios") %>% 
  ggplot(aes(x = product_description_lenght,
             y = factor(review_score), 
             fill = as.factor(review_score))) +
  geom_density_ridges(alpha=0.6, quantile_lines = TRUE, quantiles = 2) + 
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(0,4000))+
  #scale_y_discrete(limits = c(1,5))+
  labs(x = "Tamanho da descrição",
       y = "Review",
       title = "Análise do review pela descrição",
       subtitle = "Sem conclusão",
       caption = "Em número de palavras")+
  theme_classic()+
  theme(legend.position = "none")+
  #geom_vline(xintercept = mean(df_ae$product_description_lenght),color = "gray",size = 1.5,linetype = 2)+
  scale_fill_brewer(palette = 'RdGy')+
  #annotate(geom = "text", x = 1250, y = 6.5, label = "média atraso geral", color = "gray")+
  annotate(geom = "text", x = 300, y = .9, label = "mediana atraso grupo") 


# qualidade da foto

df_ae %>% filter(!between(review_score,4.01,4.9)) %>% 
  ggplot(aes(x = product_photos_qty,
             y = factor(review_score), 
             fill = as.factor(review_score))) +
  geom_density_ridges(alpha=0.6, quantile_lines = TRUE, quantiles = 2) + 
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(0,10))+
  #scale_y_discrete(limits = c(1,5))+
  labs(x = "Qualidade da foto",
       y = "Review",
       title = "Análise do review pela descrição",
       subtitle = "Sem conclusão",
       caption = "Em número de palavras")+
  theme_classic()+
  theme(legend.position = "none")+
  #geom_vline(xintercept = mean(df_ae$product_description_lenght),color = "gray",size = 1.5,linetype = 2)+
  scale_fill_brewer(palette = 'RdGy')+
  #annotate(geom = "text", x = 1250, y = 6.5, label = "média atraso geral", color = "gray")+
  annotate(geom = "text", x = 300, y = .9, label = "mediana atraso grupo") 

df_ae %>% filter(!between(review_score,4.01,4.9)) %>% 
  ggplot( aes(x=factor(review_score), y=product_photos_qty, fill=factor(review_score))) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'RdGy')+
  labs(x = "Review",
       y = "Qualidade das foto",
       title = "Análise do review pela descrição",
       subtitle = "Sem conclusão",
       caption = "Em número de palavras")+
  theme_classic()+
  theme(legend.position = "none")

# Distancia por tempo de atraso-----

df_ae %>% filter(!between(review_score,4.01,4.9))%>% 
  ggplot(aes(x=distance, y=delay_expectation_time)) + 
  #geom_point( aes(color=review_high), alpha = .5) +
  geom_point( color="#A62B4D", alpha = .7) +
  geom_smooth(method=lm, se=FALSE, color = "#414042", size = 1.5) +
  scale_y_continuous(limits = c(0,100))+
  scale_x_continuous(limits = c(0,40))+
  theme_ipsum()+
  labs(x = "Distancia Customer e Seller",
       y = "Atraso",
       caption = "Em dias",
       color = "Review")+
  theme_classic()+
  annotate(geom = "text", x = 35, y = 20, label = "Regressão linear")
  
mean(df_ae$delay_expectation_time)

mean(df_ae$distance)

df_ae %>% filter(!between(review_score,4.01,4.9))%>% count(review_score)

# Categoria de produto e review-------

df_receita %>% glimpse()

cor_cat_review <- df_receita %>%
  select(ends_with(teste), review_score) %>%
  rename_with(~ tolower(gsub("product_category_name_", "p_", .x, fixed = TRUE))) %>% 
  cor()

corrplot(cor_cat_review, method = 'color', order = 'alphabet')

nome <- df_ae %>% count(product_category_name) %>% arrange(desc(n))
nome2 <- nome %>% filter(n>=2000)
teste <- c(nome2$product_category_name)

df_ae %>%
  filter()
  ggplot(aes(product_category_name, fill = review_high)) +
  geom_bar(position = 'dodge')

df_ae %>% count(customer_id) %>% arrange(desc(n)) %>% head()
  

# Analise de recorrencia------

recompra <- orders %>%
  count(customer_id) %>%
  arrange(desc(n)) %>%
  filter(n >= 2) %>%
  pull(customer_id)

df_ocor <- df_ae %>% 
  mutate(churn = ifelse(customer_id %in% recompra, 1, 0)) 

df_ocor %>% 
  ggplot(aes(factor(review_score), fill = factor(churn)))+
  geom_bar(position = 'dodge')

df_ocor %>% count(churn)

cor(x = df_ocor$churn, y = df_ocor$review_score)

cor(x = factor(df_ae$review_score), y = ifelse(df_ae$payment_type == ""))

df_ae %>% count(order_id) %>% arrange(desc(n)) %>% head()

janitor::get_dupes(item, "order_id") %>% glimpse()

## Analise do TAM ------

df_ae %>% 
  filter(review_high == "high") %>% 
  summarise(
    nota_média = mean(review_score),
    ticket_medio = mean(payment_value),
    delivery = mean(delivery_time)
    )

df_ae %>% filter(review_high=="low")

recompra <- .6

count(df_ae %>% filter(review_high=="low"))*recompra*mean(df_ae$payment_value)
