setwd('~/Documentos/academic/house_effects/')


library(rstan)
library(tidyverse)
library(lubridate)


data_total <- list(
  `2014` = list(polls = read.csv('polls_2014_Presidential_2.csv', stringsAsFactors = F, sep = ';'),
                start_2round_day = "06/10/2014",
                election_day = "26/10/2014"),
  `2010` = list(polls = read.csv('polls_2010_Presidential_2.csv', stringsAsFactors = F, sep = ';'),
                start_2round_day = "4/10/2010",
                election_day = "31/10/2010"),
  `2006` = list(polls = read.csv('polls_2006_Presidential_2.csv', stringsAsFactors = F, sep = ';'),
                start_2round_day = "1/10/2006",
                election_day = "29/10/2006")
)





# wrangling data

wrangle_polls <- function(partial_data) {
  polls <- partial_data$polls %>%
    select(-Numero.Registro, -link) %>% 
    gather(candidate, percentage, -Entrevistas, -Instituto, -Data) %>%
    filter(stringr::str_detect(candidate, "(PT|PSDB)")) 
  
  start_2round_day <- lubridate::dmy(partial_data$start_2round_day)
  election_day <- lubridate::dmy(partial_data$election_day)
  
  
  polls <- polls %>%
    mutate(Data = ymd(Data),
           percentage = percentage %>% str_replace(',', '.') %>%  as.numeric %>% `/`(., 100)
           ) %>%
    filter(Data >= start_2round_day) %>%
    filter(!is.na(percentage)) %>%
    mutate(t = as.integer(Data - start_2round_day) + 1,
           id_instituto = Instituto %>% as.factor %>% as.integer) %>%
    group_by(Data, Instituto) %>%
    mutate(total_percentage = sum(percentage)) %>%
    ungroup() %>%
    mutate(percentage = percentage / total_percentage) %>%
    mutate(candidate = stringr::str_trim(stringr::str_replace_all(candidate, "(\\.)+", " "))) %>%
    rename(n_entrevistas = Entrevistas)
    
  polls
}



data_total <- map(data_total, wrangle_polls)













##################################################################
# E-4
# Calculating house effects
##################################################################

create_datalist <- function(df,candidate_name,actual_result) {
  # Return data list for using inside Stan
  df <- dplyr::filter(df, candidate == candidate_name)
  return(
    list(n_days = max(df$t),
         y_n = nrow(df),
         y_values = df$percentage,
         y_days = df$t,
         n_sample = df$n_entrevistas,
         id_company = df$id_instituto,
         n_companies = length(unique(df$id_instituto)),
         actual_result = actual_result
    )
  )
}


agg_model <- '
data {
  int<lower=1> n_days;            // number of days
  int<lower=1> y_n;               // number of polls
  real y_values[y_n];             // actual values in polls
  int<lower=0> y_days[y_n];       // the number of days since starting election each poll was taken
  real n_sample[y_n];             // sample size for each poll
  int<lower=0> id_company[y_n];        // id for research companies
  int<lower=1> n_companies; // number of companies 
  real actual_result;
}
parameters {
  real<lower=0, upper=1> mu[n_days];               // underlying state of vote intention
  real gamma[n_companies];
}
model {
  mu[1] ~ uniform(0, 1);
  for (i in 2:(n_days-1))
    mu[i] ~ normal(mu[i - 1], 0.0025);
  
  mu[n_days] ~ normal(actual_result, 0.0025 );

  gamma ~ normal(0, 0.02);


  for(x in 1:y_n)  {
    y_values[x] ~ normal(mu[y_days[x]]  + gamma[id_company[x]], 
        sqrt(y_values[x]*(1-y_values[x])/n_sample[x]) );
  }
  
}'


#modelo <- stan_model(model_code = agg_model)
#saveRDS(modelo, file = "modelo.rds")
modelo <- readRDS('modelo.rds')

dataset_2014 <- create_datalist(data_total$`2014`, "Dilma PT", 0.5164 )
model_2014 <- stan(model_code = agg_model, data = dataset_2014, chains = 1, iter = 2500)
traceplot(model_2014)

dataset_2010 <- create_datalist(data_total$`2010`, "Dilma Roussef PT", 0.5605 )
model_2010 <- stan(model_code = agg_model, data = dataset_2010, chains = 1, iter = 2500)
traceplot(model_2010)

dataset_2006 <- create_datalist(data_total$`2006`, "Lula PT", 0.6083 )
model_2006 <- stan(model_code = agg_model, data = dataset_2006, chains = 1, iter = 2500)
traceplot(model_2006)



extract_summary <- function(model, first_day) {
  # Extract summaries from Stan simulated data
  tibble(
    median = apply(model$mu, 2,median),
    p10 = apply(model$mu, 2,function(x) quantile(x, .1)),
    p90 = apply(model$mu, 2,function(x) quantile(x, .90)),
    p05 = apply(model$mu, 2,function(x) quantile(x, .05)),
    p95 = apply(model$mu, 2,function(x) quantile(x, .95)),
    t = 1:dim(model$mu)[2],
    days = first_day + 1:dim(model$mu)[2]
  )
}


model_2014_data <- rstan::extract(model_2014)
model_2010_data <- rstan::extract(model_2010)
model_2006_data <- rstan::extract(model_2006)

# Merging data
model_2014_data_df <- model_2014_data %>%
  extract_summary(., lubridate::dmy("06/10/2014")) %>%
  inner_join(filter(data_total$`2014`, candidate == "Dilma PT"))

model_2010_data_df <- model_2010_data %>%
  extract_summary(., lubridate::dmy("04/10/2010")) %>%
  inner_join(filter(data_total$`2010`, candidate == "Dilma Roussef PT"))

model_2006_data_df <- model_2006_data %>%
  extract_summary(., lubridate::dmy("01/10/2006")) %>%
  inner_join(filter(data_total$`2006`, candidate == "Lula PT"))



ggplot(model_2014_data_df) +
  geom_line(aes(x = Data, y = median ),colour = "red") +
  geom_ribbon(aes(x = Data, ymin = p05,  ymax = p95),fill = "red", alpha = 0.2) +
  geom_point(aes(x = Data, y = percentage, shape = Instituto)) +
  theme_bw() + labs(y = "percentage") 

ggplot(model_2010_data_df) +
  geom_line(aes(x = Data, y = median ),colour = "red") +
  geom_ribbon(aes(x = Data, ymin = p05,  ymax = p95),fill = "red", alpha = 0.2) +
  geom_point(aes(x = Data, y = percentage, shape = Instituto)) +
  theme_bw() + labs(y = "percentage") 

ggplot(model_2006_data_df) +
  geom_line(aes(x = Data, y = median ),colour = "red") +
  geom_ribbon(aes(x = Data, ymin = p05,  ymax = p95),fill = "red", alpha = 0.2) +
  geom_point(aes(x = Data, y = percentage, shape = Instituto)) +
  theme_bw() + labs(y = "percentage") 


# Analyzing house effects

extract_house_effects <- function(model) {
  tibble(
    median = apply(model$gamma, 2,median),
    p10 = apply(model$gamma, 2,function(x) quantile(x, .1)),
    p90 = apply(model$gamma, 2,function(x) quantile(x, .90)),
    p05 = apply(model$gamma, 2,function(x) quantile(x, .05)),
    p95 = apply(model$gamma, 2,function(x) quantile(x, .95)),
    id_instituto = 1:dim(model$gamma)[2]
  )
  
}

gamma_2014 <- rstan::extract(model_2014) %>% 
  extract_house_effects %>%
  inner_join( data_total$`2014` %>%
                distinct(id_instituto, Instituto)
    ) %>%
  mutate(candidate = "Dilma PT") 


# Convergence for gamma
traceplot(model_2014, "gamma")


# House effects (Companies)
gamma_2014 %>%
  ggplot(aes(x = Instituto, y = median)) +
  geom_pointrange(aes(ymin = p05, ymax = p95)) +
  theme_bw() +
  labs(y = "House Effect") +
  coord_flip()
  
  
gamma_2010 <- rstan::extract(model_2010) %>% 
  extract_house_effects %>%
  inner_join( data_total$`2014` %>%
                distinct(id_instituto, Instituto)
  ) %>%
  mutate(candidate = "Dilma Roussef PT") 


# Convergence for gamma
traceplot(model_2010, "gamma")


# House effects (Companies)
gamma_2010 %>%
  ggplot(aes(x = Instituto, y = median)) +
  geom_pointrange(aes(ymin = p05, ymax = p95)) +
  theme_bw() +
  labs(y = "House Effect") +
  coord_flip()



gamma_2006 <- rstan::extract(model_2006) %>% 
  extract_house_effects %>%
  inner_join( data_total$`2014` %>%
                distinct(id_instituto, Instituto)
  ) %>%
  mutate(candidate = "Lula PT") 


# Convergence for gamma
traceplot(model_2006, "gamma")


# House effects (Companies)
gamma_2006 %>%
  ggplot(aes(x = Instituto, y = median)) +
  geom_pointrange(aes(ymin = p05, ymax = p95)) +
  theme_bw() +
  labs(y = "House Effect") +
  coord_flip()












