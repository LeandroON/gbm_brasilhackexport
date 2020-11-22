library(caret)
library(MASS)
library(tidyverse)
library(data.table)
library(mvoutlier)
library(glmnet)
library(lubridate)
library(ranger)
library(xgboost)
library(zoo)

# OBSERVAÇÃO IMPORTANTE: esse arquivo contém apenas o script para gerar
# modelos tentativos para prever tempo de estadia dos navios - necessário
# rodar primeiro tratamento_dados.R

# baseline- regressão linear

# limpeza de outliers e NAs (grande impacto na estimação de coeficientes)
df_modelo = granel_final %>% 
  ungroup() %>% 
  filter(!is.na(tempo_ocioso)) %>% 
  left_join(tabela_prancha_last5) %>% 
  mutate(prancha_media_last_30d = ifelse(is.na(prancha_media_last_30d),
                                         0,
                                         prancha_media_last_30d),
         prancha_media_porto_last_5d = ifelse(is.na(prancha_media_porto_last_5d),
                                              0,
                                              prancha_media_porto_last_5d)) %>% 
  filter(!is.na(prancha_media_last5))

limp = sign2(df_modelo %>% select(tempo_estadia, peso_bruto))
indice_out = which(limp$wfinal01==1)

df_modelo_reg = df_modelo[indice_out,] %>% 
  select(-`Número do DUV`) %>% 
  model.matrix(object = tempo_estadia ~ . + 0)

set.seed(5451)

indice = createDataPartition(df_modelo$tempo_estadia[indice_out], p = .8, list = F)

treino = cbind(tempo_estadia = df_modelo$tempo_estadia[indice_out][indice],
               df_modelo_reg[indice,]) %>% as.data.frame()
teste = cbind(tempo_estadia = df_modelo$tempo_estadia[indice_out][-indice],
              df_modelo_reg[-indice,]) %>% as.data.frame()

modelo_1 = lm(log(tempo_estadia) ~ log(peso_bruto) + log(prancha_media_last5) +
                ., 
              data = treino %>% select(tempo_estadia, peso_bruto,
                                       prancha_media_last5,
                                       starts_with("NCM_4d_princ"),
                                       starts_with("mes_atracacao"),
                                       starts_with("porto_origem"),
                                       is_reatracado))

summary(modelo_1)
plot(modelo_1)
# plot para identificar fit
y_hat = predict(modelo_1, treino)
y = treino$tempo_estadia

plot(y_hat, y, type = "p", log = "y", main = "Modelo 1")
lines(x = c(3.5, 4, 4.5, 5, 5.5), y = exp(c(3.5, 4, 4.5, 5, 5.5)), col = "blue", lty = 2)

y_hat = predict(modelo_1, teste) %>% exp()
y = teste$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "Modelo 2")
lines(x = seq(40, 500, by = 10), y = seq(40, 500, by = 10), 
      col = "blue", lty = 2)

# tentar selecionar variáveis com lasso (glmnet)

x = model.matrix(~ . + 0,
                 df_modelo %>% 
                   select(-`Número do DUV`) %>% 
                   mutate(peso_bruto = log(peso_bruto),
                          prancha_media_last5 = log(prancha_media_last5+1),
                          prancha_media_last_30d = log(prancha_media_last_30d+1),
                          prancha_media_porto_last_5d = log(prancha_media_porto_last_5d+1)) %>% 
                   select(-tempo_estadia,
                          -area_porto, -berco,
                          -`Porto de estadia atual`, -`Atracação Prevista`,
                          -`Atracação Efetiva`, -`Desatracação Prevista`, 
                          -`Desatracação Efetiva`))

x_treino = x[indice,]
x_teste = x[-indice,]

y = df_modelo %>% 
  select(tempo_estadia) %>% 
  mutate(tempo_estadia = log(tempo_estadia))

y_treino = y[indice,]
y_teste = y[-indice,]

teste_lasso = cv.glmnet(x_treino,
                        as.matrix(y_treino),
                        type.measure = "mae", nfolds = 10, 
                        relax = T,
                        trace.it = 1)

# retreinar com variáveis selecionadas
treino = as.data.frame(cbind(x_treino, y_treino)) %>% 
  set_names(., gsub(pattern = "`", replacement = "", names(.)))
teste = as.data.frame(cbind(x_teste, y_teste)) %>% 
  set_names(., gsub(pattern = "`", replacement = "", names(.)))

modelo_2 = lm(tempo_estadia ~ log(peso_bruto) + is_reatracado + 
                NCM_4d_princ1701 + `tipo_trafegoPassagem - PAS` +
                pais_origem_mercArgentina +
                log(prancha_media_last5), data = treino)

summary(modelo_2)
plot(modelo_2)


# plot para identificar fit
y_hat = predict(modelo_2, treino)
y = treino$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "y", main = "Modelo 1")
lines(x = c(3, 3.5, 4, 4.5, 5, 5.5), y = c(3, 3.5, 4, 4.5, 5, 5.5), col = "blue", lty = 2)

y_hat = predict(modelo_2, teste)
y = teste$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "Modelo 2")
lines(x = seq(3, 5, by = 0.5), y = seq(3, 5, by = 0.5), 
      col = "blue", lty = 2)

# plot para identificar fit
y_hat = predict(modelo_2, as.data.frame(x_treino) %>% 
                  setnames(., gsub("`", "", names(.))))
y = y_treino$tempo_estadia

plot(y_hat, y, type = "p", log = "y", main = "Modelo 2")
lines(x = c(3, 3.5, 4, 4.5, 5, 5.5), y = exp(c(3, 3.5, 4, 4.5, 5, 5.5)), 
      col = "blue", lty = 2)

# random forest

# recriar base de dados, com outliers e NA encoding

df_modelo = granel_final %>% 
  ungroup() %>% 
  filter(!is.na(tempo_ocioso)) %>% 
  left_join(tabela_prancha_last5) %>% 
  mutate(prancha_media_last_30d = ifelse(is.na(prancha_media_last_30d),
                                         -999999999,
                                         prancha_media_last_30d),
         prancha_media_porto_last_5d = ifelse(is.na(prancha_media_porto_last_5d),
                                              -999999999,
                                              prancha_media_porto_last_5d),
         prancha_media_last5 = ifelse(is.na(prancha_media_last5),
                                      -999999999,
                                      prancha_media_last5))

# excluir Número do DUV das bases de treino e teste

set.seed(5451)

indice = createDataPartition(df_modelo$tempo_estadia, p = .8, list = F)

treino = df_modelo[indice,-1]
teste = df_modelo[-indice,-1]

rf_1 = ranger(data = treino,
              dependent.variable.name = "tempo_estadia",
              importance = "impurity", seed = 154)

sort(importance(rf_1), decreasing = T)

# avaliação de desempenho

y_hat = predict(rf_1, treino)$predictions
y = treino$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "Random Forest")
lines(x = seq(40, 500, by = 10), y = seq(40, 500, by = 10), 
      col = "blue", lty = 2)

y_hat = predict(rf_1, teste)$predictions
y = teste$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "Modelo 2")
lines(x = seq(40, 500, by = 10), y = seq(40, 500, by = 10), 
      col = "blue", lty = 2)

# tuning de hiperparâmetros

control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        search = "random")
metric <- "RMSE"
mtry <- sqrt(ncol(treino))
set.seed(7)
rf_tune <- train(tempo_estadia ~ ., data=treino, method="ranger", metric=metric,
                    tuneLength=15, trControl=control)
print(rf_tune)

y_hat = predict(rf_1, treino)$predictions
y = treino$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "Random Forest")
lines(x = seq(40, 500, by = 10), y = seq(40, 500, by = 10), 
      col = "blue", lty = 2)

y_hat = predict(rf_1, teste)$predictions
y = teste$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "Modelo 2")
lines(x = seq(40, 500, by = 10), y = seq(40, 500, by = 10), 
      col = "blue", lty = 2)

# avaliar modelo na base de teste

# gbm!
metodo = trainControl(method = "none")

gbm_1 = train(tempo_estadia ~ ., data = treino, method = "gbm",
              metric = metric, trControl = metodo)

y_hat = predict(gbm_1, treino)
y = treino$tempo_estadia

RMSE(log(y_hat), log(y))

y_hat = predict(gbm_1, teste)
y = teste$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "Modelo 2")
lines(x = seq(40, 500, by = 10), y = seq(40, 500, by = 10),
      col = "blue", lty = 2)

# eliminar fatores com níveis não existentes na base de treino
y_hat = predict(gbm_1, teste %>%
                  filter(!area_porto %in% c("(PAUL) TERMINAL PAUL GUSA",
                                            "Área 5 - Área para contêineres" ) &
                           !berco %in% c("2105", "Arm 15", "Arm 25",
                                         "BERÇO II", "EPORT II", "TERMAG") &
                           !cabeco %in% c("100 A 109", "38", "47", "DAT5")))
y = teste %>%
  filter(!area_porto %in% c("(PAUL) TERMINAL PAUL GUSA",
                            "Área 5 - Área para contêineres" ) &
           !berco %in% c("2105", "Arm 15", "Arm 25",
                         "BERÇO II", "EPORT II", "TERMAG") &
           !cabeco %in% c("100 A 109", "38", "47", "DAT5")) %>%
  pull(tempo_estadia)
RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "XGBoost")
lines(x = seq(5, 1500, by = 10), y = seq(5, 1500, by = 10),
      col = "blue", lty = 2)

# xgboost
metodo = trainControl(method = "none")

tgrid <- expand.grid(nrounds = 100,
                     max_depth = 6,
                     gamma = 0,
                     min_child_weight = 1,
                     subsample=0.8,
                     colsample_bytree=0.7,
                     eta = 0.1)

df_modelo2 = df_modelo %>% 
  select(-`Número do DUV`,
         -area_porto, -`Porto de estadia atual`, -berco, 
         -`Atracação Efetiva`, -`Atracação Prevista`, 
         -`Desatracação Efetiva`, -`Desatracação Prevista`)

x = model.matrix(tempo_estadia ~ . + 0, df_modelo2)

x_treino = x[indice,] 
x_teste = x[-indice,]

y = df_modelo2$tempo_estadia

y_treino = y[indice]
y_teste = y[-indice]

treino = cbind(tempo_estadia = y_treino, x_treino) %>% as.data.frame() %>% 
  set_names(., gsub(pattern = "`", replacement = "", names(.)))
teste = cbind(tempo_estadia = y_teste, x_teste) %>% as.data.frame() %>% 
  set_names(., gsub(pattern = "`", replacement = "", names(.)))

xgb_1 = train(tempo_estadia ~ ., data = treino, method = "xgbTree", 
              metric = metric, tuneGrid = tgrid, objective = "reg:squarederror",
              trControl = metodo,
              verbose=TRUE)

y_hat = predict(xgb_1, treino)
y = treino$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "XGBoost")
lines(x = seq(5, 1500, by = 10), y = seq(5, 1500, by = 10), 
      col = "blue", lty = 2)

# verificando variáveis mais significativas
varImp(xgb_1)

y_hat = predict(xgb_1, teste)
y = teste$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "XGBoost")
lines(x = seq(5, 1500, by = 10), y = seq(5, 1500, by = 10), 
      col = "blue", lty = 2)

# retreinando só com variáveis mais significativas

xgb_2 = train(tempo_estadia ~ is_reatracado + peso_bruto +
                prancha_media_last5 + diff_atracacao +
                tempo_ocioso + prancha_media_last_30d +
                pais_porto_destinoBD + NCM_4d_princ1701 +
                secao_NCM_princ17 + prancha_media_porto_last_5d +
                pais_origem_mercArgentina + `tipo_trafegoLongo Curso Importação - LCI` +
                secao_NCM_princ12 + `tipo_trafegoPassagem - PAS` + 
                pais_origem_mercBrasil + perc_secao_NCM_princ + `Data Saída do Porto` +
                `porto_origemZAZZZ - OUTROS PORTOS NAO IDENTIFICADOS - AFRICA DO SUL` +
                mes_atracacao11 + NCM_4d_princ1005 + secao_NCM_princ23 , data = treino, method = "xgbTree", 
              metric = metric, tuneGrid = tgrid, objective = "reg:squarederror",
              trControl = metodo,
              verbose=TRUE)

y_hat = predict(xgb_2, treino)
y = treino[,"tempo_estadia"]

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "XGBoost")
lines(x = seq(5, 1500, by = 10), y = seq(5, 1500, by = 10), 
      col = "blue", lty = 2)

# base de teste

y_hat = predict(xgb_2, teste)
y = teste[,"tempo_estadia"]

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "XGBoost")
lines(x = seq(5, 1500, by = 10), y = seq(5, 1500, by = 10), 
      col = "blue", lty = 2)

# treinar modelo - bootstrapping e busca aleatória dos melhores parâmetros
metodo = trainControl(method = "boot",
                      number = 20,
                      search = "random")

xgb_3 = train(tempo_estadia ~ ., data = treino, method = "xgbTree", 
              metric = metric, tuneLength = 25, objective = "reg:squarederror",
              trControl = metodo,
              verbose=TRUE)

print(xgb_3)

y_hat = predict(xgb_3, treino)
y = treino$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "XGBoost")
lines(x = seq(5, 1500, by = 10), y = seq(5, 1500, by = 10), 
      col = "blue", lty = 2)

# base de teste

y_hat = predict(xgb_3, teste)
y = teste$tempo_estadia

RMSE(log(y_hat), log(y))

plot(y_hat, y, type = "p", log = "xy", main = "XGBoost")
lines(x = seq(5, 1500, by = 10), y = seq(5, 1500, by = 10), 
      col = "blue", lty = 2)

# Consideração final - XGBoost tem maior consistência, em especial nas
# caudas da distribuição, quando comparado aos outros métodos. Ainda é
# possível melhorar o feature engineering e tuning dos hiperparâmetros.