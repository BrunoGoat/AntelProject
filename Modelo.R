# =====================
# 1. Cargar librerías
# =====================
library(readr)
library(dplyr)
library(caret)
library(randomForest)

# =====================
# 2. Cargar el archivo CSV
# =====================
df <- read.csv("Rdata/session_df.csv", stringsAsFactors = TRUE)
df <- sessions_df
# Opción 2: con dplyr::select()
df <- df %>% select(-firebase_event_origin)
# =====================
# 3. Verificar variable objetivo
# =====================
df$play <- as.factor(df$play)

# =====================
# 4. Eliminar columnas no predictivas
# =====================
df_model <- df %>%
  select(-id, -date)

# =====================
# 5. Imputación de valores faltantes
# =====================
df_model <- df_model %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.factor), ~ {
    moda <- names(sort(table(.), decreasing = TRUE))[1]
    factor(ifelse(is.na(.), moda, as.character(.)), levels = levels(.))
  }))

# =====================
# 6. División en conjunto de entrenamiento y test
# =====================
set.seed(123)
train_idx <- createDataPartition(df_model$play, p = 0.7, list = FALSE)
train_data <- df_model[train_idx, ]
test_data  <- df_model[-train_idx, ]

# =====================
# 7. Entrenamiento del modelo Random Forest
# =====================
#modelo_rf <- randomForest(play ~ ., data = train_data, ntree = 500, importance = TRUE)
#install.packages("ranger")
library(ranger)

modelo_rf <- ranger(play ~ ., data = train_data, num.trees = 500, importance = "impurity")

# =====================
# 8. Predicción sobre el conjunto de test
# =====================
#pred_rf <- predict(modelo_rf, newdata = test_data)

pred_rf <- predict(modelo_rf, data = test_data)$predictions

# =====================
# 9. Evaluación del modelo
# =====================
#conf_mat <- confusionMatrix(pred_rf, test_data$play)
#print(conf_mat)

conf_mat <- confusionMatrix(as.factor(pred_rf), test_data$play)
print(conf_mat)

# =====================
# 10. Comparación: predicho vs real
# =====================
comparacion <- data.frame(
  predicho = pred_rf,
  real = test_data$play
)


# Ver primeros 20
head(comparacion, 20)

# Agregar columna de acierto
comparacion <- comparacion %>%
  mutate(acierto = predicho == real)

# Ver conteo de aciertos y errores
table(comparacion$acierto)

# Ver porcentaje de aciertos (accuracy manual)
mean(comparacion$acierto)

# =====================
# 11. Importancia de variables
# =====================

# Gráfico
varImpPlot(modelo_rf)

# Numericamente
importance(modelo_rf)

# Como data frame
importance_df <- as.data.frame(importance(modelo_rf))
importance_df <- importance_df[order(-importance_df$MeanDecreaseAccuracy), ]
head(importance_df, 10)  # Las 10 más importantes


#---------------------------------------------------------------------------
colnames(train_data)


# Numero eventos cambiarlo ya porque esta ponderando horrible el modelo



# Testeos 

library(caret)
confusionMatrix(pred_rf, test_data$play, positive = "TRUE")

# =====================
# 4. Importancia de variables
# =====================

# Importancia como data frame
importance_df <- as.data.frame(modelo_rf$variable.importance)
colnames(importance_df) <- "importance"
importance_df$variable <- rownames(importance_df)

# Ordenar por importancia
importance_df <- importance_df %>%
  arrange(desc(importance))

# Ver top 10
head(importance_df, 10)

# Gráfico
ggplot(importance_df[1:15, ], aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "#0073C2FF") +
  coord_flip() +
  labs(title = "Importancia de Variables (ranger)",
       x = "Variable", y = "Importancia")


