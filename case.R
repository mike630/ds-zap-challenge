# Selecionando diretório
setwd('C:\\Projetos\\BUSINESS CASES\\ZAP - Cientista de Dados')

#--------------------------------------------------------------------------
# Instalando pacotes
# install.packages("jsonlite") # Instalar pacotes caso não houver
# install.packages('knitr')
# install.packages('ggplot2')
# install.packages('gridExtra')
# install.packages('psych')
# install.packages('caret')
# install.packages('mice')
# install.packages('xgboost')
# install.packages('corrplot')
#--------------------------------------------------------------------------
# Carregando pacotes requeridos
library(jsonlite)
library(knitr)
library(ggplot2)
library(gridExtra)
library(psych)
library(caret)
library(mice)
library(xgboost)
library(corrplot)
#--------------------------------------------------------------------------
# Carregando os datasets train e test nas variáveis

# Train
connection_reviews <- file("source-4-ds-train.json")
train <- stream_in(connection_reviews)

# Test
connection_reviews <- file("source-4-ds-test.json")
test <- stream_in(connection_reviews)

# Visualizando dataset e as classes
class(train)
View(train)

class(test)
View(test)

# Convertendo espaços vazios em NA's das variáveis que julgo serem importantes do dataset train
train$usableAreas <- ifelse(train$usableAreas == '', NA, train$usableAreas)

train$unitTypes <- ifelse(train$unitTypes =='', NA, train$unitTypes)

train$parkingSpaces <- ifelse(train$parkingSpaces == '', NA, train$parkingSpaces)

train$street <- ifelse(train$address$street == '', NA, train$address$street)

train$neighborhood <- ifelse(train$address$neighborhood == '', NA, train$address$neighborhood)

train$suites <- ifelse(train$suites == '', NA, train$suites)

train$bathrooms <- ifelse(train$bathrooms == '', NA, train$bathrooms)

train$totalAreas <- ifelse(train$totalAreas == '', NA, train$bathrooms)

train$bedrooms <- ifelse(train$bedrooms == '', NA, train$bedrooms)

train$price <- ifelse(train$pricingInfos$price == '', NA, train$pricingInfos$price)

train$locationId <- ifelse(train$address$locationId == '', NA, train$address$locationId)

train$businessType <- ifelse(train$pricingInfos$businessType == '', NA, train$pricingInfos$businessType)

# Convertendo a classe de cada variável categórica que são importantes para a análise preditiva.
train$unitTypes <- as.character(train$unitTypes)
train$unitTypes <- as.factor(train$unitTypes)

train$businessType <- as.character(train$businessType)
train$businessType <- as.factor(train$businessType)


# Irei utilizar o locationId ao invés de neighborhood por ele estar com os dados mais estruturados
# e há também informações valiosas com relação às zonas de cada imóvel na capital 
# paulista e que não há estas informações na variável district.

train$locationId <- as.character(train$locationId)

train$zona <- sapply(train$locationId, function(x) {strsplit(x, split= ">")[[1]][5]})
train$zona <- as.factor(train$zona)

train$bairro <- sapply(train$locationId, function(x) {strsplit(x, split= ">")[[1]][6]})
train$bairro <- as.factor(train$bairro)

#-------------------------------------------------------------------------------------------

# Farei o mesmo processo acima para o dataset test

# Convertendo espaços vazios em NA's no dataset test
test$usableAreas <- ifelse(test$usableAreas == '', NA, test$usableAreas)

test$unitTypes <- ifelse(test$unitTypes =='', NA, test$unitTypes)

test$parkingSpaces <- ifelse(test$parkingSpaces == '', NA, test$parkingSpaces)

test$street <- ifelse(test$address$street == '', NA, test$address$street)

test$neighborhood <- ifelse(test$address$neighborhood == '', NA, test$address$neighborhood)

test$suites <- ifelse(test$suites == '', NA, test$suites)

test$bathrooms <- ifelse(test$bathrooms == '', NA, test$bathrooms)

test$totalAreas <- ifelse(test$totalAreas == '', NA, test$bathrooms)

test$bedrooms <- ifelse(test$bedrooms == '', NA, test$bedrooms)

test$price <- ifelse(test$pricingInfos$price == '', NA, test$pricingInfos$price)

test$locationId <- ifelse(test$address$locationId == '', NA, test$address$locationId)

test$businessType <- ifelse(test$pricingInfos$businessType == '', NA, test$pricingInfos$businessType)

# Convertendo a classe de cada variável categórica são importantes para
# a análise preditiva.
test$unitTypes <- as.character(test$unitTypes)
test$unitTypes <- as.factor(test$unitTypes)

test$businessType <- as.character(test$businessType)
test$businessType <- as.factor(test$businessType)

# Irei utilizar o locationId ao invés de neighborhood por ele estar com os dados mais estruturados
# e há também informações valiosas com relação às zonas de cada imóvel na capital 
# paulista e que não há estas informações na variável district.

test$locationId <- as.character(test$locationId)

test$zona <- sapply(test$locationId, function(x) {strsplit(x, split= ">")[[1]][5]})
test$zona <- as.factor(test$zona)

test$bairro <- sapply(test$locationId, function(x) {strsplit(x, split= ">")[[1]][6]})
test$bairro <- as.factor(test$bairro)
# Verificando os missing values do dataset test
kable(sapply(test, function(x) sum(is.na(x))))

# Irei retirar todos os registros que há RENTAL do businessType do train, pois só há categoria SALE
# no dataset test

summary(train$businessType)

summary(test$businessType)

train2 <- train[train$businessType == 'SALE', ]

# Farei o mesmo para todos os registros da variável unitTypes do train, pois só há uma categoria
# no dataset test - APARTMENT.

summary(train$unitTypes)

summary(test$unitTypes)

train2 <- train2[train2$unitTypes == 'APARTMENT', ]

# Verificando os missing values do dataset train
kable(sapply(train2, function(x) sum(is.na(x))))

# Não irei imputar dados nos NA's do dataset, uma vez que há muitas
# observações neste conjunto de dados e isto não prejudicará na modelagem preditiva.

train2 <-  train2[!is.na(train2$usableAreas) & !is.na(train2$parkingSpaces) &
                  !is.na(train2$bathrooms) & !is.na(train2$bedrooms) &
                    !is.na(train2$suites) & !is.na(train2$zona) & !is.na(train2$bairro), ]

#  Visualizando outliers
g1 <- ggplot(train2, aes(x = '', y = usableAreas)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

g2 <- ggplot(train2, aes(x = '', y = parkingSpaces)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

g3 <- ggplot(train2, aes(x = '', y = bathrooms)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

g4 <- ggplot(train2, aes(x = '', y = totalAreas)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

g5 <- ggplot(train2, aes(x = '', y = bedrooms)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

g6 <- ggplot(train2, aes(x = '', y = suites)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

g7 <- ggplot(train2, aes(x = '', y = price)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

grid.arrange(g1,g2,g3,g4,g5,g6,g7, nrow=3)

# Sumarizando cada variável numérica que julgo serem importantes e analisando o 
# terceiro quartil para que eu possa ajustar valor máximo no dataset para que assim se 
# reduza os ruídos com outliers.
summary(train2$usableAreas)
summary(train2$parkingSpaces)
summary(train2$bathrooms)
summary(train2$totalAreas)
summary(train2$bedrooms)
summary(train2$suites)
summary(train2$price)

# Nova variável sem outliers de valores altos
train3 <- train2[train2$usableAreas < 250 & train2$parkingSpaces <= 5 &
                 train2$bathrooms <= 10 & train2$bedrooms <= 10 &
                   train2$suites <= 5 & train2$price <= 1400000,]

# Visualizando novo conjunto de dados, com poucos outliers.
p1 <- ggplot(train3, aes(x = '', y = usableAreas)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

p2 <- ggplot(train3, aes(x = '', y = parkingSpaces)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

p3 <- ggplot(train3, aes(x = '', y = bathrooms)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

p4 <- ggplot(train3, aes(x = '', y = totalAreas)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

p5 <- ggplot(train3, aes(x = '', y = bedrooms)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

p6 <- ggplot(train3, aes(x = '', y = suites)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

p7 <- ggplot(train3, aes(x = '', y = price)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=F)

grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow=3)

#----------------------------------------------------------------------------------------
# Correlação das variáveis númericas

correlacao <- cor(train3[,c('usableAreas','parkingSpaces','bathrooms','totalAreas',
                    'bedrooms','suites', 'price')], use = 'na.or.complete')

corrplot.mixed(correlacao, tl.col="black", order = 'hclust', tl.pos = 'lt', diag = 'n')

plot(train3$totalArea, train3$bathrooms)
# De forma lógica, totalArea é uma variável que corresponde à usableAreas, por conta
# disso e também por conter bastante NA's e correlação alta com bathrooms, eu irei excluir 
# esta variável da nossa análise.
train3$totalAreas <- NULL

kable(sapply(train3, function(x) sum(is.na(x))))

#----------------------------------------------------------------------------------------
# Normalizando a variável-alvo


# Gráfico de Frequência e Distribuição Normal

# Histograma da variável-alvo Price
ggplot(train3, aes(x=price)) +
  geom_histogram(fill='purple') + theme_grey() +
  scale_x_continuous(breaks= seq(0, 200, by=20))

qqnorm(train3$price)
qqline(train3$price)

skew(train3$price)

#  Uma distribuição normal tem uma skewness igual à 0. Perceba que os skew de 
# price é de 0.966, isto quer dizer que, a variável não está bem distribuída.

# Irei utilizar log da variável para normalizá-la.
train3$price <- log(train3$price+1)
skew(log(train3$price+1))
qqnorm(log(train3$price+1))
qqline(log(train3$price+1))

############################################################################################################\\\
# Tratando o dataset Test

test2 <-  test[,c('id','usableAreas', 'parkingSpaces','bathrooms','bedrooms','zona','bairro')]

# Verificando os Missing vAlues no dataset Test
md.pattern(test2, rotate.names = T)
kable(sapply(test2, function(x) sum(is.na(x))))

#Imputando dados para as variáveis categóricas
levels(test2$bairro) <- c(levels(test2$bairro), 'Não Localizado')
test2$bairro[is.na(test2$bairro)] <- 'Não Localizado'

levels(test2$zona) <- c(levels(test2$zona), 'Não Localizado')
test2$zona[is.na(test2$zona)] <- 'Não Localizado'

# Imputando dados para as variáveis numéricas
kable(sapply(test2, function(x) sum(is.na(x))))

imp <- mice(test2[,c('usableAreas', 'parkingSpaces','bathrooms')], m=5) #bedroom não tem NA

modelFit1 <- with(imp,lm(usableAreas~ bathrooms+parkingSpaces))
summary(pool(modelFit1))

data_imp <- complete(imp)

data_imp$id <- test2$id
data_imp$bedrooms <- test2$bedrooms
data_imp$zona <- test2$zona
data_imp$bairro <- test2$bairro
data_imp$price <- NA


# Combinando o dataset Test e Train
all <- rbind(train3[,c('id','usableAreas', 'parkingSpaces','bathrooms','bedrooms','zona','bairro', 'price')], data_imp)

# Verificando as variáveis numéricas
numericVars <- which(sapply(all, is.numeric))

numericVars <- numericVars[-which(names(numericVars)=='price')] # retirando a variável-alvo

# Normalizando as distribuições das variáveis numéricas
skewed <- as.data.frame(ifelse(skew(all[,numericVars]) > 0.7 | skew(all[,numericVars]) < -0.7,
                                log(all[,numericVars]+1),all[,numericVars]))

colnames(skewed) <- names(numericVars)
dim(skewed)

# Pré processando as variáveis numéricas para normalizar distribuição
kable(sapply(skewed, function(x) sum(is.na(x))))
preNumVars <- preProcess(skewed, method = c('center','scale'),na.remove = T)
preNumVars
all_NumVars <- predict(preNumVars,skewed)
dim(all_NumVars)

# Transformando variáveis categóricas em dummies para facilitar nosso modelo a prever
# de forma mais eficiente.


categoricalVars <- which(sapply(all,is.factor))

dummy <- as.data.frame(model.matrix(~.-1,all[,categoricalVars]))

dummy <- dummy[,-c(1,2,3,4,5,6,7)] 
# Removendo as categorias Zona, pois não havia p-value significativo diante dos outros
# modelos preditivos que eu testei.

# Removendo levels com pouca observação no dataset train

ZerocolTrain <- which(colSums(dummy)<10) # removendo menos que 10
colnames(dummy[ZerocolTrain])
dummy <- dummy[,-ZerocolTrain] #removendo
dim(dummy)

# Combinando as variáveis numéricas com variáveis dummy
allClean <- cbind(all$id,all_NumVars,dummy)
rownames(allClean) <- allClean$`all$id`
allClean$`all$id` <- NULL
#-----------------------------------------------------------------------------------

# Executando nosso modelo

#Cross Validation
# my_control <-trainControl(method="cv", number=10)

# Lasso Modelo
# lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.0001,0.1,by = 0.0005))
# alpha = 1 (lasso), alpha = 0 (ridge) and a value between 0 and 1 (say 0.3) is elastic net regression.
# set.seed(5000000)
# lasso_mod <- train(x=allClean[1:dim(train3)[1],], y=train3$price, method='glmnet', 
                   # trControl= my_control,tuneGrid=lassoGrid) 
# lasso_mod
# lasso_mod$bestTune
# min(lasso_mod$results$RMSE)
# min(lasso_mod$results$MAE)
# max(lasso_mod$results$Rsquared)

# Ridge Modelo
#ridgeGrid <- expand.grid(alpha = 0, lambda = seq(0.0001,0.1,by = 0.0005)) 
# alpha = 1 (lasso), alpha = 0 (ridge) and a value between 0 and 1 (say 0.3) is elastic net regression.
# set.seed(5000000)
# ridge_mod <- train(x=allClean[1:dim(train3)[1],], y=train3$price, method='glmnet', 
                   # trControl= my_control,tuneGrid=ridgeGrid) 
 
# ridge_mod
# ridge_mod$bestTune
# min(ridge_mod$results$RMSE)
# min(ridge_mod$results$MAE)
# max(ridge_mod$results$Rsquared)


# Elastic Net Modelo
# set.seed(5000000)

# elasticnet_mod <- train(x=allClean[1:dim(train3)[1],], y=train3$price, method='glmnet',
                          # trControl= my_control, tuneLength = 10) 

# Diante das análises dos modelos do método glmnet acima, podemos verificar que Elastic Net cujo qual tem um um alpha 
# entre 0 e 1, é o modelo mais eficiente entre os três.

# Salvando modelo e carregando-o.
# saveRDS(elasticnet_mod, file = 'elasticnet_mod.rda')
elasticnet_mod <- readRDS('elasticnet_mod.rda') #BAIXAR ARQUIVO .RDA DO MODELO NO GITHUB

elasticnet_mod
elasticnet_mod$bestTune
min(elasticnet_mod$results$RMSE)
min(elasticnet_mod$results$MAE)
max(elasticnet_mod$results$Rsquared)

# Prevendo as vendas
elasticnet_prev = predict(elasticnet_mod, allClean[(dim(train3)[1]+1):dim(allClean)[1],])

# eXtreme Gradient Boosting - XGBoost
# xgb_params <- list(
  #booster = 'gbtree',
  #objective = 'reg:linear',
  #colsample_bytree=1,
  #eta=0.55,
  #max_depth=4,
  #min_child_weight=3,
  #alpha=0.3,
  #lambda=0.25,
  #gamma=0, # less overfit
  #subsample=0.7)

dtrain <- xgb.DMatrix(as.matrix(allClean[1:dim(train3)[1],]), label = train3$price)
dtest <- xgb.DMatrix(as.matrix(allClean[(dim(train3)[1]+1):dim(allClean)[1],]))

# set.seed(5000000)
# xgboost_mod <- xgb.cv(xgb_params, dtrain, nrounds = 750, metrics = 'rmse', 
                      #print_every_n = 50,nfold = 5,early_stopping_rounds = 200)

# xgboost_mod$best_iteration
# xgboost_mod$best_ntreelimit
# xgboost_mod$evaluation_log$train_rmse_mean[xgboost_mod$best_iteration]
# xgboost_mod$evaluation_log$test_rmse_mean[xgboost_mod$best_iteration]

# set.seed(5000000)
# xgboost_mod2 <- xgb.train(data = dtrain, params=xgb_params, nrounds = 721)

# Salvando modelo e carregando-o.
# saveRDS(xgboost_mod2, file = 'xgboost_mod2.rda')
xgboost_mod2 <- readRDS('xgboost_mod2.rda') #BAIXAR ARQUIVO .RDA DO MODELO NO GITHUB

 # Prevendo as vendas
xgboost_prev = predict(xgboost_mod2, dtest)

# Gradiente Boost Model - GBM
# gbmGrid <- expand.grid(n.trees = 150, 
                      # interaction.depth = c(2), 
                      # shrinkage = c(0.9), 
                      # n.minobsinnode = c(10))

# nrow(gbmGrid)

# set.seed(5000000)
# gbm_mod <- train(x=allClean[1:dim(train3)[1],], y=train3$price, method = "gbm", 
                # metric = "RMSE", trControl = my_control, 
                # tuneGrid =  gbmGrid)

# Salvando modelo e carregando-o.
# saveRDS(gbm_mod, file = 'gbm_mod.rda')
gbm_mod <- readRDS('gbm_mod.rda') #BAIXAR ARQUIVO .RDA DO MODELO NO GITHUB

gbm_mod$bestTune
gbm_mod

# Prevendo as vendas
gbm_prev = predict(gbm_mod, allClean[(dim(train3)[1]+1):dim(allClean)[1],])

 # Correlação das previsões nos diferentes modelos.
relacao <- cbind.data.frame(xgboost_prev,elasticnet_prev,gbm_prev)

kable(cor(relacao))

# Como o modelo XGBoost teve um RMSE melhor do que o ElasticNet e GBM, eu irei fazer
# média ponderada dos resultados do XGBoost e GBM, já que o ElasticNet não obteve
# um RMSE eficiente.

previsao <- (((2*xgboost_prev)+gbm_prev)/3)

previsao <- round((exp(previsao)-1),2) # precisamos reverter o log para o valor real

previsao <- as.data.frame(previsao)
previsao$id <- rownames(allClean[(dim(train3)[1]+1):dim(allClean)[1],])
colnames(previsao) <- c('price','id')

write.csv(previsao,'final.csv',row.names = F)

#####################################################################################

# QUESTIONÁRIO

#A entrega obrigatória deve consistir de:
  
# Respostas (com insumos para suporte) para as seguintes questões:


# 1)Você utilizaria a métrica escolhida para seleção de modelo também para comunicar os resultados 
#para usuários e stakeholders internos? Em caso negativo, qual outra métrica você utilizaria nesse 
#caso?

# RESPOSTA: SIM, UTILIZARIA ALÉM DESSA MÉTRICA ALGUMA QUE DEMONSTRE O QUÃO VALORIZADO TAL REGIÃO É.

# 2)Em quais bairros ou em quais faixas de preço o seu modelo performa melhor?

varImp(elasticnet_mod)
# RESPOSTA: DE ACORDO COM A LINHA DE CÓDIGO ACIMA, PODEMOS NOTAR QUE OS BAIRROS JARDIM PAULISTANO,
# JARDIM AMERICA E VILA NOVA CONCEICAO SÃO OS BAIRROS MAIS IMPORTANTES DO MODELO.

mat <- xgb.importance (feature_names = colnames(allClean[1:dim(train3)[1],]),model = xgboost_mod2)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)



# 3)Se você tivesse que estimar o valor dos imóveis com apenas 3 campos, quais seriam eles?

# RESPOSTA: BAIRRO, USABLEAREAS E BATHROOMS

# 4)Como você vislumbra colocar a sua solução em produção?

# RESPOSTA: PODERIA GERAR INSIGHTS PARA FRONT-END DO ZAP, AO COLOCAR ALGUM TIPO DE FUNCIONALIDADE NO SITE
# PARA CRIAR VANTAGEM COMPETITIVA FRENTE AOS CONCORRENTES QUE DEMONSTRASSE O VALOR ESTIMADO 
# PARA TAL IMÓVEL COM BASE NA LOCALIZAÇÃO E SUAS CARACTERÍSTICAS.
  