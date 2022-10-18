########################
# Instalação de pacotes
pacotes <- c('titanic',    # carrega a base original titanic_treino 
             'tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'readxl',
             'caret'       # Funções úteis para machine learning
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################
#Carreganbdo a base de dados:
Global_Superstore2 <- read_excel("~/Documents/MBA USP/TCC/01-1_Base de dados/Global_Superstore2.xlsx")
View(Global_Superstore2)

#Nova Base de Dados

basevendas <- Global_Superstore2

#Resumo
dim(basevendas)

summary(basevendas)

basevendas$AltoValor <- with(basevendas, as.factor(HighValue == "1"))

colnames(basevendas)[colnames(basevendas) == "Sub-Category"] ="TipoProduto"

basevendas$Quantity <- as.numeric(x)

basevendas2 = basevendas %>%
  mutate(`Order Date` = as.Date(`Order Date`, "%d-%m-%y"))

colnames(basevendas2)[colnames(basevendas2) == "Order Date"] ="Data"

factor_vars <- c('Segment','Country','TipoProduto')
basevendas2[factor_vars] <- lapply(basevendas2[factor_vars], function(x) as.factor(x))

basevendas2$Quantity <- as.integer(x)


#Arvore
arvore <- rpart::rpart(AltoValor ~ Segment + Quantity + Sales,
                data=basevendas2,
                parms = list(split = 'Gini'),
                method = 'class',
)


arvore$variable.importance

summary(arvore)

printcp(arvore)

#Plotando a Arvore
rpart.plot::rpart.plot(arvore)


# Vamos separar a base em treinamento e teste #
train_cleanned <- basevendas2[1:25000,]
test_cleanned <- basevendas2[5001:51290,]

#Arvore Treino
arvore <- rpart::rpart(AltoValor ~ Segment + Quantity + Sales,
                       data=train_cleanned,
                       parms = list(split = 'Gini'),
                       method = 'class',
)

# Verificando a complexidade da árvore
arvore$frame

############################################
# Avaliar a árvore na base de treino
p_treino = stats::predict(arvore, train_cleanned)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "TRUE", "FALSE"))
p_teste = stats::predict(arvore, test_cleanned)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "TRUE", "FALSE"))

tab <- table(c_treino, train_cleanned$AltoValor)
acc <- (tab[1,1]+tab[2,2])/nrow(train_cleanned)
sprintf('Acurácia na base de treino: %s ', percent(acc))

tab <- table(c_teste, test_cleanned$AltoValor)
acc <- (tab[1,1]+tab[2,2])/nrow(test_cleanned)
sprintf('Acurácia na base de teste: %s ', percent(acc))


###############################
# Curva ROC                   #
###############################

aval_treino <- data.frame(obs=train_cleanned$AltoValor, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Avaliar a árvore na base de teste

aval_teste <- data.frame(obs=test_cleanned$AltoValor, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

##########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore)
tab_cp

plotcp(arvore)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda <- rpart::rpart(AltoValor ~ Segment + Quantity + Sales,
                            data=basevendas2,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)

p_treino = stats::predict(arvore_poda, train_cleanned)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "TRUE", "FALSE"))
p_teste = stats::predict(arvore_poda, test_cleanned)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "TRUE", "FALSE"))

#####
aval_treino <- data.frame(obs=train_cleanned$AltoValor, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Avaliar a árvore na base de teste

aval_teste <- data.frame(obs=test_cleanned$AltoValor, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC



