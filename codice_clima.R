# Caricamento dati -------------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(sm)
library(dplyr)
library(tictoc)
library(tidyr)

dati <- read.csv("~.../clima.csv", header=T, stringsAsFactors = TRUE, sep=",")

#View(dati)
dim(dati)
head(dati)
colnames(dati)

# amalisi esplorative -------------------------------------------------------------------------
# numero di soggetti == n di osservazioni 
length(unique(dati$ID_QUALTRICS)) == nrow(dati)
dim(dati)

dati$ID_QUALTRICS <- NULL

# NA
na_get_col    = function(data){
  na_vars = sapply(data, function(col) sum(is.na(col)))
  na_vars = sort(na_vars[na_vars > 0])
  na_vars = data.frame(
    variabile     = names(na_vars),
    freq_assoluta = as.numeric(na_vars),
    freq_relativa = round(as.numeric(na_vars)/nrow(data), 4)
  )
  na_vars
}
(na_tab = na_get_col(dati))

# modifica variabili
#creazione della nuova risposta:

risposte <- c("CLIM_POLSUPPORT_fueltax",        
              "CLIM_POLSUPPORT_publictransport",
              "CLIM_POLSUPPORT_sustenergy",     
              "CLIM_POLSUPPORT_protection",     
              "CLIM_POLSUPPORT_foodtax"  )

risposte <- dati[,risposte]
na_get_col(risposte)

risposte[is.na(risposte)] <- 4

y <- apply(risposte, 1, function(riga) mean(riga[riga != 4], na.rm = TRUE))
table(y, useNA = 'always')

dati <- cbind(dati, y)
range(dati$y)
dati$y <- (dati$y-1)/2

colnames(dati)
dati <- dati[,-c(117:121)] #rimuovo risposte
dim(dati)

colnames(dati)[c(1,2,4:21,23,25:28,30:33)] #var di questionario
dati <- dati[,-c(1,2,4:21,23,25:28,30:33)] #le rimuovo
na_get_col(dati)

# creazione della categoria dato mancante:

dati$DEM_POL_conservative[is.na(dati$DEM_POL_conservative)] <- 99
dati$DEM_POL_conservative <- as.factor(dati$DEM_POL_conservative)

dati$DEM_POL_right[is.na(dati$DEM_POL_right)] <- 99
dati$DEM_POL_right <- as.factor(dati$DEM_POL_right)

dati <- na.omit(dati)

dim(dati)

dati$TRUST_OPEN <- NULL
dati$BENEFIT_OPEN <- NULL

#controllo di codifica delle variabili: 

tipo_variabile <- sapply(dati %>% select(-y), class)
table(tipo_variabile)
(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))

#conversione variabili: 
table(dati$DEM_GENDER_male)
table(is.na(dati$DEM_INCOME))
table(is.na(dati$DEM_INCOME_USD))
range(dati$DEM_INCOME_USD_log)

x <- var_quantitative[c(1,3:17,19,20,101)]
x

for(v in x) dati[[v]] <- as.factor(dati[[v]])

dati$DEM_INCOME <- NULL
dati$DEM_INCOME_USD_log <- NULL

dim(dati)

# tipo variabile: 
tipo_variabile <- sapply(dati %>% select(-y), class)
table(tipo_variabile)
(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))

dim(dati)
length(table(dati$COUNTRY_NAME))
table(dati$COUNTRY_NAME)[order(table(dati$COUNTRY_NAME))]

#rimozione delle modalità senza osservazioni: 
dati <- dati %>% filter(COUNTRY_NAME != "Albania") %>% droplevels()
dati <- dati %>% filter(COUNTRY_NAME != "Brazil") %>% droplevels()
dati <- dati %>% filter(COUNTRY_NAME != "Finland") %>% droplevels()
dati <- dati %>% filter(COUNTRY_NAME != "Mexico") %>% droplevels()
dati <- dati %>% filter(COUNTRY_NAME != "Peru") %>% droplevels()

# controllo finale: 
tipo_variabile <- sapply(dati %>% select(-y), class)
table(tipo_variabile)
(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))


# Stima e verifica --------------------------------------------------------

dati$dataset <- "stima"
set.seed(26)
dati$dataset[sample(1:nrow(dati), nrow(dati)*0.3)] <- "verifica"
table(dati$dataset)

set.seed(25)

# Controllo che in verifica non ci siano modalità non presenti in stima:
for(col in var_qualitative){
  if(!(all(unique(dati[dati$dataset == "verifica", col]) %in%
           unique(dati[dati$dataset == "stima", col])))){
    cat(col,"-> in verifica sono presenti modalità non presenti in stima\n")
  }}

sapply(dati %>% select(where(is.factor)), levels)
sapply(dati %>% select(where(is.integer)), table)
sapply(dati %>% select(where(is.numeric)), summary)

#verifica della correlazione:
matrice_correlazione <- cor(
  dati %>%
    filter(dataset == "stima") %>%
    select(-all_of(c(var_qualitative, "y", "dataset"))))

matrice_correlazione[lower.tri(matrice_correlazione, diag = T)] <- NA
correlazioni <- data.frame(var1 = rep(colnames(matrice_correlazione), ncol(matrice_correlazione)),
                           var2 = rep(rownames(matrice_correlazione), each = nrow(matrice_correlazione)),
                           cor = as.vector(matrice_correlazione))

correlazioni <- na.omit(correlazioni)

correlazioni %>% filter(cor < -0.95 | cor > 0.95)
correlazioni %>% filter(cor < -0.70 | cor > 0.70)

# Standardizzo esplicative quantitative, matrici del modello e formule
for(v in var_quantitative)
  dati[[v]] <- scale(dati[[v]])

# Divisione in stima-convalida:
set.seed(25)
cb1 <- sample(1:NROW(dati[dati$dataset == "stima",]), (2/3)*NROW(dati[dati$dataset == "stima",]))
cb2 <- setdiff(1:NROW(dati[dati$dataset == "stima",]), cb1)

# Creo matrice del modello con tutto il dataset
xmat.model <- model.matrix(~ . - 1, data = dati %>% select(-c(y, dataset)))# tolgo ora intercetta

# Salvo nomi delle variabili:
varnames <- colnames(dati %>% select(-c(y, dataset)))
# Creo formula con tutte le variabili:
form.tuttevars = paste("y ~", paste(varnames,collapse = " + "), collapse = NULL)
# Salvo nomi delle variabili della matrice del modello:
varnames2 <- colnames(xmat.model)


# Analisi esplorativa insieme di stima ------------------------------------

YYY <- dati %>% filter(dataset == "stima") %>% pull(y) 
summary(YYY)

boxplot(YYY, xlab = "", main = "y")
hist(YYY, nclass = 10, main = 'y', freq = F)

# Modellazione ------------------------------------------------------------

# METRICA D'ERRORE:

# definisco la misura d'errore con cui confrontare i modelli:
m.err<-  function(y_hat, y_oss = dati$y[dati$dataset == "verifica"]){
  mse <- mean((y_hat-y_oss)^2)
  #rmse <- sqrt(mse)
  log_loss <- -mean(y_oss * log(y_hat) + (1 - y_oss) * log(1 - y_hat))
  return(t(data.frame(mse = mse, log_loss=log_loss)))
}
err <- data.frame()

# Regressione Ridge -------------------------------------------------------
library(glmnet)

# Creo i folds per la convalida incrociata:
set.seed(25)
K = 5
myfolds <- sample(1:K, NROW(dati[dati$dataset == "stima",]), replace = T)

set.seed(25)
# Stima sull'insieme di stima:
# - uso la griglia di default di R:
m.ridge <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     dati$y[dati$dataset == "stima"],
                     alpha = 0, standardize = F, foldid = myfolds, trace.it = 1)
range(m.ridge$lambda)
plot(m.ridge)


#muovo la griglia
lambda.grid <- 10^seq(-4, -1, length = 300) 
m.ridge <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     dati$y[dati$dataset == "stima"], lambda = lambda.grid,
                     alpha = 0, standardize = F, foldid = myfolds, trace.it = 1)

m.ridge$lambda.min
m.ridge$lambda.1se

plot(m.ridge)

#report:  
plot(m.ridge$glmnet.fit, xvar = "lambda", xlim = c(log(range(m.ridge$lambda))))

title("Ridge", line = 2.5)
abline(v = log(m.ridge$lambda.min), lty = 2)
abline(v = log(m.ridge$lambda.1se), lty = 3)
legend('topright', c('log(lambda.min)', 'log(lambda.1se)'), col = c(1,1), lty = c(2,3))


# Coefficienti relativi al valore di lambda con errore minore:
coef(m.ridge, s="lambda.min")

# Previsione sull'insieme di verifica:
p.ridge.min <- predict(m.ridge, newx = xmat.model[dati$dataset == "verifica",], s = "lambda.min")
p.ridge.min <- pmin(pmax(p.ridge.min, 0.00000000001), 0.999999999999)
previsioni <- p.ridge.min

# Errori
err <- list()
err$Lineare.ridge <- m.err(p.ridge.min)
err

# Regressione Lasso -------------------------------------------------------
library(glmnet)

set.seed(25)
# Stima sull'insieme di stima:
# - uso la griglia di default di R
m.lasso <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     dati$y[dati$dataset == "stima"],
                     alpha = 1, standardize = F, foldid = myfolds, trace.it = 1)
plot(m.lasso)
range(m.lasso$lambda)
#title('MSE al variare di log(lambda')

#griglia a mano
lambda.grid <- seq(exp(-11), exp(-8), length = 300) 
m.lasso <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     dati$y[dati$dataset == "stima"], lambda = lambda.grid,
                     alpha = 1, standardize = F, foldid = myfolds, trace.it = 1)

m.lasso$lambda.min
plot(m.lasso)

#report:
plot(m.lasso$glmnet.fit, xvar = "lambda")
title("Lasso: profilo dei coefficienti", line = 2.5)

abline(v = log(m.lasso$lambda.min), lty = 2)
abline(v = log(m.lasso$lambda.1se), lty = 3)

legend('topright',
       legend = c('log(lambda.min)', 'log(lambda.1se)'), lty = c(2, 3), bty = "n")
#### report

# Coefficienti relativi al valore di lambda con errore minore:
coef(m.lasso, s = "lambda.min")
beta_lasso = m.lasso$glmnet.fit$beta[,which(m.lasso$lambda == m.lasso$lambda.min)]
length(beta_lasso)

coef(m.lasso, s = "lambda.min")

# Variabili selezionate dal lasso (lambda min):
varlasso <- varnames2[beta_lasso != 0]
varlasso #variabili selezionate dal lasso
setdiff(varnames2, varlasso) # variabili non selezionate dal lasso


# Previsione sull'insieme di verifica:
p.lasso.min <- predict(m.lasso, newx = xmat.model[dati$dataset == "verifica",], s = "lambda.min")
p.lasso.min <- pmin(pmax(p.lasso.min, 0.00000000001), 0.999999999999)
previsioni <- cbind(previsioni, p.lasso.min)

# Errori:
err$Lineare_Lasso_min <- m.err(p.lasso.min)
err

# Modello Additivo con var selezionate lasso ------------------------------

library(gam)

# Solo var (quant e qual) selezionate dal lasso
# Splines di lisciamento con 3gdl equivalenti per tutte le quantitative

# Creazione formula per funzione gam:
form.gam2 = character(0)
for (i in 1:length(var_quantitative)){
  if (var_quantitative[i] %in% varlasso){
    form.gam2 = c(form.gam2, paste0("s(",var_quantitative[i],", df=3)",collapse=NULL))
  }
}
gam.quant = paste0(form.gam2, collapse="+")
gam.qual = paste0(var_qualitative, collapse="+")
formula.gam2 = as.formula(paste("y~",paste0(c(gam.qual,gam.quant),collapse="+"),collapse = NULL))

# Stima nell'insieme di stima:
m.gam2 = gam(formula.gam2, data = dati %>% filter(dataset == "stima") %>% select(-dataset))

summary(m.gam2)
#plot(m.gam2, se = T)

# Previsioni nell'insieme di verifica:
p.gam2 = predict(m.gam2, newdata = dati %>% filter(dataset == "verifica") %>% select(-dataset))
p.gam2 <- pmin(pmax(p.gam2, 0.00000000001), 0.999999999999)
previsioni <- cbind(previsioni, p.gam2)

# Errori:
err$Additivo_reduced = m.err(p.gam2)
err
# MARS --------------------------------------------------------------------

library(polspline)

# Stima sull'insieme di stima:
set.seed(25)
n = nrow(dati)

min(6*(nrow(dati)^(1/3)),nrow(dati)/4,100) #maxsize di default:
min(20, round(n/4)) #knot di default

tic('MARS')
m.mars = polymars(dati$y[dati$dataset=="stima"],
                  xmat.model[dati$dataset == "stima",], maxsize = 130)
toc() #410 sec

str(m.mars)
m.mars$model

# Dimensione del modello (cioe' num. di funz. di base) con GCV minima:
(J3 = m.mars$fitting$size[m.mars$fitting$GCV == min(m.mars$fitting$GCV)])

# Grafico dell'andamento della RSS al variare di GCV, in fase di crescita e di potatura:
plot(m.mars$fitting$size, m.mars$fitting$GCV, 
     col = m.mars$fitting$"0/1"+1, pch = 16, cex = 0.7,
     xlab = "n. funz. base", ylab = "GCV")
title('MARS')
legend( "topright", c("In avanti", "All'indietro"), pch = 16,
        col = unique(m.mars$fitting$"0/1")+1)
abline(v = J3)
abline(h = min(m.mars$fitting$GCV))
######### Grafico da includere nel report.

# Modello finale:
m.mars$model

# Effetti marginali con nodi:
#plot(m.mars, predictor1 = 4, main = names(dati)[4])

# Previsioni sull'insieme di verifica:
p.mars = predict(m.mars, x = xmat.model[dati$dataset == "verifica",])
p.mars <- pmin(pmax(p.mars, 0.00000000001), 0.999999999999)
previsioni <- cbind(previsioni, p.mars)

# Errori:
err$MARS = m.err(p.mars)
err

# Bagging -----------------------------------------------------------------

dummies <- model.matrix(~ COUNTRY_NAME - 1, data = dati)

dati <- cbind(dati, dummies)
dati$COUNTRY_NAME = NULL

country <- colnames(dati)[c(104:NCOL(dati))]
for(v in country) dati[[v]] <- as.numeric(dati[[v]])

library(ipred)
# library(rpart)

# Scelgo il numero di alberi migliore guardando l'errore Out-Of-Bag:
set.seed(25)
nbags = 20*1:14     # numero di alberi da provare
bag.ec = numeric(length(nbags))

tic('BAG')
for (i in 1:length(nbags)){
  #set.seed(25*i)
  cat("indice:", i, "nbags:", nbags[i], "\n")
  bt = bagging(y~., data = dati %>% filter(dataset == "stima") %>% select(-dataset), nbagg = nbags[i], coob = T)
  bag.ec[i] = bt$err
}
toc() #2.30 ore!!!

plot(nbags, bag.ec, xlab = "Numero di alberi", ylab = "Errore OOB", type = "l", col = 2, main='Bagging')
abline(v = 100)
# scelgo quando si stabilizza l'errore

# Stima sull'insieme di stima con numero ottimo di alberi:
set.seed(25)
m.bag = bagging(y~., data = dati %>% filter(dataset == "stima") %>% select(-dataset), nbagg = 100, coob=TRUE)

# Previsione sull'insieme di verifica:
p.bag = predict(m.bag, newdata = dati %>% filter(dataset == "verifica") %>% select(-c(y,dataset)))
p.bag <- pmin(pmax(p.bag, 0.00000000001), 0.999999999999)
previsioni <- cbind(previsioni, p.bag)

# Errori:
err$Bagging = m.err(p.bag)
err

# Boosting ------------------------------------------------

library(gbm)

# Scelgo il numero di alberi migliore guardando l'errore Out-Of-Bag (OOB) o tramite cross-validation
set.seed(25)
ntrees = seq(10, 120, by=20)   # numero di alberi
boost.ec = numeric(length(ntrees))

tic('BOOST')
for (i in seq_along(ntrees)) {
  cat("Indice:", i, " - N. alberi:", ntrees[i], "\n")
  
  bt <- gbm(
    formula = y ~ .,
    data = dati %>% filter(dataset == "stima") %>% select(-dataset),
    distribution = "gaussian",     # regressione
    n.trees = ntrees[i],
    interaction.depth = 3,         # profondità massima degli alberi
    shrinkage = 0.05,               
    bag.fraction = 0.8,             # frazione di dati usata per ogni boosting step
    train.fraction = 0.8,           # parte del training set usata per cv
    cv.folds = 5,                   # k-fold CV
    n.cores = 1,                   
    verbose = FALSE)
  
  boost.ec[i] <- min(bt$cv.error)   # errore CV minimo
}
toc() #1219 sec


# Grafico errore vs numero di alberi
plot(ntrees, boost.ec, type = "l", col = 2,
     xlab = "Numero di alberi", ylab = "Errore CV")
title('Boosting: errore al variare del numero di alberi')

# Scelgo numero di alberi ottimo
ntree.opt <- ntrees[which.min(boost.ec)]
cat("Numero di alberi ottimo:", ntree.opt, "\n")

# Stima modello finale 
set.seed(25)
m.boost <- gbm(
  formula = y ~ .,
  data = dati %>% filter(dataset == "stima") %>% select(-dataset),
  distribution = "gaussian",
  n.trees = 90,
  interaction.depth = 3,
  shrinkage = 0.05,
  bag.fraction = 0.8,
  n.cores = 1,
  verbose = FALSE)

# Previsione sull'insieme di verifica
p.boost <- predict(
  m.boost,
  newdata = dati %>% filter(dataset == "verifica") %>% select(-c(y,dataset)),
  n.trees = 90)
p.boost <- pmin(pmax(p.boost, 0.00000000001), 0.999999999999)
previsioni <- cbind(previsioni, p.boost)

# Calcolo errore 
err$Boosting <- m.err(p.boost)
err

# Risultati ---------------------------------------------------------------

library(dplyr)
library(purrr)

err_df <- map_dfr(err, ~as.data.frame(t(.x)), .id = "Modello")

# ordino per MSE crescente
err_mse <- err_df %>% arrange(mse)

# ordino per log_loss crescente
err_logloss <- err_df %>% arrange(log_loss)

err_mse[,1:2]
err_logloss[,c(1,3)]

#LASSO:
#coefficienti del lambda ottimale
coefs <- coef(m.lasso, s = "lambda.min")

coefs_df <- data.frame(Feature = rownames(coefs),Coefficient = as.numeric(coefs))

coefs_df <- coefs_df[coefs_df$Feature != "(Intercept)", ]

coefs_df <- coefs_df[order(abs(coefs_df$Coefficient), decreasing = TRUE), ]

print(coefs_df)
