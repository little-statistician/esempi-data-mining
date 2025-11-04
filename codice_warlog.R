# caricamento dati ---------------------------------------
rm(list = ls())
gc()
library(tidyverse)
library(sm)
library(dplyr)
library(tictoc)
library(tidyr)

setwd("~/.../dir")
dati <- read.csv("warlogs.csv")

#View(dati)
dim(dati)

# ESPLORATIVE -------------------------------------------------

# numero di soggetti == n di osservazioni 
length(unique(dati$report_key)) == nrow(dati)
dati$report_key <- NULL # Elimino l'identificativo di riga

# modifica variabili ---------------------------------------
dati <- dati %>% rename(y = Type) #rinomino la risposta
table(dati$y)

# rinominare i livelli
dati$y[dati$y == "criminal event"] <- "Criminal Event"
dati$y[dati$y == "CRIMINAL EVENT"] <- "Criminal Event"
dati$y[dati$y == "EXPLOSIVE HAZARD"] <- "Explosive Hazard"
dati$y[dati$y == "Friendly Fire"] <- "Friendly Action"

dati$y <- as.factor(dati$y)
droplevels(dati$y)
round(table(dati$y)/length(dati$y),4)

# valori mancanti NA ----------------------------------------

# NA codificati come spazi bianchi nelle colonne:
na_spazio_vuoto <- function(data){
  na_list <- sapply(data, function(x) which(x == ""))
  ritorno <- data.frame(variabile = names(na_list))
  for(i in 1:length(na_list)){
    ritorno$n_na[i] <- length(na_list[[i]])
    ritorno$proporzione[i] <- length(na_list[[i]])/dim(data)[1]
  }
  return(ritorno)
}
(na.obs2 <- na_spazio_vuoto(dati))

sort(table(dati$category))
library(forecast)
dati$category <- fct_lump_n(dati$category, n = 4)

sort(table(dati$region))
dati$region[dati$region == ''] <- 'Unknown'

# combinazioni lineari
head(cbind(dati$enemy_kia + dati$civilian_kia + dati$iraq_forces_killed + dati$coalition_forces_killed, dati$total_deaths), 20) 

sum(dati$enemy_kia + dati$civilian_kia + dati$iraq_forces_killed + dati$coalition_forces_killed == dati$total_deaths) #perfetta corrispondenza


#variabili temporali
head(dati$to_timestamp)
library(lubridate)
dati$to_timestamp <- as.POSIXct(dati$to_timestamp)
dati$anno <- year(dati$to_timestamp)
dati$mese <- as.factor(month(dati$to_timestamp))
dati$ora  <- hour(dati$to_timestamp)
orario <- dati$ora
orario <- as.numeric(orario)
momento <- rep(0,nrow(dati))
momento[orario %in% 6:12] = 'mattina'
momento[orario %in% 13:18] = 'pomeriggio'
momento[orario %in% 19:23] = 'sera'
momento[orario %in% 0:5] = 'notte'
momento <- factor(momento)
dati$momento <- momento

dati$ora <- NULL
dati$to_timestamp <- NULL

table(dati$momento)

#controllo finale ----------------------------------------------
tipo_variabile <- sapply(dati %>% select(-y), class)
(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))

dati$region <- as.factor(dati$region)
table(dati$attack_on)
dati$attack_on <- as.factor(dati$attack_on)

tipo_variabile <- sapply(dati %>% select(-y), class)
(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))

dim(dati)
prop.table(table(dati$y))

#save(dati, file = 'war_backup.Rdata')

# Ricodifica var Risposta -------------------------------------------------

#Creazione di dummy univariate per ciascun livello della risposta
(livelli <- levels(dati$y))
dummies <- c()

for (i in seq_along(livelli)) {
  dum <- paste0("y", i)
  dummies <- c(dummies, dum)
  dati[[dum]] <- ifelse(dati$y == livelli[i], 1, 0)
}

head(dati)
names(dati)

#Salvo gli indici della risposta e delle sue trasformate
ids.leak = which(names(dati) %in% c("y", dummies)) 
# Salvo var qualitative e quantitative (senza risposta)
tipo_var = sapply(dati[, -ids.leak], class)
table(tipo_var)

var_qualitative = names(dati)[-ids.leak][tipo_var == "factor"]

(var_quantitative = setdiff(names(dati)[-ids.leak], var_qualitative))

# Stima-verifica ----------------------------------------------------------
n = dim(dati)[1]
p = dim(dati)[2]
set.seed(25)

# 70% stima e 30% verifica
ind = sample(1:n, round(0.7*n))
stima = dati[ind, ]
ver = dati[-ind, ]

table(stima$y)
#plot(stima$y, main='Frequenza della risposta')

# bilanciamento ---------------------------------------------
#prop.table(table(stima$y))

#bilanciamento
s1 = stima[stima$y1 == 1,]
s2 = stima[stima$y2 == 1,]
s3 = stima[stima$y3 == 1,]
s4 = stima[stima$y4 == 1,]
s5 = stima[stima$y5 == 1,]
s6 = stima[stima$y6 == 1,]
s7 = stima[stima$y7 == 1,]
s8 = stima[stima$y8 == 1,]

set.seed(25)

w2 <- c(0.05, 0.08, 0.09, 0.12, 0.4, 0.8, 1, 1)
prop.table(table(stima$y)*w2)

acaso1 = sample(1:nrow(s1), nrow(s1)*w2[1])
acaso2 = sample(1:nrow(s2), nrow(s2)*w2[2])
acaso3 = sample(1:nrow(s3), nrow(s3)*w2[3])
acaso4 = sample(1:nrow(s4), nrow(s4)*w2[4])
acaso5 = sample(1:nrow(s5), nrow(s5)*w2[5])
acaso6 = sample(1:nrow(s6), nrow(s6)*w2[6])

s1.s = s1[acaso1,]
s2.s = s2[acaso2,]
s3.s = s3[acaso3,]
s4.s = s4[acaso4,]
s5.s = s5[acaso5,]
s6.s = s6[acaso6,]

stima <- as.data.frame(rbind(s1.s, s2.s, s3.s, s4.s, s5.s, s6.s, s7, s8))
dim(stima)
prop.table(table(stima$y))

# pesi -------------------------

freq <- prop.table(table(stima$y))
w <- 1/freq
w <- w/sum(w)
w <- w[stima$y]

length(w) == nrow(stima)

tapply(w, stima$y, sum)

# Controllo che in verifica non ci siano modalità non presenti in stima:
ind.lev = c()
for(col in var_qualitative){
  if(!(all(unique(stima[,col]) %in% unique(ver[,col]))))    
    #dati[dati$dataset == "verifica", col]   #dati[dati$dataset == "stima", col]
  {
    ind.lev = c(ind.lev, col)
    cat(col,"-> in stima ci sono modalità non presenti in verifica\n")
  }
}

for(i in ind.lev){
  cat("Livelli ", i, ": stima = ", unique(stima[,i]),
      " verifica = ", unique(ver[,i]), "\n")
}

#correlazioni
matr_corr = cor(stima[,var_quantitative])
matr_corr[lower.tri(matr_corr, diag = T)] = NA
correlazioni = data.frame(var1 = rep(colnames(matr_corr), ncol(matr_corr)),
                          var2 = rep(rownames(matr_corr), each = nrow(matr_corr)),
                          cor = as.vector(matr_corr))
correlazioni = na.omit(correlazioni)
correlazioni %>% filter(cor < -0.95 | cor > 0.95)
correlazioni %>% filter(cor < -0.70 | cor > 0.70) 


# Creazione risposta numerica ---------------------------------------------

y.tot = as.numeric(dati$y)
y = as.numeric(stima$y)
y.ver = as.numeric(ver$y)

# Standardizzazione, matrici del modello e formula ------------------------

# Standardizzazione separata:
stima[,var_quantitative] = scale(stima[,var_quantitative])
ver[,var_quantitative] = scale(ver[,var_quantitative])


# Creo matrice del modello per stima e verifica
X.s = model.matrix(~., data = stima[, -ids.leak])
X.v = model.matrix(~., data = ver[, -ids.leak])

#Formula del modello completo 
nomi = names(dati)

form = as.formula(paste("y ~ ", paste(nomi[-ids.leak], collapse ="+")))
form.num = as.formula(paste("y ~ ", paste(nomi[-ids.leak], collapse ="+")))

form.mult <- as.formula(
  paste("cbind(", paste(dummies, collapse = ", "), ") ~",
        paste(nomi[-ids.leak], collapse = " + ")))


# Modellazione ------------------------------------------------------------

tab = list()
err = list() 

# Modello Lineare Multivariato --------------------------------------------

form.mult = 'cbind(y1, y2, y3, y4, y5, y6, y7, y8) ~ category + region + attack_on + coalition_forces_wounded + coalition_forces_killed + iraq_forces_wounded + iraq_forces_killed + civilian_wia + civilian_kia + enemy_wia + enemy_kia + enemy_detained + total_deaths + st_x * st_y + anno + mese + momento'

# Stima
ml3 = lm(form.mult, data = stima, weights = w)

summary(ml3)
# un output per y1, uno per y2 e uno per y3

# Previsioni
ml3.pred = predict(ml3, newdata = ver)
ml3.pred = apply(ml3.pred, 1, which.max)
table(dati$y)
table(ml3.pred)

# Matrice di confusione:
ml3.tab <- table(
  factor(round(ml3.pred), levels = 1:length(dummies)),
  factor(y.ver, levels = 1:length(dummies)))

# Tasso di errata classificazione:
(ml3.err = 1 - sum(diag(ml3.tab))/sum(ml3.tab))

tab = c(tab, list(lineare.mult = ml3.tab))
err <- c(err, lineare.mult = ml3.err)

# Modello Multinomiale ----------------------------------------------------
library(nnet)

stima$y <- factor(stima$y)
w <- as.numeric(w)

# Stima
tic('Multinomiale')
mn1 = multinom(form.mult, data=stima[,-ids.leak[1]], maxit = 1000, weights = w)
toc() #49 sec

# Previsioni
mn1.pred = predict(mn1, newdata = ver, type = "prob")
mn1.pred = apply(mn1.pred, 1, which.max)
table(mn1.pred)

# Matrice di confusione
mn1.tab <- table(
  factor(round(mn1.pred), levels = 1:length(dummies)), 
  factor(y.ver, levels = 1:length(dummies)))


# Tasso di errata classificazione
(mn1.err = 1 - sum(diag(mn1.tab))/sum(mn1.tab))

tab = c(tab, list(multinomiale = mn1.tab))
err <- c(err, list(multinomiale = mn1.err)) 

# Stima - Convalida --------------------------------------------------------

set.seed(25)
ind = sample(1:nrow(stima), round((3/4)*nrow(stima)))
stima.rid = stima[ind,]
conv = stima[-ind,]
y.rid = y[ind]
w.rid = w[ind]
y.conv = y[-ind]
X.s.rid = model.matrix(~., data = stima.rid[, -ids.leak])
X.c = model.matrix(~., data = conv[, -ids.leak])
var.names <- colnames(X.s)

# Controllo che in stima non ci siano modalità non presenti in convalida
ind.lev = c()
for(col in var_qualitative){
  if(!(all(unique(stima.rid[,col]) %in% unique(conv[,col]))))    
    #dati[dati$dataset == "verifica", col]   #dati[dati$dataset == "stima", col]
  {
    ind.lev = c(ind.lev, col)
    cat(col,"-> in stima ci sono modalità non presenti in convalida\n")
  }
}
for(i in ind.lev){
  cat("Livelli ", i, ": stima = ", unique(stima.rid[,i]),
      " convalida = ", unique(conv[,i]), "\n")
}

# Regressione Ridge -------------------------------------------------------

library(glmnet)

grid = 10^seq(-5, 5, length=500)

# convalida incrociata
ridge.cv = cv.glmnet(X.s[,-1], y, alpha = 0, lambda = grid, standardize = FALSE, weights = w)
plot(ridge.cv)

grid = 10^seq(-6, 0, length=300)

#diminuisco la griglia
ridge.cv = cv.glmnet(X.s[,-1], y, alpha = 0, lambda = grid, standardize = FALSE, weights = w)
plot(ridge.cv)

# Previsioni
ridge.cv.pred = predict(ridge.cv, newx = X.v[,-1], s = ridge.cv$lambda.min)

# Matrice di confusione
ridge.cv.tab <- table(
  factor(round(ridge.cv.pred), levels = 1:length(dummies)), 
  factor(y.ver, levels = 1:length(dummies)))

# Tasso di errata classificazione
(ridge.cv.err = 1 - sum(diag(ridge.cv.tab))/sum(ridge.cv.tab))

tab <- c(tab, ridge.cv.tab)
err <- c(err, ridge.cv=ridge.cv.err) 

# Regressione Lasso -------------------------------------------------------

set.seed(25)
library(glmnet)

grid = 10^seq(-5, 5, length=500)

#  convalida incrociata
lasso.cv = cv.glmnet(X.s[,-1], y, alpha = 1, lambda = grid, standardize = FALSE, weights = w)
plot(lasso.cv)

grid = 10^seq(-3, 0, length=500)
lasso.cv = cv.glmnet(X.s[,-1], y, alpha = 1, lambda = grid, standardize = FALSE, weights = w)
plot(lasso.cv)

lasso.cv$lambda.min
lasso.cv$lambda.1se

beta.lasso.cv = coef(lasso.cv, s = "lambda.min")
length(beta.lasso.cv)
(var.lasso.cv = var.names[which(beta.lasso.cv!=0)])

lasso.cv.pred = predict(lasso.cv, newx = X.v[,-1], s = lasso.cv$lambda.min)
(lasso.cv.tab = table(round(lasso.cv.pred), y.ver))

lasso.cv.tab <- table(
  factor(round(lasso.cv.pred), levels = 1:length(dummies)), 
  factor(y.ver, levels = 1:length(dummies)))


(lasso.cv.err = 1 - sum(diag(lasso.cv.tab))/sum(lasso.cv.tab))

tab = c(tab, list(lasso.cv = lasso.cv.tab))
err <- c(err, lasso.cv = lasso.cv.err) 

# LDA ---------------------------------------------------------------------

library(MASS)

form.quant = as.formula(paste("y ~ ", paste(var_quantitative, collapse ="+")))

#form.lda = as.formula(paste("y ~ ", paste(var.lda, collapse ="+")))

#y ~ coalition_forces_wounded + coalition_forces_killed + iraq_forces_wounded + iraq_forces_killed + civilian_wia + civilian_kia + enemy_wia + enemy_kia + enemy_detained + total_deaths + st_x + st_y + anno + n_gg + w

lda = lda(form.quant, data = stima[, var_quantitative])

lda.pred = predict(lda, newdata = ver)
(lda.tab = table(lda.pred$class, ver$y))

tab = c(tab, list(lda = lda.tab))
lda_err<-(lda.err = 1 - sum(diag(lda.tab))/sum(lda.tab))
lda_err

err <- c(err, LDA=lda_err)

#QDA  - non è stimabile----------------------------------------------------------------------------

library(MASS)

form.num.quant = as.formula(paste("y ~ ", paste(var_quantitative, collapse ="+")))

qda.mod = qda(form.num.quant, data = stima[,var_quantitative])

# Modello Additivo con var selezionate lasso ------------------------------

library(VGAM)
library(splines)

# Variabili qualitative e quantitative selezionate dal LASSO
ids.fac <- which(nomi %in% var_qualitative)
# Controllo quantitative con meno di 4 valori unici
for (v in var_quantitative) {
  if (length(unique(stima[, v])) < 4) cat(v, length(unique(stima[, v])), "\n")
}

# Creazione dei termini spline per le quantitative (escludo n_gg, st_x, st_y)
quant_vars <- var_quantitative[!var_quantitative %in% c("st_x","st_y")]

form.gam2 <- character(0)
for (v in quant_vars) {
  if (v %in% var.lasso.cv) {
    form.gam2 <- c(form.gam2, paste0("bs(", v, ", df=3)"))
  }
}

gam.quant <- paste(form.gam2, collapse="+")
gam.qual <- paste(var_qualitative, collapse="+")

# spline bivariate per st_x e st_y
formula.vglm <- as.formula(
  paste("y ~", paste(c(gam.qual, gam.quant), collapse="+"), "+ bs(st_x, df=3) + bs(st_y, df=3) + I(bs(st_x, df=3) * bs(st_y, df=3))"))

#mod
tic('GAM')
mgam <- vglm(formula.vglm, 
             family = multinomial(refLevel = 1),
             data = stima, weights = w, trace = TRUE)
toc()

#summary(mgam) #si impianta
#plot(mgam, ask = TRUE, se = TRUE)
gam.prob <- predict(mgam, newdata = ver, type = "response")

# Assegna la classe con probabilità massima
gam.pred <- apply(gam.prob, 1, which.max)

gam.tab <- table(
  factor(round(gam.pred), levels = 1:length(dummies)), 
  factor(y.ver, levels = 1:length(dummies)))

(gam.err = 1 - sum(diag(gam.tab))/sum(gam.tab))

tab = c(tab, list(gam = gam.tab))
err <- c(err, gam = gam.err)

# PPR ---------------------------------------------------------------------

term = 1:20 #cambialo se non trovi il gomito 
err.tab = rep(NA, length(term))
#Solo su risposta numerica

tic('PPR')
for(i in 1:length(term)){
  mppr = ppr(y.rid ~., data = stima.rid[,-ids.leak], nterms = term[i], weights = w.rid)
  mppr.pred = predict(mppr, newdata = conv)
  tab.par = table(round(mppr.pred), y.conv)
  #Metrica d'errore: tasso di errata classificazione
  err.tab[i] = 1 - sum(diag(tab.par))/sum(tab.par)
  cat(i, " ")
}
toc() #473 sec

plot(term, err.tab, xlab = "Numero di termini PPR", ylab = "Errata classificazione",
     type = "l", main = "Tasso di errata classificazione sull'insieme di convalida")

(term.opt = term[which.min(err.tab)])
mppr = ppr(y ~., data = stima[,-ids.leak], nterms = term.opt)

mppr.pred = predict(mppr, newdata = ver)

ppr.tab <- table(
  factor(round(mppr.pred), levels = 1:length(dummies)), 
  factor(y.ver, levels = 1:length(dummies)))

(ppr.err = 1 - sum(diag(ppr.tab))/sum(ppr.tab))

tab = c(tab, list(ppr = ppr.tab))
err <- c(err, ppr = ppr.err)

# MARS --------------------------------------------------------------------

library(polspline)
mmars = polyclass(stima$y, stima[,-ids.leak], weight = w, additive = FALSE)
summary(mmars)

logl <- mmars$logl  
# Colonna 1 = numero funzioni base, colonna 2 = log-likelihood, colonna 6 = selezione (1 = scelto)
nfun <- logl[, 1]
AIC <- -logl[, 2] 

opt_idx <- which.min(AIC)
opt_nfun <- nfun[opt_idx]
opt_AIC <- AIC[opt_idx]

# Grafico
plot(nfun, AIC, type = "b", pch = 19,
     xlab = "Numero di funzioni base",
     ylab = "AIC",
     main = "Andamento AIC in funzione delle funzioni base")

#punto ottimale
points(opt_nfun, opt_AIC, col = "red", pch = 19, cex = 1.5)
abline(v = opt_nfun, col = "red", lty = 2)

mmars.pred = ppolyclass(ver[, -ids.leak], mmars)
mmars.pred = apply(mmars.pred, 1, which.max)

mars.tab <- table(
  factor(round(mmars.pred), levels = 1:length(dummies)), 
  factor(y.ver, levels = 1:length(dummies))) 

(mars.err = 1 - sum(diag(mars.tab))/sum(mars.tab))

tab <- c(tab, mars.tab)
err <- c(err, mars = mars.err)

# Albero ------------------------------------------------------

library(tree)

set.seed(25)

nomi <- colnames(dati)
ids.leak.tree = ids.leak[-which(nomi == "y")]

mtree.or = tree(y ~., 
                data = stima.rid[, -ids.leak.tree],
                control = tree.control(nobs = nrow(stima.rid), minsize = 10, mindev = 0.01), split = "gini")

prune.mtree = prune.tree(mtree.or, newdata = conv[, -ids.leak.tree])      

plot(prune.mtree)

range(prune.mtree$size)
(J.opt = prune.mtree$size[which.min(prune.mtree$dev)]) #numero di foglie
abline(v=J.opt)

mtree = prune.tree(mtree.or, best = J.opt) 
plot(mtree)
text(mtree, pretty = 4, cex = 0.7)
#plot(mtree)
#text(mtree, pretty = 4, cex = 0.7, label = "yprob")
mtree.pred = predict(mtree, newdata = ver, type = "class")
mtree.pred.prob = predict(mtree, newdata = ver, type = "vector")
(tree.tab = table(mtree.pred, ver$y))
tab = c(tab, list(tree = tree.tab))
albero_err<-(tree.err = 1 - sum(diag(tree.tab))/sum(tree.tab))
albero_err
err <- c(err, albero=albero_err)

# Bagging -----------------------------------------------------------------

set.seed(25)
library(ipred)
nbag = seq(20, 500, by = 20)
err.tab = rep(NA, length(nbag))


tic('Bagging')
for(i in 1:length(nbag)){
  bag = bagging(stima$y ~., data = stima[, -ids.leak],
                nbagg = nbag[i], coob = TRUE)
  #Errore out of bag
  err.tab[i] = bag$err
  cat(i, "")
}                 
toc() #284 sec, 10 passi

plot(nbag, err.tab, xlab = "Numero di campioni bootstrap", 
     ylab = "Errore OOB", type = "l",
     main = "Errore sulle unità out of bag")
points(340, 0.04314122, pch = 19, col = 2, lwd = 4)

(nbag.opt = nbag[which.min(err.tab)])

set.seed(8765)
bag = bagging(stima$y ~., data = stima[, -ids.leak],
              nbagg = nbag.opt, coob = TRUE)

bag.pred = predict(bag, newdata = ver)
bag.pred <- factor(bag.pred, levels = livelli)
ver$y <- factor(ver$y, levels = livelli)

bag.pred.prob = predict(bag, newdata = ver, type = "prob")

(bag.tab = table(bag.pred, ver$y))
tab = c(tab, list(bagging = bag.tab))

(bag.err = 1 - sum(diag(bag.tab))/sum(bag.tab))
err <- c(err, bagging = bag.err)


# Random Forest -----------------------------------------------------------

library(randomForest)

tic('Random Forest:')
rf1 = randomForest(x = stima[, -ids.leak], y = stima$y)
toc() #20 sec

plot(rf1$err.rate[,9], main = "Errore ut Of Bag", type = 'l', xlab = 'Numero di alberi', ylab = 'Errore di classificazione')

ntrees.opt = 200 #guarda grafico

mtries = 2:ncol(stima.rid[, -ids.leak])
err.tab = rep(NA, length(mtries))

set.seed(25)

err.tab <- numeric(length(mtries))

tic('RF')
for(i in 1:length(mtries)){
  rf = randomForest(x = stima.rid[, -ids.leak], y = stima.rid$y,
                    xtest = conv[, -ids.leak], ytest = conv$y,
                    ntree = 30, mtry = mtries[i])
  err.tab[i] = rf$test$err.rate[30,1]
  cat(i, "")
}
toc() #38 sec


plot(mtries, err.tab, type = "l", xlab = "Numero di covariate campionate",
     ylab = "Tasso di errata classificazione", main = "Errore sull'insieme di convalida")
(mtry.opt = mtries[which.min(err.tab)])
abline(v = mtry.opt)

set.seed(25)
rf = randomForest(x = stima[, -ids.leak], y = stima$y, ntree = ntrees.opt,
                  mtry = mtry.opt, importance = TRUE)

rf
varImpPlot(rf, main = 'Foresta Casuale')

rf.pred.prob = predict(rf, newdata = ver, type = "prob")

rf.pred = predict(rf, newdata = ver, levels = livelli_Y)
(rf.tab = table(rf.pred, ver$y))

(rf.err = 1 - sum(diag(rf.tab))/sum(rf.tab))

tab = c(tab, list(randomforest = rf.tab))
err <- c(err, random.forest = rf.err)

# SVM ---------------------------------------------------------------------

library(e1071)
costs = 2:20
err.tab = rep(NA, length(costs))
set.seed(25)

tic('SMV:')
for(i in 1:length(costs)){
  supvec = svm(stima.rid$y~., data = stima.rid[, -ids.leak], kernel = "radial", #linear
               cost = costs[i])
  svm.pred = predict(supvec, newdata = conv[, -ids.leak])
  svm.tab = table(svm.pred, conv$y)
  err.tab[i] = 1 - sum(diag(svm.tab))/sum(svm.tab)
  cat (i, "")
}
toc() #13 indici, 475 sec


plot(costs, err.tab, type = "l", xlab = "",
     ylab = "Tasso di errata classificazione", main = "SVM: Errore sull'insieme di convalida")

(cost.opt = costs[which.min(err.tab)])

supvec = svm(stima$y~., data = stima[, -ids.leak], kernel = "radial",
             cost = cost.opt) #, probability = TRUE
summary(supvec) 

svm.pred = predict(supvec, newdata = ver, decision.values = TRUE)   #, probability = TRUE


svm.tab <- table(factor(svm.pred, levels = livelli),
                 factor(ver$y))

(svm.err = 1 - sum(diag(svm.tab))/sum(svm.tab))

tab = c(tab, list(svm = svm.tab))
err <- c(err, SMV = svm.err)

# Risultati ---------------------------------------------------------------
err_sorted <- err[order(unlist(err), decreasing = FALSE)]

(err_df <- data.frame(
  Modello = names(err_sorted),
  Errore = unlist(err_sorted)))

varImpPlot(rf, main = 'Foresta Casuale', type = 1)
table(dati$category)
table(dati$attack_on)

