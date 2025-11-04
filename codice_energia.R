# caricamento dati ------------------------------------------------

rm(list = ls())
gc()
library(tidyverse)
library(sm)
library(dplyr)
library(tictoc)
library(tidyr)

dati = read.csv('C:/Users/Michela/Documents/University/MAGISTRALE/Bressanone/3_energia/energia.csv', header = TRUE, stringsAsFactors = TRUE)
dim(dati)
names(dati)


#View(dati)
dim(dati)
head(dati)

# ESPLORATIVE -------------------------------------------------

# numero di soggetti == n di osservazioni 
length(unique(dati$Id_Cliente))
length(unique(dati$X))
length(unique(dati$kWh))
              
nrow(dati) == length(dati$X)
dati <- dati %>% select(-X) # elimino l'id
colnames(dati)

# modifica variabili ---------------------------------------
range(dati$kWh)
dati <- dati %>% rename(y = kWh) #rinomino la risposta

# tipo variabile --------------------------------------

str(dati)
tipo_variabile <- sapply(dati %>% select(-y), class)
table(tipo_variabile)

colnames(dati)
#trasaformo tutte le categoriche in fattori 
colnames(dati)[c(1:3,7:11,13,15:18, 20, 22,25,26)]
conversione = c(colnames(dati)[c(1:3,7:11,13,15:18, 20, 22,25,26)])

for (var in conversione) dati[[var]] <- as.factor(dati[[var]])

# tipo variabili
tipo_variabile <- sapply(dati %>% select(-y), class)
table(tipo_variabile)
(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor" | tipo_variabile == "character"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))

#var quantitative poche modalità non ce ne sono
(n_unici <- apply(dati[,var_quantitative], 2, function(x) length(unique(x))))

#tra le qualitative, quelle con tante modalità sono quelle relative al comune e quelle relative agli id dei clinti e delle sedi
(n_unici <- apply(dati[,var_qualitative], 2, function(x) length(unique(x))))

colnames(dati)

#rimozione variabili -------------------------------------
table(dati$altitudine)
table(dati$montano)
table(dati$zona_altimetrica)

colnames(dati)[c(5,6,8,9,17,18,20, 28)] #eliminare
dati <- dati[,-c(5,6,8,9,17,18,20, 28)]
#dati <- dati[,-5]
colnames(dati)
dati <- dati[,-7]
colnames(dati)

# valori mancanti NA ----------------------------------------

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

# RIFARE ASSOLUTAMENTE ORA, IMPORTANTE
dati$time <- as.numeric(dati$time)

tipo_variabile <- sapply(dati %>% select(-y), class)
table(tipo_variabile)
(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor" | tipo_variabile == "character"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))


# Stima e verifica --------------------------------------------------------

dati$dataset <- "stima"
set.seed(25)
dati$dataset[sample(1:nrow(dati), nrow(dati)*0.3)] <- "verifica"
table(dati$dataset)

# Controllo che in verifica non ci siano modalità non presenti in stima:
for(col in var_qualitative){
  if(!(all(unique(dati[dati$dataset == "verifica", col]) %in%
           unique(dati[dati$dataset == "stima", col])))){
    cat(col,"-> in verifica sono presenti modalità non presenti in stima\n")
  }
}

# Elimino esplicative quantitative fortemente correlate fra loro:
matrice_correlazione <- cor(
  dati %>%
    filter(dataset == "stima") %>%
    select(-all_of(c(var_qualitative, "y", "dataset")))
)

matrice_correlazione[lower.tri(matrice_correlazione, diag = T)] <- NA
correlazioni <- data.frame(var1 = rep(colnames(matrice_correlazione), ncol(matrice_correlazione)),
                           var2 = rep(rownames(matrice_correlazione), each = nrow(matrice_correlazione)),
                           cor = as.vector(matrice_correlazione))

correlazioni <- na.omit(correlazioni)

correlazioni %>% filter(cor < -0.95 | cor > 0.95)
#dati <- dati %>% select(-"var.troppo.correlata")

# Standardizzazione separata:
dati[dati$dataset == "stima",var_quantitative] <- dati[dati$dataset == "stima",] %>% select(var_quantitative) %>% scale 
dati[dati$dataset == "verifica",var_quantitative] <- dati[dati$dataset == "verifica",] %>% select(var_quantitative) %>% scale

# Divisione in stima-convalida:
set.seed(25)
cb1 <- sample(1:NROW(dati[dati$dataset == "stima",]), (2/3)*NROW(dati[dati$dataset == "stima",]))
cb2 <- setdiff(1:NROW(dati[dati$dataset == "stima",]), cb1)


# matrice del modello 
xmat.model <- model.matrix(~.-1, data = dati %>% select(-c(y, dataset)))
# tolgo ora intercetta

# Salvo nomi delle variabili:
varnames <- colnames(dati %>% select(-c(y, dataset)))
# Creo formula con tutte le variabili:
form.tuttevars = paste("y ~", paste(varnames,collapse = " + "), collapse = NULL)
# Salvo nomi delle variabili della matrice del modello:
varnames2 <- colnames(xmat.model)


# Analisi esplorativa insieme di stima ------------------------------------

YYY <- dati %>% filter(dataset == "stima") %>% pull(y) 
summary(YYY)
par(mfrow=c(1,2))

# Distribuzione marginale della risposta in scala logaritmica
hist(YYY, nclass = 50, xlab = "y", main = "distribuzione marginale di y", xlim=c(min(YYY),max(YYY)))
range(dati$y)

hist(log(YYY + 0.5), nclass = 50, xlab = "y", main = "distribuzione marginale di log(y)", xlim=c(min(log(YYY+0.5)),max(log(YYY))))

boxplot(log(YYY+0.5), xlab = "", main = "boxplot log(y)")

dati$y <- log(dati$y + 0.5)
range(dati$y)


# Modellazione ------------------------------------------------------------

# METRICA D'ERRORE:

# definisco la misura d'errore con cui confrontare i modelli:
m.err<-  function(y_hat, y_oss = dati$y[dati$dataset == "verifica"]){
  mse <- mean((y_hat-y_oss)^2)
  rmse <- sqrt(mse)
  return(t(data.frame(mse = mse, rmse = rmse)))
}


# Modello lineare con selezione stepwise ----------------------------------

# Selezione stepwise ibrida a partire dal modello nullo usando AIC

# Modello nullo:
m0 <- lm(y ~ 1, data = dati %>% filter(dataset == "stima") %>% select(-dataset))

# Stima 
m.lm <- lm(y ~ ., data = dati %>% filter(dataset == "stima") %>% select(-dataset))

tic('lm step')
  m.lm.step <- step(m0, scope = formula(m.lm), direction = "both", trace = 0)#78 sec 
toc()

length(names(m.lm.step$model)) # dal conteggio togliere y
names(m.lm.step$model) # variabili incluse 
setdiff(names(dati %>% select(-dataset)), names(m.lm.step$model))  # variabili escluse

# Previsioni
p.lm.step <- predict(m.lm.step, newdata = dati %>% filter(dataset == "verifica") %>% select(-dataset))

# Errori:
err <- data.frame(Lineare.step = m.err(p.lm.step))
t(err)


# Regressione Ridge -------------------------------------------------------
library(glmnet)
par(mfrow=c(1,2))

# convalida incrociata 

# Creo i folds per la convalida incrociata:
set.seed(25)
K = 4
myfolds <- sample(1:K, NROW(dati[dati$dataset == "stima",]), replace = T)

set.seed(25)
# Stima sull'insieme di stima:
# - uso la griglia di default di R:
m.ridge <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     dati$y[dati$dataset == "stima"],
                     alpha = 0, standardize = F, foldid = myfolds, trace.it = 1)
range(m.ridge$lambda)
plot(m.ridge)
#title('MSE al variare di log(lambda)')
# specifico io la griglia a mano: 
lambda.grid <- 10^seq(-3, -2, length = 300)
m.ridge <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     dati$y[dati$dataset == "stima"], lambda = lambda.grid,
                     alpha = 0, standardize = F, foldid = myfolds, trace.it = 1)

m.ridge$lambda.min
m.ridge$lambda.1se


plot(m.ridge)

m.ridge <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     dati$y[dati$dataset == "stima"], lambda.min.ratio = 0.00000000001,
                     alpha = 0, standardize = F, foldid = myfolds, trace.it = 1)

par(mfrow=c(1,1))
plot(m.ridge) #report

m.ridge$lambda.min
m.ridge$lambda.1se

#report:  

legend('topright', c('log(lambda.min)', 'log(lambda.1se)'), col = c(1,1), lty = c(2,3))
mtext("Penalizzazione decrescente", side = 1, line = 2.5, col = "blue", adj = 0)


# Previsione sull'insieme di verifica:
p.ridge.min <- predict(m.ridge, newx = xmat.model[dati$dataset == "verifica",], s = "lambda.min")
p.ridge.1se <- predict(m.ridge, newx = xmat.model[dati$dataset == "verifica",], s = "lambda.1se")

# Errori
err$Lineare_Ridge_min <- m.err(p.ridge.min)
err$Lineare_Ridge_1se <- m.err(p.ridge.1se)
t(err)


# Regressione Lasso -------------------------------------------------------
library(glmnet)


# convalida incrociata 

set.seed(25)
# Stima sull'insieme di stima:
# - uso la griglia di default di R
m.lasso <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     dati$y[dati$dataset == "stima"],
                     alpha = 1, standardize = F, foldid = myfolds, trace.it = 1)
par(mfrow=c(1,1))
plot(m.lasso)

title('MSE al variare di log(lambda')
coef(m.lasso)

m.lasso$lambda.1se

# Coefficienti relativi al valore di lambda con errore minore:
coef(m.lasso, s = "lambda.1se")
table(as.vector(round(coef(m.lasso, s = "lambda.1se"), 2)))

# Previsione sull'insieme di verifica:
p.lasso.min <- predict(m.lasso, newx = xmat.model[dati$dataset == "verifica",], s = "lambda.min")
p.lasso.1se <- predict(m.lasso, newx = xmat.model[dati$dataset == "verifica",], s = "lambda.1se")

# Errori:
err$Lineare_Lasso_min <- m.err(p.lasso.min)
err$Lineare_Lasso_1se <- m.err(p.lasso.1se)
t(err)


# Modello Additivo con tutte le variabili ---------------------------------
library(gam)

# Splines di lisciamento con 3 gradi di libertà equivalenti per tutte le quantitative

# Creazione formula per funzione gam:
form.gam1 = character(0)
for (var in var_quantitative){
  form.gam1 = c(form.gam1, paste0("s(", var, ", df=3)"))  # df=3 per 3 gdl equivalenti
}

gam.quant = paste0(form.gam1, collapse = "+")
gam.qual = paste0(var_qualitative, collapse = "+")
formula.gam1 = as.formula(paste("y ~", paste(c(gam.qual, gam.quant), collapse = "+")))


# Stima modello sull'insieme di stima:
m.gam = gam(formula.gam1, data = dati %>% filter(dataset == "stima") %>% select(-dataset))

# Previsioni sull'insieme di verifica:
p.gam = predict(m.gam, newdata = dati %>% filter(dataset == "verifica") %>% select(-dataset))

# Errori:
err$Additivo = m.err(p.gam)
t(err)

# Albero di Regressione ---------------------------------------------------

library(tree)

# stima con insieme di convalida:
dim(dati)[1]/50
str(dati)


colnames(dati)
# Crescita sull'insieme di stima:

m.tree = tree(y~., data = (dati %>% filter(dataset=="stima") %>% select(-dataset))[cb1,], 
              control = tree.control(nobs = length(cb1),
                                     minsize = 50,
                                     mindev = 0.0001))
plot(m.tree)

# Potatura sull'insieme di convalida:
m.tree.prune = prune.tree(m.tree, newdata = (dati %>% filter(dataset=="stima")%>% select(-dataset))[-cb1,])

# Andamento della devianza in funzione di size:
plot(m.tree.prune)
# Evidenzio il valore di size per cui la devianza e' minima:
J = m.tree.prune$size[which.min(m.tree.prune$dev)]
J
abline(v = J, lty = 2, col = 2)
abline(h = min(m.tree.prune$dev), col=2)
title("Devianza vs taglia", cex.main = 1)
#### Grafico da includere nel report

# Albero finale:
m.tree.best = prune.tree(m.tree, best = J)
plot(m.tree.best)
text(m.tree.best, cex = 0.7, pretty = 5)
title('Albero di regressione (stima-convalida)')
# Grafico da includere nel report

# Previsioni sull'insieme di verifica:
p.tree = predict(m.tree.best, newdata = dati %>% filter(dataset=="verifica")%>% select(-dataset))

# Errori:
err$Albero = m.err(p.tree)
t(err)

# MARS --------------------------------------------------------------------

library(polspline)

# Stima sull'insieme di stima:
set.seed(25)
n = nrow(dati)

min(6*(nrow(dati)^(1/3)),nrow(dati)/4,100) #maxsize di default:
min(20, round(n/4)) #knot di default

m.mars = polymars(dati$y[dati$dataset=="stima"],
                  xmat.model[dati$dataset == "stima",])

m.mars = polymars(dati$y[dati$dataset=="stima"],
                  xmat.model[dati$dataset == "stima",], maxsize = 120) #aumento maxsize perchè non ho raggiunto il minimo

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

# Previsioni sull'insieme di verifica:
p.mars = predict(m.mars, x = xmat.model[dati$dataset == "verifica",])

# Errori:
err$MARS = m.err(p.mars)
t(err)


# Regressione Projection Pursuit (PPR) ------------------------------------

terms = 1:20 #1:20
ppr.mse = matrix(NA, nrow = length(terms), ncol = 2)

tic('PPR')
for (i in 1:length(terms)){
  cat("Funzioni dorsali:", terms[i], "\n")
  ppr.fits = ppr(y~.,
                 data = (dati %>% filter(dataset=="stima") %>% select(-dataset))[cb1,],
                 nterms = terms[i], optlevel = 1)
  prev = predict(ppr.fits, newdata = (dati %>% filter(dataset=="stima") %>% select(-dataset))[-cb1,])
  ppr.mse[i,] = c(terms[i], mean((prev- (dati %>% filter(dataset=="stima") %>% select(-dataset))$y[-cb1])^2))  
}
toc() #1023 sec = 17 min

plot(ppr.mse, main = "PPR: Errore di previsione", xlab = "Funzioni Dorsali", ylab = "MSE", type = "l")
(M = terms[which.min(ppr.mse[,2])])


m.ppr = ppr(y~.,
            data = dati %>% filter(dataset == "stima") %>% select(-dataset),
            nterms = M, optlevel = 3)
p.ppr = predict(m.ppr, newdata = dati %>% filter(dataset == "verifica") %>% select(-dataset))

err$Projection_Pursuit = m.err(p.ppr)
t(err)

# Risultati ---------------------------------------------------------------

# Tabella con modelli in ordine crescente di MSE:
(tabella = arrange(as.data.frame(t(err)), mse))
