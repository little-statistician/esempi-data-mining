# Caricamento dati --------------------------------------------------------
rm(list = ls())
gc()

library(tidyverse)
library(dplyr)
library(tidyr)
library(tictoc)

dati <- read.csv("~/.../bancafamiglie.csv", header=T, stringsAsFactors = TRUE, sep=",")

#View(dati)
dim(dati)
colnames(dati)

# ESPLORATIVE ---------------------------------------------------------------

# Controllo che ci sia solo un'osservazione per ogni id:
length(unique(dati$Codice_Cliente)) == nrow(dati)

dati$Codice_Cliente <- NULL

# Rinominare la risposta come "y"
dati <- dati %>% rename(y = TARG_TOT)
prop.table(table(dati %>% pull(y)))

# valori mancanti NA ----------------------------------------
conteggio_na_summary <- function(data) {
  n <- nrow(data)
  
  # NA per colonna
  na_col <- apply(data, 2, function(col) sum(is.na(col)))
  na_col <- na_col[na_col > 0]  # solo colonne con NA
  
  na_col_df <- data.frame(variabile = names(na_col), freq = as.numeric(na_col) / n)
  na_col_df <- na_col_df[order(na_col_df$freq), ]
  
  # NA per riga
  na_riga <- apply(data, 1, function(riga) sum(is.na(riga)))
  na_riga <- na_riga[na_riga > 0]
  
  na_riga_df <- data.frame(
    riga = which(apply(data, 1, function(riga) sum(is.na(riga))) > 0),
    na_count = na_riga)
  
  # Proporzione osservazioni con almeno un NA
  prop_oss_con_NA <- length(na_riga) / n
  
  return(list(
    na_colonna = na_col_df,
    na_riga = na_riga_df,
    prop_oss_con_NA = prop_oss_con_NA
  ))
}

res <- conteggio_na_summary(dati)
res$na_colonna
dim(dati)[2] #numero variabili
sum(res$na_colonna$freq)

# trattamento NA ---------------------------

table(dati$FIND_PPQ18SS_MONTH_DAT_MAX_FIN, useNA = 'always')
dati$FIND_PPQ18SS_MONTH_DAT_MAX_FIN <- ifelse(is.na(dati$FIND_PPQ18SS_MONTH_DAT_MAX_FIN), 0, 1)


#variabii relative all'anzanità
table(dati$ANZ_PROF, useNA = 'always')
dati$ANZ_PROF[dati$ANZ_PROF == '98'] <- NA
dati$ANZ_PROF[dati$ANZ_PROF == '99'] <- NA
dati$ANZ_PROF <- cut(dati$ANZ_PROF, breaks = c(-1, 15, 30, 96, 101), labels = c('0-15', '16-30', '31-97', 'Non numerico'))
dati$ANZ_PROF <-  ifelse(is.na(dati$ANZ_PROF), 'Non numerico', dati$ANZ_PROF)

table(dati$ANZ_BAN, useNA = 'always')
dati$ANZ_BAN[dati$ANZ_BAN == '98'] <- NA
dati$ANZ_BAN[dati$ANZ_BAN == '99'] <- NA
dati$ANZ_BAN <- cut(dati$ANZ_BAN, breaks = c(-1, 15, 30, 96, 101), labels = c('0-15', '16-30', '31-97', 'Non numerico'))
dati$ANZ_BAN <-  ifelse(is.na(dati$ANZ_BAN), 'Non numerico', dati$ANZ_BAN)
table(dati$ANZ_BAN)

table(dati$ANZ_RES, useNA = 'always')
dati$ANZ_RES <- cut(dati$ANZ_RES, breaks = c(-1, 15, 30, 96, 101), labels = c('0-15', '16-30', '31-97', 'Non numerico'))
dati$ANZ_RES <-  ifelse(is.na(dati$ANZ_RES), 'Non numerico', dati$ANZ_RES)
table(dati$ANZ_RES)

res <- conteggio_na_summary(dati)
mancanti <- res$na_colonna$variabile

for(v in mancanti) print(table(is.na(dati[[v]]), dati$y))

dati <- dati[!is.na(dati$IMP_FAM),]
dati <- dati[!is.na(dati$IMP_RED),]
dati <- dati[!is.na(dati$FIND_NUM_MEN_RES),]
dati <- dati[!is.na(dati$PPQ_NUM_MEN_RES),]

conteggio_na_summary(dati)$na_colonna$variabile

table(dati$FIND_PPQ18_IMP_FIN, useNA = 'always')
dati$FIND_PPQ18_IMP_FIN <- ifelse(dati$FIND_PPQ18_IMP_FIN > 1, 1, dati$FIND_PPQ18_IMP_FIN)
dati$FIND_PPQ18_IMP_FIN[is.na(dati$FIND_PPQ18_IMP_FIN)] <- 'Mancante'


#table(cut(dati$PPQ_18_IMP_FIN, breaks = c(-1, 999, 1000, 15000, 25000, 35000, 45000, 70000)), useNA = 'always')
table(dati$PPQ_18_IMP_FIN)
dati$PPQ_18_IMP_FIN <- ifelse(dati$PPQ_18_IMP_FIN > 1, 1, dati$PPQ_18_IMP_FIN)
dati$PPQ_18_IMP_FIN[is.na(dati$PPQ_18_IMP_FIN)] <- 'Mancante'


dati$PROF[dati$PROF == 'Societa/Associazioni'] <- 'Sconosciuto'

prop.table(table(dati$y))

# tipo variabile --------------------------------------

tipo_variabile <- sapply(dati %>% select(-y), class)
table(tipo_variabile)

# rendo fattore un character
dati <- dati %>% mutate(across(where(is.character), as.factor))

(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor" | tipo_variabile == "character"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))


table(dati$CRT_TODU_REV, useNA = 'always')

#CRT_PRE_C_AGE#CRT_PRE_C_FLG_PRE Carta - Cliente in possesso di carta 
#CRT_PRE_18_FLG_PRE Carta ultimi 18 mesi - Cliente in possesso di carta

dati$CRT_PRE_C_FLG_PRE <- as.factor(dati$CRT_PRE_C_FLG_PRE)
dati$CRT_PRE_18_FLG_PRE <- as.factor(dati$CRT_PRE_18_FLG_PRE)
dati$FIND_PPQ18SS_MONTH_DAT_MAX_FIN <- as.factor(dati$FIND_PPQ18SS_MONTH_DAT_MAX_FIN)

tipo_variabile <- sapply(dati %>% select(-y), class)
table(tipo_variabile)
(var_qualitative <- names(dati %>% select(-y))[tipo_variabile == "factor" | tipo_variabile == "character"])
(var_quantitative <- setdiff(names(dati %>% select(-y)), var_qualitative))

#var con tra var con troppe o poche modalità 
(n_unici <- apply(dati[,var_quantitative], 2, function(x) length(unique(x))))

save(dati, file = 'backup_banca.Rdata') #salvo dati puliti


# Stima-verifica ----------------------------------------------------------

dati$dataset <- "stima"
set.seed(1234)
dati$dataset[sample(1:nrow(dati), nrow(dati)*0.25)] <- "verifica"
table(dati$dataset)

# Verifico bilanciamento nella stima:
table(dati %>% filter(dataset == "stima") %>% pull(y))
table(dati %>% filter(dataset == "stima") %>% pull(y)) %>% barplot(., main = "", xlab = "Modalita'", ylab = "Frequenze assolute")
prop.table(table(dati %>% filter(dataset == "stima") %>% pull(y))) %>% barplot(., main = "", xlab = "Modalita'", ylab = "Frequenze relative")

S <- 0.5
# bilanciamento: 
s1 = dati %>% filter(dataset=="stima") %>% filter(y == 1)
s0 = dati %>% filter(dataset=="stima") %>% filter(y != 1)
set.seed(25)

acaso = sample(1:nrow(s0), nrow(s0)*0.3)

s0.s = s0[acaso, ]
dati = as.data.frame(rbind(dati %>% filter(dataset=="verifica"), s0.s, s1))

prop.table(table(dati %>% filter(dataset == "stima") %>% pull(y)))

table(dati %>% filter(dataset == "stima") %>% pull(y)) %>% barplot(., main = "", xlab = "Modalita'", ylab = "Frequenze relative")

tipo_var = sapply(dati %>% select(-c(y,dataset)), class)
var_qualitative = names(dati %>% select(-c(y,dataset)))[tipo_var =="factor"]
var_quantitative = setdiff(names(dati %>% select(-c(y,dataset))), var_qualitative)
var_qualitative
var_quantitative


# Controllo che in verifica non ci siano modalità non presenti in stima:
for(col in var_qualitative){
  if(!(all(unique(dati[dati$dataset == "verifica", col]) %in%
           unique(dati[dati$dataset == "stima", col])))){
    cat(col,"-> in verifica sono presenti modalità non presenti in stima\n")
  }
}


#correlazioni
matrice_correlazione <- dati %>%
  filter(dataset == "stima") %>%
  select(!all_of(c(var_qualitative, "y", "dataset"))) %>%
  cor()

matrice_correlazione[lower.tri(matrice_correlazione, diag = T)] <- NA
correlazioni <- data.frame(var1 = rep(colnames(matrice_correlazione), ncol(matrice_correlazione)),
                           var2 = rep(rownames(matrice_correlazione), each = nrow(matrice_correlazione)),
                           cor = as.vector(matrice_correlazione))
correlazioni <- na.omit(correlazioni)
correlazioni %>% filter(cor < -0.95 | cor > 0.95)
correlazioni %>% filter(cor < -0.70 | cor > 0.70)

# Standardizzo esplicative quantitative, matrici del modello e formule

dati[dati$dataset == "stima",var_quantitative] <- dati[dati$dataset == "stima",] %>% select(var_quantitative) %>% scale 
dati[dati$dataset == "verifica",var_quantitative] <- dati[dati$dataset == "verifica",] %>% select(var_quantitative) %>% scale


# Creo matrice del modello con tutto il dataset
# (poi mi prendero' stima o verifica, tanto e' uguale a farlo separato o unito)
xmat.model <- model.matrix(~.-1, data = dati %>% select(-c(y, dataset)))
# tolgo ora intercetta

# Salvo nomi delle variabili:
varnames <- colnames(dati %>% select(-c(y, dataset)))
# Creo formula con tutte le variabili:
form.tuttevars <- paste("y ~", paste(varnames,collapse = " + "), collapse = NULL)
# Salvo nomi delle variabili della matrice del modello:
varnames2 <- colnames(xmat.model)

# Vettore che mi servira' in seguito:
yy <- ifelse(dati$y == "1", 1, 0)


# Modellazione ------------------------------------------------------------

# Funzione per LIFT e ROC:

lift_roc<- function(previsti, g, type="bin", plot.it=TRUE){
  
  library(sm)
  if(!is.numeric(g)) stop("g not numeric")
  ind <- rev(order(previsti))
  n <- length(g)
  x1 <-  (1:n)/n
  x2 <- cumsum(g[ind])/(mean(g)*(1:n))
  if(type=="crude" & plot.it) 
    plot(x1, x2, type="l", col=2,
         xlab="frazione di soggetti previsti", ylab="lift")
  if(type=="sm") {
    a<- sm.regression(x1, x2, h=0.1, display="none")
    if(plot.it)
      plot(a$eval, a$estimate, type="l",xlim=c(0,1), col=2,
           xlab="frazione di soggetti previsti", ylab="lift")
  }
  if(type=="bin") {
    b <-  binning(x1,x2, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    if(plot.it) plot(x, c(x2[1],b$means,1), type="b", xlim=c(0,1),
                     ylim=c(1,max(x2)), cex=0.75, col=2,
                     xlab="frazione di soggetti previsti",
                     ylab="fattore di miglioramento")
    
    x1<- x
    x2<- c(x2[1],b$means,1)
  }
  if(plot.it) {cat("premere <cr>"); readline()}
  u1<- cumsum(1-g[ind])/sum(1-g)
  u2<- cumsum(g[ind])/sum(g)
  if(type=="crude" & plot.it)
    plot(u1, u2, type="l", xlim=c(0,1), ylim=c(0,1), col=2,
         xlab="1-specificita`", ylab="sensibilita`")
  if(type=="sm") {
    # browser()
    eps<- 0.00001
    a<- sm.regression(u1,log((u2+eps)/(1-u2+2*eps)), h=0.1, display="none")
    q<- exp(a$estimate)/(1+exp(a$estimate))
    if(plot.it) plot(a$eval, q, type="l", xlim=c(0,1), ylim=c(0,1),
                     xlab="1-specificita`", ylab="sensibilita`", col=2)
  }
  if(type=="bin") {
    b <- binning(u1,u2, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    y<- c(0,b$means,1)
    if(plot.it)
      plot(x, y, type="b", xlim=c(0,1),
           ylim=c(0,1),cex=0.75, xlab="1-specificita`",
           ylab="sensibilita`", col=2)
    u1<- x
    u2<- y
  }                      
  if(plot.it) {
    abline(0,1, lty=2, col=3)
  }
  invisible(list(x1,x2,u1,u2))
}

# Metrica di errore:

lift_00625 <- function(previsti, g) {
  # previsti = probabilità previste dal modello
  # g = variabile target binaria (0/1)
  n <- length(g)
  k <- ceiling(0.0625 * n)   # 6,25% dei clienti
  ind <- order(previsti, decreasing = TRUE) #prob in ordine decres
  top_successi <- sum(g[ind][1:k])
  
  media <- mean(g) #tasso di successo medio in tutta la popolazione
  top_rate <- top_successi / k #tasso di successo nei top selezionati
  # lift = rapporto tra tasso top e tasso medio
  lift <- top_rate / media
  return(lift)
}

valore_lift <- list()

# ridge -----------------------------------------

library(glmnet)
# Creo i folds per la convalida incrociata:
set.seed(42)
K = 5
myfolds <- sample(1:K, NROW(dati[dati$dataset == "stima",]), replace = T)

set.seed(25)

lambda.grid <- 10^seq(-3.5, 1, length = 300) 
m.ridge <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     yy[dati$dataset == "stima"], lambda = lambda.grid,
                     alpha = 0, standardize = F, foldid = myfolds, trace.it = 1)

plot(m.ridge)
m.ridge$lambda.min

plot(m.ridge$glmnet.fit, xvar = "lambda")

title("Ridge", line = 2.5)
abline(v = log(m.ridge$lambda.min), lty = 2)
abline(v = log(m.ridge$lambda.1se), lty = 3)
legend('topright', c('log(lambda.min)', 'log(lambda.1se)'), col = c(1,1), lty = c(2,3))


# Coefficienti relativi al valore di lambda con errore minore:
coef(m.ridge, s="lambda.min")

# Previsioni sull'insieme di verifica:
p.ridge = predict(m.ridge, newx = xmat.model[dati$dataset == "verifica",], s = "lambda.min")


valore_lift$Lineare_Ridge = lift_00625(p.ridge, dati$y[dati$dataset=='verifica'])
valore_lift

#lasso ------------------------------------

library(glmnet)

set.seed(25)
# Stima sull'insieme di stima:
m.lasso <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     yy[dati$dataset == "stima"],
                     alpha = 1, standardize = F, foldid = myfolds, trace.it = 1)

lambda.grid <- 10^seq(-3.5, 0, length = 300) 
m.lasso <- cv.glmnet(xmat.model[dati$dataset == "stima",],
                     yy[dati$dataset == "stima"], lambda = lambda.grid,
                     alpha = 1, standardize = F, foldid = myfolds, trace.it = 1)


plot(m.lasso)
m.lasso$lambda.min
length(coef(m.lasso))

plot(m.lasso$glmnet.fit, xvar = "lambda")

title("Lasso", line = 2.5)
abline(v = log(m.lasso$lambda.min), lty = 2)
abline(v = log(m.lasso$lambda.1se), lty = 3)
legend('topright', c('log(lambda.min)', 'log(lambda.1se)'), col = c(1,1), lty = c(2,3))



# Coefficienti relativi al valore di lambda con errore minore:
coef(m.lasso, s = "lambda.min")
beta_lasso = m.lasso$glmnet.fit$beta[,which(m.lasso$lambda == m.lasso$lambda.min)]
# Variabili selezionate dal lasso (lambda min):
varlasso <- varnames2[beta_lasso != 0]
varlasso #variabili selezionate dal lasso
setdiff(varnames2, varlasso) # variabili non selezionate dal lasso

# Previsioni sull'insieme di verifica:
p.lasso = predict(m.lasso, newx = xmat.model[dati$dataset == "verifica",], s = "lambda.min")


valore_lift$Lineare_Lasso = lift_00625(p.lasso, dati$y[dati$dataset=='verifica'])
valore_lift


# Modello Additivo con var selezionate lasso ------------------------------

library(gam)

# Solo var (quant e qual) selezionate dal lasso
# Splines di lisciamento con 3gdl equivalenti per tutte le quantitative

# Creazione formula per funzione gam:
form.gam2 = character(0)
for (i in 1:length(var_quantitative)){
  if (var_quantitative[i] %in% varlasso){
    form.gam2 = c(form.gam2, paste0("s(",var_quantitative[i],", df = 3)",collapse=NULL))
  }
}
gam.quant = paste0(form.gam2, collapse="+")
gam.qual = paste0(var_qualitative, collapse="+")
formula.gam2 = as.formula(paste("y~",paste0(c(gam.qual,gam.quant),collapse="+"),collapse = NULL))

# Stima nell'insieme di stima:
m.gam.sel = gam(formula.gam2, data = dati %>% filter(dataset == "stima"), family = "binomial")

summary(m.gam.sel)
#plot(m.gam, se = T)

# Previsioni sull'insieme di verifica:
p.gam.sel = predict(m.gam.sel, newdata = dati %>% filter(dataset == "verifica"), type = "response")

valore_lift$GAM.sel = lift_00625(p.gam.sel, dati$y[dati$dataset=='verifica'])
valore_lift


# MARS --------------------------------------------------------------------

library(polspline)


# Divisione in stima-convalida 
set.seed(25)
cb1 <- sample(1:NROW(dati[dati$dataset == "stima",]), (2/3)*NROW(dati[dati$dataset == "stima",]))
cb2 <- setdiff(1:NROW(dati[dati$dataset == "stima",]), cb1)

min(6*(nrow(dati)^(1/3)),nrow(dati)/4,100) #maxsize di default:
min(20, round(nrow(dati)/4)) #knot di default


# Stima sull'insieme di stima:
set.seed(25)
m.mars <- polymars(dati$y[dati$dataset == "stima"][cb1],
                   xmat.model[dati$dataset == "stima",][cb1,],
                   ts.resp = dati$y[dati$dataset == "stima"][-cb1],
                   ts.pred = xmat.model[dati$dataset == "stima",][-cb1,],
                   classify = T)


str(m.mars)
# m.mars

# Modello finale:
m.mars$model

(J3 <- m.mars$fitting$size[which.min(m.mars$fitting$`RSS 2`)])

# Grafico dell'andamento della RSS al variare di size, in fase di crescita e di potatura:
plot(m.mars$fitting$size, m.mars$fitting$RSS, 
     col = m.mars$fitting$"0/1"+2, pch = 16, cex = 0.7,
     xlab = "n. funz. base", ylab = "RSS")
legend( "topright", c("In avanti", "All'indietro"), pch = 16,
        col = unique(m.mars$fitting$"0/1")+2)
title('MARS: Andamento RSS al variare della taglia size')


# Grafico dell'andamento della RSS al variare di GCV, in fase di crescita e di potatura:
plot(m.mars$fitting$size, m.mars$fitting$GCV, 
     col = m.mars$fitting$"0/1"+1, pch = 16, cex = 0.7,
     xlab = "n. funz. base", ylab = "GCV")
legend( "topright", c("In avanti", "All'indietro"), pch = 16,
        col = unique(m.mars$fitting$"0/1")+1)
#abline(v = J3)
title('MARS: Andamento GCV al variare della taglia size')
######### Grafico da includere nel report.



# Variabili che interagiscono:
int <- which(m.mars$model[,3] != 0)
m.mars$model[int, c(1,3)]
cbind(colnames(xmat.model)[m.mars$model[int,1]], colnames(xmat.model)[m.mars$model[int,3]])

# Previsioni sull'insieme di verifica:
p.mars <- predict(m.mars, x = xmat.model[dati$dataset == "verifica",])[,1]


valore_lift$mars = lift_00625(p.mars, dati$y[dati$dataset=='verifica'])
valore_lift

#r.mars = lift_roc(p.mars, yy[dati$dataset=="verifica"],type="bin")

# Boosting ----------------------------------------------------------------


library(ada)

set.seed(25)
# Stima nell'insieme di stima e convalida:
tic('Boosting')
m.boost = ada(y~., data = (dati %>% filter(dataset=="stima") %>% select(-dataset))[cb1,], 
              test.x = (dati %>% filter(dataset=="stima") %>% select(-c(y,dataset)))[-cb1,], 
              test.y = (dati %>% filter(dataset=="stima") %>% pull(y))[-cb1],
              iter = 400, verbose=T)
toc()
#94 sec con 50 iter
#307 sec con 150 iter
#421 sec con 200 iter

# da modificare guardando plot(m.boost)

# Grafico errore nel dataset di stima (rosso) e di convalida (verde):
plot(m.boost, test = T)

# Importanza delle variabili:
varplot(m.boost)


#m.boost <- readRDS('boost.rds')
# Previsioni sull'insieme di verifica:
p.boost = predict(m.boost, newdata = dati %>% filter(dataset == "verifica") %>% select(-dataset),
                  type = "prob")[,2]

valore_lift$boost = lift_00625(p.boost, dati$y[dati$dataset=='verifica'])
valore_lift


# Boosting con Stumps -----------------------------------------------------

# Il comando, in caso, e':
m.boost.stump = ada(y~., data = (dati %>% filter(dataset=="stima") %>% select(-dataset))[cb1,], 
                    test.x = (dati %>% filter(dataset=="stima") %>% select(-c(y,dataset)))[-cb1,], 
                    test.y = (dati %>% filter(dataset=="stima") %>% pull(y))[-cb1],
                    control = rpart.control(maxdepth=1, cp=-1, minsplit=0, xval=0),
                    iter = 600)

# Se l'errore non cambia rispetto a prima, vuol dire che non ci sono interazioni.

plot(m.boost.stump, test = T)

# Previsioni sull'insieme di verifica:
p.boost.stump = predict(m.boost.stump, newdata = dati %>% filter(dataset == "verifica") %>% select(-dataset),
                        type = "prob")[,2]

valore_lift$boost_stump = lift_00625(p.boost.stump, dati$y[dati$dataset=='verifica'])
valore_lift


# Support Vector Machines -------------------------------------------------

library(e1071)

# SVM con nucleo radiale (default)

# Scelgo il parametro di regolazione migliore in base al lift:
ranges = 2:13

err_svm = matrix(NA,nrow = length(ranges),2)
colnames(err_svm) = c("cost","lift")
set.seed(42)

tic('SVM')
for (i in 1:length(ranges)){
  cat("indice:",i,"  misura di costo:", ranges[i],"\n")
  s1 = svm(factor(y)~., 
           data = (dati %>% filter(dataset=="stima") %>% select(-dataset))[cb1,],
           cost = ranges[i],
           probability = T)
  pr1 = predict(s1,
                newdata = (dati %>% filter(dataset=="stima") %>% select(-dataset))[-cb1,],
                probability = T)
  pr = attr(pr1, "probabilities")[,1]
  uso = lift_00625(pr, dati$y[dati$dataset=='stima'][-cb1])
  err_svm[i,] = c(ranges[i], uso)
}
toc() #3321 sec = 55 min!!! a Bolzano (calcolando il tasso di errata classificazione)
# 8.3 minuti a casa (con le modifiche) !!

# Grafico:
plot(err_svm, type="b")

## Stima sull'insieme di stima con cost ottimo:
bestcost = ranges[which.max(err_svm[,2])]
bestcost
abline(v=bestcost, col = 2)
title('SVM: lift al variare della misura di costo')

set.seed(42)
m.svm = svm(factor(y)~., data = dati %>% filter(dataset == "stima") %>% select(-dataset),
            cost = bestcost, probability = T)
# trasforma le stime in probabilita'

# Previsioni sull'insieme di verifica:
#m.svm <- readRDS('svm.rds')
p.svm = predict(m.svm,
                newdata = dati %>% filter(dataset == "verifica") %>% select(-dataset),
                decision.values = T, probability = T)
p.svm = attr(p.svm,"probabilities")[,2]

valore_lift$svm = lift_00625(p.svm, dati$y[dati$dataset=='verifica'])
valore_lift

# Albero di Classificazione -----------------------------------------------

library(tree)

# Creo i folds per la convalida incrociata (se non gia' creati prima):
set.seed(42)
K <- 5
myfolds <- sample(1:K, NROW(dati[dati$dataset == "stima",]), replace = T)


# Crescita e potatura mediante convalida incrociata:
set.seed(25)
complexity = 2:100 #40 numero max di foglie che voglio, valutare dopo dal plot di complexity
lift.tree = matrix(NA, nrow = length(complexity), ncol = K)
sss <- dati %>% filter(dataset=="stima") %>% select(-dataset)


tic('Albero')
for (i in 1:K) {
  mtree = tree(factor(y) ~ ., data = sss[myfolds != i, ],
               control = tree.control(nobs = NROW(sss[myfolds != i, ]),
                                      minsize = 15,
                                      mindev = 0.001),
               split = "deviance")
  for (j in 1:length(complexity)) {
    tp = prune.tree(mtree, best = complexity[j])
    pp = predict(tp, newdata = sss[myfolds == i, ])
    pp_pos = pp[,'1']
    lift.tree[j, i] = lift_00625(pp_pos, sss$y[myfolds == i])
  }
}
toc()


# Andamento della devianza media in funzione di size:
plot(x = complexity, y = apply(lift.tree, 1, mean), type = "b",
     xlab = "Numero foglie", ylab = "Lift", main = "Albero di Classificazione")
# Evidenzio il valore di size per cui la devianza e' minima:
best_j <- which.max(apply(lift.tree, 1, mean))
best_j
#(best_j) + 1 foglie
abline(v = (best_j +1), lty = 2, col = 2)
# Grafico da includere nel report

# Albero finale:
m.tree <- tree(factor(y)~., data = (dati %>% filter(dataset=="stima") %>% select(-dataset)),
               control = tree.control(nobs = nrow(dati[dati$dataset=="stima",]),
                                      minsize = 15,
                                      mindev = 0.001))
m.tree.best <- prune.tree(m.tree, best = (best_j+1))
plot(m.tree.best)
text(m.tree.best, cex = 0.7, pretty = 5, srt=90)
# Grafico da includere nel report

# Previsioni sull'insieme di verifica:
p.tree.cv = predict(m.tree.best, newdata = dati %>% filter(dataset=="verifica"), type = "vector")[,2]

valore_lift$albero = lift_00625(p.tree.cv, dati$y[dati$dataset=='verifica'])
valore_lift


# Analisi Discriminante Lineare -------------------------------------------

library(MASS)

var_qualitative
var_quantitative

#formula con sono quantitative
form.quant <- paste("y ~", paste(var_quantitative, collapse = " + "), collapse = NULL)

# Stima sull'insieme di stima:
m.lda = lda(formula(form.quant), data = dati %>% filter(dataset == "stima"))

m.lda

# Previsioni sull'insieme di verifica:
p.lda = predict(m.lda, newdata = dati %>% filter(dataset == "verifica"))$posterior[,2]


valore_lift$LDA = lift_00625(p.lda, dati$y[dati$dataset=='verifica'])
valore_lift


var_quantitative
varlasso

vars <- intersect(var_quantitative, varlasso)

#formula quantitative + sel lasso
form.2 <- paste("y ~", paste(vars, collapse = " + "), collapse = NULL)

# Stima sull'insieme di stima:
m.lda.ridotto = lda(formula(form.2), data = dati %>% filter(dataset == "stima"))

# Previsioni sull'insieme di verifica:
p.lda.ridotto = predict(m.lda.ridotto, newdata = dati %>% filter(dataset == "verifica"))$posterior[,2]


valore_lift$LDA.ridotto = lift_00625(p.lda.ridotto, dati$y[dati$dataset=='verifica'])
valore_lift

# Analisi Discriminante Quadratica ----------------------------------------

library(MASS)

# Stima sull'insieme di stima:
m.qda = qda(formula(form.2), data = dati %>% filter(dataset == "stima"))
# Mentre la LDA dava solo un warning, qui da' proprio errore.
# In tal caso occorre per forza usare le variabili selezionate.

m.qda

# Previsioni sull'insieme di verifica:
p.qda = predict(m.qda, newdata = dati %>% filter(dataset == "verifica"))$posterior[,2]

valore_lift$QDA.ridotto = lift_00625(p.qda, dati$y[dati$dataset=='verifica'])
valore_lift


# Risultati ---------------------------------------------------------------

valore_lift

