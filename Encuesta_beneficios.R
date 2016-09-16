library(FactoMineR)
library(questionr)
library(amap) #crear la matriz de burt
library(profileR)
library(foreign)
library(psych)
library(knitr)
library(xtable)
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(dendextendRcpp))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
library(corrplot)
#library(Stuff)
suppressPackageStartupMessages(library(dplyr))
library(broom)
library(EnQuireR)
library(car)



encuesta <- BENEFICIOS...ENCUESTA
remove(BENEFICIOS...ENCUESTA)


rownames(otro)

summary(encuesta)

encuesta_v2 <- encuesta[,3:44]

----#preparacion de datos-----
encuesta_v2$PREPAGA <- factor(ifelse(encuesta_v2$PREPAGA==1,'SI','NO'))
encuesta_v2$IDIOMA <- factor(ifelse(encuesta_v2$IDIOMA==1,'SI','NO'))
encuesta_v2$GIMNASIO <- factor(ifelse(encuesta_v2$GIMNASIO==1,'SI','NO'))
encuesta_v2$FRUTA <- factor(ifelse(encuesta_v2$FRUTA==1,'SI','NO'))
encuesta_v2$PREPAGA <- factor(ifelse(encuesta_v2$PREPAGA==1,'SI','NO'))
encuesta_v2$LICENCIAS.EXTENDIDAS <- factor(ifelse(encuesta_v2$LICENCIAS.EXTENDIDAS==1,'SI','NO'))
encuesta_v2$HOME.OFFICE <- factor(ifelse(encuesta_v2$HOME.OFFICE==1,'SI','NO'))
encuesta_v2$VIERNES.OFF <- factor(ifelse(encuesta_v2$VIERNES.OFF==1,'SI','NO'))
encuesta_v2$FLEX.TIME <- factor(ifelse(encuesta_v2$FLEX.TIME==1,'SI','NO'))
encuesta_v2$DESCUENTOS.COMPRAS <- factor(ifelse(encuesta_v2$DESCUENTOS.COMPRAS==1,'SI','NO'))
colnames(encuesta_v2)[10] <- 'COMEDOR_TICK_COMIDA'
encuesta_v2$COMEDOR_TICK_COMIDA <- factor(ifelse(encuesta_v2$COMEDOR_TICK_COMIDA==1,'SI','NO'))
encuesta_v2$CHARTER <- factor(ifelse(encuesta_v2$CHARTER==1,'SI','NO'))
encuesta_v2$POSGRADOS.MBA <- factor(ifelse(encuesta_v2$POSGRADOS.MBA==1,'SI','NO'))
encuesta_v2$REINTEGRO.BONO.GASTO.ESCOLAR <- factor(ifelse(encuesta_v2$REINTEGRO.BONO.GASTO.ESCOLAR==1,'SI','NO'))
encuesta_v2$AJUAR <- factor(ifelse(encuesta_v2$AJUAR==1,'SI','NO'))
encuesta_v2$DESCUENTO.UNIVERSIDADES <- factor(ifelse(encuesta_v2$DESCUENTO.UNIVERSIDADES==1,'SI','NO'))
encuesta_v2$BONO.DE.FIN.DE.AÑO <- factor(ifelse(encuesta_v2$BONO.DE.FIN.DE.AÑO==1,'SI','NO'))
encuesta_v2$REINTEGRO.BONO.GASTO.ESCOLAR <- factor(ifelse(encuesta_v2$REINTEGRO.BONO.GASTO.ESCOLAR==1,'SI','NO'))
encuesta_v2$BONO.POR.DESEMPEÑO <- factor(ifelse(encuesta_v2$BONO.POR.DESEMPEÑO==1,'SI','NO'))
encuesta_v2$REGALOS.DIAS.FESTIVOS <- factor(ifelse(encuesta_v2$REGALOS.DIAS.FESTIVOS==1,'SI','NO'))
encuesta_v2$GUARDERIA <- factor(ifelse(encuesta_v2$GUARDERIA==1,'SI','NO'))
encuesta_v2$REINTEGRO.BONO.GASTO.ESCOLAR <- factor(ifelse(encuesta_v2$REINTEGRO.BONO.GASTO.ESCOLAR==1,'SI','NO'))


encuesta_matrix_v2 <- burt(encuesta_v2)

scatterplot.matrix(encuesta_matrix_v2)

res.ca.rows = CA(encuesta_matrix, invisible="col")
res.ca.col = CA(encuesta_matrix, invisible="row") 

factor_ana <- MCA(encuesta_v2[1:12])
factor_ana$eig

dimdesc(factor_ana)

summary(encuesta)

plotellipses(factor_ana)

#clustering


d2 = dist(encuesta_matrix,method = "euclidean")
encuesta.clust.sin = as.dendrogram(hclust(d2, method = "single")) %>% set("branches_lwd", 2)
encuesta.clust.com = as.dendrogram(hclust(d2, method = "complete")) %>% set("branches_lwd", 2)
encuesta.clust.avg = as.dendrogram(hclust(d2, method = "average")) %>% set("branches_lwd", 2)
encuesta.clust.ward = as.dendrogram(hclust(d2, method = "ward.D")) %>% set("branches_lwd", 2)
encuesta.dend = dendlist("Cercano" = encuesta.clust.sin, "Lejano" = encuesta.clust.com, "Promedio" = encuesta.clust.avg,"Ward"=encuesta.clust.ward)

corrplot(cor.dendlist(encuesta.dend), "pie", "lower")

plot(encuesta.clust.sin %>% set("branches_k_color", k=3) %>% set("branches_lwd", 2), main = "Cercano")

hclust(d2, method = "single")


plot(encuesta$monto,encuesta$edad)
plot(encuesta$monto,encuesta$edad,col=encuesta$edad)


#analisis encuesta

ENdensity(factor_ana)
dimensiones <- p_inertia(encuesta[,-7])

clustergente <- ENMCA(encuesta)
res.catdes <- catdes(encuesta, num.var=6)
res.catdes

#meter el cluster

clusterjer1 <- hclust(d2, method = "single")
cortegrupos <- cutree(clusterjer1,5)
encuesta$clust <- factor(cortegrupos)


res.enmca  = ENMCA(encuesta_v2[,1:12],report=TRUE)


res.enmca  = ENMCA(encuesta[,-7])




plot(encuesta$edad,encuesta$frecuencia)



