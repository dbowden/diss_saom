library(RSiena)
library(dplyr)
library(tidyr)
library(igraph)

# import variables -----------

## get node entry/exit 
comp <- read.csv("../../Network_Creation/nodes/palestine_nodes.csv")

#create an object with list of nodes active in each period
c0 <- subset(comp, start==0)
c1 <- subset(comp, start <= 1 & end >= 1)
c2 <- subset(comp, start <= 2 & end>=2)
c3 <- subset(comp, start <= 3 & end >= 3)

rm(comp)

#create a list of all nodes that appear at some point
v <- 1:22

## get ties
edges <-read.csv("../../Network_Creation/edges/palestine_ege_events_waves.csv")

edges <- edges[,1:3]

e0 <- filter(edges, wave==0)
e0 <- graph.data.frame(e0[,1:2], vertices=v, directed=T)
e0 <- get.adjacency(e0, sparse = F)
e0[!(rownames(e0) %in% c0$srcnum),] <- 10
e0[,!(colnames(e0) %in% c0$srcnum)] <- 10

e1 <- filter(edges, wave==1)
e1 <- graph.data.frame(e1[,1:2], vertices=v, directed=T)
e1 <- get.adjacency(e1, sparse = F)
e1[!(rownames(e1) %in% c1$srcnum),] <- 10
e1[,!(colnames(e1) %in% c1$srcnum)] <- 10

e2 <- filter(edges, wave==2)
e2 <- graph.data.frame(e2[,1:2], vertices=v, directed=T)
e2 <- get.adjacency(e2, sparse = F)
e2[!(rownames(e2) %in% c2$srcnum),] <- 10
e2[,!(colnames(e2) %in% c2$srcnum)] <- 10

e3 <- filter(edges, wave==3)
e3 <- graph.data.frame(e3[,1:2], vertices=v, directed=T)
e3 <- get.adjacency(e3, sparse = F)
e3[!(rownames(e3) %in% c3$srcnum),] <- 10
e3[,!(colnames(e3) %in% c3$srcnum)] <- 10

edges <- sienaDependent(array(c(e0,e1,e2,e3), dim = c(22, 22, 4)))

rm(e0,e1,e2,e3,c0,c1,c2,c3,v)

## get violence dv 
dv <- read.csv("../../Network_Creation/dv/palestine_dv.csv")

dv$dv <- ifelse(dv$isr > 0, 1, 0)

dv <- subset(dv, select=c(srcnum, wave, dv))

#create empty frame
src <- 1:22
wave <- seq(0,3)

frame <- data.frame(srcnum = numeric(), wave = numeric())

for(i in src){
  srcnum = rep(i, length(wave))
  x = cbind(srcnum, wave)
  frame = rbind(frame, x)
}

dv <- merge(frame, dv, all=T)
rm(frame, src, srcnum, wave, x, i)

dv$dv[is.na(dv$dv)==T] <- 0

#convert to wide
dv <- spread(dv, wave, dv)

dv <- as.matrix(dv[,2:5])

conflict <- sienaDependent(dv, type = "behavior")
rm(dv)

# specify model effects ------------------

pal <- sienaDataCreate(conflict, edges, comp)
rm(comp, conflict, edges)

eff <- getEffects(pal)

# homophily for conflict
eff <- includeEffects(eff, sameX, interaction1 = 'conflict', name = 'edges')

# alter effect for conflict
eff <- includeEffects(eff, altX, interaction1 = 'conflict', name = 'edges')

# ego effect for conflict
eff <- includeEffects(eff, egoX, interaction1 = 'conflict', name = 'edges')

# transitivity and reciprocity
eff <- includeEffects(eff, recip, transTrip, name = 'edges')

alg <- sienaAlgorithmCreate(projname = 'palestine')

# run model ------------------------
#print01Report(syr, eff, modelname = 'syr_sum')

s1 <- siena07(alg, data = pal, effects = eff, batch = T, initC = T, useCluster = T, nbrNodes = 3)
s1

#conflict alter has sig neg effect on tie formation

# take another stab ----

eff2 <- getEffects(pal)

eff2 <- includeEffects(eff2, transRecTrip,  name='edges')

eff2 <- includeEffects(eff2, inPop, name='edges')

eff2 <- includeEffects(eff2, sameX, interaction1='conflict', type='creation', name='edges')

s2 <- siena07(alg, data=pal, effects=eff2, batch=T, initC=T, useCluster=T, nbrNodes=3, returnDeps=T)
s2

# m3 ----

eff3 <- getEffects(pal)

eff3 <- includeEffects(eff3, avSim, interaction1='edges', name='conflict')

s3 <-  siena07(alg, data=pal, effects=eff3, batch=T, initC=T, useCluster=T, nbrNodes=3, returnDeps=T)
s3
