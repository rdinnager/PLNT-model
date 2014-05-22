setwd("C:/Users/din02g/Google Drive/Kimberley")
setwd("/home/din02g/Google Drive/Kimberley")

PlDat<-read.csv('Kimberley80_Plants.csv')

PlDat$SPEC<-paste(PlDat$GNAME,PlDat$SNAME,sep="_")
split<-strsplit(PlDat$SPEC," ", fixed=T)
PlDat$SPEC<-unlist(lapply(split,function(x) {x[1]}))
PlMat<-table(PlDat$QUADRAT,PlDat$SPEC)

specnames<-colnames(PlMat)
specnames[specnames=="Acacia_sp."]<-"Acacia_sp_Sect_Juliflorae"
specnames[specnames=="Bulbophyllum_sp.?"]<-"Bulbophyllum_sp."
specnames[specnames=="Corchorus_aff.capsularis"]<-"Corchorus_aff_capsularis"
specnames[specnames=="Cyathostemma_sp.1"]<-"Cyathostemma_sp1"
specnames[specnames=="Hypoestes_?sparsiflora"]<-"Hypoestes_sparsiflora1"
specnames[specnames=="Hypoestes_sparsiflora"]<-"Hypoestes_sparsiflora2"
specnames[specnames=="Livistona_sp"]<-"Livistona_sp_victoria"
specnames[specnames=="Omolonthus_novo-guineensis"]<-"Omolanthus_novo-guineensis"
specnames[specnames=="Pavetta_sp.(KFK"]<-"Pavetta_sp."
specnames[specnames=="Polygonum_sp"]<-"Polygonum_sp1"
specnames[specnames=="Polygonum_sp."]<-"Polygonum_sp2"
specnames[specnames=="Psychotria_affin."]<-"Psychotria_nesophila1"
specnames[specnames=="Psychotria_nesophila"]<-"Psychotria_nesophila2"
specnames[specnames=="Scleria_aff."]<-"Scleria_psilorrhiza"
specnames[specnames=="Trema_tomentosa"]<-"Trema_tomentosavar.viridus"
specnames[specnames=="Triumfetta_?denticulata"]<-"Triumfetta_denticulata"
specnames[specnames=="Triumfetta_affin."]<-"Triumfetta_reflexa"

colnames(PlMat)<-specnames
PlMat[PlMat>0]<-1

library(ape)
PlTree<-read.tree("Kimberley_Plants2.tre")
Tspecnames<-PlTree$tip.label
Tspecnames<-gsub("'","",Tspecnames,fixed=T)
#Tspecnames[Tspecnames=="'Brachychiton_sp.'"]<-"Brachychiton_sp."
#Tspecnames[Tspecnames=="'Bulbophyllum_sp.'"]<-"Bulbophyllum_sp."
#Species to Exclude (Bryophytes?)c("Isopterygium_sp.","Octoblepharum_albidum")


PlTree$tip.label<-Tspecnames
PlMat[,c("Isopterygium_sp.","Octoblepharum_albidum")]<-0

library(picante)

Plchrono1<-PlTree
Plchrono1$edge.length[Plchrono1$edge.length==0]<-0.001
Plchrono<-chronopl(Plchrono1,1)

write.csv(PlMat,file="KimPlantComm.csv")
write.tree(Plchrono,file="KimPlantChrono.tre")



PDPL<-psv(PlMat,Plchrono)
PDPL2<-newpsv(PlMat,Plchrono)

#Birds
bdiet<-read.csv("diets.csv",stringsAsFactors=F)
BdDat<-read.csv('Kimberley80_Birds.csv')
BdDat$SPEC<-paste(BdDat$SNAME,BdDat$GNAME,sep=" ")
BdMat<-table(BdDat$QUADRAT,BdDat$SPEC)

Bleg<-read.csv("BLeg.csv")
Bspecnames<-colnames(BdMat)
rownames(Bleg)<-Bleg$CName
Lspecnames<-Bleg[Bspecnames,2]
Lspecnames<-gsub(" ","_",Lspecnames)

BdTree<-read.tree("Kimberley_Birds.tre")

BTspecnames<-BdTree$tip.label

Lspecnames[Lspecnames=="Sphecotheres_viridis_flaviventris"]<-"Sphecotheres_viridis"
Lspecnames[Lspecnames=="Gerygone_chloronotus"]<-"Gerygone_chloronota"
Lspecnames[Lspecnames=="Artamus_leucorynchus"]<-"Artamus_leucorhynchus"

colnames(BdMat)<-as.character(Lspecnames)

BTspecnames[BTspecnames=="Sphecotheres_vieilloti"]<-"Sphecotheres_viridis"
BTspecnames[BTspecnames=="Trichoglossus_haematodus"]<-"Trichoglossus_rubritorquis"
BTspecnames[BTspecnames=="Ducula_bicolor"]<-"Ducula_spilorrhoa"
BTspecnames[BTspecnames=="Psitteuteles_goldiei"]<-"Psitteuteles_versicolor"

BdTree$tip.label<-BTspecnames

Bdchrono1<-BdTree
Bdchrono1$edge.length[Bdchrono1$edge.length==0]<-0.001
Bdchrono<-chronopl(Bdchrono1,1)

PDBD<-psv(BdMat,Bdchrono)
PDBD2<-newpsv(BdMat,Bdchrono)

PDPL2<-newpsv(PlMat,Plchrono)

bdiet$Binomial_Name[bdiet$Binomial_Name=="Sphecotheres_viridis_flaviventris"]<-"Sphecotheres_viridis"
bdiet$Binomial_Name[bdiet$Binomial_Name=="Gerygone_chloronotus"]<-"Gerygone_chloronota"
bdiet$Binomial_Name[bdiet$Binomial_Name=="Artamus_leucorynchus"]<-"Artamus_leucorhynchus"

pbirdnames<-as.character(bdiet$Binomial_Name[bdiet$Plant.based.diet==1])
pbirdnames<-pbirdnames[pbirdnames %in% Lspecnames]
#birdnames[birdnames=="Sphecotheres_viridis_flaviventris"]<-"Sphecotheres_viridis"
#birdnames[birdnames=="Artamus_leucorynchus"]<-"Artamus_leucorhynchus"
#birdnames[birdnames=="Lichenostomus_plumulus"]<-"Lichenostomus_flavescens"

abirdnames<-as.character(bdiet$Binomial_Name[bdiet$Animal.based.Diet==1])
abirdnames<-abirdnames[abirdnames %in% Lspecnames]

#abirdnames[abirdnames=="Sphecotheres_viridis_flaviventris"]<-"Sphecotheres_viridis"
#abirdnames[abirdnames=="Artamus_leucorynchus"]<-"Artamus_leucorhynchus"
#abirdnames[abirdnames=="Lichenostomus_plumulus"]<-"Lichenostomus_flavescens"
#abirdnames[abirdnames=="Rhipidura_albiscapa"]<-"Rhipidura_leucophrys"
#abirdnames[abirdnames=="Gerygone_chloronotus"]<-"Gerygone_levigaster"
#abirdnames[abirdnames=="Microeca_fascinans"]<-"Microeca_flavigaster"
#abirdnames[abirdnames=="Pachycephala_lanioides"]<-"Pachycephala_rufiventris"
#abirdnames<-abirdnames[-which(abirdnames=="Alcedo_azurea")]
#abirdnames<-abirdnames[-which(abirdnames=="Monarcha_melanopsis")]

BDMAT<-BdMat[,pbirdnames]
aBDMAT<-BdMat[,abirdnames]

write.csv(BDMAT,file="KimBirdPlantComm.csv")
write.csv(aBDMAT,file="KimBirdAnimalComm.csv")
write.tree(Bdchrono,file="KimBirdChrono.tre")

PPDBD<-psv(BDMAT,Bdchrono)
aPDBD<-psv(aBDMAT,Bdchrono)

PPDBD2<-ses.mpd(BDMAT,cophenetic(BdTree))
aPDBD2<-psv(aBDMAT,BdTree)

PPDBD$diet<-"Plant-based"
aPDBD$diet<-"Animal-based"
dat1<-cbind(PPDBD,PDPL)
dat2<-cbind(aPDBD,PDPL)
colnames(dat1)<-c("BPSV","BSR","BVAR","diet","PPSV","PSR","PVAR")
colnames(dat2)<-c("BPSV","BSR","BVAR","diet","PPSV","PSR","PVAR")
newdat<-rbind(dat1,dat2)

MOD<-lm(PPDBD$PSVs~scale(PDPL$PSVs,scale=F)*scale(PDPL$SR,scale=F))
MOD2<-lm(PPDBD$PSVs~scale(PDPL$PSVs,scale=F)+scale(PDPL$SR,scale=F))

MOD<-lm(aPDBD$PSVs~scale(PDPL$PSVs,scale=F)*scale(PDPL$SR,scale=F))
plot(aPDBD$PSVs~PDPL$PSVs)
curve(0.27+1.53706*x-2.46714*x^2,add=T)


MOD<-lm(newdat$BPSV~scale(newdat$PPSV,scale=F)*scale(newdat$PSR,scale=F)*newdat$diet)
summary(MOD)


#Plots
plot(Plchrono,type="f",font=4,cex=0.75,edge.width=3)
plot(Bdchrono,type="f",font=4,cex=1.2,edge.width=3)

plot(PPDBD$PSVs~PDPL$PSVs,cex=0.01*PDPL$SR)

#Partial Leverage plot
Bpsv<-lm(PPDBD$PSVs~scale(PDPL$SR)+scale(PDPL$SR):scale(PDPL$PSVs))
Ppsv<-lm(PDPL$PSVs~scale(PDPL$SR)+scale(PDPL$SR):scale(PDPL$PSVs))

res<-resid(Bpsv)+coef(Bpsv)[1]
pred<-resid(Ppsv)+coef(Ppsv)[1]

plot(res~pred,pch=19,ylab="Phylogenetic Diversity of Birds with Plant-based Diets",xlab="Phylogenetic Diversity of Perrenial Plants",cex=2)
abline(lm(res~pred))

dat<-as.data.frame(cbind(res,pred))
ggplot( dat, aes( y=res, x=pred ) ) +
  geom_point(shape=21,size=5,bg="black" ) +
  ylab("Phylogenetic Diversity of Birds with Plant-based Diets") + xlab("Phylogenetic Diversity of Perennial Plants")+
  stat_smooth( method="lm", colour="black",size=1.2,fullrange=F)+theme_bw()+
  theme(axis.title.x = element_text(  size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),axis.title.y = element_text( size=20),
        axis.text.y  = element_text(vjust=0.5, size=16),legend.text = element_text( size = 16))


aBpsv<-lm(aPDBD$PSVs~scale(PDPL$SR)+scale(PDPL$SR):scale(PDPL$PSVs))
aPpsv<-lm(PDPL$PSVs~scale(PDPL$SR)+scale(PDPL$SR):scale(PDPL$PSVs))

res<-resid(aBpsv)+coef(aBpsv)[1]
pred<-resid(aPpsv)+coef(aPpsv)[1]

plot(res~pred,pch=19,ylab="Phylogenetic Diversity of Birds with Animal-based Diets",xlab="Phylogenetic Diversity of Perennial Plants",cex=2)
abline(lm(res~pred))

dat<-as.data.frame(cbind(res,pred))
ggplot( dat, aes( y=res, x=pred ) ) +
  geom_point(shape=21,size=5,bg="black" ) +
  ylab("Phylogenetic Diversity of Birds with Plant-based Diets") + xlab("Phylogenetic Diversity of Perennial Plants")+
  stat_smooth( method="lm", colour="black",size=1.2,fullrange=F)+theme_bw()+
  theme(axis.title.x = element_text(  size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),axis.title.y = element_text( size=20),
        axis.text.y  = element_text(vjust=0.5, size=16),legend.text = element_text( size = 16))



MOD<-lm(PPDBD$PSVs~scale(PDPL$PSVs)*scale(PDPL$SR))
summary(MOD)
plot(PPDBD$PSVs~I(PDPL$PSVs*PDPL$SR))

MOD<-lm(PPDBD$PSVs~scale(PDPL$PSVs)*scale(PDPL$SR)*I(scale(PDPL$PSVs)^2)*I(scale(PDPL$SR)^2))
summary(MOD)

PSRPL<-psr(PlMat,PlTree)
PSRBD<-psr(BdMat,BdTree)

MOD<-lm(PPDBD$SR~scale(PDPL$PSVs)*scale(PDPL$SR))
summary(MOD)

MOD<-lm(aPDBD$SR~scale(PDPL$PSVs)*scale(PDPL$SR))
summary(MOD)

MOD<-lm(PPDBD$PSVs~scale(log(PDPL$PSVs))*scale(log(PDPL$SR)))
summary(MOD)

newpsv<-function (samp, tree, compute.var = TRUE) 
{
  samp[samp > 0] <- 1
  flag = 0
  if (is.null(dim(samp))) {
    samp <- rbind(samp, samp)
    flag = 2
  }
  if (is(tree)[1] == "phylo") {
    if (is.null(tree$edge.length)) {
      tree <- compute.brlen(tree, 1)
    }
    tree <- prune.sample(samp, tree)
    samp <- samp[, tree$tip.label]
    Cmatrix <- vcv.phylo(tree, corr = FALSE)
  }
  else {
    Cmatrix <- tree
    species <- colnames(samp)
    preval <- colSums(samp)/sum(samp)
    species <- species[preval > 0]
    Cmatrix <- Cmatrix[species, species]
    samp <- samp[, colnames(Cmatrix)]
  }
  SR <- rowSums(samp)
  nlocations <- dim(samp)[1]
  nspecies <- dim(samp)[2]
  PSVs <- NULL
  for (i in 1:nlocations) {
    index <- seq(1:nrow(Cmatrix))[samp[i, ] == 1]
    n <- length(index)
    if (n > 1) {
      C <- Cmatrix[index, index]
      PSV <- (n * sum(diag(as.matrix(C))) - sum(C))/(n * n)
    }
    else {
      PSV <- NA
    }
    PSVs <- c(PSVs, PSV)
  }
  PSVout <- cbind(PSVs, SR)
  if (flag == 2) {
    PSVout <- PSVout[-2, ]
    return(PSVout)
  }
  else {
    if (compute.var == FALSE) {
      return(data.frame(PSVout))
    }
    else {
      PSVvar <- NULL
      X <- Cmatrix - (sum(sum(Cmatrix - diag(nspecies))))/(nspecies * 
                                                             (nspecies - 1))
      X <- X - diag(diag(X))
      SS1 <- sum(X * X)/2
      SS2 <- 0
      for (i in 1:(nspecies - 1)) {
        for (j in (i + 1):nspecies) {
          SS2 <- SS2 + X[i, j] * (sum(X[i, ]) - X[i, 
                                                  j])
        }
      }
      SS3 <- -SS1 - SS2
      S1 <- SS1 * 2/(nspecies * (nspecies - 1))
      S2 <- SS2 * 2/(nspecies * (nspecies - 1) * (nspecies - 
                                                    2))
      if (nspecies == 3) {
        S3 <- 0
      }
      else {
        S3 <- SS3 * 2/(nspecies * (nspecies - 1) * (nspecies - 
                                                      2) * (nspecies - 3))
      }
      for (n in 2:nspecies) {
        approxi <- 2/(n * (n - 1)) * (S1 + (n - 2) * 
                                        S2 + (n - 2) * (n - 3) * S3)
        PSVvar <- rbind(PSVvar, c(n, approxi))
      }
      vars <- rep(0, nlocations)
      PSVout <- cbind(PSVout, vars)
      for (g in 1:nlocations) {
        if (PSVout[g, 2] > 1) {
          PSVout[g, 3] <- PSVvar[PSVout[g, 2] - 1, 2]
        }
        else {
          PSVout[g, 3] <- NA
        }
      }
      return(data.frame(PSVout))
    }
  }
}

#test new diversity metrics

Plchrono1<-PlTree
Plchrono1$edge.length[Plchrono1$edge.length==0]<-0.001
Plchrono<-chronopl(Plchrono1,1)
PlDist<-cophenetic(Plchrono)
BdDist<-cophenetic(BdTree)

PlPCoA<-cmdscale(PlDist,ncol(PlDist)-1)
Plpcoa<-pcoa(PlDist)
PlPhylTraits<-Plpcoa$vectors

Plnulldist<-as.matrix(PlDist)
Plnulldist[lower.tri(Plnulldist)]<-mean(Plnulldist[lower.tri(Plnulldist)])
Plnulldist[upper.tri(Plnulldist)]<-mean(Plnulldist[upper.tri(Plnulldist)])
Plnulldist<-as.dist(Plnulldist)

Plnullpcoa<-pcoa(Plnulldist)
PlnullTraits<-Plnullpcoa$vectors

SpecVector<-function(x,names){
  names[x]
}
speclist<-apply(PlMat>0,1,SpecVector,names=colnames(PlMat))

Traiter<-function(x,traits){
  traits[x,]
}

traitlist<-lapply(speclist,Traiter,traits=PlPhylTraits)
nulltraitlist<-lapply(speclist,Traiter,traits=PlnullTraits)

newFDvar<-function(x){
  n<-nrow(x)
  vars<-diag(var(x))*(n-1)/n
  return(mean(vars))
}
FMean<-function(x){
  apply(x,2,mean)
}


newFD<-sapply(traitlist,newFDvar)
nullFD<-sapply(nulltraitlist,newFDvar)
Fmeans<-sapply(traitlist,FMean)
FTVar<-apply(Fmeans,1,var)

PlGamMat<-do.call('rbind', traitlist)

rownames(PlPhylTraits) %in% colnames(PlMat)
PlMat<-PlMat[,-c(199,251)]
PlMat[,250]<-PlMat[,250]+PlMat[,251]
PlMat[,332]<-PlMat[,332]+PlMat[,333]
PlMat<-PlMat[,-c(251,333)]
PlMat[PlMat>0]<-1

PlMat<-PlMat[,order(colnames(PlMat))]
PlPhylTraits<-PlPhylTraits[order(rownames(PlPhylTraits)),]

library(FD)
FD<-dbFD(PlPhylTraits,PlMat,stand.x=F,calc.FRic=F)






library(rstan)
#Data
vcovmat<-vcv(Bdchrono,corr=T)
inn<-colnames(vcovmat) %in% colnames(BdMat)
vcovmat<-vcovmat[inn,inn]
in2<-colnames(BdMat) %in% colnames(vcovmat)
BdMat<-BdMat[,in2]
BdMat<-BdMat[,colnames(vcovmat)]

nspec<-ncol(BdMat)
ncomm<-nrow(BdMat)
K<-2
phymat<-vcovmat
y<-BdMat

set_cppo('debug')
set_cppo("fast")
phy_code <- "
    data {
    int<lower=1> nspec; // number of species
    int<lower=1> ncomm; // number of communities
    int<lower=2> K; // dimensionality of the Latent Spaces
    corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
    matrix[ncomm,nspec] y; // Species X Site occupancy matrix
  }
  transformed data {
    matrix[nspec,nspec] L;
    L <- cholesky_decompose(phymat);
  }
  parameters {
    real<lower=0,upper=200> e_u_sigma; // environmental filter multiplier
    real<lower=0,upper=200> e_v_sigma; // community optimum variance  
    real<lower=0,upper=200> c_u_sigma; // competition multiplier
    real mu; // intercept
    real beta;
    vector[nspec] r;
    row_vector[ncomm] s;
    vector[nspec] e_u_t[K];
    row_vector[ncomm] e_v_t[K];
    vector[nspec] c_u_t[K];
  }
  transformed parameters {
    vector[nspec] e_u[K];
    vector[nspec] c_u[K];
    row_vector[ncomm] e_v[K];
    vector[nspec] e_u_sigma1;
    vector[nspec] c_u_sigma1;
    matrix[ncomm,nspec] prob;
    e_u_sigma1 <- rep_vector(e_u_sigma,nspec);
    c_u_sigma1 <- rep_vector(c_u_sigma,nspec);
    for (i in 1:K){
      e_u[i] <- e_u_sigma1 .* (L * e_u_t[i]);
      e_v[i] <- e_v_sigma * e_v_t[i];
      c_u[i] <- c_u_sigma1 .* (L * c_u_t[i]);
    }
  #print(\"Transform!\")
  }
  model {
    matrix[ncomm,nspec] e_dists[K];
    matrix[ncomm,nspec] e_dist;
    matrix[nspec,nspec] c_dists[K];
    matrix[nspec,nspec] c_dist;
    matrix[ncomm,nspec] compmat;
    matrix[ncomm,nspec] pr; 
    matrix[ncomm,nspec] probs;
    e_dist <- rep_matrix(0,ncomm,nspec);
    c_dist <- rep_matrix(0,nspec,nspec);
    for (i in 1:K){
      e_u_t[i] ~ normal(0,1);
      e_v_t[i] ~ normal(0,1);
      c_u_t[i] ~ normal(0,1);
      #print(cols(e_u[i]),\" \",cols(e_v[i]));
      e_dists[i] <- (rep_matrix(e_u[i],ncomm) - rep_matrix(e_v[i],nspec))';
      #print(\"Try Again\");
      e_dists[i] <- e_dists[i] .* e_dists[i];
      e_dist <- e_dist + e_dists[i];
      for (n in 1:nspec){
        for (m in 1:nspec){
          c_dists[i][m,n] <- square(c_u[i][n] - c_u[i][m]);
        }
      }
      c_dist <- c_dist + c_dists[i];
    }
    #print(\"One\");
    for (n in 1:nspec){
      for (m in 1:ncomm){
        e_dist[m,n] <- sqrt(e_dist[m,n]);
      }
      for (j in 1:nspec){
        c_dist[j,n] <- exp(-c_dist[j,n]/4);
      }
    }
    #print(e_dist);
    pr <- mu + (rep_matrix(r,ncomm) + rep_matrix(s,nspec))' - e_dist;
    for (n in 1:nspec){
      for (m in 1:ncomm){
        probs[m,n] <- inv_logit(pr[m,n]);
      }
    }
    for (i in 1:ncomm){
      matrix[nspec,nspec] newmat;
      row_vector[nspec] comp;
      newmat <- 1 - (rep_matrix(probs[i]',nspec) .* c_dist);
      for (j in 1:nspec){
        comp[j] <- sum(newmat[j]);
      }
      compmat[i] <- comp;
    }
    #print(\"Two\");
    #print(compmat);
    pr <- pr - beta*compmat;

    for (n in 1:nspec){
      for (m in 1:ncomm){
        probs[m,n] <- inv_logit(pr[m,n]);
      }
    }
    #print(\"Three\");
    mu ~ normal(0,100);
    beta ~ normal(0,100);
    r ~ normal(0,100);
    s ~ normal(0,100);  
    
    for (i in 1:nspec){
      for (j in 1:ncomm){
        int_step(y[j,i]) ~ bernoulli(probs[j,i]);
      }
    }
    #int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

  }
"
phy_dat <- list(nspec = nspec, ncomm = ncomm, K = K, phymat = phymat, y = y)

fit <- stan(model_code = phy_code, data = phy_dat, 
            iter = 100, chains = 1)

vcovmat<-vcv(Bdchrono)


set_cppo("fast")
#set_cppo('debug')

simplephy_code <- "
data {
  int<lower=1> nspec; // number of species
  int<lower=1> ncomm; // number of communities
  int<lower=2> K; // dimensionality of the Latent Spaces
  corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
  matrix[ncomm,nspec] y; // Species X Site occupancy matrix
  real e_v_sigma;
  }
transformed data {
  matrix[nspec,nspec] L;
  L <- cholesky_decompose(phymat);
#print(L);
}
parameters {
  real<lower=0,upper=100> e_u_sigma; // environmental filter multiplier
  real<lower=0,upper=100> e_sigma;
  real mu; // intercept
  vector[nspec] r;
  row_vector[ncomm] s;
  vector[nspec] e_u_t[K];
  row_vector[ncomm] e_v_t[K];
  matrix[ncomm,nspec] err;
  row_vector<lower=0,upper=100>[ncomm] beta;
}
transformed parameters {
  vector[nspec] e_u[K];
  row_vector[ncomm] e_v[K];
  vector[nspec] e_u_sigma1;
  e_u_sigma1 <- rep_vector(e_u_sigma,nspec);
  for (i in 1:K){
    e_u[i] <- e_u_sigma1 .* (L * e_u_t[i]);
    e_v[i] <- e_v_sigma * e_v_t[i];
  }
#print(\"Transform!\")
  #print(e_u, e_v)
}
model {
  matrix[ncomm,nspec] e_dists[K];
  matrix[ncomm,nspec] e_dist;
  matrix[ncomm,nspec] pr; 
  matrix[ncomm,nspec] probs;
  matrix[ncomm,nspec] betas;
  e_dist <- rep_matrix(0,ncomm,nspec);
  for (i in 1:K){
    e_u_t[i] ~ normal(0,1);
    e_v_t[i] ~ normal(0,1);
#print(cols(e_u[i]),\" \",cols(e_v[i]));
    e_dists[i] <- (rep_matrix(e_u[i],ncomm)' - rep_matrix(e_v[i],nspec)');
#print(\"Try Again\");
    for (m in 1:nspec){
      for (n in 1:ncomm) {
        e_dists[i][n,m]<-if_else(e_dists[i][n,m]==0,0.001,e_dists[i][n,m]);
      }
    }
    e_dists[i] <- e_dists[i] .* e_dists[i];
    e_dist <- e_dist + e_dists[i];
  }
  
#print(e_dist);
#print(\"One\");
  for (n in 1:nspec){
    for (m in 1:ncomm){
      e_dist[m,n] <- sqrt(e_dist[m,n]);
      err[m,n] ~ normal(0,e_sigma);
    }
  }
#print(e_dist);
  betas <- rep_matrix(beta,nspec)';
  pr <- mu + (rep_matrix(r,ncomm)' + rep_matrix(s,nspec)') - (betas .* e_dist) + err;
  for (n in 1:nspec){
    for (m in 1:ncomm){
      probs[m,n] <- inv_logit(pr[m,n]);
    }
  }
#print(probs);
#print(\"Two\");
#print(compmat);

#print(\"Three\");
  mu ~ normal(0,100);
  r ~ normal(0,100);
  s ~ normal(0,100);  

  for (i in 1:nspec){
    for (j in 1:ncomm){
      int_step(y[j,i]) ~ bernoulli(probs[j,i]);
    }
  }
#int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y, e_v_sigma=2)

fit <- stan(model_code = simplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=T)

fit2 <- stan(fit = fit, data = phy_dat, 
            iter = 1000, chains = 1, warmup=100)


esimplephy_code <- "
data {
int<lower=1> nspec; // number of species
int<lower=1> ncomm; // number of communities
int<lower=2> K; // dimensionality of the Latent Spaces
corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
matrix[ncomm,nspec] y; // Species X Site occupancy matrix
}
transformed data {
matrix[nspec,nspec] L;
L <- cholesky_decompose(phymat);
#print(L);
}
parameters {
//real<lower=0,upper=500> e_u_sigma; // environmental filter multiplier
//real<lower=0,upper=500> e_v_sigma; // community optimum variance  
real mu; // intercept
vector[nspec] r;
row_vector[ncomm] s;
//vector[nspec] e_u_t[K];
//row_vector[ncomm] e_v_t[K];
}
transformed parameters {
//vector[nspec] e_u[K];
//row_vector[ncomm] e_v[K];
//vector[nspec] e_u_sigma1;
//e_u_sigma1 <- rep_vector(e_u_sigma,nspec);
//for (i in 1:K){
//e_u[i] <- e_u_sigma1 .* (L * e_u_t[i]);
//e_v[i] <- e_v_sigma * e_v_t[i];
//}
#print(\"Transform!\")
}
model {
//matrix[ncomm,nspec] e_dists[K];
//matrix[ncomm,nspec] e_dist;
matrix[ncomm,nspec] pr; 
matrix[ncomm,nspec] probs;
//e_dist <- rep_matrix(0,ncomm,nspec);
//for (i in 1:K){
//e_u_t[i] ~ normal(0,1);
//e_v_t[i] ~ normal(0,1);
#print(cols(e_u[i]),\" \",cols(e_v[i]));
//e_dists[i] <- (rep_matrix(e_u[i],ncomm)' - rep_matrix(e_v[i],nspec)');
#print(\"Try Again\");
//e_dists[i] <- e_dists[i] .* e_dists[i];
//e_dist <- e_dist + e_dists[i];
//}
#print(\"One\");
//for (n in 1:nspec){
//for (m in 1:ncomm){
//e_dist[m,n] <- sqrt(e_dist[m,n]);
//}
//}
#print(e_dist);
pr <- mu + (rep_matrix(r,ncomm)' + rep_matrix(s,nspec)');
for (n in 1:nspec){
for (m in 1:ncomm){
probs[m,n] <- inv_logit(pr[m,n]);
}
}
print(probs);
#print(\"Two\");
#print(compmat);

#print(\"Three\");
mu ~ normal(0,100);
r ~ normal(0,100);
s ~ normal(0,100);  

for (i in 1:nspec){
for (j in 1:ncomm){
int_step(y[j,i]) ~ bernoulli(probs[j,i]);
}
}
#int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y)

fit <- stan(model_code = esimplephy_code, data = phy_dat, 
            iter = 100, chains = 1)

fit2 <- stan(fit = fit, data = phy_dat, 
             iter = 100, chains = 1, init="0")


csimplephy_code <- "
data {
int<lower=1> nspec; // number of species
int<lower=1> ncomm; // number of communities
int<lower=2> K; // dimensionality of the Latent Spaces
corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
matrix[ncomm,nspec] y; // Species X Site occupancy matrix
}
transformed data {
matrix[nspec,nspec] L;
L <- cholesky_decompose(phymat);
#print(L);
}
parameters {
real<lower=0> e_u_sigma; // environmental filter multiplier
real<lower=0> e_v_sigma; // community optimum variance  
real mu; // intercept
vector[nspec] r;
row_vector[ncomm] s;
vector[nspec] e_u_t[K];
row_vector[ncomm] e_v_t[K];
real beta;
}
transformed parameters {
vector[nspec] e_u[K];
row_vector[ncomm] e_v[K];
vector[nspec] e_u_sigma1;
e_u_sigma1 <- rep_vector(e_u_sigma,nspec);
for (i in 1:K){
e_u[i] <- e_u_sigma1 .* (L * e_u_t[i]);
e_v[i] <- e_v_sigma * e_v_t[i];
}
#print(\"Transform!\")
}
model {
matrix[ncomm,nspec] e_dists[K];
matrix[ncomm,nspec] e_dist;
matrix[ncomm,nspec] pr; 
matrix[ncomm,nspec] probs;
e_dist <- rep_matrix(0,ncomm,nspec);
for (i in 1:K){
e_u_t[i] ~ normal(0,1);
e_v_t[i] ~ normal(0,1);
#print(cols(e_u[i]),\" \",cols(e_v[i]));
e_dists[i] <- (rep_matrix(e_u[i],ncomm)' - rep_matrix(e_v[i],nspec)');
#print(\"Try Again\");
e_dists[i] <- e_dists[i] .* e_dists[i];
e_dist <- e_dist + e_dists[i];
}
#print(e_dist);
#print(\"One\");
for (n in 1:nspec){
for (m in 1:ncomm){
e_dist[m,n] <- sqrt(e_dist[m,n]);
}
}
#print(e_dist);
pr <- mu + (rep_matrix(r,ncomm)' + rep_matrix(s,nspec)') - beta*e_dist;
for (n in 1:nspec){
for (m in 1:ncomm){
probs[m,n] <- inv_logit(pr[m,n]);
}
}
print(probs);
#print(\"Two\");
#print(compmat);


mu ~ normal(0,100);
r ~ normal(0,100);
s ~ normal(0,100);  
beta ~ normal(0,100);
e_u_sigma~cauchy(0,50);
e_v_sigma~cauchy(0,50);

print(\"Three\");

for (i in 1:nspec){
for (j in 1:ncomm){
int_step(y[j,i]) ~ bernoulli(probs[j,i]);
}
}
#int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y)

fit <- stan(model_code = csimplephy_code, data = phy_dat, 
            iter = 100, chains = 1, init=list(list(e_u_sigma=1,e_v_sigma=1,mu=0,r=rep(0,nspec),s=rep(0,ncomm),e_u_t=rbind(rep(0,nspec),rep(0,nspec)), 
                                                   e_v_t=rbind(rep(0,ncomm),rep(0,ncomm)),beta=0)),control=list(test_grad=T))

fit <- stan(model_code = csimplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=TRUE)

fit2 <- stan(fit = fit, data = phy_dat, 
             iter = 100, chains = 1, algorithm="NUTS")



simplephy_code <- "
data {
int<lower=1> nspec; // number of species
int<lower=1> ncomm; // number of communities
int<lower=2> K; // dimensionality of the Latent Spaces
corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
matrix[ncomm,nspec] y; // Species X Site occupancy matrix
}
parameters {
real<lower=0,upper=100> e_u_sigma; // environmental filter multiplier
real<lower=0,upper=100> e_v_sigma; // community optimum variance  
real mu; // intercept
vector[nspec] e_u[K];
row_vector[ncomm] e_v[K];
}
model {
matrix[ncomm,nspec] e_dists[K];
matrix[ncomm,nspec] e_dist;
matrix[ncomm,nspec] pr; 
matrix[ncomm,nspec] probs;
e_dist <- rep_matrix(0,ncomm,nspec);
for (i in 1:K){
//e_u[i] ~ multi_normal(rep_vector(0,nspec),e_u_sigma*phymat);
//e_v[i] ~ normal(0,e_v_sigma);
#print(cols(e_u[i]),\" \",cols(e_v[i]));
e_dists[i] <- (rep_matrix(e_u[i],ncomm)' - rep_matrix(e_v[i],nspec)');
#print(\"Try Again\");
e_dists[i] <- e_dists[i] .* e_dists[i];
e_dist <- e_dist + e_dists[i];
}
#print(e_dist);
#print(\"One\");
for (n in 1:nspec){
for (m in 1:ncomm){
e_dist[m,n] <- sqrt(e_dist[m,n]);
}
}
#print(e_dist);
pr <- mu - e_dist;
for (n in 1:nspec){
for (m in 1:ncomm){
probs[m,n] <- inv_logit(pr[m,n]);
}
}
#print(probs);
#print(\"Two\");
#print(compmat);

#print(\"Three\");
mu ~ normal(0,100);

for (i in 1:nspec){
for (j in 1:ncomm){
int_step(y[j,i]) ~ bernoulli(probs[j,i]);
}
}

//for (i in 1:K){
//e_u[i] ~ normal(0,e_u_sigma);
//e_v[i] ~ normal(0,e_v_sigma);
//}
#int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y)

fit <- stan(model_code = simplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=T)

fit2 <- stan(fit = fit, data = phy_dat, iter = 100, chains = 1)




tsimplephy_code <- "
data {
int<lower=1> nspec; // number of species
int<lower=1> ncomm; // number of communities
int<lower=2> K; // dimensionality of the Latent Spaces
corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
matrix[ncomm,nspec] y; // Species X Site occupancy matrix
}
parameters {
real<lower=0,upper=100> e_u_sigma; // environmental filter multiplier
real<lower=0,upper=100> e_v_sigma; // community optimum variance  
real mu; // intercept
vector[K] e_u[nspec];
row_vector[K] e_v[ncomm];
}
model {
matrix[ncomm,nspec] e_dist;
matrix[ncomm,nspec] pr; 
matrix[ncomm,nspec] probs;

for (i in 1:nspec){
for (j in 1:ncomm){
//e_u[i] ~ multi_normal(rep_vector(0,nspec),e_u_sigma*phymat);
//e_v[i] ~ normal(0,e_v_sigma);
#print(cols(e_u[i]),\" \",cols(e_v[i]));

#print(\"Try Again\");
e_dist[j,i] <- e_v[j]*e_u[i];
pr[j,i] <- mu - e_dist[j,i];
probs[j,i] <- inv_logit(pr[j,i]);
int_step(y[j,i]) ~ bernoulli(probs[j,i]);
}
}
#print(e_dist);
#print(\"One\");

#print(e_dist);
#print(probs);
#print(\"Two\");
#print(compmat);

#print(\"Three\");
mu ~ normal(0,100);

for (i in 1:K){
for (m in 1:nspec){
e_u[m][i] ~ normal(0.0,e_u_sigma);
}
for (n in 1:ncomm){
e_v[n][i] ~ normal(0.0,e_v_sigma);
}
}
#int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y)

fit <- stan(model_code = tsimplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=T)

fit2 <- stan(fit = fit, data = phy_dat, iter = 100, chains = 1)





tsimplephy_code <- "
data {
int<lower=1> nspec; // number of species
int<lower=1> ncomm; // number of communities
int<lower=2> K; // dimensionality of the Latent Spaces
corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
matrix[ncomm,nspec] y; // Species X Site occupancy matrix
}
parameters {

real mu; // intercept

}
model {

matrix[ncomm,nspec] pr; 
matrix[ncomm,nspec] probs;
for (i in 1:nspec){
for (j in 1:ncomm){


pr[j,i] <- mu;
probs[j,i] <- inv_logit(pr[j,i]);
int_step(y[j,i]) ~ bernoulli(probs[j,i]);

}
}

mu ~ normal(0,100);



}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y)

fit <- stan(model_code = tsimplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=T)

fit2 <- stan(fit = fit, data = phy_dat, iter = 100, chains = 1)


simplephy_code <- "
data {
int<lower=1> nspec; // number of species
int<lower=1> ncomm; // number of communities
int<lower=2> K; // dimensionality of the Latent Spaces
corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
matrix[ncomm,nspec] y; // Species X Site occupancy matrix
}
parameters {
real<lower=0,upper=100> e_u_sigma; // environmental filter multiplier
real<lower=0,upper=100> e_v_sigma; // community optimum variance  
real mu; // intercept
vector[nspec] e_u[K];
row_vector[ncomm] e_v[K];
}
model {
matrix[ncomm,nspec] e_dists[K];
matrix[ncomm,nspec] e_dist;
matrix[ncomm,nspec] pr; 
matrix[ncomm,nspec] probs;
e_dist <- rep_matrix(0,ncomm,nspec);
for (i in 1:K){
e_u[i] ~ multi_normal(rep_vector(0,nspec),e_u_sigma*phymat);
e_v[i] ~ normal(0,e_v_sigma);
#print(cols(e_u[i]),\" \",cols(e_v[i]));
e_dists[i] <- rep_matrix(e_u[i],ncomm)' .* rep_matrix(e_v[i],nspec)';
#print(\"Try Again\");
e_dist <- e_dist + e_dists[i];
}
pr <- mu - e_dist;
for (n in 1:nspec){
for (m in 1:ncomm){
probs[m,n] <- inv_logit(pr[m,n]);
int_step(y[m,n]) ~ bernoulli(probs[m,n]);
}
}
#print(probs);
#print(\"Two\");
#print(compmat);

#print(\"Three\");
mu ~ normal(0,100);

for (i in 1:nspec){
for (j in 1:ncomm){

}
}

//for (i in 1:K){
//e_u[i] ~ normal(0,e_u_sigma);
//e_v[i] ~ normal(0,e_v_sigma);
//}
#int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y)

fit <- stan(model_code = simplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=T)

fit2 <- stan(fit = fit, data = phy_dat, iter = 100, chains = 1)

simplephy_code <- "
data {
int<lower=1> nspec; // number of species
int<lower=1> ncomm; // number of communities
int<lower=2> K; // dimensionality of the Latent Spaces
corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
matrix[ncomm,nspec] y; // Species X Site occupancy matrix
}
parameters {
real<lower=0,upper=100> e_u_sigma; // environmental filter multiplier
real<lower=0,upper=100> e_v_sigma; // community optimum variance  
real mu; // intercept
vector[nspec] e_u[K];
row_vector[ncomm] e_v[K];
}
model {
matrix[ncomm,nspec] e_dists[K];
matrix[ncomm,nspec] e_dist;
matrix[ncomm,nspec] pr; 
matrix[ncomm,nspec] probs;
e_dist <- rep_matrix(0,ncomm,nspec);
for (i in 1:K){
e_u[i] ~ multi_normal(rep_vector(0,nspec),e_u_sigma*phymat);
e_v[i] ~ normal(0,e_v_sigma);
#print(cols(e_u[i]),\" \",cols(e_v[i]));
e_dists[i] <- rep_matrix(e_u[i],ncomm)' - rep_matrix(e_v[i],nspec)';
#print(\"Try Again\");
for (m in 1:nspec){
for (n in 1:ncomm){
e_dists[i][n,m]<-fabs(e_dists[i][n,m]);
}
}
e_dist <- e_dist + e_dists[i];
}
pr <- mu - e_dist;
for (n in 1:nspec){
for (m in 1:ncomm){
probs[m,n] <- inv_logit(pr[m,n]);
int_step(y[m,n]) ~ bernoulli(probs[m,n]);
}
}
#print(probs);
#print(\"Two\");
#print(compmat);

#print(\"Three\");
mu ~ normal(0,100);

for (i in 1:nspec){
for (j in 1:ncomm){

}
}

//for (i in 1:K){
//e_u[i] ~ normal(0,e_u_sigma);
//e_v[i] ~ normal(0,e_v_sigma);
//}
#int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y)

fit <- stan(model_code = simplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=T)

fit2 <- stan(fit = fit, data = phy_dat, iter = 100, chains = 1)






simplephy_code <- "
data {
  int<lower=1> nspec; // number of species
  int<lower=1> ncomm; // number of communities
  int<lower=2> K; // dimensionality of the Latent Spaces
  corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
  matrix[ncomm,nspec] y; // Species X Site occupancy matrix
  real e_u_sigma;
}
transformed data {
  matrix[nspec,nspec] L;
  L <- cholesky_decompose(phymat);
  #print(L);
}
parameters {
  real<lower=0,upper=100> e_v_sigma; // environmental filter multiplier
  real<lower=0,upper=100> e_sigma;
  real mu; // intercept
  row_vector<lower=0,upper=100>[ncomm] beta;
  real<lower=0,upper=20> r_sig;
  real<lower=0,upper=20> s_sig;
  vector[nspec] r;
  row_vector[ncomm] s;
  vector[nspec] e_u_t[K];
  row_vector[ncomm] e_v_t[K];
  matrix[ncomm,nspec] err;
}
transformed parameters {
  vector[nspec] e_u[K];
  row_vector[ncomm] e_v[K];
  vector[nspec] e_u_sigma1;
  vector[nspec] rs;
  row_vector[ncomm] ss;
  matrix[ncomm,nspec] error;

  e_u_sigma1 <- rep_vector(e_u_sigma,nspec);
  for (i in 1:K){
    e_u[i] <- e_u_sigma1 .* (L * e_u_t[i]);
    e_v[i] <- e_v_sigma * e_v_t[i];
  }
  error <- err*e_sigma;
  rs <- r*r_sig;
  ss <- s*s_sig;
  #print(\"Transform!\")
  #print(e_u, e_v)
}
model {
  matrix[ncomm,nspec] e_dists[K];
  matrix[ncomm,nspec] e_dist;
  matrix[ncomm,nspec] pr; 
  matrix[ncomm,nspec] probs;
  matrix[ncomm,nspec] betas;
  e_dist <- rep_matrix(0,ncomm,nspec);
  for (i in 1:K){
    e_u_t[i] ~ normal(0,1);
    e_v_t[i] ~ normal(0,1);
    #print(cols(e_u[i]),\" \",cols(e_v[i]));
    e_dists[i] <- (rep_matrix(e_u[i],ncomm)' - rep_matrix(e_v[i],nspec)');
    #print(\"Try Again\");
    e_dists[i] <- e_dists[i] .* e_dists[i];
    e_dist <- e_dist + e_dists[i];
  }

  #print(e_dist);
  #print(\"One\");

  #print(e_dist);
  pr <- mu + (rep_matrix(rs,ncomm)' + rep_matrix(ss,nspec)') - (rep_matrix(beta,nspec)' .* e_dist) + error;
  for (n in 1:nspec){
    for (m in 1:ncomm){
      probs[m,n] <- inv_logit(pr[m,n]);
      int_step(y[m,n]) ~ bernoulli(probs[m,n]);
      err[m,n] ~ normal(0,1);
    }
  }
  #print(probs);
  #print(\"Two\");
  #print(compmat);

  #print(\"Three\");
  mu ~ normal(0,100);
  r ~ normal(0,1);
  s ~ normal(0,1);

  #int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y, e_u_sigma=2)

fit <- stan(model_code = simplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=T)

fit <- stan(fit = fit, data = phy_dat, 
            iter = 20, chains = 1, init="0")

fit2 <- stan(fit = fit, data = phy_dat, 
             iter = 2000, chains = 4, warmup=1000, init="0", pars=c("mu","e_v_sigma","e_sigma","r_sig","s_sig","rs","ss","beta","e_u","e_v"))

fit2 <- stan(model_code = simplephy_code, data = phy_dat, 
             iter = 2000, chains = 4, warmup=1000, init="0")


simplephy_code <- "
data {
int<lower=1> nspec; // number of species
int<lower=1> ncomm; // number of communities
int<lower=2> K; // dimensionality of the Latent Spaces
corr_matrix[nspec] phymat; // phylogenetic covariance matrix  
matrix[ncomm,nspec] y; // Species X Site occupancy matrix
real e_u_sigma;
}
transformed data {
matrix[nspec,nspec] L;
L <- cholesky_decompose(phymat);
#print(L);
}
parameters {
real<lower=0,upper=100> e_v_sigma; // environmental filter multiplier
real<lower=0,upper=100> e_sigma;
real mu; // intercept
#real<lower=0,upper=100> beta;
real<lower=0,upper=20> r_sig;
real<lower=0,upper=20> s_sig;
vector[nspec] r;
row_vector[ncomm] s;
vector[nspec] e_u_t[K];
row_vector[ncomm] e_v_t[K];
matrix[ncomm,nspec] err;
}
transformed parameters {
vector[nspec] e_u[K];
row_vector[ncomm] e_v[K];
vector[nspec] e_u_sigma1;
vector[nspec] rs;
row_vector[ncomm] ss;
matrix[ncomm,nspec] error;

e_u_sigma1 <- rep_vector(e_u_sigma,nspec);
for (i in 1:K){
e_u[i] <- e_u_sigma1 .* (L * e_u_t[i]);
e_v[i] <- e_v_sigma * e_v_t[i];
}
error <- err*e_sigma;
rs <- r*r_sig;
ss <- s*s_sig;
#print(\"Transform!\")
#print(e_u, e_v)
}
model {
matrix[ncomm,nspec] e_dists[K];
matrix[ncomm,nspec] e_dist;
matrix[ncomm,nspec] pr; 
matrix[ncomm,nspec] probs;
matrix[ncomm,nspec] betas;
e_dist <- rep_matrix(0,ncomm,nspec);
for (i in 1:K){
e_u_t[i] ~ normal(0,1);
e_v_t[i] ~ normal(0,1);
#print(cols(e_u[i]),\" \",cols(e_v[i]));
e_dists[i] <- (rep_matrix(e_u[i],ncomm)' - rep_matrix(e_v[i],nspec)');
#print(\"Try Again\");
e_dists[i] <- e_dists[i] .* e_dists[i];
e_dist <- e_dist + e_dists[i];
}

#print(e_dist);
#print(\"One\");

#print(e_dist);
pr <- mu + (rep_matrix(rs,ncomm)' + rep_matrix(ss,nspec)') - e_dist + error;
for (n in 1:nspec){
for (m in 1:ncomm){
probs[m,n] <- inv_logit(pr[m,n]);
int_step(y[m,n]) ~ bernoulli(probs[m,n]);
err[m,n] ~ normal(0,1);
}
}
#print(probs);
#print(\"Two\");
#print(compmat);

#print(\"Three\");
mu ~ normal(0,100);
r ~ normal(0,1);
s ~ normal(0,1);

#int_step(to_vector(y)) ~ bernoulli(to_vector(probs));

}
"
phy_dat <- list(nspec = as.integer(nspec), ncomm = as.integer(ncomm), K = as.integer(K), phymat = phymat, y = y, e_u_sigma=1)

fit <- stan(model_code = simplephy_code, data = phy_dat, 
            iter = 100, chains = 1, test_grad=T)

fit <- stan(fit = fit, data = phy_dat, 
            iter = 20, chains = 1, init="0")

fit2 <- stan(fit = fit, data = phy_dat, 
             iter = 2000, chains = 4, warmup=1000, init="0", pars=c("mu","e_v_sigma","e_sigma","r_sig","s_sig","rs","ss","e_u","e_v"))

fit2 <- stan(model_code = simplephy_code, data = phy_dat, 
             iter = 2000, chains = 4, warmup=1000, init="0")



e_u<-extract(fit2,"e_u")
e_v<-extract(fit2,"e_v")

e_u2<-lapply(c(1:dim(e_u$e_u)[1]),function(x) e_u$e_u[x,,])
e_v2<-lapply(c(1:dim(e_v$e_v)[1]),function(x) e_v$e_v[x,,])

doer<-function(x,y){
  rbind(t(x),t(y))
}

tester<-mapply(doer,e_u2,e_v2,SIMPLIFY=F)
tester2<-tester[2001:4000]

ref<-tester2[[1]]
tester2<-tester2[2:2000]

protrans<-function(x,y){
  
}

test1<-procrustes(as.matrix(tester2[[4]]),as.matrix(ref),T)
test<-lapply(tester2,function(x,y) procrustes(as.matrix(x),as.matrix(y))$X.new,y=ref)

plot(NULL,xlim=c(-3,3),ylim=c(-3,3))
lapply(test,function(x) points(matrix(x[1,],ncol=2)))

#
