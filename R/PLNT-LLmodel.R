require(plyr)
require(BB)

#' log likelihood function for Phylogenetic Latent Niche Traits Model (PLNT-Model)
#' @param comm A community matrix with species in the columns and sites in the rows.
#' Matrix is composed of 0's and 1's where 0 is the absence of a species and 1 is the presence
#' @param phylo A phylogeny of the species in the community matrix in the form of the 
#' the phylogenetic covariance matrix (can be generated a expectation of Brownian motion 
#' evolution). 
#' @param params
#' @export
ll.PLNT<- function(params,comm, phylo) {
  ## meld phylogenetic correlation matrix with diagonal matrix which represents 
  ## no correlation b/w species to form new hybrid CV matrix
  CVmat<-(params$PhyEff)*phylo + (1-params$PhyEff)*diag(1,params$Ni)
  ## Apply cholesky decomposition of of CV matrix to normally distributed
  ## Phi vector to force values to be (phylogenetically) correlated
  Phi<-llply(params$Phi, function (x) params$Sigi*(x%*%chol(CVmat)))
  E<-llply(params$E, function (x) params$Sigj*x) ## apply environmental variance (Sigj)
  ## Make V vectors into matrices
  Emat<-llply(E,function(x) matrix(rep(x,length.out=params$Nj*params$Ni),
                                   nrow=params$Ni,ncol=params$Nj,byrow=TRUE) )
  ## Make Phi vectors into matrices
  Phimat<-llply(Phi,function(x) matrix(rep(x,length.out=params$Nj*params$Ni),
                                       nrow=params$Ni,ncol=params$Nj))
  ## Make Sig value into matrices
  Sigmat<-rlply(params$Nd,matrix(rep(params$Sig,length.out=params$Nj*params$Ni),
                                 nrow=params$Ni,ncol=params$Nj))
  ## Do matrix function to calculate fundamental probabilities of species in sites
  ## according to model ()
  Pf<-mapply(function(x,y,z) ((x-y)/z)^2, Emat, Phimat, Sigmat, SIMPLIFY=FALSE)
  Pf<-exp((-1)*do.call("+",Pf)) #Pf = Fundamental probability of existence of 
  ## species j (rows) in site q (columns)
  ## Calculate alpha competition coefficients for each competition dimension
  Alphmat<-llply(params$Alph,function(x) ((matrix(rep(x,length.out=params$Ni^2),
                                                  nrow=params$Ni,ncol=params$Ni,byrow=TRUE)-
                                             matrix(rep(x,length.out=params$Ni^2),nrow=params$Ni,ncol=params$Ni))/params$w)^2)
  Alphmat<-exp((-1)*do.call("+",Alphmat)) ## multiply competition dimensions
  diag(Alphmat)<-NA
  test<-BBsolve(runif(Ni*Nj,0,1),BBfun,Pf=Pf,Alph=Alphmat,Ni=params$Ni,Nj=params$Nj,
                control=list(trace=TRUE))
  Pr<-matrix(test$par,nrow=params$Ni,ncol=params$Nj) ## Pr = Realized probability of existence 
  ## of species i (rows) in site j (columns)
  LL<-matrix(NA,nrow=params$Ni,ncol=params$Nj)
  LL[comm==1]<-log(Pr) ## Log Likelihood of observing species
  LL[comm==0]<-log(1-Pr) ## Log Likelihood of not observing species
  return(sum(LL)) ## Return full likelihood!
}
## Numerical solver function for probability of a species existing at a site taking into
## account other species and their competitive effects
BBfun<- function(x, Pf, Alph, Ni, Nj){
  xmat<-matrix(x,nrow=Ni,ncol=Nj)
  xnew<-alply(xmat,2,identity)
  alph1<-mapply(function(x,y) x*y, xnew, list(Alph),SIMPLIFY=FALSE)
  alpha<-t(laply(alph1,function(x) aaply(x,1,function(y) sum(y,na.rm=TRUE))))
  y<-pmax(matrix(0,nrow=Ni,ncol=Nj),Pf-(alpha*xmat))-xmat
  return(as.vector(y))
}

#params<-list()
#params$V<-list(c(1,2,3),c(2,2,2)) #should be a length Nk list of vectors of length Nq
#params$Phi<-list(c(1,2),c(1,3)) #length Nk list of vectors of length Nj
#params$Sig<-c(2,3) #vector of length Nj
#params$K<-c(1,1)
#params$Alph<-list(c(1,2),c(2,2.5))
#params$Nj<-2 #number of species
#params$Nq<-3 #number of sites
#params$Nk<-2 #number of environmental dimensions
#params$w<-1 #width of gaussian resource curves

#Alph<-Alphmat
#x<-as.vector(Pf)
#Nj<-params$Nj
#Nq<-params$Nq


#Testing Cholesky Decomposition method for generating correlated variable
#cvm<-cor(cbind(runif(10),runif(10),runif(10),runif(10)))
#test<-cor(replicate(4,rnorm(10000))%*%chol(cvm)) #chol equals Cholesky Decomposition
#work pretty well:
#cvm-test
