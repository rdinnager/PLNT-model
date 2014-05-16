require(plyr)
require(BB)
#' log likelihood function for Phylogenetic Latent Niche Traits Model (PLNT-Model)
#' @param comm A community matrix with species in the columns and sites in the rows.
#' Matrix is composed of 0's and 1's where 0 is the absence of a species and 1 is the presence
#' @param phylo A phylogeny of the species in the community matrix in the form of the 
#' Cholesky decomposition of the phylogenetic covariance matrix (can be generated a expectation 
#' of Brownian motion evolution). Provide Cholesky decomposition so it does not have to be
#' performed during every iteration.
#' @param params
ll.PLNT<- function(comm, phylo, params) {
  Vmat<-llply(params$V,function(x) matrix(rep(x,length.out=params$Nq*params$Nj),
                                          nrow=params$Nj,ncol=params$Nq,byrow=TRUE) )
  Phimat<-llply(params$Phi,function(x) matrix(rep(x,length.out=params$Nq*params$Nj),
                                              nrow=params$Nj,ncol=params$Nq))
  Sigmat<-rlply(params$Nk,matrix(rep(params$Sig,length.out=params$Nq*params$Nj),
                                 nrow=params$Nj,ncol=params$Nq))
  Pf<-mapply(function(x,y,z) ((x-y)/z)^2, Vmat, Phimat, Sigmat, SIMPLIFY=FALSE)
  Pf<-exp((-1)*do.call("+",Pf)) #Pf = Fundamental probability of existence of 
  #species j (rows) in site q (columns)
  # Calculate alpha competition coefficients
  Alphmat<-llply(params$Alph,function(x) ((matrix(rep(x,length.out=params$Nj^2),
                                                  nrow=params$Nj,ncol=params$Nj,byrow=TRUE)-
                   matrix(rep(x,length.out=params$Nj^2),nrow=params$Nj,ncol=params$Nj))/params$w)^2)
  Alphmat<-exp((-1)*do.call("+",Alphmat))
  diag(Alphmat)<-NA
  test<-BBsolve(runif(Nj*Nq,0,1),BBfun,Pf=Pf,Alph=Alphmat,Nj=params$Nj,Nq=params$Nq,
                control=list(trace=TRUE))
  Pr<-matrix(test$par,nrow=params$Nj,ncol=params$Nq) #Pr = Realized probability of existence 
  #of species j (rows) in site q (columns)
}
#' Numerical solver function for probability of a species existing at a site taking into
#' account other species and their competitive effects
BBfun<- function(x, Pf, Alph, Nj, Nq){
  xmat<-matrix(x,nrow=Nj,ncol=Nq)
  alpha<-matrix(rep(aaply(Alph,1,function(x) sum(x,na.rm=TRUE)),length.out=Nq*Nj),nrow=Nj,ncol=Nq)
  y<-pmax(matrix(0,nrow=Nj,ncol=Nq),Pf-(alpha*xmat))-xmat
  return(as.vector(y))
}

params<-list()
params$V<-list(c(1,2,3),c(2,2,2)) #should be a length Nk list of vectors of length Nq
params$Phi<-list(c(1,2),c(1,3)) #length Nk list of vectors of length Nj
params$Sig<-c(2,3) #vector of length Nj
params$K<-c(1,1)
params$Alph<-list(c(1,2),c(2,2.5))
params$Nj<-2 #number of species
params$Nq<-3 #number of sites
params$Nk<-2 #number of environmental dimensions
params$w<-1 #width of gaussian resource curves

Alph<-Alphmat
x<-as.vector(Pf)
Nj<-params$Nj
Nq<-params$Nq


#Testing Cholesky Decomposition method for generating correlated variable
cvm<-cor(cbind(runif(10),runif(10),runif(10),runif(10)))
test<-cor(replicate(4,rnorm(10000))%*%chol(cvm)) #chol equals Cholesky Decomposition
#work pretty well:
cvm-test
