This vignette shows how I fit a Phylogenetic Latent Niche Trait model on
a fish dataset that was kindly provided to me by Mattew Helmus.

The first step is to load in the fish dataset. This data consists of the
presence or absence of 11 sunfish species measured in 890 lakes
through-out "" United States. Additionally there is a phylogenetic tree
containing all of these sunfish, and also some environmental data for
each lake that we won't make use of initially.

    ## load P/A data
    fishPA <- read.csv("D:/Users/Dinnage/Projects/PLNT-model/data/HelmusFishPA.csv")
    kable(head(fishPA))

<table>
<thead>
<tr class="header">
<th align="right">WBICid</th>
<th align="right">Ambloplites_rupestris</th>
<th align="right">Lepomis_cyanellus</th>
<th align="right">Lepomis_gibbosus</th>
<th align="right">Lepomis_gulosus</th>
<th align="right">Lepomis_humilis</th>
<th align="right">Lepomis_macrochirus</th>
<th align="right">Lepomis_megalotis</th>
<th align="right">Micropterus_dolomieu</th>
<th align="right">Micropterus_salmoides</th>
<th align="right">Pomoxis_annularis</th>
<th align="right">Pomoxis_nigromaculatus</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">8000</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="right">8300</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="odd">
<td align="right">8900</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="right">22100</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="odd">
<td align="right">24000</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
<tr class="even">
<td align="right">24100</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
</tr>
</tbody>
</table>

    ## load phylogenetic tree, which is in the form of a phylogenetic covariance matrix
    fishPhylo <- read.csv("D:/Users/Dinnage/Projects/PLNT-model/data/HelmusFishPhyloCV.csv")
    rownames(fishPhylo)<-fishPhylo[,1]
    fishPhylo<-fishPhylo[,-1]
    ## visualize phylogenetic covariance matrix (actually a phylogenetic correlation matrix to be exact)
    library(lattice)
    levelplot(as.matrix(fishPhylo), scales=list(x=list(rot=90)), xlab="", ylab="", col.regions = gray(0:100/100))

![plot of chunk load
data](filzbach_run_files/figure-markdown_strict/load%20data.png)

The fundamental probability of species *i* existing at a site *j* is
determined by the following equation:

\$\$ P\_{i, j}\^{F} = K\_{i}\\cdot \\exp\\left (\\sum\_{d}\^{d\_{max}} -1\\cdot \\left [ \\frac{E\_{d,j} - \\phi \_{d,i}}{\\sigma\_{d,i} } \\right ]\^{2} \\right ) \$\$

This is the likelihood function for the model:

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

To run Filzbach using this likelihood function and the above data we use
the `FilzbachR` package. There is a somewhat out of date version
avaliable here:
[<http://research.microsoft.com/en-us/downloads/1cfe685f-d31a-4013-b8bb-a376d0a03480/>](http://research.microsoft.com/en-us/downloads/1cfe685f-d31a-4013-b8bb-a376d0a03480/)
which does not work on R 3.0.0 or above. I received an updated version
from the principle author of the package, [Vassily
Lyutsarev](mailto:vassilyl@microsoft.com). I have been assured by the
authors that a new version will be made available online soon, along
with, hopefully, a version that works on platforms besides Windows.

    library(filzbach)
    ## make wrapper function to translate filzbach parameters into likelihood function parameters
    ## here we are going to use two dimensions for the latent niche traits (V1 and V2; Phi1 and Phi2)
    ll.wrap <- function(PhyEff, Phi1, Phi2, E1, E2, Sig, Sigi, Sigj, Ni, Nj, Nk, comm, phylo) {
      params<-list()   
    }
