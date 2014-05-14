#' log likelihood function for Phylogenetic Latent Niche Traits Model (PLNT-Model)
#' @param comm A community matrix with species in the columns and sites in the rows.
#' Matrix is composed of 0's and 1's where 0 is the absence of a species and 1 is the presence
#' @param phylo A phylogeny of the species in the community matrix in the form of a
#' covariance matrix (can be generated a expectation of Brownian motion evolution)
ll.PLNT<- function(comm, phylo, params) {
  
}