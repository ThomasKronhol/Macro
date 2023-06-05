# First assignment, Macroeconometrics 
#update.packages()
#Collecting data from Fred, using their API
#install.packages("tinytex")
#install.packages("ggplot2", "dplyr")
library(fredr)
library(ggplot2)
library(dplyr)
library(purrr)
library(vars)
library(xts)
library(gridExtra)
library(tseries)
library(tidyverse)
library(palmerpenguins)
library(quarto)
library(tinytex)
library(quantmod)
library(mvtnorm)
library(MASS)
library(parallel)
library(fredr)
library(quantmod)
library(xts)
library(ggplot2)
library(gridExtra)
library(mvtnorm)
library(datetimeutils)
library(MASS)
library(mvtnorm)
library(HDInterval)
library(parallel)

rm(list=ls())
set.seed(1234567)
#Setting up API key from FRED
# fredr_set_key("54706a95d44824ac499f1012d9b3a401")
# 
# start_date <- as.Date("1987-01-01")
# end_date <- as.Date("2023-01-01")
# 
# # Define a vector of series IDs
# timeseries <- c("INDPRO", "CPIAUCSL","FEDFUNDS", "UMCSENT", "TOTCI", "CSUSHPISA", "NFCI") #incl. house prices
# #timeseries <- c("INDPRO", "CPIAUCSL", "UMCSENT", "TOTCI", "NFCI")
# data <- list()
# 
# # Retrieve the data for the series and date range ::::: change m to q for quarterly
# for (name in timeseries) {
#   fred_data <- fredr(series_id = name, observation_start = start_date, observation_end = end_date, frequency = "m")
#   data[[name]] <- fred_data
# }
# 
# #Deleting columns not required
# indp <- data[["INDPRO"]]
# indp$ln_value <- log(indp$value)
# indp_ <- indp[, c("date","ln_value")]
# indp_$date <- as.Date(indp_$date, format = "%d-%m-%Y")
# tindp <- xts(indp_$ln_value, order.by = indp_$date)
# 
# cpi <- data[["CPIAUCSL"]]
# cpi$ln_value <- log(cpi$value)
# cpi_ <- cpi[, c("date","ln_value")]
# tcpi <- xts(cpi_$ln_value, order.by = indp_$date)
# 
# FF <- data[["FEDFUNDS"]]
# FF_ <- FF[, c("date","value")]
# tFF <- xts(FF_$value, order.by = indp_$date)
# 
# expec <- data[["UMCSENT"]]
# expec_ <- expec[, c("date","value")]
# texpec <- xts(expec_$value, order.by = indp_$date)
# 
# lend <- data[["TOTCI"]]
# lend$ln_value <- log(lend$value)
# lend_ <- lend[, c("date","ln_value")]
# tlend <- xts(lend_$ln_value, order.by = indp_$date)
# 
# hp <- data[["CSUSHPISA"]]
# hp$ln_value <- log(hp$value)
# hp_ <- hp[, c("date","ln_value")]
# thp <- xts(hp_$ln_value, order.by = indp_$date)
# 
# fci <- data[["NFCI"]]
# fci_ <- fci[, c("date","value")]
# tfci <- xts(fci_$value, order.by = indp_$date)
# 
# #Merging the series into vector Y
# y = na.omit(merge(tindp, tcpi, tFF, texpec, tlend, thp, tfci))
# colnames(y)<- c("IP", "CPI","FF", "EXP", "LEND","HP","NFCI")
# #Y = na.omit(merge(tindp, tcpi, texpec, tlend, tfci))
# #colnames(Y)<- c("indu", "cpi", "exp", "lend","fci")
# head(y)
# y = as.ts(y)

# useful functions
############################################################
orthogonal.complement.matrix.TW = function(x){
  # x is a mxn matrix and m>n
  # the function returns a mx(m-n) matrix, out, that is an orthogonal complement of x, i.e.:
  # t(x)%*%out = 0 and det(cbind(x,out))!=0
  N     = dim(x)
  tmp   = qr.Q(qr(x, tol = 1e-10),complete=TRUE)
  out   = as.matrix(tmp[,(N[2]+1):N[1]])
  return(out)
}

r.conditional.generalized.normal = function(S.inv, nu, Vn, n, B0){
  # A function to sample a random draw from a conditional generalized normal distribution
  # of the unrestricted elements of the n-th row of matrix B0 
  # given the parameters from the remaining rows of B0
  # Depends on package mvtnorm
  # use: library(rmvtnorm)
  
  rn            = nrow(Vn)
  Un            = chol(nu*solve(Vn%*%S.inv%*%t(Vn)))
  w             = t(orthogonal.complement.matrix.TW(t(B0[-n,])))
  w1            = w %*% t(Vn) %*% t(Un) / sqrt(as.numeric(w %*% t(Vn) %*% t(Un) %*% Un %*% Vn %*% t(w)))
  if (rn>1){
    Wn          = cbind(t(w1),orthogonal.complement.matrix.TW(t(w1)))
  } else {
    Wn          = w1
  }
  alpha         = rep(NA,rn)
  u             = rmvnorm(1,rep(0,nu+1),(1/nu)*diag(nu+1))
  alpha[1]      = sqrt(as.numeric(u%*%t(u)))
  if (runif(1)<0.5){
    alpha[1]    = -alpha[1]
  }
  if (rn>1){
    alpha[2:rn] = rmvnorm(1,rep(0,nrow(Vn)-1),(1/nu)*diag(rn-1))
  }
  bn            = alpha %*% Wn %*% Un
  B0n           = bn %*% Vn
  
  output        = list(bn=bn, B0n=B0n)
  return(output)
}


rgn             = function(n,S.inv,nu,V,B0.initial){
  # n     - a positive integer, the number of draws to be sampled
  # S     - an NxN positive definite matrix, a parameter of the generalized-normal distribution
  # nu    - a positive scalar, degrees of freedom parameter
  # V     - an N-element list, with fixed matrices
  # B0.initial - an NxN matrix, of initial values of the parameters
  N             = nrow(B0.initial)
  no.draws      = n
  
  B0            = array(NA, c(N,N,no.draws))
  B0.aux        = B0.initial
  
  for (i in 1:no.draws){
    for (n in 1:N){
      rn            = nrow(V[[n]])
      SS_tmp        = nu*solve(V[[n]]%*%S.inv%*%t(V[[n]]))
      SS_tmp        = 0.5 * (SS_tmp + t(SS_tmp))
      Un            = chol(SS_tmp)
      w             = t(orthogonal.complement.matrix.TW(t(B0.aux[-n,])))
      w1            = w %*% t(V[[n]]) %*% t(Un) / sqrt(as.numeric(w %*% t(V[[n]]) %*% t(Un) %*% Un %*% V[[n]] %*% t(w)))
      if (rn>1){
        Wn          = cbind(t(w1),orthogonal.complement.matrix.TW(t(w1)))
      } else {
        Wn          = w1
      }
      alpha         = rep(NA,rn)
      u             = rmvnorm(1,rep(0,nu+1),(1/nu)*diag(nu+1))
      alpha[1]      = sqrt(as.numeric(u%*%t(u)))
      if (runif(1)<0.5){
        alpha[1]    = -alpha[1]
      }
      if (rn>1){
        alpha[2:rn] = rmvnorm(1,rep(0,nrow(V[[n]])-1),(1/nu)*diag(rn-1))
      }
      bn            = alpha %*% Wn %*% Un
      B0.aux[n,]    = bn %*% V[[n]]
    }
    B0[,,i]         = B0.aux
  }
  
  return(B0)
}

normalization.wz2003  = function(B0,B0.hat.inv, Sigma.inv, diag.signs){
  # This function normalizes a matrix of contemporaneous effects
  # according to the algorithm by Waggoner & Zha (2003, JOE)
  # B0        - an NxN matrix, to be normalized
  # B0.hat    - an NxN matrix, a normalized matrix
  
  N                 = nrow(B0)
  K                 = 2^N
  distance          = rep(NA,K)
  for (k in 1:K){
    B0.tmp.inv      = solve(diag(diag.signs[k,]) %*% B0)
    distance[k]     = sum(
      unlist(
        lapply(1:N,
               function(n){
                 t(B0.tmp.inv - B0.hat.inv)[n,] %*%Sigma.inv %*% t(B0.tmp.inv - B0.hat.inv)[n,]
               }
        )))
  }
  B0.out            = diag(diag.signs[which.min(distance),]) %*% B0
  
  return(B0.out)
}

normalize.Gibbs.output.parallel          = function(B0.posterior,B0.hat){
  # This function normalizes the Gibbs sampler output from function rgn
  # using function normalization.wz2003 
  # B0.posterior  - a list, output from function rgn
  # B0.hat        - an NxN matrix, a normalized matrix
  
  N                 = nrow(B0.hat)
  K                 = 2^N
  
  B0.hat.inv        = solve(B0.hat)
  Sigma.inv         = t(B0.hat)%*%B0.hat
  
  diag.signs        = matrix(NA,2^N,N)
  for (n in 1:N){
    diag.signs[,n]  = kronecker(c(-1,1),rep(1,2^(n-1)))
  }
  
  B0.posterior.n    = mclapply(1:dim(B0.posterior)[3],function(i){
    normalization.wz2003(B0=B0.posterior[,,i],B0.hat.inv, Sigma.inv, diag.signs)
  },mc.cores=1
  )
  B0.posterior.n  = simplify2array(B0.posterior.n)
  
  return(B0.posterior.n)
}




normalize.Gibbs.output          = function(B0.posterior,B0.hat){
  # This function normalizes the Gibbs sampler output from function rgn
  # using function normalization.wz2003 
  # B0.posterior  - a list, output from function rgn
  # B0.hat        - an NxN matrix, a normalized matrix
  
  N                 = nrow(B0.hat)
  K                 = 2^N
  
  B0.hat.inv        = solve(B0.hat)
  Sigma.inv         = solve(B0.hat.inv %*% t(B0.hat.inv))
  
  diag.signs        = matrix(NA,2^N,N)
  for (n in 1:N){
    diag.signs[,n]  = kronecker(c(-1,1),rep(1,2^(n-1)))
  }
  
  for (i in 1:dim(B0.posterior)[3]){
    if (i%%100==0){ cat(i," ")}
    norm.post                   = normalization.wz2003(B0=B0.posterior[,,i],B0.hat.inv, Sigma.inv, diag.signs)
    B0.posterior[,,i]           = norm.post
  }
  return(B0.posterior)
}

rnorm.ngn       = function(B0.posterior,B,Omega){
  # This function simulates draws for the multivariate normal distribution
  # of the autoregressive slope matrix of an SVAR model
  # from a normal-generalized-normal distribution according to algorithm 
  # by Waggoner & Zha (2003, JEDC)
  # B0.posterior  - a list, output from function rgn
  # B             - an NxK matrix, a parameter determining the mean of the multivariate conditionally normal distribution given B0
  # Omega         - a KxK positive definite matrix, a covariance matrix of the multivariate normal distribution
  
  N             = nrow(B)
  K             = ncol(B)
  no.draws      = dim(B0.posterior)[3]
  L             = t(chol(Omega))
  
  Bp.posterior  = lapply(1:no.draws,function(i){
    Bp          = matrix(NA, N, K)
    for (n in 1:N){
      Bp[n,]    = as.vector(t(B0.posterior[n,,i] %*% B) + L%*%rnorm(K))
    }
    return(Bp)
  })
  Bp.posterior  = simplify2array(Bp.posterior)
  return(Bp.posterior)
}

###################################################################################################################
#Simulation of data
T       = 10
N       = 3
p       = 1
K       = 1 + N * p

y       = apply(matrix(rnorm(T * N), ncol = N), 2, cumsum)

# create Y and X
############################################################
Y       = y[(p+1):nrow(y),]
X       = matrix(1,nrow(Y),1)
for (i in 1:p){
  X     = cbind(X,y[((p+1):nrow(y))-i,])
}
Y       = t(Y)
X       = t(X)

# set the priors
# kappa0     = 10
# kappa1     = .1
# kappa2     = 10
# 
# priors     = list(
#   B        = cbind(rep(0,N), diag(N), matrix(0, N, (p-1)*N)),
#   Omega    = diag(c(kappa2,kappa1*((1:p)^(-2))%x%rep(1,N))),
#   S        = kappa0*diag(N),
#   nu       = N
# )

#Exclusions (can be changed to different exclusions then cholesky)
FF.V           = vector("list",N)
for (n in 1:N){
  FF.V[[n]]   = cbind(diag(n),matrix(0,n,N-n))
}

# B0.initial = matrix(0,N,N)
# for (n in 1:N){
#   unrestricted    = apply(FF.V[[n]],2,sum)==1
#   B0.initial[n,unrestricted] = rnorm(sum(unrestricted))
# }
# 
# # #############################################
# # ## Gibbs sampler for posterior simulations ##
# # #############################################
# Gibbs.sampler.base <- function(p,Y,X,priors,S1,S2, FF.V, B0.initial){
#   
#   N       = nrow(Y)
#   p       = 1 # calculate from X and Y (K and N)
#   K       = 1+N*p
#   S1      = S1
#   S2      = S2
#   kappa0 = 10
#   kappa1 = 10
#   kappa2 = 0.1
#   
#   B0.posterior    <- array(NA,c(N,N,(S1+S2)))
#   Bp.posterior    <- array(NA,c(N,(1+N*p),(S1+S2)))
#   
#   for (s in 1:(S1+S2)){
#     
#     # Computing posterior parameters
#     Omega.inv      = solve(priors$Omega)
#     Omega.post.inv = X%*%t(X) + Omega.inv
#     Omega.post     = solve(Omega.post.inv)
#     B.post         = (Y%*%t(X) + priors$B%*%Omega.inv) %*% Omega.post
#     S.post         = Y%*%t(Y) + solve(priors$S) + priors$B%*%Omega.inv%*%t(priors$B) - B.post%*%Omega.post.inv%*%t(B.post) 
#     nu.post        = ncol(Y) + priors$nu
#     
#     # Use B0.initial for first iteration, otherwise the latest draw from B0.posterior
#     if (s==1) {
#       B0.s = B0.initial
#     } else {
#       B0.s = B0.posterior[,,s-1]
#     }
#     
#     # sampling one draw B0 from the posterior distribution using Gibbs
#     # rgn.function samples from a random conditional generalized normal distribution
#     B0.tmp                  = rgn(n=1, S.inv=S.post, nu=nu.post, V=FF.V, B0.initial=B0.s)
#     B0.posterior[,,s]       = B0.tmp[,,1]
#     
#     # sample one draw B+ from the normal conditional posterior
#     Bp.tmp              = rnorm.ngn(B0.tmp, B=B.post,Omega=Omega.post)
#     Bp.posterior[,,s]   = Bp.tmp[,,1]
#   }
#   # END OF GIBBS
#   #Discard first S1 draws
#   B0.posterior <- B0.posterior[,,(S1+1):(S1+S2)]
#   Bp.posterior <- Bp.posterior[,,(S1+1):(S1+S2)]
#   
#   #normalisation of B0.posterior and Bp.posterior
#   B0.hat             = diag(sign(diag(B0.tmp[,,1]))) %*% B0.tmp[,,1]
#   
#   B0.posterior.N    <- array(NA,c(N,N,S2))
#   Bp.posterior.N    <- array(NA,c(N,(1+N*p),S2))
#   
#   B0.posteror.N.tmp      =  normalize.Gibbs.output.parallel(B0.posterior,B0.hat=B0.hat)
#   for (s in 1:S2){
#     B0.posterior.N[,,s]    = B0.posteror.N.tmp[,,s]
#     Bp.posterior.N[,,s]    = B0.posterior.N[,,s]%*%solve(B0.posterior[,,s])%*%Bp.posterior[,,s]
#   }
#   
#   return(list(B0.posterior.N = B0.posterior.N,
#               Bp.posterior.N = Bp.posterior.N))
# }
# 
# # Run Basic function
# Basics = Gibbs.sampler.base(p=1,Y=Y,X=X,priors=priors,S1=100,S2=200, FF.V=FF.V, B0.initial=B0.initial)
# Basics

### Setting new priors
#

priors   = list(
  B        = cbind(rep(0,N), diag(N), matrix(0, N, (p-1)*N)),
  Omega    = diag(c(10,((1:p)^(-2))%x%rep(1,N))),
  S        = diag(N),
  nu       = N,
  S.kappa0  = 1,
  nu.kappa0 = 1,
  S.kappa1  = 1,
  nu.kappa1 = 1, 
  s.lambda = 1, 
  nu.lambda = 4
)

#
# ########################################################################################################
# # GIBBS for extended model
# # The B0.initial is used as an initial matrix used in the Gibbs sampler
B0.initial = matrix(0,N,N)
for (n in 1:N){
  unrestricted    = apply(FF.V[[n]],2,sum)==1
  B0.initial[n,unrestricted] = rnorm(sum(unrestricted))
}
#
#
Gibbs.sampler.extended <- function(p,Y,X,priors,S1,S2, FF.V, B0.initial){
#
  N       = nrow(Y)
  p       = 1 # calculate from X and Y (K and N)
  K       = 1+N*p
  S1      = S1
  S2      = S2

  kappa0          <- rep(NA, S1 + S2)
  kappa1          <- rep(NA, S1 + S2)
  B0.posterior    <- array(NA,c(N,N,(S1+S2)))
  Bp.posterior    <- array(NA,c(N,(1+N*p),(S1+S2)))
  
  lambda          <- rep(NA, S1 + S2)
  
  lambda[1] <- 1
  kappa0[1] <- 1
  kappa1[1] <- 1
  
  for (s in 1:(S1+S2)){

    # Computing posterior parameters
    # Only Omega, B and S depend on kappa1

    Omega.inv      = solve(priors$Omega)
    Omega.post.inv = (1/lambda[s])*X%*%t(X) + (1/kappa1[s])*Omega.inv
    Omega.post     = solve(Omega.post.inv)
    B.post         = ((1/lambda[s])*Y%*%t(X) + priors$B%*%((1/kappa1[s])*Omega.inv)) %*% Omega.post
    S.post         = solve((1/lambda[s])*Y%*%t(Y) + (1/kappa0[s])*solve(priors$S) + priors$B%*%((1/kappa1[s])*Omega.inv)%*%t(priors$B) - B.post%*%Omega.post.inv%*%t(B.post) )
    nu.post        = ncol(Y) + priors$nu

    # Use B0.initial for first iteration, otherwise the latest draw from B0.posterior
    
    if (s==1) {
      B0.s = B0.initial
    } else {
      B0.s = B0.posterior[,,s-1]
    }
    
    # sampling one draw B0 from the posterior distribution using Gibbs  
    # rgn.function samples from a random conditional generalized normal distribution
    
    B0.tmp                  = rgn(n=1, S.inv=S.post, nu=nu.post, V=FF.V, B0.initial=B0.s)
    B0.posterior[,,s]       = B0.tmp[,,1]
    
    # sample one draw B+ from the normal conditional posterior
    Bp.tmp              = rnorm.ngn(B0.tmp, B=B.post,Omega=Omega.post)
    Bp.posterior[,,s]   = Bp.tmp[,,1]
    
    #compute posterior for the shrinkage parameter S.kappa and nu
    S.kappa0.post = priors$S.kappa0 + sum(B0.posterior[,,s]^2)
    
    # S.kappa.post = sum(priors$S.kappa + (B0.posterior[i,,s]-priors$B[i,])%*%Omega.inv%*%t(B0.posterior[i,,s]-priors$B[i,]))
    
    # nu.kappa0.post  = priors$nu.kappa0 + i #change outside of loop count number rows (otherwise make as a sum of i's)
    nu.kappa0.post  = priors$nu.kappa0 + sum(unlist(lapply(FF.V, nrow)))
    
    S.kappa1.post   = priors$S.kappa1
    for (i in 1:N){
      S.kappa1.post = S.kappa1.post + (Bp.posterior[i,,s]- B0.posterior[i,,s]%*%priors$B)%*%Omega.inv%*%t(Bp.posterior[i,,s]-B0.posterior[i,,s]%*%priors$B)
    }
    # S.kappa.post = sum(priors$S.kappa + (B0.posterior[i,,s]-priors$B[i,])%*%Omega.inv%*%t(B0.posterior[i,,s]-priors$B[i,]))
    
    nu.kappa1.post  = priors$nu.kappa1 + N*(p*N+1) 
    
    #### LAMBDA
    
    S.lambda.post   = priors$s.lambda
    for (i in 1:N){
      S.lambda.post = S.lambda.post + (Bp.posterior[i,,s]- B0.posterior[i,,s]%*%priors$B)%*%Omega.inv%*%t(Bp.posterior[i,,s]-B0.posterior[i,,s]%*%priors$B)
    }
    
    nu.lambda.post  = priors$nu.lambda + N*ncol(Y) 
    
    ### LAMBDA
    
    #Draw kappa0, kappa1 and lambda from IG2
    if (s != S1+S2) {
      kappa0[s+1]    = S.kappa0.post / rchisq(1, df=nu.kappa0.post) 
      kappa1[s+1]    = S.kappa1.post / rchisq(1, df=nu.kappa1.post)
      lambda[s+1]    = S.lambda.post / rchisq(1, df=nu.lambda.post)
    }
  }
  
  #Discard first S1 draws
  
  B0.posterior <- B0.posterior[,,(S1+1):(S1+S2)]
  Bp.posterior <- Bp.posterior[,,(S1+1):(S1+S2)]
  kappa0       <- kappa0[(S1+1):(S1+S2)]
  kappa1       <- kappa1[(S1+1):(S1+S2)]
  lambda       <- lambda[(S1+1):(S1+S2)]
  
  #normalisation of B0.posterior and Bp.posterior
  
  B0.hat             = diag(sign(diag(B0.tmp[,,1]))) %*% B0.tmp[,,1]
  # t(chol((nu.post-N)*S.post))# normalisation using this B0.hat should work
  
  B0.posterior.N    <- array(NA,c(N,N,S2))
  Bp.posterior.N    <- array(NA,c(N,(1+N*p),S2))
  
  B0.posteror.N.tmp      =  normalize.Gibbs.output.parallel(B0.posterior,B0.hat=B0.hat)
  for (s in 1:S2){
    B0.posterior.N[,,s]    = B0.posteror.N.tmp[,,1]
    Bp.posterior.N[,,s]    = B0.posterior.N[,,s]%*%solve(B0.posterior[,,s])%*%Bp.posterior[,,s]
  }
  
  return(list(B0.posterior.N = B0.posterior.N,
              Bp.posterior.N = Bp.posterior.N,
              Omega.post = Omega.post, 
              kappa0 = kappa0,
              kappa1 = kappa1,
              lambda = lambda))
}
# Run function
extended = Gibbs.sampler.extended(p=1,Y=Y,X=X,priors=priors,S1=100,S2=200, FF.V=FF.V, B0.initial=B0.initial)
extended

par(mfrow=c(2,1), mar=c(4,4.5,2,2),cex.axis=1.5, cex.lab=1.5)
hist(extended$Bp.posterior.N[,2,][1,], main=expression(B[+21]),xlab="", ylab=expression(blabla), col="magenta",breaks=50)
hist(extended$Omega.post[,2,][1,], main=expression(B[+22]),xlab="", ylab="", col="magenta",breaks=50)
# hist(posterior.draws.y[,2], main="",xlab="", ylab=expression(alpha[1]), col="magenta",breaks=50)
# hist(posterior.draws.r[,2], main="",xlab="", ylab="", col="magenta",breaks=50)
# hist(posterior.draws.y[,3], main="",xlab="", ylab=expression(sigma^2), col="magenta",breaks=50)
# hist(posterior.draws.r[,3], main="",xlab="", ylab="", col="magenta",breaks=55)



# library(HDInterval)
# 
# mcxs1  = "#05386B"
# mcxs2  = "#379683"
# mcxs3  = "#5CDB95"
# mcxs4  = "#8EE4AF"
# mcxs5  = "#EDF5E1"
# purple = "#b02442"
#             
# mcxs1.rgb   = col2rgb(mcxs1)
# mcxs1.shade1= rgb(mcxs1.rgb[1],mcxs1.rgb[2],mcxs1.rgb[3], alpha=120, maxColorValue=255)
# mcxs2.rgb   = col2rgb(mcxs2)
# mcxs2.shade1= rgb(mcxs2.rgb[1],mcxs2.rgb[2],mcxs2.rgb[3], alpha=120, maxColorValue=255)
# 
# h=12*3
# 
# # Impulse response functions
# # Forecast Error Variance Decomposition
# ############################################################
# p=12
# S1=100
# S2=200
# K= 1+N*p
# 
# B.posterior       = array(NA,c(N,N,S2))
# A.posterior       = array(NA,c(N,K,S2))
# for (s in 1:S2){
#   B               = solve(Basics$B0.posterior.N[,,s])
#   B.posterior[,,s]= B
#   A.posterior[,,s]= B %*% Basics$Bp.posterior.N[,,s]
# }
# 
# IRF.posterior     = array(NA,c(N,N,h+1,S2))
# IRF.inf.posterior = array(NA,c(N,N,S2))
# FEVD.posterior    = array(NA,c(N,N,h+1,S2))
# J                 = cbind(diag(N),matrix(0,N,N*(p-1)))
# for (s in 1:S2){
#   A.bold          = rbind(A.posterior[,2:(1+N*p),s],cbind(diag(N*(p-1)),matrix(0,N*(p-1),N)))
#   IRF.inf.posterior[,,s]          = J %*% solve(diag(N*p)-A.bold) %*% t(J) %*% B.posterior[,,s]
#   A.bold.power    = A.bold
#   for (i in 1:(h+1)){
#     if (i==1){
#       IRF.posterior[,,i,s]        = B.posterior[,,s]
#     } else {
#       IRF.posterior[,,i,s]        = J %*% A.bold.power %*% t(J) %*% B.posterior[,,s]
#       A.bold.power                = A.bold.power %*% A.bold
#     }
#     for (n in 1:N){
#       for (nn in 1:N){
#         FEVD.posterior[n,nn,i,s]  = sum(IRF.posterior[n,nn,1:i,s]^2)
#       }
#     }
#     FEVD.posterior[,,i,s]         = diag(1/apply(FEVD.posterior[,,i,s],1,sum))%*%FEVD.posterior[,,i,s]
#   }
# }
# FEVD.posterior    = 100*FEVD.posterior
# 
# #variable number for shock. 
# np = 7
# 
# # plot IRFs and FEVDs
# ############################################################
# IRF.posterior.mps = IRF.posterior[,np,,]
# IRFs.k1           = apply(IRF.posterior.mps,1:2,median)
# #IRF.posterior.mps = IRF.posterior.mps*(0.25/IRFs.k1[np,1])
# IRFs.k1           = apply(IRF.posterior.mps,1:2,median)
# IRFs.inf.k1       = apply(IRF.posterior.mps,1,mean)
# rownames(IRFs.k1) = colnames(y)
# 
# IRFs.k1.hdi    = apply(IRF.posterior.mps,1:2,hdi, credMass=0.68)
# hh          = 1:(h+1)
# 
# pdf(file="C:/Users/molle/Documents/GitHub/Macro/FF-irf-mps-base_5k.pdf", height=9, width=12)
# pl = par(mfrow=c(4,2), mar=c(4,4.5,2,2),cex.axis=1.5, cex.lab=1.5)
# for (n in 1:N){
#   ylims     = range(IRFs.k1[n,hh],IRFs.k1.hdi[,n,1:6],0)
#   plot(hh,IRFs.k1[n,hh], type="l", ylim=ylims, axes=FALSE, xlab="", ylab=rownames(IRFs.k1)[n])
#   if (n==5 | n==6){
#     axis(1,c(1,12,24,37),c("1 Month","1 year","2 years","3 years"))
#     #    axis(1,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37),c("","1 Months","","","6 Months","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","3 Years"))
#   } else {
#     axis(1,c(1,12,24,37),c("1 Month","1 year","2 years","3 years"))
#     #    axis(1,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37),c("","1 Months","","","","6 Months","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","3 Years"))
#   }
#   axis(2,c(ylims[1],0,ylims[2]),round(c(ylims[1],0,ylims[2]),3))
#   polygon(c(hh,(h+1):1), c(IRFs.k1.hdi[1,n,hh],IRFs.k1.hdi[2,n,(h+1):1]), col=mcxs2.shade1,border=mcxs2.shade1)
#   abline(h=0)
#   lines(hh, IRFs.k1[n,hh],lwd=2,col=mcxs1)
# }
# dev.off()
# 
# 
# 
