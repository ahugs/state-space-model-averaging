library(bsts)
library(dlm)
library(parallel)
library(astsa)


BaseEstimator <- setRefClass("BaseEstimator",
        fields = list(
          freqs = 'numeric',
          lags = 'numeric',
          models = 'list',
          x = 'list'
        ),
        methods = list(
          fit_single_frequency = function(freq, y, lags) {
            stop("Function 'fit_single_frequency' must be implemented")
          },
          fit = function(y) {
           model_results = mclapply(freqs, FUN=function(freq) fit_single_frequency(freq=freq, y=y, lags), mc.cores=4)
           for(model_result in model_results){
             if(model_result[['success']] == 0) {
               print(model_results[['e']])
             }
           }
           x <<- lapply(model_results, FUN=function(x) x[['x']])
           models <<- lapply(model_results, FUN=function(x) x[['models']])
          }
        )
)

BSTSEstimator <- setRefClass("BSTSEstimator",
       contains = "BaseEstimator",
       fields = list(
         niter = "numeric"
       ),
       methods = list (
         fit_single_frequency  = function(freq, y, lags) {
           N = length(y)
           bsts_models = list()
           bsts_x = rep(NA, N)

           tryCatch(
             for(i in 1:freq){
               y_sub = ts(y[seq(i, N, freq)])
               model <- bsts(y_sub, AddAr(list(), y=y_sub, lags=lags), niter=niter)
               bsts_models[[i]] = model
               burn = SuggestBurn(0.1, model)
               bsts_x[seq(i, N, freq)] = colMeans(model$state.contributions[-(1:burn),,])
             },
             error=function(e) {return(list(x=NA, models=NA, success=0, error=e))}
           )
           return(list(x=bsts_x, models=bsts_models, success=1, error=NA))
         }
       )
)


DLMEstimator <- setRefClass("DLMEstimator",
     contains = "BaseEstimator",
     methods = list (
       fit_single_frequency = function(freq, y, lags) {
         N = length(y)
         dlm_models = list()
         dlm_x = rep(NA, N)

         
         tryCatch(
           for(i in 1:freq){
             y_sub = ts(y[seq(i, N, freq)])
             
             model <- dlmMLE(y=y_sub,parm=c(rep(0, lags), 0.5^2, 0.5^2),
                             build=function(parm) dlmModARMA(ar=parm[1:lags], 
                                                             sigma2=parm[lags+1],
                                                             dV=parm[lags+2]))
             if(model$convergence != 0) {
               model <- dlmMLE(y=y_sub,parm=c(rep(0, lags), 0.5^2, 0.5^2), method='SANN',
                              build=function(parm) dlmModARMA(ar=parm[1:lags], 
                                                              sigma2=parm[lags+1],
                                                              dV=parm[lags+2]))
             }
             if(model$convergence != 0){
               stop("Failed to converge")
             }
             dlm_models[[i]] = model
             states <- dlmSmooth(y=y_sub, mod=dlmModARMA(ar=model$par[1:lags],
                                                         sigma2=model$par[lags+1],
                                                         dV=model$par[lags+2]))$s
             if (dim(data.frame(states))[2] > 1) {
               states = states[-1,1]
             } else {
               states = states[-1]
             }
             dlm_x[seq(i, N, freq)] = states
           },
         error=function(e) {return(list(x=NA, models=NA, success=0, error=e))}
         )
         
         return(list(x=dlm_x, models=dlm_models, success=1, error=NA))
         }
       )
)

StofferEstimator <- setRefClass("StofferEstimator",
    contains = "BaseEstimator",
    methods = list (
      fit_single_frequency = function(freq, y, lags) {
        N = length(y)
        stoffer_x = rep(NA, N)
        stoffer_models = list()
        
        tryCatch(
          for(i in 1:freq){
            y_sub = ts(y[seq(i, N, freq)])
            init.par = c(rep(0, lags), 0.5^2, 0.5^2)
          
            
            A = matrix(c(1, rep(0, lags-1)), 1, lags)
            Sigma0 = diag(100, lags)
            mu0 = matrix(0, lags)
            # Function to evaluate the likelihood
            Linn=function(para){
              phi = t(rbind(para[1:lags],diag(lags)[-lags,] ))
              sig_evol = matrix(0, lags, lags)
              sig_evol[1,1] = para[lags+1]
              sig_obs = para[lags+2]
              kf = Kfilter0(length(y_sub), y_sub, A, mu0=mu0, Sigma0, phi,  sig_evol, sig_obs)
              return(kf$like)
            }
            
            # Estimation
            est = optim(init.par, Linn, gr=NULL, method="BFGS", hessian=TRUE)
            if(est$convergence != 0){
              stop("Failed to converge")
            }
            stoffer_models[[i]] = est
            
            phi = est$par[1:lags]
            phi = t(rbind(phi,diag(lags)[-lags,] ))
            sig_evol = matrix(0, lags, lags)
            sig_evol[1,1] = est$par[lags+1]
            sig_obs = est$par[lags+1]
            states = Ksmooth0(length(y_sub), y_sub, A, mu0=mu0, Sigma0, phi,  sig_evol, sig_obs)$xs
            
            stoffer_x[seq(i, N, freq)] = states
          },
          error=function(e) {return(list(x=NA, models=NA, success=0, error=e))}
        )
        return(list(x=stoffer_x, models=stoffer_models, success=1, error=NA))
      }
    )
)


NoModelEstimator <- setRefClass("NoModelEstimator",
    contains = "BaseEstimator",
    methods = list (
      fit_single_frequency = function(freq, y, lags) {
        return(list(x=y, models=list(), success=1, error=NA))
      }
    )

)
