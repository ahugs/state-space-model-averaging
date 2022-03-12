library(bsts)
library(parallel)

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
             error=function(e) {print(e)}
           )
           return(list(x=bsts_x, models=bsts_models))
         }
       )
)


DLMEstimator <- setRefClass("DLMEstimator",
     contains = "BaseEstimator",
     #TODO: Implement 
     methods = list (
       fit_single_frequency = function(freq, y, lags) {
         stop("Function 'fit_single_frequency' must be implemented")
       }
     )
)

StofferEstimator <- setRefClass("StofferEstimator",
    contains = "BaseEstimator",
    #TODO: Implement 
    methods = list (
      fit_single_frequency = function(freq, y, lags) {
        stop("Function 'fit_single_frequency' must be implemented")
      }
    )
)
