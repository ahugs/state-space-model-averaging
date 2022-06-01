source("R/weighter.R")
source("R/estimators.R")
source("R/process.R")
source("R/models.R")
source("R/experiment.R")


params=list(alpha=0.15, gamma=0.035, beta=0.0525, I_0=10, S_0=990, E_0=0,
            R_0=0, sig_g=0.02, sig_y=0.02, N=1000)
model_generators = list(
  ma = function(params) ModelAverager(estimator=StofferEstimator(freqs=c(1,2,3,4), lags=1),
                                      weighter=LinearWeighter(),
                                      process=SEIRProcess(params)),
  ar1 = function(params) AR(estimator=StofferEstimator(freqs=c(1), lags=1),
                            process=SEIRProcess(params)),
  ar2 = function(params) AR(estimator=StofferEstimator(freqs=c(1), lags=2),
                            process=SEIRProcess(params)),
  ar3 = function(params) AR(estimator=StofferEstimator(freqs=c(1), lags=3),
                            process=SEIRProcess(params))
)

experiment = Experiment(model_generators=model_generators,
                        experiment_params=rep(list(params),200),
                        process=SEIRProcess(list(N=1000)))
experiment$run()
experiment$save_results()
experiment$plot()