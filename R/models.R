ModelAverager <- setRefClass("ModelAverager",
   fields = c(
     'weighter',
     'estimator',
     'process',
     'weights',
     'models',
     'raw_states',
     'process_states',
     'y_states',
     'fitted_states'
   ),
   methods = list(
     initialize = function(estimator, weighter, process) {
        weighter <<- weighter
        estimator <<- estimator
        process <<- process
     },
     fit = function(y) {
       estimator$fit(y)
       raw_states <<- estimator$x
       models <<- estimator$models
       process_states <<- process$generate(raw_states)
       y_states <<- process$generate(list(y))
       fitted_states <<- weighter$combine(y_states, process_states)
     },
     predict = function(steps) {
       if(class(ma$weights) == "uninitializedField") {
         stop("Cannot predict before calling fit")
       }
     }
   )
)

AR <- setRefClass("AR",
   fields = c(
     'estimator',
     'process',
     'models',
     'raw_states',
     'fitted_states'
   ),
   methods = list(
     initialize = function(estimator, process) {
       estimator <<- estimator
       process <<- process
     },
     fit = function(y) {
       estimator$fit(y)
       raw_states <<- estimator$x
       models <<- estimator$models
       process_states = process$generate(raw_states)
       fitted_states <<- lapply(process_states, function(df) df[,1])
     },
     predict = function(steps) {
       if(class(ma$weights) == "uninitializedField") {
         stop("Cannot predict before calling fit")
       }
     }
   )
)



