BaseWeighter <- setRefClass("BaseWeighter",
    fields = list(
      w='list'
    ),
    methods = list (

      combine  = function(vals, w) {
        stop( "Function combine() must be implemented" )
      }
    )
)

LinearWeighter <- setRefClass("LinearWeighter",
    contains = 'BaseWeighter',
    methods = list (
      fit_lm = function(base_states, process_states) {
        return(lm(as.matrix(base_states) ~ as.matrix(process_states)-1)$coefficients)
      },
      get_weights = function(base_states, process_states) {
        w <<- lapply(names(base_states), function(s) fit_lm(base_states[[s]], process_states[[s]]))
        names(w) <<- names(base_states)
      },
      combine  = function(base_states, process_states) {
        get_weights(base_states, process_states)
        weighted_states = lapply(names(w), function(s) apply(process_states[[s]], MARGIN=1,
                                                             FUN=function(row) sum(row*w[[s]])))
        names(weighted_states) = names(w)
        return(weighted_states)
      }
    )
)

ExponentialWeighter <- setRefClass("ExponentialWeighter",
     contains = 'BaseWeighter',
     methods = list (
       # TODO: Implement
       combine  = function(vals, w) {
         stop( "Function combine() must be implemented" )
       }
     )
)


