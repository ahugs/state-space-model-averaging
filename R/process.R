library(purrr)

BaseProcess <- setRefClass("BaseProcess",
      methods = list(
        generate_from_y = function(y) {
          stop("Function 'generate_from_y' must be implemented")
        },
        generate = function(l) {
          ret = lapply(l, function(x) generate_from_y(x))
          state_names = names(ret[[1]])
          ret = lapply(state_names, function(s) lapply(ret, FUN = function(x) x[[s]]))
          ret = lapply(ret, function(x) as.data.frame(matrix(unlist(x),nrow=length(x[[1]]),byrow=FALSE)))
          names(ret) = state_names
          return(ret)
        }
      )
)

SEIRProcess <- setRefClass("SEIRProcess",
      contains = "BaseProcess",
      fields = list(
        states = "character",
        N = "numeric",
        S_0 = "numeric",
        E_0 = "numeric",
        I_0 = "numeric",
        R_0 = "numeric",
        alpha = "numeric",
        gamma = "numeric",
        beta = "numeric",
        sig_y = "numeric",
        sig_g = "numeric"
      ),
      methods = list(
        set_params = function(params) {
          invoke(.self$initFields, params)
        },
        initialize = function(params) {
          states <<- c('S', 'E', 'I', 'R')
          set_params(params)
        },
        generate_from_y = function(y) {
          pop_0 = S_0 + E_0 + I_0 + R_0
          S = rep(NA, N); S[1] = S_0
          E = rep(NA, N); E[1] = E_0
          I = rep(NA, N); I[1] = I_0
          R = rep(NA, N); R[1] = R_0
          for(i in 1:length(y)){
            I[i+1] = (1 + y[i])*I[i]
            S[i+1] = S[i] - beta*S[i]*I[i]/pop_0
            E[i+1] = E[i] + beta*S[i]*I[i]/pop_0 - alpha*E[i]
            R[i+1] = R[i] + gamma*I[i]
            }
          return(list(S=S, E=E, I=I, R=R))
        },
        generate_from_params = function(params=NULL) {
          if(!is.null(params)) {
            set_params(params)
          }
          pop_0 = S_0 + E_0 + I_0 + R_0
          S = rep(NA, N); S[1] = S_0
          E = rep(NA, N); E[1] = E_0
          I = rep(NA, N); I[1] = I_0
          R = rep(NA, N); R[1] = R_0
          y = rep(NA, N)
          g = rep(NA, N)

          for(i in 1:(N-1)){
            g[i+1] = alpha * E[i]/I[i] - gamma + rnorm(1, 0, sig_g)
            y[i+1] = g[i+1] + rnorm(1, 0, sig_y)
            I[i+1] = (1 + g[i+1])*I[i]
            S[i+1] = S[i] - beta*S[i]*I[i]/pop_0
            E[i+1] = E[i] + beta*S[i]*I[i]/pop_0 - alpha*E[i]
            R[i+1] = R[i] + gamma*I[i]
          }
          return(list(S=S, E=E, I=I, R=R, g=g[-1], y=y[-1]))
        }
      )
)


IProcess <- setRefClass("IProcess",
       contains = "BaseProcess",
       fields = list(
         states = "character",
         params = "list",
         I_0 = "numeric"
       ),
       methods = list(
         initialize = function(params) {
           states = c("I")
           I_0 <<- params[['I_0']]
         },
         generate_from_y = function(y) {
           I = rep(NA, length(y) + 1); I[1] = I_0
           for(i in 1:length(y)){
             I[i+1] = (1 + y[i])*I[i]
           }
           return(list(I=I))
         },
         # TODO: Implement
         generate_from_params = function(N) {
           stop("Function generate_from_params must be implemented")
         }
       )
)
