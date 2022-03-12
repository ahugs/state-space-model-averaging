library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)

TEXT_FORMAT = theme(axis.text.x = element_text(size = 15),
                    axis.text.y = element_text(size = 15),
                    axis.title.x = element_text(size = 15),
                    axis.title.y = element_text(size = 15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=12))

Experiment <- setRefClass("Experiment",
    #' Runs an experiment
    #'
    #' For the provided parameters and process object, generate true states.
    #' Apply models specified by model_generators to get estimated states.
    #' Write results and plot summary of results.
    #'
   fields = list(
     model_generators = "list",
     experiment_params = "list",
     process = "ANY",
     true_states = "list",
     run_times = "list",
     fitted_states = "list",
     models = "list",
     results_df='data.frame',
     diffs_df='data.frame',
     results_dir="character"
   ),
   methods = list(
     initialize = function(model_generators, process, experiment_params) {
       model_generators <<- model_generators
       experiment_params <<- experiment_params
       process <<- process
       true_states <<- list()
       run_times <<- list()
       fitted_states <<- list()
       experiment_timestamp = Sys.time()
       results_dir <<- glue("results/experiment_{format(experiment_timestamp,'%y%m%d%H%M%S')}")
       dir.create(results_dir)
     },
     run = function(){

       for (i in seq_along(experiment_params)){
          results = run_from_params(experiment_params[[i]])
          true_states[[i]] <<- results$true_states
          fitted_states[[i]] <<- results$fitted_states
          run_times[[i]] <<- results$run_time
       }
       agg_results()
     },
     run_from_params = function(run_params) {
       true_states = process$generate_from_params(run_params)
       models = lapply(model_generators, function(model) model(run_params))
       run_time = lapply(models, function(m) system.time(m$fit(true_states$y))[['elapsed']])
       fitted_states = lapply(models, function(m) m$fitted_states)
       return(list(true_states=true_states,run_time=run_time, fitted_states=fitted_states))
     },
     agg_results = function() {
       # fitted states
       df = data.frame(do.call(rbind, fitted_states))
       df$run_id = seq(1,length(experiment_params),1)
       df = df %>% pivot_longer(!run_id, names_to = "model", values_to = "results")
       df = unnest_longer(df, col='results', indices_to='state', values_to='t')
       df = unnest_wider(df, col='t', names_sep='')
       df = data.frame(df)[c('run_id', 'model', 'state',paste('t', seq(1,process$N), sep=''))]

       # run parameters
       params_df = data.frame(do.call(rbind, lapply(experiment_params, unlist)))
       params_df$run_id = seq(1,length(experiment_params),1)

       # run times
       run_times_df = as.data.frame(do.call(rbind, lapply(run_times, unlist)))
       run_times_df$run_id = seq(1,length(experiment_params),1)
       run_times_df = run_times_df %>% pivot_longer(!run_id, names_to='model', values_to='time_elapsed')

       # true states
       true_states_df = data.frame(do.call(rbind, experiment$true_states))
       true_states_df$run_id = seq(1,length(experiment$experiment_params),1)
       true_states_df = true_states_df %>% pivot_longer(!run_id, names_to = "state", values_to = "t")
       true_states_df = data.frame(unnest_wider(true_states_df, col='t', names_sep=''))
       true_states_df$model = "true"

       results_df = rbind(df, true_states_df)
       results_df = merge(params_df, results_df, by='run_id', all.y=TRUE)
       results_df <<- merge(run_times_df, results_df, by=c('run_id', 'model'), all.y=TRUE)
     },
     save_results = function() {
       write.csv(results_df, glue("{results_dir}/results.csv"))
     },
     plot = function() {

       true_states_df = results_df %>% filter(model == "true") %>%
         select(c('run_id', 'state', paste('t', seq(1,process$N),sep='')))
       model_results_df = results_df %>% filter(model != "true") %>%
         select(c('run_id', 'model', 'state', paste('t', seq(1,process$N),sep='')))
       combined_df = merge(true_states_df, model_results_df, by=c('run_id', 'state'),
                           suffixes=c('true', 'fitted'))
       diffs_df =  cbind(combined_df[, c('run_id', 'state', 'model')],
                        (combined_df[, paste('t', seq(1, process$N), 'fitted', sep='')] -
                         combined_df[, paste('t', seq(1, process$N), 'true', sep='')])
       )
       diffs_df$state = factor(diffs_df$state, levels=process$states)
       diffs_df = diffs_df %>% mutate(me = apply(select(.,-c('run_id','state','model')), 1, mean))
       diffs_df <<- diffs_df %>% mutate(rmse = apply(select(.,-c('run_id','state','model')), 1,
                                                     function(row) sqrt(mean(row^2))))

       plot_mean_run_time()
       plot_avg_states()
       plot_hist_errors()
       plot_mean_errors()
       plot_median_errors()
       plot_winners()
       plot_failure_rate()
     },
     plot_mean_run_time = function() {
        mean_run_time_df = results_df %>% filter(model != 'true') %>%
                          group_by(run_id, model) %>% summarise(time_elapsed=mean(time_elapsed)) %>%
                          group_by(model) %>% summarise(time_elapsed=mean(time_elapsed))
        mean_run_time_p <- ggplot(mean_run_time_df, aes(x=model, y=time_elapsed, fill=model)) +
                           geom_bar(stat = 'identity', alpha=0.25) +
                            ggtitle(sprintf("Mean Run Time (s)")) +
                            TEXT_FORMAT
        print(mean_run_time_p)
        ggsave(glue("{results_dir}/mean_run_times.pdf"))
     },
     plot_avg_states = function(){
       avg_states_df = experiment$results_df %>% group_by(model, state) %>%
         summarise_at(.vars = paste("t", seq(1, experiment$process$N), sep=''),
                      .funs = c(mean=function(x) mean(x, na.rm=TRUE)))
       for(state in unique(avg_states_df$state)){
         state_df = data.frame(avg_states_df[avg_states_df$state==state, ] %>% select(-c('state')))
         colnames(state_df) = c('model', seq(1, experiment$process$N))
         state_df = reshape2::melt(state_df, id.vars='model', variable.name='t')
         state_df$t = as.numeric(state_df$t)
         state_p = ggplot(state_df, aes(x=t, y=value, col=model)) + geom_line() +
                   ggtitle(glue("Average State By Model, {state}")) +
                   TEXT_FORMAT

         print(state_p)
         ggsave(glue("{results_dir}/avg_state_{state}.pdf"))
       }
     },
     plot_hist_errors = function(){
       for(state in unique(diffs_df$state)){
         hist_p <- ggplot(diffs_df[diffs_df$state==state, ]) +
                   geom_histogram(aes(x=me, fill=model, bins=40)) +
                   facet_wrap(~model) +
                   ggtitle(sprintf("Mean Error Distribution of State %s", state)) +
                   TEXT_FORMAT
         print(hist_p)
         ggsave(glue("{results_dir}/hist_{state}.pdf"))
       }
     },
     plot_mean_errors = function() {
       mean_df = diffs_df %>% group_by(state, model) %>% summarise(mean=mean(rmse, na.rm=TRUE))
       mean_p = ggplot(mean_df, aes(x=model, y=mean, fill=model)) +
                geom_bar(stat='identity', alpha=0.25) +
                facet_wrap(~state) +
                ggtitle("Mean RMSE By Model") +
                TEXT_FORMAT
       print(mean_p)
       ggsave(glue("{results_dir}/mean.pdf"))
     },
     plot_median_errors = function() {
       median_df = diffs_df %>% group_by(state, model) %>% summarise(median=median(rmse, na.rm=TRUE))
       median_p = ggplot(median_df, aes(x=model, y=median, fill=model)) +
         geom_bar(stat='identity', alpha=0.25) +
         facet_wrap(~state) +
         ggtitle("Median RMSE By Model") +
         TEXT_FORMAT
       print(median_p)
       ggsave(glue("{results_dir}/median.pdf"))
     },
     plot_winners = function() {
        winner_df = diffs_df %>% group_by(run_id, state) %>% slice(which.min(rmse))
        winner_p = ggplot(winner_df, aes(x=model, fill=model)) +
         geom_bar(alpha=0.25) +
         facet_wrap(~state) +
         ggtitle("Winner Count (By Lowest RMSE)") +
         TEXT_FORMAT
        print(winner_p)
        ggsave(glue("{results_dir}/winner.pdf"))
     },
    plot_failure_rate = function(){
      failure_df = diffs_df %>% group_by(model, state) %>% summarise(failure=mean(is.na(rmse), na.rm=TRUE))
      failure_p = ggplot(failure_df, aes(x=model, y=failure, fill=model)) +
                  geom_bar(stat = 'identity', alpha=0.25) +
                  facet_wrap(~state) +
                  ggtitle("Percent Run Errors") +
                  TEXT_FORMAT
      print(failure_p)
      ggsave(glue("{results_dir}/failure_rate.pdf"))
    }
   )
)




