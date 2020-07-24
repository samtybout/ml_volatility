source("core_functions.R")


# Error Estimation ----

error_sim = function(vol_range, trials, min_size = 100){
  sim_results = data.frame()
  for(vol_sim in vol_range){
    for(trial in 1:trials){
      sim_data = process_series(sim_path(vol_sim/2, vol_sim/2, i_max = 1000))
      size = series_size(sim_data, TRUE)
      if(size > min_size){
        vol_est = ml_vol(sim_data, TRUE)
        sim_results = rbind(sim_results, data.frame(vol_sim = vol_sim, vol_est = vol_est, size = size))
      }
    }
  }
  sim_results$err = sim_results$vol_est - sim_results$vol_sim
  sim_results$err_frac = sim_results$err / sim_results$vol_sim
  return(sim_results)
}

sd_sections = function(sim_results, breaks = 0:10*10){
  n = length(breaks)
  results = data.frame()
  for(section in 1:(n-1)){
    low = breaks[section]
    high = breaks[section+1]
    data = sim_results[sim_results$size > low & sim_results$size < high,]
    n = nrow(data)
    results = rbind(results, data.frame(min = low, max = high, sd = sd(data$err_frac), n = n))
  }
  return(results)
}

sd_window = function(sim_results, window_size = 100, step = 1){
  min_size = min(sim_results$size)
  max_size = max(sim_results$size)
  bottom_range = round(min_size):(round(max_size) - window_size)
  results = data.frame()
  for(bottom in bottom_range){
    data = sim_results[sim_results$size > bottom & sim_results$size < bottom+window_size,]
    n = nrow(data)
    results = rbind(results, data.frame(min = bottom, max = bottom+window_size, sd = sd(data$err_frac), n = n))
  }
  return(results)
}


estimation_test = function(n_trials){
  frame = data.frame()
  for (trial in 1:n_trials){
    size = 0
    len = 0
    while (size < 2 | len < 3){
      vol = runif(1,0.0001,3)
      series = sim_path(vol/2,vol/2,i_max = 100, D_max = 500, t_max = 500)
      size = series_size(series, processed = FALSE)
      len = dim(series)[1]
    }
    estimate = ml_vol(series,processed  = FALSE)
    frame = rbind(frame, data.frame(sim_vol = vol, est_vol = estimate, size = size,
                                    err = (estimate-vol)/vol))
  }
  return(frame)
}


# test = err[err$size > 20 & err$size < 30,]
# hist(test$err_frac, breaks = ((-15:20)/10))

# Standard deviation of estimates seems to stabilize at 15% of true value for size > 100




#----

err = 0
while(err < 5){
  len = 0
  while (len < 5){
    vol = runif(1,0.001,3)
    sim = sim_path(vol/2,vol/2,i_max = 100)
    len = dim(sim)[1]
  }
  estimate = ml_vol(sim,FALSE)
  err = estimate / vol
}
print(sim)
print(vol)
print(estimate)

## Error Analysis ----

err = error_sim(1:20/100, 500)

err_reduced = err[err$size < 1000,]
#err_reduced = err_reduced[err_reduced$size > 10,]
err_reduced = err_reduced[err_reduced$err_frac < 10,]
plot(err_reduced$size, err_reduced$err_frac)



err_sd = sd_sections(err, 1:10*100)
err_sd$ln_sd = log(err_sd$sd)
err_sd$sd_predicted = size_to_dev(err_sd$max)
plot(err_sd$max, err_sd$sd)
plot(err_sd$min, err_sd$ln_sd)



err_win = sd_window(err)
plot(err_win$min, err_win$sd)

err_win_10 = sd_window(err, window_size = 10)
plot(err_win_10$min, err_win_10$sd)

err_win_10$sd_ln = log(err_win_10$sd)
err_win_10$sd_predicted = size_to_dev(err_win_10$min)
plot(err_win_10$min, err_win_10$sd_ln)

plt = ggplot(data = err_win_10)
plt = plt + theme_minimal()
plt = plt + geom_point(aes(x = min, y = sd), alpha = 1/5)
plt = plt + geom_line(aes(x = min, y = sd_predicted))
plt

plt = ggplot(data = err)
plt = plt + theme_minimal()
plt = plt + geom_point(aes(x = size, y = err_frac), alpha = 1/5)
plt

err_win_10$mid = (err_win_10$min+err_win_10$max)/2
ln_sd_line = lm(err_win_10$sd_ln ~ err_win_10$mid)

c1 = ln_sd_line$coefficients[2]
c2 = exp(ln_sd_line$coefficients[1])


