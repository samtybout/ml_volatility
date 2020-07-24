# Setup ----
library(ggplot2)
library(gridExtra)
source("core_functions.R")
source("analysis.R")


# Plot diversity series----
d_series_plot = function(series){
  plt = ggplot(series)
  plt = plt + theme_minimal()
  plt = plt + geom_line(aes(x = t, y = D))
  plt = plt + scale_x_reverse()
  return(plt)
}

projection_plot = function(series, t, xmin = 0, bounds = c(250,0), confidence = 0.95){
  plt = d_series_plot(series)
  projection = conf_series_from_D_series(time_section(series,t), t:xmin, confidence = confidence)
  plt = plt + geom_line(data = projection, color = "blue", linetype = "dotted", size = 1.5, lineend = "round",
                        aes(x = projection$t, y = projection$lower))
  plt = plt + geom_line(data = projection, color = "blue", linetype = "dotted", size = 1.5, lineend = "round",
                        aes(x = projection$t, y = projection$upper))
  plt = plt + xlim(bounds) +
    xlab("Time (Ma)") + ylab("Species")
  return(plt)
}

density_plot = function(n0,t_series,spp,ext,n_max,interpolate=FALSE, contours = TRUE,
                        confints = c(), breaks = c(0.05,0.1,0.25,0.5,0.9,0.95)){
  data = data.frame()
  for(time in t_series){
    for(n in 0:n_max){
      data = rbind(data, data.frame(t = time, n = n, p = pnt(n0,n,time,spp,ext)))
    }
  }
  plt = ggplot(data,aes(x = t, y = n))
  plt = plt + geom_raster(aes(fill = p),interpolate=interpolate)
  if(contours){
    plt = plt + geom_contour(aes(z = p),colour = "white",breaks = breaks,
                             show.legend = TRUE)
  }
  for(interval in confints){
    conf_data = conf_series(n0,t_series,spp,ext,interval)
    plt = plt + geom_line(data=conf_data,aes(x = t, y = lower),colour = "white") + 
      geom_line(data=conf_data,aes(x = t, y = upper),colour = "white")
  }
  plt = plt + theme_minimal()
  plt
}

# Back-projection----
p1 = projection_plot(pholadomyidae_ns, 70)+ggtitle("Pholadomyidae", )+
  theme(plot.title=element_text(hjust=0.5))
p2 = projection_plot(lucinidae_ns, 70)+ggtitle("Lucinidae", )+
  theme(plot.title=element_text(hjust=0.5))
p3 = projection_plot(mytilidae_ns, 70)+ggtitle("Mytilidae", )+
  theme(plot.title=element_text(hjust=0.5))
p4 = projection_plot(pectinidae_ns, 70)+ggtitle("Pectinidae", )+
  theme(plot.title=element_text(hjust=0.5))
p5 = projection_plot(veneridae_ns, 70)+ggtitle("Veneridae", )+
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(p1,p2,p3,p4,p5, ncol = 1)

p2 = projection_plot(lucinidae_ns, 0,50)+ggtitle("Lucinidae", )+
  theme(plot.title=element_text(hjust=0.5))
p3 = projection_plot(mytilidae_ns, 0,50)+ggtitle("Mytilidae", )+
  theme(plot.title=element_text(hjust=0.5))
p4 = projection_plot(pectinidae_ns, 0,50)+ggtitle("Pectinidae", )+
  theme(plot.title=element_text(hjust=0.5))
p5 = projection_plot(veneridae_ns, 0,50)+ggtitle("Veneridae", )+
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(p2,p3,p4,p5)

# Plot fossil spp and ext estimates----
estimate_params = function(family_list, min_D){
  param_data = data.frame()
  allnames = names(family_list)
  namelist = c()
  for (i in 1:length(family_list)){
    family = family_list[[i]]
    if (dim(family)[1] >= min_D){
      family = series_from_clade(family)
      vol = ml_vol(family)
      maxD = max(family$D)
      name = allnames[i]
      param_data = rbind(param_data, data.frame(vol = vol, maxD = maxD, name = name))
      namelist = c(namelist,name)
    }
  }
  rownames(param_data) = namelist
  param_data
}
param_data = estimate_params(family_list, 100)
plt = ggplot(param_data, aes(x = vol, y = maxD))
plt = plt + geom_text(aes(label = name), vjust = -0.5, hjust = -0.1)
plt = plt + theme_minimal()
plt = plt + geom_point(colour = "dark green")
plt


# Plot estimation accuracy for simulated volatility----
sim_data = estimation_test(500)

plt = ggplot(sim_data,aes(x = sim_vol, y = est_vol, colour = size))
ms = max(sim_data$size)
plt = plt + scale_color_gradientn(colours = c("red","orange","yellow"),
                                  values = c(0.0,0.01,0.2,1),
                                  breaks = c(0,20,300,ms))
plt = plt + theme_minimal()
plt = plt + geom_point()
plt = plt + coord_fixed()
plt

plt = ggplot(sim_data,aes(x=size,y=err))
plt = plt + theme_minimal()
plt = plt + geom_point(colour = "blue")
plt
# Volatility example plot----
l1 = sim_path(1,1,10)
l2 = sim_path(1,1,10)
l3 = sim_path(1,1,10)
h1 = sim_path(2,2,10)
h2 = sim_path(2,2,10)
h3 = sim_path(2,2,10)

plt = ggplot()
for(series in list(l1,l2,l3)){
  plt = plt + geom_line(data = series, aes(x = t, y = D), colour = "blue")
}
for(series in list(h1,h2,h3)){
  plt = plt + geom_line(data = series, aes(x = t, y = D), colour = "red")
}
plt = plt + 
  theme_minimal() +
  xlab("Time") +
  ylab("Diversity")
plt

d_series_plot(pectinidae_ns)

d_series_plot(series_from_clade(compile_spp(pectinidae_ns))) +
  xlim(275,0) + ylim(0,125) +
  xlab("Time") + ylab("Diversity")




# Plot diversity of all bivalves----
all_bivalves = load_data("G:/My Drive/Thesis - Synced/all_bivalves.csv")
bivalve_spp = compile_spp(all_bivalves)
bivalve_series = series_from_clade(bivalve_spp)
bivalve_plot = d_series_plot(bivalve_series) +
  xlab("Age (Ma)") +
  ylab("Diversity")


bivalves_mesozoic = time_section(bivalve_series, 65, 252)
bivalves_cenozoic = time_section(bivalve_series, 0,65)

p1 = vol_dist_plot("Mytilidae", vol_results, vol_results_ns, breaks)
p2 = vol_dist_plot("Lucinidae", vol_results, vol_results_ns, breaks)
p3 = vol_dist_plot("Veneridae", vol_results, vol_results_ns, breaks)
p4 = vol_dist_plot("Pectinidae", vol_results, vol_results_ns, breaks)
p5 = vol_dist_plot("Pholadomyidae", vol_results, vol_results_ns, breaks)

grid.arrange(p1,p2,p3,p4,p5,nrow = 5)

ggplot(data = mytilidae_dist) + geom_histogram(aes(x = vol_est), breaks = breaks)

#mytilidae_dist = ml_vol_dist(mytilidae, 1000)

mytilidae_dist = Mytilidae_ns_vol_dist

breaks = seq(0.10, 0.40, 0.001)

mbreaks = seq(0.1, 0.15, 0.001)

mytilidae_dist_p = vol_dist_p(mytilidae_dist, mbreaks)

mytilidae_dist_p$mid = (mytilidae_dist_p$lower + mytilidae_dist_p$upper) / 2

mytilidae_dist_p$age_freq = vol_freq(mytilidae_dist, mbreaks)$freq

mytilidae_dist_p$est_p = normal_bins(mean(mytilidae_dist$vol_est), 
                                     size_to_dev(mean(mytilidae_dist$size)), mbreaks)

# Error sources comparison
plt = ggplot(mytilidae_dist_p, aes(x = mid))+
  geom_col(aes(y = age_freq), fill = "blue", color = "blue") + 
  geom_line(aes(y = p), color = "white", size = 2)+
  geom_line(aes(y = est_p), color = "white", size = 2) + 
  geom_line(aes(y = p), color = "purple3", size = 1)+
  geom_line(aes(y = est_p), color = "red", size = 1) + 
  xlab("Volatility") +
  ylab("Probability") +
  theme_minimal()
plt

lbreaks = seq(0.18,0.26,0.001)
Lucinidae_dist_p = vol_dist_p(Lucinidae_ns_vol_dist, lbreaks)
Lucinidae_dist_p$mid = (Lucinidae_dist_p$lower + Lucinidae_dist_p$upper) / 2
Lucinidae_dist_p$age_freq = vol_freq(Lucinidae_ns_vol_dist, lbreaks)$freq
Lucinidae_dist_p$est_p = normal_bins(mean(Lucinidae_ns_vol_dist$vol_est),
                                     size_to_dev(mean(Lucinidae_ns_vol_dist$size)), lbreaks)


# Error sources comparison
plt = ggplot(Lucinidae_dist_p, aes(x = mid))+
  geom_col(aes(y = age_freq), fill = "blue", color = "blue") + 
  geom_line(aes(y = p), color = "white", size = 2)+
  geom_line(aes(y = est_p), color = "white", size = 2) + 
  geom_line(aes(y = p), color = "purple3", size = 1)+
  geom_line(aes(y = est_p), color = "red", size = 1) + 
  xlab("Volatility") +
  ylab("Probability") +
  theme_minimal()
plt



vol_dist_plot = function(family_name, full_data, ns_data, breaks = breaks, pmax = 0.27){
  plt = ggplot()
  dist_full = vol_dist_p(full_data[[family_name]], breaks)
  dist_ns = vol_dist_p(ns_data[[family_name]], breaks)
  dist_full$mid = (dist_full$low + dist_full$high)/2
  dist_ns$mid = (dist_ns$low + dist_ns$high)/2
  plt = plt + geom_line(data = dist_full, aes(x = mid, y = p), color = "red")
  plt = plt + geom_line(data = dist_ns, aes(x = mid, y = p), color = "blue")
  mean_full = mean(full_data[[family_name]]$vol_est)
  mean_ns = mean(ns_data[[family_name]]$vol_est)
  plt = plt + geom_segment(data = dist_full, aes(x = mean_full, xend = mean_full,
                                                 y = 0, yend = pmax),
                           color = "red", linetype = "dashed")
  plt = plt + geom_segment(data = dist_ns, aes(x = mean_ns, xend = mean_ns,
                                               y = 0, yend = pmax),
                           color = "blue", linetype = "dashed")
  plt = plt + ylim(c(0,pmax))
  plt = plt + xlab(NULL) + ylab(NULL)
  plt = plt + theme_minimal()
  plt = plt + ggtitle(family_name) + theme(plot.title = element_text(hjust = 0.5))
  return(plt)
}

plot_p_dist = function(vol_dist, min_vol, max_vol, binwidth){
  breaks = seq(min_vol, max_vol, binwidth)
  data = vol_dist_p(vol_dist, breaks)
  data$mid = (data$lower + data$upper)/2
  plt = ggplot(data = data, aes(x = mid, y = p))
  plt = plt + geom_col(width = binwidth)
  return(plt)
}