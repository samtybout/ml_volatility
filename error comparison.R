source("core_functions.R")
source("analysis.R")

# This analysis takes about an hour
vol_results = lapply(family_list, ml_vol_dist, 1000)
vol_results_ns = lapply(family_list_ns, ml_vol_dist, 1000)

for(family_name in names(vol_results)){
  result = vol_results[[family_name]]
  write.csv(result, file = paste(family_name, "_vol_dist", ".csv",sep = ""))
}

for(family_name in names(vol_results_ns)){
  result = vol_results_ns[[family_name]]
  write.csv(result, file = paste(family_name, "_ns_vol_dist", ".csv",sep = ""))
}