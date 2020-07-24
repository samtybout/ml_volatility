source("core_functions.R")


## Analysis ----

pbdb_data = load_data()
pbdb_data = select_fams(pbdb_data)
family_list = split(pbdb_data, pbdb_data$family, drop = TRUE)
family_list_compiled = lapply(family_list, compile_spp)
family_list_series = lapply(family_list_compiled, series_from_clade)


# Clades to analyze: Mytilidae, Lucinidae, Veneridae, Pectinidae, Pholadomyidae
mytilidae = family_list_series$Mytilidae
lucinidae = family_list_series$Lucinidae
veneridae = family_list_series$Veneridae
pectinidae = family_list_series$Pectinidae
pholadomyidae = family_list_series$Pholadomyidae


# Data validation ----

data_no_singletons = remove_singletons(pbdb_data)

family_list_ns = split(data_no_singletons, data_no_singletons$family, drop = TRUE)
family_list_ns_compiled = lapply(family_list_ns, compile_spp)
family_list_ns_series = lapply(family_list_ns_compiled, series_from_clade)




mytilidae_ns = family_list_ns$Mytilidae
lucinidae_ns = family_list_ns$Lucinidae
veneridae_ns = family_list_ns$Veneridae
pectinidae_ns = family_list_ns$Pectinidae
pholadomyidae_ns = family_list_ns$Pholadomyidae

mytilidae_ns = family_list_ns_series$Mytilidae
lucinidae_ns = family_list_ns_series$Lucinidae
veneridae_ns = family_list_ns_series$Veneridae
pectinidae_ns = family_list_ns_series$Pectinidae
pholadomyidae_ns = family_list_ns_series$Pholadomyidae

all_fam_series = series_from_clade(compile_spp(data_no_singletons))

aic_results = list()
relative_likelihood = data.frame()
for(family_name in names(family_list_ns_series)){
  family = family_list_ns_series[[family_name]]
  if(family_name == "Pholadomyidae"){
    family = family[family$t > 70 & family$t < 170,]
  }
  else{
    family = family[family$t < 100,]
  }
  test = aic_test(family)
  aic_results[[family_name]] = test
  rl = exp((min(test$AIC) - max(test$AIC))/2)
  relative_likelihood = rbind(relative_likelihood, data.frame(family = family_name, rl = rl))
}

