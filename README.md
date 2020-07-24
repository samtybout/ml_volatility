# Estimating Evolutionary Volatility in a Maximum-Likelihood Framework

The scripts in this repository are built to generate and analyze maximum-likelihood estimates of evolutionary volatility using fossil data. The input data should come from the [paleobiology database](www.pbdb.org). There is an example dataset, `pbdb_data_full.csv`, in this repository. Below is a demonstration of how to use the code:


## Initialization

All of the important functions used in volatility analysis are stored in `core_functions.R`.

```{r setup}
source("core_functions.R")
set.seed(0)
```

The script has one dependency, a package called `data.table`, which lets it process occurence data more efficiently.

## Loading Data

This code is built to work with `.csv` files downloaded from the Paleobiology Database. The way it's built right now requires that all occurrences have a positive species identification. If you're downloading data yourself, make sure you get the `accepted_name` field, as well as the `genus` and `family` fields. Here we'll be using the same bivalve dataset explored in the text:

```{r data}
pbdb_data = load_data(path = "./pbdb_data_full.csv")
```
In particular, let's look at the family Mytilidae:
```{r mytilidae}
pbdb_data = split(pbdb_data, pbdb_data$family)$Mytilidae
```
The `load_data` function takes a filepath to a CSV, and returns a data table. You may have to adjust the `skip` argument if your CSV has a header with a different number of lines than the example file here.

There are a few R packages, like `velociraptr`, which can download PBDB data directly from R. I prefer to go to the website for any large, detailed queries.

## Processing Data

Let's start by removing any occurrences that are the only representative of their species in the dataset. This is accomplished with the `remove_singletons` function.
```{r}
pbdb_data = remove_singletons(pbdb_data)
```
Doing this isn't strictly necessary; the script can handle data with singletons in it, but can't directly calculate likelihoods, which means it can't find Akaike Information Criterion scores.

PBDB datasets consist of fossil occurrences, but the volatility analyses in this script want a series of speciation and extinction events. The function `compile_spp` takes a set of occurrences and returns a table of species with the age of their first and last occurrence in the dataset.

```{r compile species}
mytilidae_spp = compile_spp(pbdb_data)
head(mytilidae_spp)
```

This function also assigns a virtual age to each fossil occurence, randomly drawn from within its time bin. Note the difference in ages:

```{r randomization example}
set.seed(1)
head(compile_spp(pbdb_data))
set.seed(2)
head(compile_spp(pbdb_data))
```
From here, we can turn the species data into a series of speciation and extinction events using `series_from_clade`.

```{r series}
mytilidae_series = series_from_clade(mytilidae_spp)
head(mytilidae_series)
```
`t` is the age in Ma of the event, `dD` is the change in diversity (1 for speciation, -1 for extinction), `type` is `TRUE` for speciation and `FALSE` for extinction, `D` is the standing diversity just after the event, and `dt` is the time in Ma elapsed since the last event. For periods of time where few fossils are known, you may see `D` go to 0 as it does for the earliest part of Mytilidae's history.

## Estimating volatility

Once we have the data in series form, we can find the maximum-likelihood volatility with `ml_vol`.

```{r ml_vol}
ml_vol(mytilidae_series)
```
We can also find the maximum-likelihood speciation and extinction rates separately, if we don't think they're equal, by using `ml_params`.
```{r ml_params}
ml_params(mytilidae_series)
```
The first number is speciation rate and the second is extinction rate. Note that volatility is equal to the sum of the speciation and extinction rates.
