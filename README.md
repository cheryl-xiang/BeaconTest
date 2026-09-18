# Beacon
Beacon is an R package...

## Installation (R/Rstudio)
```
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")  # ensure devtools is installed correctly

devtools::install_github('cheryl-xiang/BeaconTest') # install beacon
library(deMULTIplex2) # load Beacon for use
```

## Dependencies
none...?

# Beacon Overview

### Functions
...

# Tutorial

## Step 1: Data Loading
The HTO count matrix should have HTOs as rows and cells as columns (transpose if necessary). Remove any extra non-HTO rows.

```
hto_counts <- read.csv('path/to/barcode_matrix')                                   # Load data
hto_counts <- t(hto_counts)                                                        # Transpose
hto_counts <- hto_counts[, !rownames(hto_count) %in% c('nUMI', 'nUMI_total')]      # Remove non-HTOs
```

## Step 2: Set Parameters
Beacon has four tunable parameters with the following default values: 

| Parameter | Default | Description |
|-----------|---------|-------------|
| sig_max | 3 | Maximum signal threshold |
| min_count | 2 | Minimum count filter |
| max_bf | 0.97 | Maximum Bayes factor |
| max_iter | 30 | Maximum iterations |

```
params <- list()
params$sig_max <- 3
params$min_count <- 2
params$max_bf <- 0.97
params$max_iter <- 30
```

## Step 3: Run Beacon
`beacon_calls()` will return a vector of numeric assignments:
- `1-N` = singlet assigned to barcode N
- `1000` = doublet/multiplet
- `0` = negative

```
calls <- beacon_calls(hto_count, params)
```

An example workflow for Beacon on the Bar11 dataset can be found here: [Beacon workflow notebook](beacon_tutorial.ipynb)

## Citation

If you use Beacon in your work please cite...

## References
...