# sdam

## Digital tools for the SDAM project at Aarhus University

This package provides digital tools for the [*Social Dynamics and complexity
in the Ancient Mediterranean*](https://sdam-au.github.io/sdam-au/) (SDAM) project 
at the School of Culture and Society at Aarhus University.



<br/>

### Installation

You can install the `sdam` package from these GitHub repositories using the R console or RStudio if you wish.

```r
## Install package
devtools::install_github("mplex/cedhar", subdir="pkg/sdam")

devtools::install_github("sdam-au/sdam")
```


### Usage

```r
## Load package
library("sdam")
```

```r
## Load epigraphic data
data("EDH")
```

```r
# Look at how many inscriptions?
length(EDH)
# [1] 82465
```


```r
# Also look at the object structure
str(EDH)
```


### Get particular inscriptions

```r
# obtain epigraphic data from Thracia
thr <- get.edh(search="inscriptions", province="Thr")
```

```r
# how many inscriptions in Thracia?
length(thr)
# [1] 395
```


### Relative dating

TBD





### Documentation

Look at the ['sdam' R package documentation](https://sdam-au.github.io/sdam/)

