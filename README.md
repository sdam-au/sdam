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

Since `sdam` v.0.2.1 there is the option in `edhw()` to extract variables from a `EDH` dataset *fragments*.

```r
# extract variables of interes from object `thr` and convert it into a data frame
thrdf0 <- edhw(vars=c("ID", "not_after", "not_before"), x=thr, as="df")

# what object types is?
is(thrdf0)
#[1] "data.frame" "list"       "oldClass"   "vector"  
```

```r
# to remove missing data activate 'na.rm' argument
thrdf <- edhw(vars=c("ID", "not_after", "not_before"), x=thr, as="df", na.rm=TRUE)
```

Now we compare the outcomes of these options.

```r
cbind(head(thrdf0), head(thrdf))
#      ID not_after not_before     ID not_after not_before
#1 004345      0250       0151 004345      0250       0151
#2 004366      0324       0308 004366      0324       0308
#3 004753      0300       0151 004753      0300       0151
#4 004798      0241       0238 004798      0241       0238
#5 007032      0209       0208 007032      0209       0208
#6 007047      <NA>       0068 007092      0136       0135
```

Hence, not available data is removed in `thrdf`.







### Documentation

Look at the ['sdam' R package documentation](https://sdam-au.github.io/sdam/) to find more TBD.

