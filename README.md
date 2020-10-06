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
```
or 

```r
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



### People

It is possible to extract the `people` component from the `EDH` API dataset, which is an integrated list with their own items.

For instance, the following commands extract people named in Roman inscriptions in Thracia and Syria into a data frame object:

```r
edhw(x=get.edh(search="inscriptions", province="Thr"), vars="people", as="df")
edhw(x=get.edh(search="inscriptions", province="Syr"), vars="people", as="df")
```

The same can apply to the rest of Roman provinces.

It is also possible to `limit` the amount of records to process, and with the following example we can extract the first four inscriptions by `hd_nr` id.

```r
edhw(vars="people", as="df", limit=4)
#        id age: years             cognomen gender                                              name    nomen person_id praenomen           status   tribus
#1 HD000001       <NA>               Optata female                               Noniae P.f. Optatae    Nonia         1      <NA>             <NA>     <NA>
#2 HD000001       <NA>               Artemo   male                                 C. Iulio Artemoni   Iulius         2        C.             <NA>     <NA>
#3 HD000001       <NA>              Optatus   male                            C. Iulius C.f. Optatus   Iulius         3        C.             <NA>     <NA>
#4 HD000002         70                Paris   male                                  C. Sextius Paris  Sextius         1        C.             <NA>     <NA>
#5 HD000003       <NA> Sisenna+ Rutilianus+   male      [P. M]ummio [P.f. Gal. S]isenna[e Rutiliano] Mummius+         1       P.+ senatorial order Galeria+
#6 HD000004       <NA>                [---]   <NA> [---?]AV(?)S(?)[---]L(?)L(?)A M. Porci Nigri ser.     <NA>         1      <NA>           slaves     <NA>
#7 HD000004       <NA>                Niger   <NA>                                    M. Porci Nigri  Porcius         2        M.             <NA>     <NA>
```

while a specific record is retrieved by using the `id` argument


```r
edhw(vars="people", as="df", id=4444)
#        id     cognomen gender                   name    nomen person_id praenomen           status
#1 HD004444    Gordianus   male     M. Antoni Gordiani Antonius         1        M.             <NA>
#2 HD004444 Tranquillina female Sabiniae Tranquillinae  Sabinia         2      <NA>             <NA>
#3 HD004444  Mercurialis   male       Iul. Mercurialis  Iulius*         3      <NA> equestrian order
```






### Documentation

Look at the ['sdam' R package documentation](https://sdam-au.github.io/sdam/) to find more about the distinct variables in `EDH` dataset 
and applications with the different functions.

