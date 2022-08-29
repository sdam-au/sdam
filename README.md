# sdam

## Digital Tools for the SDAM Project at Aarhus University

Provides digital tools for performing analyses within [*Social Dynamics and complexity
in the Ancient Mediterranean*](https://sdam-au.github.io/sdam-au/) (SDAM), which is a research group based at the Department of History and Classical Studies at Aarhus University.


<br/>

### Installation

Install `sdam` package from these GitHub repositories using the R console or RStudio.

```r
## install release candidate
devtools::install_github("sdam-au/sdam")
```
or 

```r
## install beta version
devtools::install_github("mplex/cedhar", subdir="pkg/sdam")
```



### Usage

```r
## Load package
library("sdam")
packageVersion("sdam")
#[1] '1.1.1'
```

```r
## Load epigraphic data
data("EDH")
```

```r
# Look at how many inscriptions?
length(EDH)
# [1] 84701
```


```r
# Also look at the object structure
str(EDH)
```


### Get particular inscriptions

Starting with `sdam` v0.8.0, it is possible to extract inscriptions from the *new* `API` of the `EDH` database. 
For instance, the following commands extract people named in Roman inscriptions in Iudaea and record them with 
a `list` data object in `iud`.


```r
# obtain epigraphic data from Iudaea
iud <- get.edh(search="inscriptions", province="Iud")
```

```r
# how many inscriptions in Iudaea?
length(iud)
# [1] 187
```


### Relative dating

In `sdam` there is the option  to extract variables from a `EDH` dataset *fragments* with function `edhw()`, 
which allos making transformations as weel. For instance, te ode snippet below is to place Roman inscriptions 
in lists from Iudaea into a data frame in object `iud_df`.

```r
# extract variables of interes from object `iud` and convert it into a data frame
iud_df <- edhw(x=iud, vars=c("not_after", "not_before"), as="df", na.rm=FALSE)

# what object types is?
is(iud_df)
#[1] "data.frame" "list"       "oldClass"   "vector"  
```


To remove missing data form `iud_df`, we activate `na.rm` argument in the function as for `iud_df0`

```r
# extract variables of interes from object `iud` and convert it into a data frame
iud_df0 <- edhw(x=iud, vars=c("not_after", "not_before"), as="df", na.rm=TRUE)
```

Now we compare outcomes with these two options.

```r
cbind(head(iud_df,10), head(iud_df0,10))
#         id not_before not_after       id not_before not_after
#1  HD001461       0071      0130 HD001461       0071      0130
#2  HD001958       0301      0400 HD001958       0301      0400
#3  HD001964       0132      0135 HD001964       0132      0135
#4  HD001973       0071      0130 HD001973       0071      0130
#5  HD001985       0198      0209 HD001985       0198      0209
#6  HD002376       0051      0220 HD002376       0051      0220
#7  HD004074       0026      0036 HD004074       0026      0036
#8  HD004735       0070      0079 HD004735       0070      0079
#9  HD006228       0212      0220 HD006228       0212      0220
#10 HD007068       <NA>      <NA> HD011646       0117      0138
```

Hence, not available data is removed in `iud_df0`.



### People

It is possible to extract the `people` component from the `EDH` API dataset, which is an integrated list with their own items.

For instance, the following commands extract people named in Roman inscriptions in Iudaea and Syria into a data frame object 
with a *long* (default) and a data frame with a *wide* format:

```r
edhw(x=get.edh(search="inscriptions", province="Iud"), vars="people", as="df")
edhw(x=get.edh(search="inscriptions", province="Syr"), vars="people", as="df", wide=TRUE)
```

The same can apply to the rest of Roman provinces recorded in the `EDH` dataset.


#### Specifying output

It is also possible to `limit` the amount of records to process, and with the following example we can extract the first four 
inscriptions by `hd_nr` id in a data frame with a long format.

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

while a specific record is retrieved by using the `id` argument as follows:


```r
edhw(vars="people", as="df", id=4444)
#        id     cognomen gender                   name    nomen person_id praenomen           status
#1 HD004444    Gordianus   male     M. Antoni Gordiani Antonius         1        M.             <NA>
#2 HD004444 Tranquillina female Sabiniae Tranquillinae  Sabinia         2      <NA>             <NA>
#3 HD004444  Mercurialis   male       Iul. Mercurialis  Iulius*         3      <NA> equestrian order
```


### Probability of existence

Since `sdam` v0.3.0 there is a dedicated function to compute probability of existence of date points or intervals. 
We obtain inscriptions from Iudaea in EDH data base, and then compute the aoristic sum with a 5-bin chronological phases. 

```r
prex(x=get.edh(search="inscriptions", province="Iud"), vars=c("not_before", "not_after"), cp="bin5")
#   Arch     Class      Hell       Rom       Byz 
#    0.000     0.000  1337.904 13405.017     0.000
```

Since most of the inscriptions are within the Roman period, we try an eight-bin chronological phases option. 

```r
prex(x=get.edh(search="inscriptions", province="Iud"), vars=c("not_before", "not_after"), cp="bin8")
#   Arch     Class      Hell      ERom      MRom      LRom      EByz      LByz 
#   0.0000    0.0000 1337.9040 2396.4529 1200.5623  320.5379    0.0000    0.0000
```




### Vignettes

* [Datasets in `"sdam"` package](https://sdam-au.github.io/sdam/articles/Intro.html)
* [Dates and missing dating data](https://sdam-au.github.io/sdam/articles/Dates.html)
* [Re-encoding `people` in the `EDH` dataset](https://sdam-au.github.io/sdam/articles/Encoding.html)
* [Cartographical maps and networks](https://sdam-au.github.io/sdam/articles/Maps.html)



### See also
* [Social Dynamics and complexity in the Ancient Mediterranean Project](https://sdam-au.github.io/sdam-au/)
* [`"sdam"` manual](https://github.com/mplex/cedhar/blob/master/typesetting/reports/sdam.pdf)
* [Code snippets using `"sdam"`](https://github.com/sdam-au/R_code)
  + [Shipwrecks network in the Mediterranean Basin (23-June-2022)](https://htmlpreview.github.io/?https://github.com/sdam-au/R_code/blob/master/HTML/Shipwrecks%20Network%20in%20the%20Mediterranean%20Basin.html)
  + [Centrality measures in ORBIS Roman transport network (24-Nov-2021)](https://htmlpreview.github.io/?https://github.com/sdam-au/R_code/blob/master/HTML/Centrality%20measures%20in%20ORBIS%20Roman%20transport%20network.html)
  + [ORBIS Roman World transport network (14-Sep-2021, update: 22-Sep-2021)](https://htmlpreview.github.io/?https://github.com/sdam-au/R_code/blob/master/HTML/ORBIS%20Roman%20World%20transport%20network%20centrality%20measures.html)
  + [Plotting Roman Roads (update: 14-Sep-2021)](https://htmlpreview.github.io/?https://github.com/sdam-au/R_code/blob/master/HTML/Plotting%20Roman%20Roads%20(update).html)
  + [Plotting Roman Roads and Time (17-Aug-2021)](https://htmlpreview.github.io/?https://github.com/sdam-au/R_code/blob/master/HTML/Plotting%20Roman%20Roads%20and%20Time.html)
