# fastSave: Save your R sessions faster!

[![Travis build status](https://travis-ci.org/barkasn/fastSave.svg?branch=master)](https://travis-ci.org/barkasn/fastSave)

As the scale of the data that is processed with R increases so
   does time to save sessions to disks. This package allows taking advantage of 
   parallel compression to reduce saving times.
   
# Benchmarks
## Performance comparison against built-in single-core save
<img src="vignettes/figures/method.vs.savetime.png" width="500px" />

## 3D comparison of gzip and lbzip2 algorithms
<img src="vignettes/figures/3dperformance.png" width="600px" />
<a href="https://raw.githubusercontent.com/barkasn/fastSave/master/vignettes/plotly/fastSave3d.html">Right-click and select 'Save link as...' to download interactive plot</a>

Benchmarks were performed on a dual Intel(R) Xeon(R) E5670  @ 2.93GHz system with 48 GB of RAM. 
The code to run the benchmarks on your system can be found in inst/benchmarks.R

# Installation
fastSave only works on **unix systems** and requires the **pigz** to be installed, to use the lbzip2 functions you will also need **lbzip2** to be installed. On ubuntu you can install the requirements with:
```sh
sudo apt-get install pigz lbzip2
```

To install the fastSave R packages type the following on the R command prompt:
```R
library(devtools)
install_github('barkasn/fastSave')
```
## Use
fastSave provides the following  functions:

* save.lbzip2()
* save.image.lbzip2()
* load.lbzip2()
* load.lbzip2.e()

* save.pigz()
* save.image.pigz()
* load.pigz()
* load.pigz.e()

* preserve.state()

* save.fast()
* load.fast()

* saveRDS.lbzip2()
* readRDS.lbzip2()

* saveRDS.pigz()
* readRDS.pigz()

save.pigz() and save.image.fast() produce files compatible with the standard load() function.  The save.lbzip2() and save.image.lbzip2() functions produce files that can be loaded with load.lbzip2(). Their advantage is that they allow the load operation to be parallelized as well.

To use fastSave simply load the package and replace your calls to save() and save.image() with save.lbzip2() and save.image.lbzip2(). You can use the n.cores argument to provide the number of cores to use. The default is 4 as most modern systems have at least 4 cores and saturation occurs beyond 8 cores anyway.
```R
## load the library
library(fastSave)

## Make an object
x <- rep(0,100)

## Save current enviromnment with function compatible with built-in save() and load()
## by default it will use ".RData" as a file
save.image.pigz(file="mydata.RData", n.cores = 8)

## Save selected object only in "mydata.RData"
save.pigz(x, file="x.RData",n.cores = 8)

## These files can be loaded with the normal load() function or with load.pigz()
load.pigz("x.RData")
```

```R
## Alternatively you can use the .lbzip2 functions. These produce files that may
## not be compatible with the built in R functions but allow parallel loading.
## Note that saving with the lbzip2 functions takes slightly longer that
## with the functions above, but loading time is dramatically shorter.

## These functions are recommended unless you need to keep compatibility with 
## users of built-in save.

## The use of the suffix .RDataFS is recommened for these files
## By default saves into files ".RDataFS"
save.image.lbzip2("mydata.RDataFS")
save.lbzip2(x, file="mydata.RDataFS")

## Load with selected function
load.lbzip2("mydata.RDataFS")
```

