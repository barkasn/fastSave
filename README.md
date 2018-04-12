# fastSave: Save your R sessions faster!

As the scale of the data that is processed with R increases so
   does time to save sessions to disks. This package allows taking advantage of 
   parallel compression to reduce saving times.
   
# Benchmarks
<img src="vignettes/figures/ncores.vs.time.png" width="500px">

<img src="vignettes/figures/datasize.vs.time.png" width="500px">

Benchmarks were performed on a dual  Intel(R) Xeon(R) E5620  @ 2.40GHz system with 32 GB or RAM. 
The code to run the benchmarks on your system can be found in inst/benchmarks.R

# Installation
fastSave only works on unix systems and requires the 'pigz' to be installed. On ubuntu you can install 'pigz' with 

```sh
sudo apt-get install pigz
```

To install fastSave, open R and run:

```R
library(devtools)
install_github('barkasn/fastSave')
```
## Use
To use fast save simply load the package and replace your calls to save() and save.image() with save.fast() and save.image.fast().
```R
library(fastSave)
x <- 5
save.image.fast()
save.fast(x)
```

You can optionally specify the number of cores to use with the n.cores argument:
```R
library(fastSave)
x <- 5
save.image.fast(n.cores = 8)
```
