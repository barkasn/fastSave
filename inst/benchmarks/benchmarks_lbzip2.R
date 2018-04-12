## A script to produce benchmark plots

library(tibble)
library(tidyverse)


getSavingTime <- function(data.size,n.cores) {
 #a <- runif(data.size)
  a <- rep(1.0, data.size)
  system.time(save.lbzip2(a,file='dummy.RData',n.cores=n.cores))[3]
}

testSavingTime <- function(settle.time = 0.2, nrep=2, data.start = 0.1, data.end = 0.4, data.step = 0.1,
                           n.core.start=1,n.core.end=8,n.core.step=1) {
  gb.size <- 1.34e8
  data.size.seq <- seq(data.start,data.end,data.step)
  n.core.seq <- seq(n.core.start,n.core.end,n.core.step)
  nrep.seq <- seq(1,nrep)
  params <- expand.grid(data.size.seq, n.core.seq, nrep.seq)
  results <- rep(0, nrow(params))
  colnames(params) <- c('data.size','n.cores')
  for (i in 1:nrow(params)) {
    cat(paste0('(',i,'/',nrow(params),'): '))
    cat(paste0('Running with ',params[i,1], ' GB and ',params[i,2],' core(s)... '))
    results[i] <- getSavingTime(params[i,1] * gb.size, params[i,2])
    cat(paste0(results[i],'\n'))
    Sys.sleep(settle.time)
  }
  cbind(params, time=results)
}

## fast test
fast.test.results <- testSavingTime(settle.time = 0.1, nrep = 1, data.start = 0.1, data.end = 0.6, data.step = 0.2,
                               n.core.start = 2, n.core.end = 6, n.core.step = 1)
saveRDS(fast.test.results, 'inst/benchmarks/lbzip2.save.fast.test.results.RData')


### Plotting Fast test
library(ggplot2)
ggplot(fast.test.results, aes(x=data.size,y=time,color=factor(n.cores))) +  geom_point()
ggplot(fast.test.results, aes(color=factor(data.size),y=time,x=n.cores)) +  geom_point()

## slow test
slow.test.results <- testSavingTime(settle.time = 0.1, nrep = 1, data.start = 0.5, data.end = 5.0, data.step = 0.5,
                                    n.core.start = 1, n.core.end = 8, n.core.step = 1)

saveRDS(slow.test.results, 'inst/benchmarks/lbzip2.save.slow.test.results.RData')

## Plotting slow test
ggplot(slow.test.results, aes(x=data.size,y=time,color=factor(n.cores))) +  geom_point()
ggplot(slow.test.results, aes(color=factor(data.size),y=time,x=n.cores)) +  geom_point()



