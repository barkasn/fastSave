lbzip2.bench <- readRDS('inst/benchmarks/lbzip2.save.slow.test.results.RData')
internal.bench <- readRDS('inst/benchmarks/internal.test.results.RData')
original.bench <- readRDS('inst/benchmarks/slow.test.results.RData')

head(lbzip2.bench)
colnames(lbzip2.bench) <- c('data.size','n.cores','rep','time')
lbzip2.bench$type <- c('lbzip2')

head(internal.bench)
colnames(internal.bench) <- c('data.size','rep','time')
internal.bench$n.cores <- c(1)
internal.bench <- internal.bench[,c('data.size','n.cores','rep','time')]
internal.bench$type <- c('internal')

head(original.bench)
colnames(original.bench) <- c('data.size','n.cores','rep','time')
original.bench$type <- c('pgiz')

head(lbzip2.bench)
head(internal.bench)
head(original.bench)

all.bench <- rbind(lbzip2.bench,internal.bench,original.bench)

head(all.bench)
library(tidyverse)
all.bench %>% ggplot(aes(x=data.size,y=time,color=factor(type))) + geom_point() + facet_grid(~n.cores)

all.bench %>%
  filter(type == 'internal' | (type=='pgiz' & n.cores==8) | (type=='lbzip2' & n.cores==8)) %>%
  ggplot(aes(x=data.size,y=time,color=factor(type))) + geom_point()

