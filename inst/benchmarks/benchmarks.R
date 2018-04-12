## A script to produce benchmark plots

library(tibble)
library(tidyverse)


getSavingTime <- function(data.size,n.cores) {
 #a <- runif(data.size)
  a <- rep(1.0, data.size)
  system.time(save.fast(a,file='dummy.RData',n.cores=n.cores))[3]
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
saveRDS(fast.test.results, 'data/fast.test.results.RData')


### Plotting Fast test
library(ggplot2)
ggplot(fast.test.results, aes(x=data.size,y=time,color=factor(n.cores))) +  geom_point()
ggplot(fast.test.results, aes(color=factor(data.size),y=time,x=n.cores)) +  geom_point()

## slow test
slow.test.results <- testSavingTime(settle.time = 5, nrep = 5, data.start = 0.5, data.end = 5.0, data.step = 0.5,
                                    n.core.start = 1, n.core.end = 8, n.core.step = 1)
saveRDS(slow.test.results, 'data/slow.test.results.RData')

## Plotting slow test
ggplot(slow.test.results, aes(x=data.size,y=time,color=factor(n.cores))) +  geom_point()
ggplot(slow.test.results, aes(color=factor(data.size),y=time,x=n.cores)) +  geom_point()


## Test background performace of built in save
getInternalSavingTime <- function(data.size) {
  #a <- runif(data.size)
  a <- rep(1.0, data.size)
  system.time(save(a,file='dummy.RData'))[3]
}

testInternalSavingTime <- function(settle.time = 0.2, nrep=2, data.start = 0.1, data.end = 0.4, data.step = 0.1) {
  gb.size <- 1.34e8
  data.size.seq <- seq(data.start,data.end,data.step)
  nrep.seq <- seq(1,nrep)
  params <- expand.grid(data.size.seq, nrep.seq)
  results <- rep(0, nrow(params))
  colnames(params) <- c('data.size')
  for (i in 1:nrow(params)) {
    cat(paste0('(',i,'/',nrow(params),'): '))
    cat(paste0('Running with ',params[i,1], ' GB... '))
    results[i] <- getInternalSavingTime(params[i,1] * gb.size)
    cat(paste0(results[i],'\n'))
    Sys.sleep(settle.time)
  }
  cbind(params, time=results)
}

## Slow test of internal save function
internal.test.results <- testInternalSavingTime(settle.time = 5, nrep = 2, data.start = 0.5, data.end = 5.0, data.step = 0.5)
saveRDS(internal.test.results, 'data/internal.test.results.RData')

onecore.comparison <- rbind(
  internal.test.results[,c('data.size','time')],
  slow.test.results[slow.test.results$n.cores == '1',c('data.size','time')]
)
onecore.comparison$type <- rep(c('internal','1core'),times=c(nrow(internal.test.results),sum(slow.test.results$n.cores == '1') ))

colnames(slow.test.results) <- c('data.size','n.cores','nrep','time')
slow.test.results$n.cores <- as.character(slow.test.results$n.cores)

colnames(internal.test.results) <- c('data.size','nrep','time')
internal.test.results$n.cores <- 'internal'
internal.test.results <- internal.test.results[,c('data.size','n.cores','nrep','time')]

allres <- rbind(slow.test.results, internal.test.results)

## Produce plots
as.tibble(allres) %>% filter(n.cores %in% c('internal','1','2','3','4')) %>%
  ggplot(aes(x=data.size, y=time, color=n.cores)) + geom_point() + geom_smooth(method="lm") + theme_bw() +
  scale_x_continuous(name = 'data size (GB)') +  scale_y_continuous(name='time(s)') + scale_color_discrete(name='#cores') +
  ggtitle('fastSave (#core > 2) is faster than builtin save') + scale_y_continuous(limits = c(0,80))
ggsave('vignettes/figures/datasize.vs.time.png',width=10,height=8)

as.tibble(allres) %>% filter(data.size==4, n.cores %in% c('internal','1','2','3','4','5','6','7','8')) %>%
  ggplot(aes(x=n.cores, y=time,color=n.cores)) + geom_boxplot() + theme_bw() + scale_y_continuous(limits = c(0,80)) +
  scale_y_continuous(name='time(s)') +  scale_x_discrete(name='#ncores') + guides(color=FALSE)
  ggtitle('fastSave performance for 4 GB dataset')
ggsave('vignettes/figures/ncores.vs.time.png',width=10,height=8)

as.tibble(allres) %>% filter(data.size==4, n.cores %in% c('internal','6')) %>%
  ggplot(aes(x=n.cores, y=time,color=n.cores)) + geom_boxplot() + theme_bw() + scale_y_continuous(limits = c(0,30)) +
  ggtitle('fastSave reduces saving time by over 50%') + scale_y_continuous(name='time(s)',lim=c(0,30)) +
  scale_x_discrete(name='#ncores') + guides(color=FALSE)
ggsave('vignettes/figures/ncores.vs.time.png',width=10,height=8)

## calculate time reduction
as.tibble(allres) %>% filter(data.size==4, n.cores %in% c('internal','6')) %>%
  group_by(n.cores) %>% summarise(mean(time))

