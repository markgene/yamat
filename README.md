# YAMAT: Yet Another Methylation Array Toolkit

## Install

```{r, install_yamat}
if (! ("devtools" %in% installed.packages()) install.packages("devtools")
devtools::install_github("markgene/yamat")
```

## Plot of Control Probes

```{r, plot_control_probes}
library(yamat)
library(minfiData)
plots <- plot_control_probes(RGsetEx, s = 1)
plots$one_plot
```

## Remove Batch Effect

If you have an object of `MethylSet` or `GenomicMethylSet` class, use the default 
method, which will remove the batch effect of methylation and unmethylation 
signals (log2 transformed) separately. If you have an object of `RatioSet` or 
`GenomicRatioSet` class, which does not keep methylation and unmethylation 
signals, set `method` as `"cn"` or `"beta"` for copy number or beta value. 

```{r, batch_effect}
library(yamat)
library(minfiData)
x <- remove_batch_effect(x = MsetEx.sub, batch = "status")
y <- minfi::getMeth(x) - minfi::getMeth(MsetEx.sub)
summary(y)[, 1:2]

## 5723646052_R02C02  5723646052_R04C01  
## Min.   :-6165.48   Min.   :-9716.360  
## 1st Qu.: -307.99   1st Qu.: -112.932  
## Median :   -7.55   Median :   -6.406  
## Mean   :  -65.72   Mean   :  186.622  
## 3rd Qu.:   48.16   3rd Qu.:  202.632  
## Max.   : 8372.45   Max.   :20231.482 
```

