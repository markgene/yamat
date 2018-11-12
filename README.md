# YAMAT: Yet Another Methylation Array Toolkit

## Install

```{r, install_yamat}
if (! ("devtools" %in% installed.packages()) install.packages("devtools")
devtools::install_github("markgene/yamat")
```

## Overview

*yamat* currently has three modules:

* Quality control (QC).
* Normalization.
* Batch effect removal.

I am using the data in *minfiData* package below.

```{r}
library(yamat)
library(minfiData)
```

## Quality Control

QC are done in three aspects:

* Observed and expected intensities of control probes.
* Detection p-values.
* Gender.

I will add another two aspects in the future:

* Mean of methylation and unmethylation signals of a group of samples.
* Beta distribution plots of a group of samples, perhaps with bimodality 
test.

### Observed and Expected Values of Control Probes

`control_probe_intensities()` returns a `data.frame` containing the following 
columns: `Address`, `Type`, `Color`, `ExtendedType`, `Channel`, `Expected_Grn`, 
`Expected_Red`, `Sample`, `Group`, `Intensity`.

```{r, control_probe_intensities}
df <- control_probe_intensities(RGsetEx[, 1])
```

`plot_control_probes()` returns a list of plots and a `data.frame` of control 
probe intensities, for *one* sample.

```{r, plot_control_probes}
plots <- plot_control_probes(RGsetEx, s = 1)
plots$one_plot
```

### Detection P-values

`summary_detectionP()` returns a `data.frame` summarizes the detection p-values.

```{r, detp}
detP_summary <- summary_detectionP(minfi::detectionP(RGsetEx))
```

### Gender

`get_gender()` returns a `data.frame` with the columns `predictedSex` 
(a character with values M and F), `xMed` and `yMed` in addition to 
the `DataFrame` returned by `minfi::pData()`. `xMed` and `yMed` are the 
chip-wide medians of measurements on the two sex chromosomes. The function 
calls `minfi::getSex()` under the hood.

```{r gender}
minfi::pData(RGsetEx) <- get_gender(RGsetEx)
```

## Normalization

There are eight normalization methods, including six implemented in *minfi* 
package. The additional two methods are:

* *dkfz* is used in the paper [Capper et al. DNA methylation-based classification 
of central nervous system tumours. Nature (2018)](https://www.ncbi.nlm.nih.gov/pubmed/29539639). 
* *yamat* normalizes samples individually with Illumina method implemented in 
*minfi* package, instead of use one sample as reference.

```{r normalization}
gmset <- normalize(RGsetEx, norm_method = "dkfz", map_to_genome = TRUE)
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

