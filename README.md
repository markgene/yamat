# YAMAT: Yet Another Methylation Array Toolkit

## Install

Install dependencies:

```{r, install_dependencies}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("minfi")
BiocManager::install("GEOquery")
```

Install `yamat`:

```{r, install_yamat}
if (! ("devtools" %in% installed.packages()) install.packages("devtools")
devtools::install_github("markgene/yamat")
```

Other suggested packages:

```{r, install_suggested}
# This will install 450K annotation and manifest packages: 
# IlluminaHumanMethylation450kmanifest, 
# IlluminaHumanMethylation450kanno.ilmn12.hg19
BiocManager::install("minfiData")

# This will install EPIC annotation and manifest packages:
# IlluminaHumanMethylationEPICmanifest, 
# IlluminaHumanMethylationEPICanno.ilm10b2.hg19
BiocManager::install("minfiDataEPIC")

# EPIC version 2 - manifest and annotation packages
BiocManager::install("IlluminaHumanMethylationEPICv2manifest")
BiocManager::install("IlluminaHumanMethylationEPICv2anno.20a1.hg38")
```

## Overview

*yamat* currently has three modules:

* Quality control (QC).
* Normalization.
* Batch effect removal.

It also provides the function `get_gse()` to download IDAT files by GEO 
series (GSE) accession.

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

There are nine normalization methods, including six implemented in *minfi* 
package. The additional two methods are:

* *dkfz* is used in the paper [Capper et al. DNA methylation-based 
classification of central nervous system tumours. Nature (2018)](https://www.ncbi.nlm.nih.gov/pubmed/29539639). 
* *yamat* normalizes samples individually with Illumina method implemented 
in *minfi* package, instead of use one sample as reference.
* *methylcnv* is used in the paper [Feng, G. et al. A Statistical Method to Estimate DNA Copy Number from Illumina High-Density Methylation Arrays. Systems Biomedicine (2013)](https://www.tandfonline.com/doi/pdf/10.4161/sysb.25896). It
adjusts the median of log2 copy number on each array to a target value 
of 13. I implement the method with an argument of target value.

```{r normalization}
gmset <- normalize(RGsetEx, norm_method = "dkfz", map_to_genome = TRUE)
```

Note: normalization methods of the same name sometimes have different 
implementation. For example, the quantile normalization in *minfi* package 
and *CopyNumber450k8 are different.

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
