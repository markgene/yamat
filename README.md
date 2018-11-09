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

