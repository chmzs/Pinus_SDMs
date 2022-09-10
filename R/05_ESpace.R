---
title: "Pinus_Wallce"
author: "chmzs"
date: `format(Sys.Date())`
output: 
  html_document:
    df_print: paged
    toc: yes
    number_sections: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = TRUE,
  tidy = TRUE,
  fig.align = "left",
  fig.width = 8,
  fig.showtext = TRUE
)
```

表征环境空间(Characterize Environmental Space)的多物种分析

```{r}
occs_P1 = occs_Pa   ## 提取环境值且spthin后的occs_*
occs_P2 = occs_Pd
bgEnvsVals_P1 = bgEnvsVals_Pa  ## bg点提取envs并与occs_P*合并后的bgEnvsVals_P*
bgEnvsVals_P2 = bgEnvsVals_Pd
```


### P1_P2 (*Pinus armandii* and *Pinus densata*)

#### 环境序列分析(Environmental Ordination analysis)

环境排序是为了进行主成分分析（PCA），它旋转多维数据（例如，对于许多变量），以描述显示最高变化的轴的子集。要进行PCA，通过勾选/不勾选生物气候变量，选择两个物种可用的变量。选择 "仅发生率"或 "发生率和背景"作为图的选择，并设置X轴和Y轴的组成部分。PCA散点图出现在结果标签中。[\@Wallace](https://wallaceecomod.github.io/wallace/articles/tutorial-v2.html)

```{r P1_P2 PCA}
# 对*Pinus armandii*和*Pinus densata*的环境空间进行PCA分析并绘图，以降低维度。
# 选择对occsBg进行PCA分析。

# 选择分析的环境变量
pcaSel_P1_P2 <- c("bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# Run the pca
espace_pca_P1_P2 <- espace_pca(
  sp.name1 = occs_P1$scientific_name[1],
  sp.name2 = occs_P2$scientific_name[1],
  occs.z1 = occs_P1[, pcaSel_P1_P2],
  occs.z2 = occs_P2[, pcaSel_P1_P2],
  bgPts.z1 = bgEnvsVals_P1[, pcaSel_P1_P2],
  bgPts.z2 = bgEnvsVals_P2[, pcaSel_P1_P2]
)
```

```{r P1_P2 PCA Plot}
## Generate plots
# PCA Scatter Plot
if ("occsBg" == "occs") {
  x <- espace_pca_P1_P2$scores[espace_pca_P1_P2$scores$bg == "sp", ]
  x.f <- factor(x$sp)
} else if ("occsBg" == "occsBg") {
  x <- espace_pca_P1_P2$scores[espace_pca_P1_P2$scores$sp == "bg", ]
  x.f <- factor(x$bg)
}
ade4::s.class(x, x.f,
  xax = 1, yax = 2,
  col = c("red", "blue"), cstar = 0, cpoint = 0.1
)

# PCA Correlation circle
ade4::s.corcircle(espace_pca_P1_P2$co,
  xax = 1, yax = 2,
  lab = pcaSel_P1_P2, full = FALSE, box = TRUE
)

# PCA screeplot
screeplot(espace_pca_P1_P2, main = NULL)
# Print PCA summary of results
summary(espace_pca_P1_P2)
```

#### Occs密度分布 (Occurrence Density Grid)

计算*Pinus armandii*和*Pinus densata*的环境空间中物种密集分布的部分和背景环境条件的可用性

```{r P1_P2 Dens}
# Create density grid
espace_occDens_P1_P2 <- espace_occDens(
  sp.name1 = occs_P1$scientific_name[1],
  sp.name2 = occs_P2$scientific_name[1],
  pca = espace_pca_P1_P2
)
# Plots
graphics::par(mfrow = c(1, 2))
ecospat::ecospat.plot.niche(espace_occDens_P1_P2[[occs_P1$scientific_name[1]]],
  title = occs_P1$scientific_name[1]
)
ecospat::ecospat.plot.niche(espace_occDens_P1_P2[[occs_P2$scientific_name[1]]],
  title = occs_P2$scientific_name[1]
)
```

#### 生态位重叠度 (Niche overlap)

评估*Pinus armandii*与*Pinus densata*之间的生态位重叠度，并计算出其occurrence density grid。运行equivalence test (FALSE)和similarity test TRUE.

生态位重叠度是基于模块Occurrence Density Grid中估计的可用环境空间中的Occs密度和bg密度。重叠度是用Schoener's D指标进行量化的。

```{r P1_P2 nicheOv}
## Run tests
espace_nicheOv_P1_P2 <- espace_nicheOv(
  z1 = espace_occDens_P1_P2[[occs_P1$scientific_name[1]]],
  z2 = espace_occDens_P1_P2[[occs_P2$scientific_name[1]]],
  iter = 100,
  similarity = TRUE
)

# Plots
layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3), 4, 3, byrow = FALSE))
# layout.show(nf)
# graphics::par(mfrow = c(1,2))
ecospat::ecospat.plot.niche.dyn(
  espace_occDens_P1_P2[[occs_P1$scientific_name[1]]],
  espace_occDens_P1_P2[[occs_P2$scientific_name[1]]],
  0.5,
  title = paste0(occs_P1$scientific_name[1], 
                 " and ", 
                 occs_P1$scientific_name[1]),
  col.unf = "blue",
  col.exp = "red",
  col.stab = "purple",
  colZ1 = "blue",
  colZ2 = "red",
  transparency = 25
)
# Plot
ecospat::ecospat.plot.overlap.test(
  espace_nicheOv_P1_P2$simil,
  "D", "Similarity test"
)
```
