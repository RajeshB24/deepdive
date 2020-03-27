
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deepdive

**deepnet-\>deeptree-\>deepforest**

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/deepdive?color=green)](https://cran.r-project.org/package=deepdive)
[![](https://img.shields.io/badge/Dev-1.0.1-green.svg)](https://rajeshb24.github.io/deepdive/)
<!-- badges: end -->

This package aims to provide simple intuitive functions to create quick
prototypes of artificial neural network or deep learning models for
general purpose application. In addition, check out experimental
algorithms from my personal research , **deeptree** and **deepforest**
for special cases to achieve better accuracy / generalization.

**deeptree**: This algorithm builds a CART tree to divide the solution
space in to leaves and fits an artificial neural network to each leaf.
This approach takes advantage of distinct properties of a tree and
neural network. It has tendency to overfit but multiple parameters can
be tuned to achieve better generalization. This model has a provision to
stack predictions from other models(currently stacking is only available
for regression).

**deepforest**: This algorithm builds multiple deepnets/deeptrees from
which either best deepnet can be selected by passing all variable and
data to each network or random deepnets/deeptrees based on random cuts
of variable/data can be combined together over a error choice.

*Reach me Out* : <rajeshbalakrishnan24@gmail.com> for any suggestions
and doubts or you can always leave a comment on github.

## Installation

You can install released version from CRAN or development from github
deepdive from [GitHub](https://github.com/RajeshB24/deepdive) with:

``` r

#CRAN

install.packages("deepdive")

#Development Version
devtools::install_github("RajeshB24/deepdive")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(deepdive)
 x <- data.frame(a = runif(1000)*100,
 b = runif(1000)*200,
 c = runif(1000)*100
 )
 y<- data.frame(y=20*x$a +30* x$b+10*x$c +10)

 #Training increase iterations for convergence
 modelnet<-deepnet(x,y,c(2,2),
 activation = c('relu',"sin"),
 reluLeak = 0.001,
 modelType = "regress",
 iterations =20,
 eta=0.8,
 optimiser="adam")
```

    ## iteration 3: 3611.32786255431

    ## iteration 7: 2756.36166658289

    ## iteration 11: 2010.70092102176

    ## iteration 15: 2017.43535666894

    ## iteration 20: 2138.26387949984

``` r
 #predict
# predDeepNet<-predict.deepnet(modelnet,newData=x)

 #evaluate
#sqrt(mean((predDeepNet$pred_y-y$y)^2))
```
