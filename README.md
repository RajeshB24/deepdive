
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deepdive

**deepnet-\>deeptree-\>deepforest**

<!-- badges: start -->

<!-- badges: end -->

This package aims to provide simple intuitive functions to create quick
protoypes of artificial neural network or deep learning models for
general purpose application. In addition, check out experimental
algorithms from my personal research , **deeptree** and **deepforest**
for special cases to achieve better accuracy / generalisation.

**deeptree**: This algorithms builds a tree to divide the solution space
in to leaves and fits a artificial neural network to each leaf. This
approach takes adavantage distinct propeties of a tree and neural
network. It has tendency to overfit but multiple parameters can be tuned
to achieve better generalisation. This model has a provision to stack
predictions from other models.

**deepforest**: This algorithm builds multiple deepnets/deeptrees from
which either best deepnet can be selected by passing all variable and
data to each network or random deepnets/deeptrees based on random cuts
of variable/data can be combined together over a error choice.

*Reach me Out* : <rajeshbalakrishnan24@gmail.com> for any suggestions
and doubts or you can always leave a comment on github.

## Installation

You can install the released version of deepdive from
[GitHub](https://github.com/RajeshB24/deepdive) with:

``` r
devtools::install_github("RajeshB24/deepdive")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(deepdive)
# basic example code
 x <- data.frame(a = runif(100)*100,
 b = runif(100))
 y<- data.frame(y=20*x$a +30* x$b+10)

 #train (increase iteration  to improve accuracy)
 modelnet<-deepnet(x,y,c(1),activation = "relu",
 reluLeak = 0.001,
 modelType = "regress",
 iterations =5,
 eta=0.1,
 optimiser="adam")
#> [1] "iteration 1: 12.7222587425007"
#> [1] "iteration 2: 12.2207170978186"
#> [1] "iteration 3: 11.6763382294951"
#> [1] "iteration 4: 11.1270441230926"
#> [1] "iteration 5: 10.6030121196171"
 
  #predict
 # predDeepNet<-predict.deepnet(modelnet,newData=x)

 #evaluate
 #rmse=sqrt(mean((predDeepNet$pred_y-y$y)^2))
```
