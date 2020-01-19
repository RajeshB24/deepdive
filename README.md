
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
 x <- data.frame(a = runif(1000)*100,
 b = runif(1000)*200,
 c = runif(1000)*100,
 d = runif(1000)*200)
 y<- data.frame(y=20*x$a +30* x$b+10*x$c +10)

 #train
 modelnet<-deepnet(x,y,c(2,2),activation = "relu",
 reluLeak = 0,
 modelType = "regress",
 iterations =500,
 eta=0.8,
 optimiser="adam")
#> [1] "iteration 5: 162.407295141373"
#> [1] "iteration 10: 71.7759579934866"
#> [1] "iteration 15: 65.4201447567828"
#> [1] "iteration 20: 66.29810035875"
#> [1] "iteration 25: 52.4799621881113"
#> [1] "iteration 30: 52.3535091307526"
#> [1] "iteration 35: 52.7971253120086"
#> [1] "iteration 40: 51.2822009573164"
#> [1] "iteration 45: 50.2386116417325"
#> [1] "iteration 50: 50.2804036063058"
#> [1] "iteration 55: 50.1646200284332"
#> [1] "iteration 60: 49.94615776016"
#> [1] "iteration 65: 49.9044788338999"
#> [1] "iteration 70: 49.8211212461055"
#> [1] "iteration 75: 49.659490495746"
#> [1] "iteration 80: 49.4390040164216"
#> [1] "iteration 85: 49.1395041445895"
#> [1] "iteration 90: 48.7219952416706"
#> [1] "iteration 95: 48.1651006096818"
#> [1] "iteration 100: 47.5677082901223"
#> [1] "iteration 105: 46.7993542034149"
#> [1] "iteration 110: 45.7660508662465"
#> [1] "iteration 115: 44.5250599025923"
#> [1] "iteration 120: 43.0691293961841"
#> [1] "iteration 125: 41.1439987606712"
#> [1] "iteration 130: 38.6342626307947"
#> [1] "iteration 135: 35.4115949460945"
#> [1] "iteration 140: 31.4160749778159"
#> [1] "iteration 145: 26.6141013581378"
#> [1] "iteration 150: 21.1251907250746"
#> [1] "iteration 155: 15.332081666663"
#> [1] "iteration 160: 9.82212142117622"
#> [1] "iteration 165: 5.29832290972073"
#> [1] "iteration 170: 2.52866682919656"
#> [1] "iteration 175: 1.86771113509437"
#> [1] "iteration 180: 1.75427625986802"
#> [1] "iteration 185: 1.40693686438566"
#> [1] "iteration 190: 1.26436618982643"
#> [1] "iteration 195: 1.19047583857758"
#> [1] "iteration 200: 1.06278884888769"
#> [1] "iteration 205: 0.998103049166865"
#> [1] "iteration 210: 0.928931016242238"
#> [1] "iteration 215: 0.872288499615747"
#> [1] "iteration 220: 0.819758284998762"
#> [1] "iteration 225: 0.771768168166232"
#> [1] "iteration 230: 0.72921235864621"
#> [1] "iteration 235: 0.688554043612652"
#> [1] "iteration 240: 0.652135447478944"
#> [1] "iteration 245: 0.617987355081407"
#> [1] "iteration 250: 0.586421259705922"
#> [1] "iteration 255: 0.557005099137967"
#> [1] "iteration 260: 0.529650690774101"
#> [1] "iteration 265: 0.504071629645665"
#> [1] "iteration 270: 0.480205099877877"
#> [1] "iteration 275: 0.457874402266067"
#> [1] "iteration 280: 0.436943034741135"
#> [1] "iteration 285: 0.417294308534892"
#> [1] "iteration 290: 0.398847402510113"
#> [1] "iteration 295: 0.381533647917126"
#> [1] "iteration 300: 0.36526593493677"
#> [1] "iteration 305: 0.349968523663881"
#> [1] "iteration 310: 0.335571134447042"
#> [1] "iteration 315: 0.322019485059007"
#> [1] "iteration 320: 0.309252652357237"
#> [1] "iteration 325: 0.297223622928734"
#> [1] "iteration 330: 0.285880087100001"
#> [1] "iteration 335: 0.275175587103177"
#> [1] "iteration 340: 0.265058589790299"
#> [1] "iteration 345: 0.255493672203913"
#> [1] "iteration 350: 0.246427428604686"
#> [1] "iteration 355: 0.23783161918155"
#> [1] "iteration 360: 0.229669987036092"
#> [1] "iteration 365: 0.221924367212558"
#> [1] "iteration 370: 0.214564606398497"
#> [1] "iteration 375: 0.207576754641758"
#> [1] "iteration 380: 0.200928824347233"
#> [1] "iteration 385: 0.194607930629491"
#> [1] "iteration 390: 0.188590845621821"
#> [1] "iteration 395: 0.182870440941724"
#> [1] "iteration 400: 0.177431578773433"
#> [1] "iteration 405: 0.172253405440905"
#> [1] "iteration 410: 0.1673105557617"
#> [1] "iteration 415: 0.162594556138412"
#> [1] "iteration 420: 0.158093191743367"
#> [1] "iteration 425: 0.153793434640463"
#> [1] "iteration 430: 0.149677952416041"
#> [1] "iteration 435: 0.145746439574927"
#> [1] "iteration 440: 0.141982853116748"
#> [1] "iteration 445: 0.13837785159289"
#> [1] "iteration 450: 0.134918103426438"
#> [1] "iteration 455: 0.131599287984489"
#> [1] "iteration 460: 0.12841339275984"
#> [1] "iteration 465: 0.125356393672791"
#> [1] "iteration 470: 0.122419375914474"
#> [1] "iteration 475: 0.119595496202321"
#> [1] "iteration 480: 0.116878172532844"
#> [1] "iteration 485: 0.114263001923053"
#> [1] "iteration 490: 0.111741416670876"
#> [1] "iteration 495: 0.109313647907111"
#> [1] "iteration 500: 0.106973109359721"
```
