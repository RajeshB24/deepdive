
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deepdive

<!-- badges: start -->

<!-- badges: end -->

This package aims to provide simple intuitive functions to create quick
protoypes of artificial neural network or deep learning models for
general purpose application. In addition, check out experimental
algorithms from my personal research , deeptree and deepforest for
special cases to achieve better accuracy / generalisation.This github
page will be updated for roadmap items and updates for improved
fucntionalities and performance updates.

Feel free to reach me out at : <rajeshbalakrishnan24@gmail.com> for any
suggestions and doubts or you can always leave a comment on github.

RoadMap: 1.Mini batch gradient descent for better performance
2.Parallelsation of deeptree and deepforest 3. \#\# Installation

You can install the released version of deepdive from
[GitHub](https://github.com/RajeshB24/deepdive) with:

``` r
devtools::install_github("RajeshB24/deepdive")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(deepdive)
## basic example code
 x <- data.frame(a = runif(1000)*100,
 b = runif(1000)*200,
 c = runif(1000)*100,
 d = runif(1000)*200)
 y<- data.frame(y=20*x$a +30* x$b+10*x$c +10)

 #train
 modelnet<-deepnet(x,y,c(2,2),activation = "relu",
 reluLeak = 0,
 modelType = "regress",
 iterations =4000,
 eta=0.8,
 optimiser="adam")
#> [1] "iteration 40: 49.5510395722666"
#> [1] "iteration 80: 47.5209857573121"
#> [1] "iteration 120: 36.674376575073"
#> [1] "iteration 160: 12.1289696091467"
#> [1] "iteration 200: 1.93086061444675"
#> [1] "iteration 240: 0.808750098867407"
#> [1] "iteration 280: 0.494573066264881"
#> [1] "iteration 320: 0.342941350760865"
#> [1] "iteration 360: 0.257559624355655"
#> [1] "iteration 400: 0.203658384802617"
#> [1] "iteration 440: 0.165998136341685"
#> [1] "iteration 480: 0.138566603455909"
#> [1] "iteration 520: 0.117995599812321"
#> [1] "iteration 560: 0.102227813235802"
#> [1] "iteration 600: 0.0896398802183575"
#> [1] "iteration 640: 0.0794460562142076"
#> [1] "iteration 680: 0.0710174875154451"
#> [1] "iteration 720: 0.0638697611537178"
#> [1] "iteration 760: 0.0578447526715657"
#> [1] "iteration 800: 0.0527000429667451"
#> [1] "iteration 840: 0.0481752006090655"
#> [1] "iteration 880: 0.0442122768950516"
#> [1] "iteration 920: 0.0407691638755149"
#> [1] "iteration 960: 0.0377878882639883"
#> [1] "iteration 1000: 0.0351335882230483"
#> [1] "iteration 1040: 0.0327415634546343"
#> [1] "iteration 1080: 0.0306461575327707"
#> [1] "iteration 1120: 0.028781935919307"
#> [1] "iteration 1160: 0.0271070630608411"
#> [1] "iteration 1200: 0.0255808604106501"
#> [1] "iteration 1240: 0.0241855288098509"
#> [1] "iteration 1280: 0.022910524011942"
#> [1] "iteration 1320: 0.021750880904187"
#> [1] "iteration 1360: 0.0206914230170582"
#> [1] "iteration 1400: 0.0197197747585929"
#> [1] "iteration 1440: 0.0188282999442206"
#> [1] "iteration 1480: 0.0180102294692994"
#> [1] "iteration 1520: 0.01725887785414"
#> [1] "iteration 1560: 0.0165652448710933"
#> [1] "iteration 1600: 0.01592199203369"
#> [1] "iteration 1640: 0.0153236399484235"
#> [1] "iteration 1680: 0.0147654797031894"
#> [1] "iteration 1720: 0.0142429702782207"
#> [1] "iteration 1760: 0.0137528861046103"
#> [1] "iteration 1800: 0.01329310077038"
#> [1] "iteration 1840: 0.0128612254437926"
#> [1] "iteration 1880: 0.012455323414874"
#> [1] "iteration 1920: 0.0120720157700017"
#> [1] "iteration 1960: 0.0117093443795325"
#> [1] "iteration 2000: 0.0113660862818214"
#> [1] "iteration 2040: 0.0110399325080925"
#> [1] "iteration 2080: 0.0107288928537881"
#> [1] "iteration 2120: 0.0104321453375902"
#> [1] "iteration 2160: 0.0101487871588763"
#> [1] "iteration 2200: 0.00987816642109026"
#> [1] "iteration 2240: 0.00962417655234536"
#> [1] "iteration 2280: 0.00938289853567132"
#> [1] "iteration 2320: 0.00915316725581931"
#> [1] "iteration 2360: 0.00893412464605082"
#> [1] "iteration 2400: 0.00872465993367558"
#> [1] "iteration 2440: 0.00852348112837055"
#> [1] "iteration 2480: 0.00832971815375747"
#> [1] "iteration 2520: 0.00814294036827557"
#> [1] "iteration 2560: 0.00796172804014815"
#> [1] "iteration 2600: 0.00778597990817228"
#> [1] "iteration 2640: 0.00761551873818562"
#> [1] "iteration 2680: 0.00744979456200046"
#> [1] "iteration 2720: 0.00728843628875042"
#> [1] "iteration 2760: 0.00713134505617462"
#> [1] "iteration 2800: 0.00697838881294293"
#> [1] "iteration 2840: 0.00683125737302955"
#> [1] "iteration 2880: 0.00668936118568361"
#> [1] "iteration 2920: 0.00655107936096402"
#> [1] "iteration 2960: 0.00642030602501203"
#> [1] "iteration 3000: 0.00629489954666116"
#> [1] "iteration 3040: 0.0061718825236612"
#> [1] "iteration 3080: 0.00605230969875288"
#> [1] "iteration 3120: 0.00593505465541927"
#> [1] "iteration 3160: 0.00582102599267914"
#> [1] "iteration 3200: 0.00570896507430532"
#> [1] "iteration 3240: 0.00560024869419978"
#> [1] "iteration 3280: 0.00549425913618715"
#> [1] "iteration 3320: 0.00538944728419628"
#> [1] "iteration 3360: 0.00528795771642777"
#> [1] "iteration 3400: 0.00518878015239306"
#> [1] "iteration 3440: 0.0050923175637942"
#> [1] "iteration 3480: 0.00499737604829063"
#> [1] "iteration 3520: 0.00490522512349508"
#> [1] "iteration 3560: 0.0048148824058818"
#> [1] "iteration 3600: 0.00472695100013893"
#> [1] "iteration 3640: 0.00464121950575321"
#> [1] "iteration 3680: 0.0045576263237731"
#> [1] "iteration 3720: 0.00447538866315311"
#> [1] "iteration 3760: 0.0043951478916041"
#> [1] "iteration 3800: 0.00431692118286432"
#> [1] "iteration 3840: 0.00423967998336254"
#> [1] "iteration 3880: 0.00416386038828475"
#> [1] "iteration 3920: 0.00408933597891855"
#> [1] "iteration 3960: 0.00401626627336216"
#> [1] "iteration 4000: 0.00394450080036075"
```
