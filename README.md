
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

*Reach me Out@* : <rajeshbalakrishnan24@gmail.com> for any suggestions
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
#> [1] "iteration 5: 151.1436174519"
#> [1] "iteration 10: 66.7387858067151"
#> [1] "iteration 15: 65.4829172266394"
#> [1] "iteration 20: 63.768503842343"
#> [1] "iteration 25: 51.3141805922564"
#> [1] "iteration 30: 51.9176559486274"
#> [1] "iteration 35: 52.1754336670127"
#> [1] "iteration 40: 50.665270908585"
#> [1] "iteration 45: 49.7849830553537"
#> [1] "iteration 50: 49.8270579233158"
#> [1] "iteration 55: 49.6061736288289"
#> [1] "iteration 60: 49.321634427835"
#> [1] "iteration 65: 49.0734090836989"
#> [1] "iteration 70: 48.6170057855314"
#> [1] "iteration 75: 47.8908134776841"
#> [1] "iteration 80: 46.830830137904"
#> [1] "iteration 85: 45.3348879271093"
#> [1] "iteration 90: 43.3188586034546"
#> [1] "iteration 95: 40.8889443695318"
#> [1] "iteration 100: 38.2284880853402"
#> [1] "iteration 105: 35.1402790710011"
#> [1] "iteration 110: 31.5653005112191"
#> [1] "iteration 115: 27.5511511998195"
#> [1] "iteration 120: 23.2285680815864"
#> [1] "iteration 125: 18.8175009816101"
#> [1] "iteration 130: 14.601044300559"
#> [1] "iteration 135: 10.8979208281998"
#> [1] "iteration 140: 7.95665674844703"
#> [1] "iteration 145: 6.03429865328661"
#> [1] "iteration 150: 4.72790957232224"
#> [1] "iteration 155: 3.69786239244636"
#> [1] "iteration 160: 2.82122953417081"
#> [1] "iteration 165: 2.15924551080176"
#> [1] "iteration 170: 1.73306951951041"
#> [1] "iteration 175: 1.47099791358584"
#> [1] "iteration 180: 1.24899003377828"
#> [1] "iteration 185: 1.07120386423489"
#> [1] "iteration 190: 0.938816659795801"
#> [1] "iteration 195: 0.826685075903498"
#> [1] "iteration 200: 0.736183107273482"
#> [1] "iteration 205: 0.661720380201782"
#> [1] "iteration 210: 0.596853643479557"
#> [1] "iteration 215: 0.542027818890704"
#> [1] "iteration 220: 0.493914880244036"
#> [1] "iteration 225: 0.451030945975103"
#> [1] "iteration 230: 0.413986704503065"
#> [1] "iteration 235: 0.382262550420875"
#> [1] "iteration 240: 0.355129072537415"
#> [1] "iteration 245: 0.331656859693252"
#> [1] "iteration 250: 0.31111093644111"
#> [1] "iteration 255: 0.292833629660864"
#> [1] "iteration 260: 0.276573595541049"
#> [1] "iteration 265: 0.261877800536575"
#> [1] "iteration 270: 0.248560804034312"
#> [1] "iteration 275: 0.236439472758725"
#> [1] "iteration 280: 0.225336334578512"
#> [1] "iteration 285: 0.215111757022559"
#> [1] "iteration 290: 0.205660388497536"
#> [1] "iteration 295: 0.196877972876607"
#> [1] "iteration 300: 0.188697813632451"
#> [1] "iteration 305: 0.181043345590072"
#> [1] "iteration 310: 0.173870375789184"
#> [1] "iteration 315: 0.167147676060471"
#> [1] "iteration 320: 0.16083669874714"
#> [1] "iteration 325: 0.154912200810723"
#> [1] "iteration 330: 0.149347849416637"
#> [1] "iteration 335: 0.144118578039804"
#> [1] "iteration 340: 0.139190150432568"
#> [1] "iteration 345: 0.134532138427414"
#> [1] "iteration 350: 0.130129075640277"
#> [1] "iteration 355: 0.125948470743452"
#> [1] "iteration 360: 0.121974499873223"
#> [1] "iteration 365: 0.118191632229376"
#> [1] "iteration 370: 0.114595079887269"
#> [1] "iteration 375: 0.111165700347324"
#> [1] "iteration 380: 0.107907765323017"
#> [1] "iteration 385: 0.104820497155988"
#> [1] "iteration 390: 0.10187424339015"
#> [1] "iteration 395: 0.0990566463611809"
#> [1] "iteration 400: 0.0963655292621"
#> [1] "iteration 405: 0.0937953892877075"
#> [1] "iteration 410: 0.0913332652263422"
#> [1] "iteration 415: 0.0889753059214701"
#> [1] "iteration 420: 0.0867221569205152"
#> [1] "iteration 425: 0.0845684774743681"
#> [1] "iteration 430: 0.0825101335336981"
#> [1] "iteration 435: 0.0805357447347269"
#> [1] "iteration 440: 0.0786395857498647"
#> [1] "iteration 445: 0.0768157829913054"
#> [1] "iteration 450: 0.0750585615420054"
#> [1] "iteration 455: 0.0733631487434695"
#> [1] "iteration 460: 0.0717264255636272"
#> [1] "iteration 465: 0.0701454826171085"
#> [1] "iteration 470: 0.0686183694812765"
#> [1] "iteration 475: 0.0671416707737415"
#> [1] "iteration 480: 0.0657127390818125"
#> [1] "iteration 485: 0.0643307014786547"
#> [1] "iteration 490: 0.0629923475868846"
#> [1] "iteration 495: 0.0616991122351131"
#> [1] "iteration 500: 0.0604463876666075"
```
