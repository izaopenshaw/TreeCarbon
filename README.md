# TreeCarbon: UK tree allometrics in R
# Version 1.0
## Goal
The goal of **TreeCarbon** is to estimate the carbon from diameter at breast height (DBH) measurements and tree species information using allometry from the Woodland Carbon Code Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2.0)." (2018), allodb.R, BIOMASS.R and Bunce (1968). The package also propagates error for estimates.
## Authors and contributors
Authors: Isabel Openshaw and Justin Moat  
Contributors: Phil Wilkes

## 1.0 Introduction

Accurately estimating tree biomass is a challenge, as destructive sampling is rarely done, this means most estimates rely on allometric equations that use measurements like diameter at breast height (DBH) and tree height. But these estimates vary greatly. The variation isn't just with the allometric equations themselves; it's in the variables  we use within them: DBH, height, wood density, and biomass-to-carbon conversion factors. These numbers are fundamental to how we calculate natural carbon for; carbon markets, tree planting schemes, and offsetting initiatives and within national tree planting and net zero targets.

To address this, we've developed the TreeCarbon R package—a tool that calculates and compares biomass and carbon estimates, whilst quantifying the uncertainties involved. The package is built around UK tree species and protocols, making it directly relevant to UK-based projects, but it’s flexible enough to be adapted for use in other regions with local allometric equations and inputs. It pulls together key UK allometries and common routines, including the [Woodland Carbon Code Protocol Assessment (2018)](https://www.woodlandcarboncode.org.uk/images/PDFs/WCC_CarbonAssessmentProtocol_V2.0_March2018.pdf)) [Bunce (1968)](https://doi.org/10.2307/2258105), [allodb](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13756) and [BIOMASS.R](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12753) (NB this package is predominately for tropical forests). It also adds support for terrestrial laser scanning workflows. The aim is to make it easier for researchers, practitioners, and decision-makers to see how different methods and estimates compare.

TreeCarbon includes uncertainty estimation routines and supports batch processing, users can compare across datasets and methods. It helps show the assumptions embedded into these allometric equations and provides a way to easily test, compare, and quantify the uncertainty they convey. 


### 1.0.1 Version history

<!-- badges: start -->
* 03/2025 version 1.0 release
<!-- badges: end -->

### 1.0.2	Script development  

Further improvements to the tool will be included in future releases, if you wish to add to this tool, please do fork it or send suggestion to the Authors

### 1.0.3 Installation

You can install the development version from GitHub with (you will need the devtools package):

```
install.packages('devtools') #if needed
devtools::install_github("gistin/TreeCarbon")
```

### 1.0.4 Quick example

This is a basic example which shows you how to use the most common problem features of this package:

``` r
devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE)

library(TreeCarbon)
## example to calculate the above ground carbon for a European Oak (Quercus robur)
## with DBH 75 cm and height of 25m, using the woodland carbon code V2.0
fc_agc("Quercus robur", 75, 25, output.all = F)
```
## Detailed walk through
``` r
#quick example
fc_agc("Quercus robur", 75, 25, output.all = F)

#detailed walk through
#single tree example
common_name = "Oak"
dbh = 75 #cm
height = 25  #m

####Woodland carbon code (WCC) V2.0 walkthrough
#The woodland carbon code use multiple different allometrics to firstly derive
#tarrif number which leads to Stem volume, Stem biomass and Crown biomass 
#and also estimates of root biomass.
#Each of these can be called individually (see help file) or as one with fc_agc
#Common names are looked up and matched to the close species (UK centric),
#if the name is not then the type can be used (broadleaf or conifer) to give
#broad WCC figures for the UK

fc_agc(common_name, dbh, height, output.all = T)
#where spcode = lookup code in WCC
#NSG = wood density from the WCC look up


#wood density is defaulted to the WWC or you can specify your own
#Here the case of a beech, with dbh 72 cm and 24m tall and using wood density from the 
#BIOMASS package
#install.packages("BIOMASS") #if needed

wd <- BIOMASS::getWoodDensity('Fagus', 'sylvatica', region='Europe')
fc_agc('beech', 72, 24, nsg = wd$meanWD)

###using BIOMASS package allometry (note these equations are orientated to tropical areas)
coords <- c(-0.088837, 51.071610)
biomass(72, 24,'Fagus', 'sylvatica', coords )

###Using allodb package
allodb(24, "Fagus", "sylvatica", coords, output.all = FALSE)

###Using Bunce 1968 allometry NB onlys uses dbh not height
Bunce("beech",72)


###Batch processing with vectors (list or a dataframe of species)
names <- rep(c("Oak", "Beech"),5)
dbhs <- rnorm(10,mean = 74, sd = 0.5)
heights <- rnorm(10,mean = 24, sd = 1)

fc_agc(names,dbhs,heights)

###test against several allometries
allometries("Quercus", "robur", dbh = 20, height = 10)

###other routines
#WWC seedling and saplings are calculated differently to trees, only height 
#and type are needed (conifer or broadleaf)
#note height is in cm
sap_seedling2C(50, 'conifer')
#Carbon of CO2 conversion ie 1 tonne carbon - 3.6
ctoco2e(1)
```

## Acknowledgements
This work is part of the Nature Unlock program at [Royal Botanic Gardens Kew](https://www.kew.org/wakehurst/nature-unlocked), and the [Nature Returns programme](https://www.kew.org/science/nature-returns).  It was funded or sponsored by the following UK government departments: HM Treasury (Shared Outcomes Fund), Department for Environment, Food and Rural Affairs, Department for Energy Security and Net Zero.

## Main citations

**Woodland Carbon Code (WCC):**
> Jenkins, T.A.R., Mackie, E.D., Matthews, R.W., Miller, G., Randle, T.J., & White, M.E. (2018). FC Woodland Carbon Code: Carbon Assessment Protocol (v2.0). Forestry Commission. https://www.woodlandcarboncode.org.uk/

**Bunce (1968):**
> Bunce, R.G.H. (1968). Biomass and Production of Trees in a Mixed Deciduous Woodland: I. Girth and Height as Parameters for the Estimation of Tree Dry Weight. *The Journal of Ecology*, 56(3), 759-775. https://doi.org/10.2307/2258105

**BIOMASS R package:**
> Réjou-Méchain, M., Tanguy, A., Piponiot, C., Chave, J., & Hérault, B. (2017). BIOMASS: An R package for estimating above-ground biomass and its uncertainty in tropical forests. *Methods in Ecology and Evolution*, 8(9), 1163-1167. https://doi.org/10.1111/2041-210X.12753

**allodb R package:**
> Gonzalez-Akre, E., Piponiot, C., Lepore, M., et al. (2022). allodb: An R package for biomass estimation at globally distributed extratropical forest plots. *Methods in Ecology and Evolution*, 13(2), 330-338. https://doi.org/10.1111/2041-210X.13756

**Chave pantropical allometry:**
> Chave, J., Réjou-Méchain, M., Búrquez, A., et al. (2014). Improved allometric models to estimate the aboveground biomass of tropical trees. *Global Change Biology*, 20(10), 3177-3190. https://doi.org/10.1111/gcb.12629

**Error propagation methodology:**
> Chave, J., Condit, R., Aguilar, S., et al. (2004). Error propagation and scaling for tropical forest biomass estimates. *Philosophical Transactions of the Royal Society B*, 359(1443), 409-420. https://doi.org/10.1098/rstb.2003.1425

**Carbon content:**
> Thomas, S.C., & Martin, A.R. (2012). Carbon content of tree tissues: A synthesis. *Forests*, 3(2), 332-352. https://doi.org/10.3390/f3020332


### Licence

This package is licensed under the [GNU General Public License v3.0 (GPL-3)](https://www.gnu.org/licenses/gpl-3.0.html). 

You are free to use, modify, and distribute this software, provided that any derivative works are also licensed under GPL-3.
