# License

TreeCarbon is licensed under the **GNU General Public License v3.0 (GPL-3)**.

## Summary

You are free to:
- **Use** — run the software for any purpose
- **Study** — examine and modify the source code
- **Share** — redistribute copies
- **Improve** — distribute modified versions

Under the following conditions:
- **Disclose source** — Source code must be made available when distributing
- **Same license** — Modifications must be distributed under GPL-3
- **State changes** — Changes made to the code must be documented

## Full License

See the [GNU GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.txt) for the complete license text.

---

## Citation Requirements

When using TreeCarbon in publications, please cite:

### The Package

```bibtex
@Manual{TreeCarbon,
  title = {TreeCarbon: Calculate Carbon Estimates Using Multiple Allometric Methods},
  author = {Isabel Openshaw and Justin Moat},
  year = {2024},
  note = {R package version 0.0.0.9002},
  url = {https://github.com/izaopenshaw/TreeCarbon},
}
```

### Methodological Sources

Please also cite the original sources for the methods you use:

#### Woodland Carbon Code (WCC)

```bibtex
@TechReport{WCC2021,
  title = {Woodland Carbon Code: Carbon Assessment Protocol},
  author = {{UK Woodland Carbon Code}},
  year = {2021},
  institution = {Scottish Forestry},
  note = {Version 2.2},
  url = {https://woodlandcarboncode.org.uk/},
}
```

#### Bunce (1968)

```bibtex
@Article{Bunce1968,
  title = {Biomass and Production of Trees in a Mixed Deciduous Woodland. 
           I. Girth and Height as Parameters for the Estimation of Tree Dry Weight},
  author = {R. G. H. Bunce},
  year = {1968},
  journal = {Journal of Ecology},
  volume = {56},
  number = {3},
  pages = {759--775},
  doi = {10.2307/2258105},
}
```

#### BIOMASS Package

```bibtex
@Article{RejouMechain2017,
  title = {BIOMASS: An R Package for Estimating Above-Ground Biomass 
           and Its Uncertainty in Tropical Forests},
  author = {Maxime Rejou-Mechain and Ariane Tanguy and Camille Piponiot 
            and Jerome Chave and Bruno Herault},
  year = {2017},
  journal = {Methods in Ecology and Evolution},
  volume = {8},
  number = {9},
  pages = {1163--1167},
  doi = {10.1111/2041-210X.12753},
}

@Article{Chave2014,
  title = {Improved Allometric Models to Estimate the Aboveground Biomass 
           of Tropical Trees},
  author = {Jerome Chave and others},
  year = {2014},
  journal = {Global Change Biology},
  volume = {20},
  number = {10},
  pages = {3177--3190},
  doi = {10.1111/gcb.12629},
}
```

#### allodb Package

```bibtex
@Article{GonzalezAkre2022,
  title = {allodb: An R Package for Biomass Estimation at Globally 
           Distributed Extratropical Forest Plots},
  author = {Erika Gonzalez-Akre and others},
  year = {2022},
  journal = {Methods in Ecology and Evolution},
  volume = {13},
  number = {2},
  pages = {330--338},
  doi = {10.1111/2041-210X.13756},
}
```

#### Carbon Fraction Methods

```bibtex
@Article{ThomasMartin2012,
  title = {Carbon Content of Tree Tissues: A Synthesis},
  author = {Sean C. Thomas and Adam R. Martin},
  year = {2012},
  journal = {Forests},
  volume = {3},
  number = {2},
  pages = {332--352},
  doi = {10.3390/f3020332},
}

@TechReport{IPCC2006,
  title = {2006 IPCC Guidelines for National Greenhouse Gas Inventories, 
           Volume 4: Agriculture, Forestry and Other Land Use},
  author = {{IPCC}},
  year = {2006},
  institution = {Intergovernmental Panel on Climate Change},
  url = {https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol4.html},
}
```

#### Wood Density Data

```bibtex
@Article{Zanne2009,
  title = {Global Wood Density Database},
  author = {Amy E. Zanne and others},
  year = {2009},
  journal = {Dryad Digital Repository},
  doi = {10.5061/dryad.234},
}
```

---

## Get All Citations in R

```r
# View all citations
citation("TreeCarbon")

# Copy BibTeX to clipboard (requires clipr package)
# clipr::write_clip(toBibtex(citation("TreeCarbon")))
```

---

© 2024 Isabel Openshaw & Justin Moat, Royal Botanic Gardens, Kew



