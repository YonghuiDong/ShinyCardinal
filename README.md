<img src="https://github.com/YonghuiDong/ShinyCardinal/blob/main/inst/app/www/img/logo.png" align="right" alt="" width="400" />

# ShinyCardinal v0.3.4
Shiny App for Mass Spectrometry Imaging.

# TOC
* [News](#news)
* [Installation](#installation)
* [Usage](#usage)
* [Acknowledgements](#acknowledgements)
* [Contribution](#contribution)
* [Citation](#citation)

## News

2024-05-14: Release of v0.3.4

- This version makes it easier for users to select MSI files using method 1.

## Installation

You can use one of the following ways to use and install ShinyCardinal.

#### (a) Install as a standalone App

>**Note**
>
> This is the easiest way to install and use ShinyCardinal (recommended)
>

<img align="center" height="50" src="https://edent.github.io/SuperTinyIcons/images/svg/windows.svg"> [Download](https://sourceforge.net/projects/shinycardinal/)




#### (b) Inatall as an R package

```r
remotes::install_github("YonghuiDong/ShinyCardinal")
```
>**Note**
>
> After installation, you can use `library(ShinyCardinal); run_app()` or `ShinyCardinal::run_app()` to start the app.

#### (c) Use online web version

https://gincpm.shinyapps.io/ShinyCardinal

>**Note**
>
>1. No installation needed; you can use the web version directly.
>2. It can be slowly and it may not work for MSI data over 2GB.

## Usage

<img align="center" height="50" src="https://edent.github.io/SuperTinyIcons/images/svg/youtube.svg"> Please watch our [Tutorial Videos](https://www.youtube.com/@MSI_WIS/videos).

## Contribution

You are very welcome to contribute by submitting new features, pull requests, and suggestions, all of which are greatly appreciated.

## Acknowledgements

ShinyCardinal wouldn't be possible without the solid foundation laid by [Cardinal](https://cardinalmsi.org). My heartfelt gratitude to the developers of Cardinal. :heart: :heart: :heart:

## Citation

If you use ShinyCardinal, please cite the following references:

- Dong.Y., and Heinig, W., 2024. Mass Spectrometry Imaging Data Analysis with ShinyCardinal. [Link](https://doi.org/10.21203/rs.3.rs-4072606/v1)

- Bemis, K.A., Foell, M.C., Guo, D., Lakkimsetty, S.S., and Vitek, O., 2023. Cardinal v.3: a versatile open-source software for mass spectrometry imaging analysis. **Nature Methods**, 20, 1883-1886. [Link](https://doi.org/10.1038/s41592-023-02070-z)


