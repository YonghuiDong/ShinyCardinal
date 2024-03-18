<img src="https://github.com/YonghuiDong/ShinyCardinal/blob/main/inst/app/www/img/logo.png" align="right" alt="" width="400" />

# ShinyCardinal
Shiny App for Mass Spectrometry Imaging.

# TOC
* [Installation](#installation)
* [Usage](#usage)
* [Acknowledgements](#acknowledgements)
* [Contribution](#contribution)
* [Cite](#cite)

## Installation

### (a) Install as a standalone App

>**Note**
>
>1. This is the easiest way to install and use ShinyCardinal.
>2. It does not required any addition soeftware.
>3. No programming skills needed.

- #### ![](https://img.shields.io/badge/Windows-grey?style=for-the-badge&logo=microsoft)

  
- #### ![](https://img.shields.io/badge/macOS-grey?style=for-the-badge&logo=apple)


### (b) Inatall as an R package

>**Note**
>
>1. You need to have [R](https://www.r-project.org) pre-installed.
>2. After installing ShinyCardinal, you can use `library(ShinyCardinal); run_app()` or `ShinyCardinal::run_app()` to start the app.

- #### Install from GitHub
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("YonghuiDong/ShinyCardinal")
```
- #### Install from BioConductor

```r
 if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ShinyCardinal")
```

### (c) Install as a Docker image

>**Note**
>
>1. You can use the web version directly.
>2. It can be slowly and it does not work for MSI data over 4GB.

### (d) Use online web version

>**Note**
>
>1. You can use the web version directly.
>2. It can be slowly and it does not work for MSI data over 4GB.

## Usage

<i class="fa fa-youtube" aria-hidden="true"></i> 

Please watch our [YouTube Tutorial Videos](https://www.youtube.com/@MSI_WIS/videos)

## Contribution

You are very welcome to contribute by submitting new features, pull requests, and suggestions, all of which are greatly appreciated.

## Acknowledgements

## Cite

