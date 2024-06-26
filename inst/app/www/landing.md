<b><span style="color:#6682c4; font-size:30px;">𓅫 Welcome to ShinyCardinal</span></b>

<img src='www/img/logo.png' height="180" style = "float: right; margin: 20px;"/>

<br></br>

Mass spectrometry imaging (MSI) is a well-established analytical technique that allows direct mapping of a wide variety of chemical classes from different biological samples, providing information concerning analyte identity, relative abundance, and spatial distribution.

<b><span style="color:#d17789; font-size:20px;">ShinyCardinal</span></b> is the web application of R package <a href="https://cardinalmsi.org" target="_new">Cardinal</a>. It is designed to aid researchers in reading, processing, visualizing and analyzing their MSI data. It offers a structured and intuitive workflow, making it a flexible and user-friendly tool for users of all levels.

---

<b><span style="color:#5c5d61; font-size:22px;">Workflow</span></b>

The workflow includes pre-processing (e.g., missing value imputation, QC-based feature filtering, normalization, transformation and scaling), univariate and multivariate statistical analyses, and visualization of the given metabolomics data. To aid interpretation, a comprehensive HTML report is generated that includes interactive visualizations, tables, summary statistics, and detailed explanations for the users, facilitating an in-depth inspection of the results (Fig. 1).

<img src='www/img/workflow.png' alt='workflow' title='workflow' style="vertical-align:middle;margin:0px 100px" width='600'/>


Fig. 1 Workflow and main features of ShinyCardinal.

<br></br>

<b><span style="color:#5c5d61; font-size:15px;">Main Features</span></b>

- Cross platform.

- Vendor neutral: accept imzML format.

- Accept multiple MSI data (runs).

- MSI data pre-processing, including normalization, smoothing, baseline correction, peak-picking and alignment.

- Background noise and matrix peaks removal.

- Visualization of mass spectra and molecular ion images

- Region of interest (ROI) analysis, including ROI selection, visualization, comparison (statistics) and profiling.

- Image segmentation using principal component analysis (PCA) and spatial shrunken centroid (SSC).

- Network analysis, colocalization analysis, and pseudo-MS/MS generation.

- Metabolite identification.


<b><span style="color:#5c5d61; font-size:22px;">How to use ShinyCardinal</span></b>

Using ShinyCardinal is straightforward. Simply follow the step-by-step instructions provided in each tab to perform your data analysis. For further guidance, consult the user manual, or experiment with the included demo data to get a hands-on feel for the software.

<b><span style="color:#5c5d61; font-size:22px;">Demo data</span></b>

To assist in preparing input data for MetaboReport, you can use the two demo datasets as reference.

> **Data Set1: MALDI imaging**: 
>
> Description: A 15-micron tissue section of wild type mouse cerebellum was coated with 2,5-dihydroxybenzoic acid (DHB) matrix and analysed by MSI in positive ion mode over an m/z range of 300-2000 using a Thermo MALDI LTQ Orbitrap XL instrument. The spatial resolution was 50 micrometers.
>
> Download Link: <a href="https://www.ebi.ac.uk/metabolights/editor/MTBLS487/descriptors" target="_new"><i class="fa fa-link" aria-hidden="true"></i></i></a>
>
> **Data Set 2: SIMS imaging**
>
> Description:
>
> Download link:
>
> **Data Set 3: DESI imaging**
>
> Description: DESI imaging data were acquired in the negative-ion mode over an m/z range of 200-1050 using a Thermo Exactive instrument (Thermo Scientific GmbH, Bremen, Germany) coupled to a home-built automated DESI-imaging source. All Thermo raw files of one imaging experiment were converted to imzML format using the imzML converter v1.1.4.5i.
>
> Download link: <a href="https://www.ebi.ac.uk/metabolights/editor/MTBLS176/files" target="_new"><i class="fa fa-link" aria-hidden="true"></i></i></a>
>

---
<a href= 'https://www.weizmann.ac.il'><img src='www/img/WIS.png' alt='WIS' title='Weizmann Institute of Science' width='400'/></a>

<br></br>
