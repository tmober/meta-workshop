Teresa Ober
July 8, 2021

-   [Getting Started](#getting-started)
    -   [Part 1: Introduction to Systematic
        Review](#part-1-introduction-to-systematic-review)
        -   [Session Aims:](#session-aims)
        -   [Session Organization:](#session-organization)
    -   [Part 2: Introduction to
        Meta-analysis](#part-2-introduction-to-meta-analysis)
        -   [Session Aims:](#session-aims-1)
        -   [Session Organization:](#session-organization-1)
-   [Resources](#resources)
    -   [General Resources for R and
        RStudio](#general-resources-for-r-and-rstudio)
    -   [General Resources for Conducting Meta-analysis in
        R](#general-resources-for-conducting-meta-analysis-in-r)
    -   [Additional Documentation of R Packages for
        Meta-analysis](#additional-documentation-of-r-packages-for-meta-analysis)
        -   [Metafor](#metafor)
        -   [Robumeta](#robumeta)
    -   [Pre-registration of Systematic
        Reviews](#pre-registration-of-systematic-reviews)
    -   [Preferred Reporting Items for Systematic Reviews and
        Meta-Analyses
        (PRISMA)](#preferred-reporting-items-for-systematic-reviews-and-meta-analyses-prisma)

# Getting Started

Before the workshop, please consider taking a moment to complete the
following steps:  
1) Download the latest version of R statistical environment from the
website here: <https://cran.cmm.msu.ru/> .  
2) Though not required, you will probably find it helpful to also
download RStudio: <https://www.rstudio.com/products/rstudio/download/> .
For our purposes, the free “RStudio Desktop” version will work just
fine.  
- If you’re not familiar with RStudio at all, [Chapter 1 of this online
textbook](https://rafalab.github.io/dsbook/getting-started.html)
provides a gentle introduction.  
3) During the workshop, we will be using the following packages, so if
you are able to install R, please also consider installing them ahead of
time. Note that you should only need to install the package once (unless
you later re-install or update R). From then on, you can call the
package into your library by using the “library()” or “require()”
functions.  
– [*tidyverse*](https://www.tidyverse.org/packages/): Data
manipulation  
– [*dplyr*](https://dplyr.tidyverse.org/): Data manipulation  
– [*psych*](https://cran.r-project.org/web/packages/psych/index.html):
Creating quick descriptive summaries of data  
– [*metafor*](https://www.metafor-project.org/doku.php): Meta-analysis  
–
[*robumeta*](https://cran.r-project.org/web/packages/robumeta/index.html):
Robust variance estimation meta-analysis  
– [*ggplot2*](https://ggplot2.tidyverse.org/): Plotting

Most of these packages have packages dependencies (i.e., other packages
they use), so you may notice other packages getting installed, as well.
\# Overview of Workshop Topics

## Part 1: Introduction to Systematic Review

### Session Aims:

-   Receive overview of systematic review processes  
-   Discuss the scope and the formulation of a review question  
-   Consider the purpose and format of a protocol  
-   Gain familiarity with systematic searching/screening strategies  
-   Learn data extraction, quality assessment, and synthesis

### Session Organization:

1.  What is a systematic review?  
2.  Best practices for conducting systematic reviews  
3.  Question Formulation  
4.  Creating a Protocol  
5.  Searching for Literature  
6.  Screening Literature  
7.  Data Extraction  
8.  Risk of Bias and Quality Assessment  
9.  Synthesis

## Part 2: Introduction to Meta-analysis

### Session Aims:

-   Receive overview of steps involved in a meta-analysis  
-   Discuss effect size estimates and challenges to extraction  
-   Gain an orientation to fixed- and random-effects models  
-   Experience data extraction and meta-analysis  
-   Outline the reporting of meta-analysis findings

### Session Organization:

1.  What is a meta-analysis?  
2.  Effect size estimates  
3.  Fixed- and Random-Effects  
4.  Resources for Conducting Meta-analysis  
5.  Reporting Results

# Resources

## General Resources for R and RStudio

-   Irizarry, R. A. (2019). *Introduction to data science: Data analysis
    and prediction algorithms with R.*
    <https://rafalab.github.io/dsbook>

## General Resources for Conducting Meta-analysis in R

-   Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021).
    *Doing Meta-Analysis with R: A Hands-On Guide.* Boca Raton, FL and
    London: Chapmann & Hall/CRC Press. ISBN 978-0-367-61007-4.
    <https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/>

## Additional Documentation of R Packages for Meta-analysis

Polanin, J. R., Hennessy, E. A., & Tanner-Smith, E. E. (2017). A review
of meta-analysis packages in R. *Journal of Educational and Behavioral
Statistics*, *42*(2), 206-242.
<https://doi.org/10.3102/1076998616674315>

### Metafor

The metafor Package: Tips and Notes.
<https://metafor-project.org/doku.php/tips>

### Robumeta

Tanner-Smith, E. E., Tipton, E., & Polanin, J. R. (2016). Handling
complex meta-analytic data structures using robust variance estimates: A
tutorial in R. *Journal of Developmental and Life-Course Criminology*,
*2*(1), 85-112. <https://doi.org/10.1002/jrsm.1091>

Fisher, Z., & Tipton, E. (2015). robumeta: An R-package for robust
variance estimation in meta-analysis. *arXiv preprint*.
<https://arxiv.org/abs/1503.02220>

## Pre-registration of Systematic Reviews

-   [PROSPERO](https://www.crd.york.ac.uk/prospero/): For
    pre-registration of health-related systematic reviews.  
-   [Research Registry](https://www.researchregistry.com/): Altnerative
    to PROSPERO that does not restrict systematic reviews to those
    related to human health. However, also costs money to submit a
    pre-registration.  
-   Many other alternatives, as well, that are not specific to
    systematic reviews, including the [Open Science
    Framework](https://osf.io/prereg/),
    [AsPredicted.Org](https://aspredicted.org/), etc.

## Preferred Reporting Items for Systematic Reviews and Meta-Analyses (PRISMA)

Moher, D., Liberati, A., Tetzlaff, J., Altman, D. G., & PRISMA Group.
(2009). Preferred reporting items for systematic reviews and
meta-analyses: the PRISMA statement. *PLoS Medicine*, *6*(7), e1000097.
<https://doi.org/10.1371/journal.pmed.1000097>

Moher, D., Shamseer, L., Clarke, M., Ghersi, D., Liberati, A.,
Petticrew, M., … & Stewart, L. A. (2015). Preferred reporting items for
systematic review and meta-analysis protocols (PRISMA-P) 2015 statement.
*Systematic Reviews*, *4*(1), 1-9.
<https://doi.org/10.1186/2046-4053-4-1>
