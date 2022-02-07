# ROBMED: A robust bootstrap test for mediation analysis


This is an `SPSS` `R` extension bundle for the robust bootstrap test ROBMED ([Alfons, Ates & Groenen, 2021](https://doi.org/10.1177/1094428121999096)) for mediation analysis.  It links to the `R` package [`robmed`](https://github.com/aalfons/robmed).


## About ROBMED

The robust bootstrap test ROBMED for mediation analysis is less sensitive to deviations from model assumptions (such as outliers or heavily tailed distributions) than the standard bootstrap test of Preacher & Hayes ([2004](http://dx.doi.org/10.3758/BF03206553), [2008](http://dx.doi.org/10.3758/BRM.40.3.879)).  ROBMED utilizes the robust MM-regression estimator ([Yohai, 1987](https://projecteuclid.org/euclid.aos/1176350366)) instead of the OLS estimator for regression, and runs bootstrap tests with the fast and robust bootstrap methodology ([Salibián-Barrera & Zamar, 2002](https://projecteuclid.org/euclid.aos/1021379865); [Salibián-Barrera & Van Aelst, 2008](https://doi.org/10.1016/j.csda.2008.05.007)).

More information can be found in our article:

Alfons, A., Ates, N.Y., & Groenen, P.J.F. (2021). A Robust Bootstrap Test for
Mediation Analysis. *Organizational Research Methods*. DOI [10.1177/1094428121999096](https://doi.org/10.1177/1094428121999096).


## Installing the extension bundle

Before you can install the extension bundle `ROBMED`, you need to install [`R`](https://cran.r-project.org/) and the [`R` plug-in for `SPSS`](https://github.com/IBMPredictiveAnalytics/R_Essentials_Statistics/releases).  Make sure that you install the version of `R` that is required for your version of `SPSS`.  Starting with `SPSS` version 28.0, `R` and the integration plug-in are automatically installed with `SPSS`.

In `R`, install the add-on package [`robmed`](https://github.com/aalfons/robmed). Always make sure that the latest version of the package is installed.

Afterwards download the file [`ROBMED.spe`](https://github.com/aalfons/ROBMED-RSPSS/raw/master/ROBMED.spe) and store it on your local computer.  You can then install the extension bundle:

1. From the `SPSS` menu, select **Extensions > Install Local Extension Bundle...**

2. In the **Open an Extension Bundle** dialog, select the file `ROBMED.spe` and click **Open**.


## Using the extension bundle

To run the robust boostrap test ROBMED for mediation analysis:

1. From the SPSS menu, select **Analyze > Regression > ROBMED: Robust Mediation Analysis**

2. In the `ROBMED` dialog, select a numeric **Y variable** (the dependent variable), one or more **X variable(s)** (the independent variables of interest), and one or more numeric **Mediator(s) (M)** (the hypothesized mediator variables).

3. Optionally, you can select one or more **Covariate(s)** (additional control variables).

4. In case of multiple hypothesized mediators, select between the parallel and serial multiple mediator model via **Multiple mediator model**.  The default is to use parallel mediators.

5. Click **Run**.


### Ordinal and nominal variables

It is important to note that `SPSS` and `R` store and handle categorical variables differently.  

For numeric variables on an ordinal measurement scale, `ROBMED` passes on the *values* to `R`.  In other words, the ordinal scale is assumed to be linear, which is typical `SPSS` behavior in linear regression models.

For all other categorical variables (numeric variables on a nominal measurement scale, string variables on an ordinal or nominal measurement scale), `ROBMED` preserves the categorical nature and passes on the *value labels* to `R`.  Those variables are then converted into groups of dummy variables for the analysis, which is typical `R` behavior.  Make sure that all value labels actually occur in the data, otherwise the analysis may give an error.


### Further details

Other elements of the `ROBMED` dialog can be used to further customize your analysis.

* **Confidence level**:  Enter an integer between 90 and 99 for the desired confidence level of the bootstrap confidence interval of the indirect effect.  The default is 95 for a 95% confidence level.

* **Number of bootstrap samples**:  Enter an integer between 1000 and 50000 for the desired number of bootstrap samples.  The default is 5000 samples.  For lower values, the obtained confidence intervals may not be very accurate, as the limits are based on percentiles of the bootstrap distribution.

* **Diagnostic plot of regression weights**:  Check this box to create a diagnostic plot of the weights from the robust regressions. This plot allows to easily detect deviations from normality assumptions such as skewness or heavy tails.


Additional options can be entered by clicking on the **Options** button.  This opens a subdialog that allows to customize the MM-estimator of regression as well as the random number generator.

* **Efficiency at the normal distribution**: This allows to set the desired efficiency of the MM-estimator under normally distributed error terms.  Higher efficiency may increase the bias under deviations from normality, although this bias will still be bounded (unlike OLS estimation, for which the bias can become arbitrarily large under such deviations from the model assumptions).  Possible values are 80%, 85% (the default), 90% or 95% efficiency.

* **Maximum number of iterations**:  The computation of the MM-estimator of regression requires an iterative algorithm.  Enter an integer between 1000 and 1000000 for the maximum number of iterations to be executed in this algorithm.  If this number of iterations is reached, the algorithm will terminate *without convergence*.  The default is a maximum of 10000 iterations.  In practice, far fewer iterations should be necessary for convergence.

* **Seed**:  Optionally, enter an integer to be used as seed for the random number generator.  It is important to note that setting a seed is necessary for reproducibility of results.

* **Version**:  This allows to set the version of the random number generator to be used.  In `R` version 3.6.0, the default random number generator was improved slightly, so the purpose of this option is to allow reproducibility of results obtained with `R` 3.5.3 or earlier.  Possible values are *Current* (the default) for the random number generator of the `R` version currently used, or *Compatibility with R 3.5.3*.  Note that this option has no effect if `R` version 3.5.3 or earlier is used.


## Report issues and request features

If you experience any bugs or issues or if you have any suggestions for additional features, please submit an issue via the [*Issues*](https://github.com/aalfons/ROBMED-RSPSS/issues) tab of this repository.  Please have a look at existing issues first to see if your problem or feature request has already been discussed.


## Ask for help

If you need help using the extension bundle, or if you are interested in collaborations related to this project, please get in touch with the [maintainer](https://personal.eur.nl/alfons/).
