# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------


# Documentation and online help text
help_text <- "
Command ROBMED requires the R Integration Plug-in and the R package 'robmed'.

ROBMED Y=dependent variable X=independent variables M=hypothesized mediators
      [COV=additional control variables] [MODEL={PARALLEL}]
                                                {SERIAL}
 [/OPTIONS [CONF=integer value] [BOOT=integer value]
           [EFFICIENCY=integer value] [MAXITER=integer value]
           [SEED=integer value] [RNG={CURRENT}]     ]
                                     {COMPATIBILITY}
 [/PLOTS [WEIGHT={0}]]
                 {1}

ROBMED /HELP prints this information and does nothing else.


Example:
ROBMED Y=TeamCommitment X=ValueDiversity M=TaskConflict
  /OPTIONS CONF=95 BOOT=5000
           EFFICIENCY=85 MAXITER=10000
           SEED=20211117 RNG=CURRENT
  /PLOTS WEIGHT=1.


It is important to note that SPSS and R store and handle categorical variables
differently.

For numeric variables on an ordinal measurement scale, ROBMED passes on the
values to R.  In other words, the ordinal scale is assumed to be linear, which
is typical SPSS behavior in linear regression models.

For all other categorical variables (numeric variables on a nominal measurement
scale, string variables on an ordinal or nominal measurement scale), ROBMED
preserves the categorical nature and passes on the value labels to R.  Those
variables are then converted into groups of dummy variables for the analysis,
which is typical R behavior.  Make sure that all value labels actually occur in
the data, otherwise the analysis may give an error.


Parameters:

Y:  a numeric variable on an ordinal or scale measurement level to be used as
    dependent variable. (*required*)

X:  one or more independent variables of interest. (*required*)

M:  one or more numeric variables on an ordinal or scale measurement level to
    be used as hypothesized mediators. (*required*)

COV:  variables to be used as additional control variables

MODEL:  in case of multiple hypothesized mediators, a character string
        specifying the type of mediation model.  Possible values are 'PARALLEL'
        (the default) for the parallel multiple mediator model, or 'SERIAL' for
        the serial multiple mediator model.

CONF:  integer value for the desired confidence level of the bootstrap
       confidence interval of the indirect effect.  The default is 95 for
       a 95% confidence level.

BOOT:  integer value for the desired number of bootstrap samples.  The default
       is 5000 samples.  For lower values, the obtained confidence intervals
       may not be very accurate, as the limits are based on percentiles of the
       bootstrap distribution.

EFFICIENCY:  integer value for the desired efficiency (as percentage) of the
             MM-estimator under normally distributed error terms.  Higher
             efficiency may increase the bias under deviations from normality,
             although this bias will still be bounded.  Possible values are
             80, 85 (the default), 90 or 95.

MAXITER:  integer value for the maximum number of iterations in the algorithm
          of the robust regression estimator.  If this number of iterations is
          reached, the algorithm will terminate without convergence.  The
          default is a maximum of 10000 iterations.  In practice, far fewer
          iterations should be necessary for convergence.

SEED:  optional integer value to be used as seed for the random number
       generator.  Setting a seed is necessary for reproducibility of
       results.

RNG:  version of the random number generator to be used.  In R version 3.6.0,
      the default random number generator was improved slightly, so the purpose
      of this option is to allow reproducibility of results obtained with R
      version 3.5.3 or earlier.  Possible values are CURRENT (the default) for
      the random number generator of the R version currently used, or
      COMPATIBILITY with R 3.5.3.  Note that this option has no effect if R
      version 3.5.3 or earlier is used.

WEIGHT:  set to 1 to create a diagnostic plot of the weights from the robust
         regressions, and to 0 otherwise. This plot allows to easily detect
         deviations from normality assumptions such as skewness or heavy tails.
"


# SPSS R extension requires a function Run() with only one argument 'args' to
# parse the SPSS syntax

Run <- function(args){
  # the first list element contains the command name and the second the
  # arguments
  args <- args[[2]]

  # process the syntax and assign argument values to R objects
  oobj <- spsspkg.Syntax(templ = list(
    spsspkg.Template(kwd = "Y", subc = "", ktype = "existingvarlist",
                     islist = FALSE, var = "y"),
    spsspkg.Template(kwd = "X", subc = "", ktype = "existingvarlist",
                     islist = TRUE, var = "x"),
    spsspkg.Template(kwd = "M", subc = "", ktype = "existingvarlist",
                     islist = TRUE, var = "m"),
    spsspkg.Template(kwd = "COV", subc = "", ktype = "varname",
                     islist = TRUE, var = "covariates"),
    spsspkg.Template(kwd = "MODEL", subc = "", ktype = "str",
                     islist = FALSE, var = "model"),
    spsspkg.Template(kwd = "CONF", subc = "OPTIONS", ktype = "int",
                     islist = FALSE, var = "conf"),
    spsspkg.Template(kwd = "BOOT", subc = "OPTIONS", ktype = "int",
                     islist = FALSE, var = "boot"),
    spsspkg.Template(kwd = "EFFICIENCY", subc = "OPTIONS", ktype = "int",
                     islist = FALSE, var = "efficiency"),
    spsspkg.Template(kwd = "MAXITER", subc = "OPTIONS", ktype = "int",
                     islist = FALSE, var = "maxiter"),
    spsspkg.Template(kwd = "SEED", subc = "OPTIONS", ktype = "int",
                     islist = FALSE, var = "seed"),
    spsspkg.Template(kwd = "RNG", subc = "OPTIONS", ktype = "str",
                     islist = FALSE, var = "rng"),
    spsspkg.Template(kwd = "WEIGHT", subc = "PLOTS", ktype = "int",
                     islist = FALSE, var = "plot")
  ))

  # show help or run R code
  if ("HELP" %in% attr(args, "names")) writeLines(help_text)
  else result <- spsspkg.processcmd(oobj, args, "call_robmed")
}


## function to take objects parsed by SPSS and call function robmed() from the
## R package robmed

call_robmed <- function(y, x, m, covariates = NULL, model = "parallel",
                        conf = 95, boot = 5000, efficiency = 85,
                        maxiter = 10000, seed = NULL, rng = "current",
                        plot = 1) {

  # check if package robmed is available
  tryCatch(library("robmed"), error = function(e) {
    stop("The R package 'robmed' is required but could not be loaded.",
         call. = FALSE)
  })
  # check version of package robmed
  robmed_version <- packageVersion("robmed")
  if (robmed_version < "0.10.0") {
    stop("Some functionality requires at least version 0.10.0 of the R ",
         "package 'robmed', but you have version ", robmed_version,
         ".\nPlease update to the latest version of the R package 'robmed'.",
         call. = FALSE)
  }

  # get variables from active data set
  x <- unlist(x)
  m <- unlist(m)
  covariates <- unlist(covariates)
  variables <- c(x, y, m, covariates)
  data <- spssdata.GetDataFromSPSS(variables, missingValueToNA = TRUE,
                                   factorMode = "labels")

  # check if there are any ordered factors, and if so overwrite them with the
  # underlying numeric values in the SPSS data
  is_ordinal <- sapply(data, is.ordered)
  if (any(is_ordinal)) {
    # check if the ordinal variables are actually numeric (in SPSS, both
    # numeric and string type variables can have an ordinal measurement scale)
    ordinal_data <- spssdata.GetDataFromSPSS(variables[is_ordinal],
                                             missingValueToNA = TRUE,
                                             factorMode = "none")
    is_numeric <- sapply(ordinal_data, is.numeric)
    # replace variables, if any
    if (any(is_numeric)) {
      replace <- which(is_ordinal)[is_numeric]
      data[, replace] <- ordinal_data[, is_numeric]
    }
  }

  # translate options
  model <- tolower(model)
  level <- conf / 100
  control <- reg_control(efficiency = efficiency / 100,
                         max_iterations = maxiter)
  switch_rng <- rng == "compatibility"
  plot <- isTRUE(as.logical(plot))

  # run function robmed()
  result <- tryCatch({
    if (switch_rng) RNGversion("3.5.3")
    if (!is.null(seed)) set.seed(seed)
    robust_boot <- robmed(data, x = x, y = y, m = m, covariates = covariates,
                          model = model, R = boot, level = level,
                          control = control)
    summary(robust_boot, plot = plot)
  }, error = function(e) stop(e))

  # show output
  print_SPSS(result)

  # clean up
  tryCatch(rm(list = ls()), warning = function(w) NULL)

  # return NULL invisibly
  invisible()

}


## print methods for SPSS like output

# generic function
print_SPSS <- function(x, ...) UseMethod("print_SPSS")

# print a robust model fit
print_SPSS.summary_lmrob <- function(x, response, ...) {
  # header for regression block (this will be printed automatically by SPSS
  # as it is passed as 'outline' argument for summary and coefficients tables)
  outline <- sprintf("Outcome variable: %s", response)
  # print model summary
  summary <- data.frame("Robust R" = sqrt(x$R2$R2), "Robust R Square" = x$R2$R2,
                        "Adjusted Robust R Square" = x$R2$adj_R2,
                        "Robust Std. Error of the Estimate" = x$s$value,
                        "Robust F" = x$F_test$statistic,
                        "df1" = x$F_test$df[1], "df2" = x$F_test$df[2],
                        "Sig." = x$F_test$p_value,
                        check.names = FALSE, stringsAsFactors = FALSE)
  spsspivottable.Display(summary, title = "Model Summary", outline = outline,
                         hiderowdimlabel = TRUE)
  # print coefficient matrix
  coefficients <- replace_dimnames(x$coefficients)
  spsspivottable.Display(coefficients, title = "Coefficients",
                         outline = outline, hiderowdimlabel = FALSE)
  # print information on outliers
  outlier_info <- robmed:::get_outlier_info(x$outliers)
  if (is.null(outlier_info$indices)) msg <- outlier_info$msg
  else {
    indices_to_print <- paste(outlier_info$indices, collapse = ", ")
    msg <- paste0(outlier_info$msg, indices_to_print, "\n", sep = "")
  }
  spss.TextBlock("Robustness weights", msg, outline = outline)
  # return NULL invisibly
  invisible()
}

# print summary of a mediation model fit
print_SPSS.summary_reg_fit_mediation <- function(x, outline, ...) {

  ## initializations
  p_x <- length(x$x)
  p_m <- length(x$m)
  simple <- p_m == 1L && p_x == 1L
  have_covariates <- length(x$covariates) > 0L

  ## in case of multiple mediators, print information on type of mediation model
  if (p_m > 1L) {
    prefix <- switch(x$model, parallel = "Parallel", serial = "Serial")
    header <- paste(prefix, "multiple mediator model\n\n")
  } else header <- ""
  ## information on variables
  if (simple) {
    info <- sprintf("Y = %s\nX = %s\nM = %s", x$y, x$x, x$m)
  } else {
    width <- max(nchar(p_x), nchar(p_m)) + 1L
    info_y <- sprintf(paste0("%-", width, "s = %s"), "Y", x$y)
    if (p_x == 1L) info_x <- sprintf(paste0("%-", width, "s = %s"), "X", x$x)
    else {
      x_labels <- paste0("X", seq_len(p_x))
      info_x <- sprintf(paste0("%-", width, "s = %s"), x_labels, x$x)
    }
    if (p_m == 1L) info_m <- sprintf(paste0("%-", width, "s = %s"), "M", x$m)
    else {
      m_labels <- paste0("M", seq_len(p_m))
      info_m <- sprintf(paste0("%-", width, "s = %s"), m_labels, x$m)
    }
    info <- paste(c(info_y, info_x, info_m), collapse = "\n")
  }
  # add information on covariates, if any
  if (have_covariates) {
    covariates <- paste("Covariates:", paste(x$covariates, collapse = ", "))
    info <- paste(info, covariates, sep = "\n\n")
  }
  # add sample size
  info <- paste(info, sprintf("Sample size: %d", x$n), sep = "\n\n")
  # write text block
  spss.TextBlock("Information on Data", paste0(header, info))

  ## print summary of regression m ~ x + covariates
  if (p_m == 1L) print_SPSS(x$fit_mx, response = x$m)
  else {
    for (m in x$m) {
      print_SPSS(x$fit_mx[[m]], response = m)
    }
  }

  ## print summary of regression y ~ m + x + covariates
  print_SPSS(x$fit_ymx, response = x$y)

  ## print summary of total and direct effects of x on y
  plural <- if (p_x == 1L) "" else "s"
  # total effect
  total <- replace_dimnames(x$total)
  spsspivottable.Display(total,
                         title = sprintf("Total Effect%s of X on Y", plural),
                         outline = outline, hiderowdimlabel = FALSE)
  # direct effect
  direct <- replace_dimnames(x$direct)
  spsspivottable.Display(direct,
                         title = sprintf("Direct Effect%s of X on Y", plural),
                         outline = outline, hiderowdimlabel = FALSE)

  ## return NULL invisibly
  invisible()
}

# print results of a bootstrap test for indirect effect
print_SPSS.boot_test_mediation <- function(x, outline, ...) {
  # initializations
  p_x <- length(x$fit$x)
  m <- x$fit$m
  p_m <- length(m)
  simple <- p_m == 1L && p_x == 1L
  # extract indirect effect
  indirect <- cbind(Data = x$fit$indirect, Boot = x$indirect)
  if (simple) rownames(indirect) <- m
  else if (p_m > 1L) {
    # make labels for individual indirect effects prettier
    rn <- gsub("->", " -> ", rownames(indirect), fixed = TRUE)
    # in case of multiple independent variables, make labels for the
    # corresponding total indirect effects prettier
    if (p_x > 1L) {
      replace <- sapply(paste(x$fit$x, "Total", sep = "_"),
                        function(current_x) which(current_x == rn),
                        USE.NAMES = FALSE)
      rn[replace] <- paste(x$fit$x, "Total", sep = ": ")
    }
    # update labels for indirect effects
    rownames(indirect) <- rn
  }
  # extract confidence interval
  ci <- if (simple) t(x$ci) else x$ci
  colnames(ci) <- c("Lower Bound", "Upper Bound")
  # combine indirect effect and confidence interval
  indirect <- cbind(indirect, ci)
  # print table
  plural <- if (simple) "" else "s"
  spsspivottable.Display(indirect,
                         title = sprintf("Indirect effect%s of X on Y", plural),
                         outline = outline, hiderowdimlabel = FALSE)
  # print additional notes
  note_ci <- paste("Level of confidence:", format(100 * x$level), "%")
  note_boot <- sprintf("Number of bootstrap replicates: %d", x$R)
  notes <- paste(note_ci, note_boot, sep = "\n\n")
  spss.TextBlock("Analysis Notes", notes)
  # return NULL invisibly
  invisible()
}

# print summary of robust mediation analysis results
print_SPSS.summary_test_mediation <- function(x, ...) {
  # header for output
  spsspkg.StartProcedure("Robust Mediation Analysis")
  # subheader for total, direct and indirect effects
  outline <- "Total, Direct and Indirect Effects of X on Y"
  # print summary of mediation model fit
  print_SPSS(x$summary, outline = outline)
  # print summary of indirect effect
  print_SPSS(x$object, outline = outline)
  # if requested, print diagnostic plot
  p <- x$plot
  if (!is.null(p)) print_SPSS(p)
  # stop output
  spsspkg.EndProcedure()
  # return NULL invisibly
  invisible()
}

# print ggplot2 graphics (diagnostic plot)
print_SPSS.ggplot <- function(x, ...) {
  # make labels a little larger
  x <- x +
    theme(axis.title = element_text(size = 13),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 13))
  # print graphics
  if (have_MacOS()) {
    # Simply printing the graphics to the default graphics device does not
    # work properly under MacOS because SPSS requires X11 for this, which is
    # no longer included with MacOS.  Some users might have XQuartz installed,
    # but we can't rely on this.  As a workaround, a temporary file containing
    # the plot is created, which is then displayed in the SPSS output with
    # spssRGraphics.Submit().  According to the documentation, this function
    # only supports PNG, JPG and BMP files.
    file <- "tmp_weight_plot.png"
    x <- x +
      labs(title = "Diagnostic plot of regression weights") +
      theme(plot.title = element_text(face = "bold", size = 18),
            plot.title.position = "plot")
    png(filename = file, width = 540, height = 600)
    print(x)
    dev.off()
    spssRGraphics.Submit(file)
    tryCatch(file.remove(file), warning = function(w) NULL,
             error = function(e) NULL)
  } else {
    # print graphics as usual
    spssRGraphics.SetGraphicsLabel("Diagnostic plot of regression weights")
    print(x)
  }
  # return NULL invisibly
  invisible()
}


## utility functions

# replace R style column names of coefficient matrices with SPSS style names
replace_dimnames <- function(x) {
  # extract row names
  rn <- rownames(x)
  # replace '(Intercept)' with '(Constant)'
  rn <- gsub("(Intercept)", "(Constant)", rn, fixed = TRUE)
  # extract current colnames
  cn <- colnames(x)
  # replace 't value' and 'z value' with 't' and 'z', respectively
  cn <- gsub("t value", "t", cn, fixed = TRUE)
  cn <- gsub("z value", "z", cn, fixed = TRUE)
  # replace column name for p-value with 'Sig.'
  cn <- gsub("Pr(>|t|)", "Sig.", cn, fixed = TRUE)
  cn <- gsub("Pr(>|z|)", "Sig.", cn, fixed = TRUE)
  # replace column names and return object
  dimnames(x) <- list(rn, cn)
  x
}

# # check if a Windows machine is used
# have_windows <- function() {
#   sys_info <- Sys.info()  # may not be available on all platforms
#   if (is.null(sys_info)) .Platform$OS.type == "windows"
#   else sys_info["sysname"] == "Windows"
# }

# check if a Mac is used
have_MacOS <- function() {
  sys_info <- Sys.info()  # may not be available on all platforms
  if (is.null(sys_info)) {
    .Platform$OS.type == "unix" && grepl("^darwin", R.version$os)
  } else sys_info["sysname"] == "Darwin"
}
