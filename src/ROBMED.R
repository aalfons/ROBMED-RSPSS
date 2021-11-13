# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


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
                     islist = FALSE, var = "x"),
    spsspkg.Template(kwd = "M", subc = "", ktype = "existingvarlist",
                     islist = TRUE, var = "m"),
    spsspkg.Template(kwd = "COV", subc = "", ktype = "varname",
                     islist = TRUE, var = "covariates"),
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
  if ("HELP" %in% attr(args, "names")) writeLines("Help not yet available.")
  else result <- spsspkg.processcmd(oobj, args, "run_robmed")
}


## function to take objects parsed by SPSS and call function robmed() from the
## R package robmed

run_robmed <- function(y, x, m, covariates = NULL, conf = 95, boot = 5000,
                       efficiency = 85, maxiter = 10000, seed = NULL,
                       rng = "current", plot = 1) {

  # check if package robmed is available
  tryCatch(library("robmed"), error = function(e) {
    stop("The R package 'robmed' is required but could not be loaded.",
         call. = FALSE)
  })

  # get variables from active data set
  m <- unlist(m)
  covariates <- unlist(covariates)
  variables <- c(x, y, m, covariates)
  data <- spssdata.GetDataFromSPSS(variables, missingValueToNA = TRUE,
                                   factorMode = "labels")

  # translate options
  level <- conf / 100
  control <- reg_control(efficiency = efficiency / 100,
                         max_iterations = maxiter)
  switch_rng <- rng == "compatibility"
  rng_version <- "3.5.3"

  # translate plot options
  plot <- isTRUE(as.logical(plot))
  if (plot && packageVersion("robmed") < "0.8.0") {
    warning("Diagnostic plot not available in this version of the R package ",
            "'robmed'.\nPlease update to the latest version.", call. = FALSE)
  }

  # run function robmed()
  result <- tryCatch({
    if (switch_rng) RNGversion(rng_version)
    if (!is.null(seed)) set.seed(seed)
    robust_boot <- robmed(data, x = x, y = y, m = m, covariates = covariates,
                          R = boot, level = level, control = control)
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
  summary <- data.frame("Robust R" = sqrt(x$R2$R2), "Rob. R Square" = x$R2$R2,
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
  # print information on outliers (only available in summary from 0.7.0 onwards)
  if (packageVersion("robmed") >= "0.7.0") {
    outlier_info <- robmed:::get_outlier_info(x$outliers)
    if (is.null(outlier_info$indices)) msg <- outlier_info$msg
    else {
      indices_to_print <- paste(outlier_info$indices, collapse = ", ")
      msg <- paste0(outlier_info$msg, indices_to_print, "\n", sep = "")
    }
    spss.TextBlock("Robustness weights", msg, outline = outline)
  }
  # return NULL invisibly
  invisible()
}

# print summary of a mediation model fit
print_SPSS.summary_reg_fit_mediation <- function(x, outline, ...) {

  ## initializations
  p_m <- length(x$m)
  have_covariates <- length(x$covariates) > 0L

  ## information on variables
  if (p_m == 1L) {
    info <- sprintf("Y = %s\nX = %s\nM = %s", x$y, x$x, x$m)
  } else {
    width <- nchar(p_m) + 1L
    info_y <- sprintf(paste0("%-", width, "s = %s"), "Y", x$y)
    info_x <- sprintf(paste0("%-", width, "s = %s"), "X", x$x)
    m_labels <- paste0("M", seq_len(p_m))
    info_m <- sprintf(paste0("%-", width, "s = %s"), m_labels, x$m)
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
  spss.TextBlock("Information on Data", info)

  ## print summary of regression m ~ x + covariates
  if (p_m == 1L) {
    print_SPSS(x$fit_mx, response = x$m)
  } else {
    for (m in x$m) {
      print_SPSS(x$fit_mx[[m]], response = m)
    }
  }

  ## print summary of regression y ~ m + x + covariates
  print_SPSS(x$fit_ymx, response = x$y)

  ## print summary of total and direct effects of x on y
  have_old_version <- packageVersion("robmed") < "0.5.0"
  # total effect
  total <- if (have_old_version) x$c_prime else x$total
  total <- replace_dimnames(total)
  spsspivottable.Display(total, title = "Total Effect of X on Y",
                         outline = outline, hiderowdimlabel = FALSE)
  # direct effect
  direct <- if (have_old_version) x$c else x$direct
  direct <- replace_dimnames(direct)
  spsspivottable.Display(direct, title = "Direct Effect of X on Y",
                         outline = outline, hiderowdimlabel = FALSE)

  ## return NULL invisibly
  invisible()
}

# print results of a bootstrap test for indirect effect
print_SPSS.boot_test_mediation <- function(x, outline, ...) {
  # initializations
  m <- x$fit$m
  p_m <- length(m)
  plural <- if (p_m == 1L) "" else "s"
  title <- sprintf("\nIndirect effect%s of X on Y:\n", plural)
  # extract indirect effect
  a <- x$fit$a
  b <- x$fit$b
  ab <- a * b
  if (p_m > 1L) ab <- c(Total = sum(ab), ab)
  ab <- cbind(Data = ab, Boot = x$ab)
  if (p_m == 1L) rownames(ab) <- m
  # extract confidence interval
  ci <- if (p_m == 1L) t(x$ci) else x$ci
  colnames(ci) <- c("Lower Bound", "Upper Bound")
  # combine and print
  ab <- cbind(ab, ci)
  spsspivottable.Display(ab, title = title, outline = outline,
                         hiderowdimlabel = FALSE)
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
