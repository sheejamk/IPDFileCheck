#######################################################################
#' Function to find the keyword for generating random numbers the  distribution
#' @param text name of the probability distribution
#' @return the keyword that should be used in R for generating random numbers
#' @examples
#' find_keyword_rand_generation("gamma")
#' @export
#' @details
#' This function returns the keyword for generating random number using
#' a keyword provided that is generally used for prob distribution
#' (but R might require a different keyword)
find_keyword_rand_generation <- function(text) {
  # text can not be null or NA
  if (is.null(text)) {
    stop("text should not be null")
  }else{
    if (is.na(text)) stop("text should not be NA")
  }
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "LOGNORMAL" || text == "LOG NORMAL") {
    keyword <- "rlnorm"
  }
  if (text == "GAMMA") {
    keyword <- "rgamma"
  }
  if (text == "BINOMIAL") {
    keyword <- "rbinom"
  }
  if (text == "BETA") {
    keyword <- "rbeta"
  }
  if (text == "UNIFORM") {
    keyword <- "runif"
  }
  if (text == "GAUSSIAN" || text == "NORMAL") {
    keyword <- "rnorm"
  }
  if (text == "WEIBULL") {
    keyword <- "rweibull"
  }
  if (text == "EXPONENTIAL" || text == "EXPO") {
    keyword <- "rexp"
  }
  if (text == "POISSON") {
    keyword <- "rpois"
  }
  if (is.null(keyword)) {
    stop("Error - Didnt find a suitable probability distribution")
  }
  return(keyword)
}
#######################################################################
#' Function to find the parameters that determine the probability distribution
#' @param name_distri name of the probability distribution
#' @return the parameters that determine the distribution
#' @examples
#' find_required_parameter_combs("gamma")
#' @export
#' @details
#' For each of the probability distribution we require certain parameters
#' and this function provides that required list of parameters.
find_required_parameter_combs <- function(name_distri) {
  # name_distri can not be null or NA
  if (is.null(name_distri)) {
    stop("name of the distribution should not be null")
  }else{
    if (is.na(name_distri)) stop("name of the distribution should not be NA")
  }
  text <- trimws(toupper(name_distri))
  keyword <- NULL
  if (text == "LOGNORMAL" || text == "LOG NORMAL") {
    keyword <- list(c("meanlog", "sdlog"), c("mean", "sd"), c("mean", "sdlog"),
                    c("meanlog", "sd"))
  }
  if (text == "GAMMA") {
    keyword <- c("shape", "rate")
  }
  if (text == "BINOMIAL") {
    keyword <- c("size", "prob")
  }
  if (text == "BETA") {
    keyword <- c("shape1", "shape2")
  }
  if (text == "UNIFORM") {
    keyword <- c("min", "max")
  }
  if (text == "GAUSSIAN" || text == "NORMAL") {
    keyword <- c("mean", "sd")
  }
  if (text == "WEIBULL") {
    keyword <- c("shape", "scale")
  }
  if (text == "EXPONENTIAL" || text == "EXPO") {
    keyword <- c("rate")
  }
  if (text == "POISSON") {
    keyword <- c("lambda")
  }
  if (is.null(keyword)) {
    stop("Error -Didnt find a suitable probability distribution")
  }
  return(keyword)
}
#######################################################################
#' Function to return the two parameters from a given expression separated
#' by comma,
#' @param expr an expression
#' @return parameters in the expression expr
#' @examples
#' get_name_value_probdistrb_def("gamma(mean = 10, sd =1)")
#' @export
#' @details
#' It will return the parameters of the distribution separated by commas
#' and given in usual notation as brackets. It will identify those in between
#' first occurrence of "( "and last occurrence of ")"
#' and from the characters in between search for comma to indicate different
#' parameters then it will extract (from those extracted parameters separated
#' by commas) that on the left side of "equal" sign
#' get_name_value_probdistrb_def("gamma(mean = sqrt(2), b =17)") will be ok
#' but get_name_value_probdistrb_def("gamma(shape, scale")) and
#' get_name_value_probdistrb_def("gamma(shape =1 & scale =1")) will show error
get_name_value_probdistrb_def <- function(expr) {
  # expr can not be null or NA
  if (is.null(expr)) {
    stop("expression should not be null")
  }else{
    if (is.na(expr)) stop("expression should not be NA")
  }
  # Find the position of last bracket
  posi_last <- sort(unlist(stringr::str_locate_all(expr, "\\)")))
  if (length(posi_last) == 0)
    stop("Expecting the expression with open and closed brackets")
  last <- posi_last[length(posi_last)]
  # Find the position of first bracket
  posi_first <- sort(unlist(stringr::str_locate_all(expr, "\\(")))
  first <- posi_first[1]
  # Find the parameters separated by commas that determine the distribution
  res <- stringr::str_sub(expr, start = first + 1, end = last - 1)
  # Find the position of comma
  posi <- sort(unlist(stringr::str_locate_all(res, ",")))
  expr_found <- list()
  final <- length(posi) + 1
  i <- 1
  # Now get the expressions between those commas
  while (i <= final) {
    if (i == 1) {
      start <- 1
    } else {
      start <- posi[i - 1] + 1
    }
    if (i == final) {
      end <- nchar(res)
    } else {
      if (i == 1) {
        end <- posi[i + 1] - 1
      } else {
        end <- posi[i] - 1
      }
    }
    this_param <- trimws(stringr::str_sub(res, start = start, end = end))
    expr_found <- append(expr_found, this_param)
    i <- i + 2
  }

  final2 <- length(expr_found)
  param_found <- list()
  values_found <- list()
  for (i in 1:final2) {
    # Now get the position of  equal sign and get the parameter name
    posi <- unlist(stringr::str_locate_all(unlist(expr_found[i]), "="))
    if (length(posi) != 2 || posi[1] != posi[2]) {
      stop("Error - this expression has more than one equal sign or none found")
    }
    this_param <- trimws(stringr::str_sub(expr_found[i], start = 1,
                                          end = posi[1] - 1))
    param_found <- append(param_found, this_param)

    result <- tryCatch({
      eval(parse(text = expr_found[i]))
    }, error = function(e) {
      stop("Error - the parameter = value expression could not be evaluated")
    })
    this_value <- result
    # contains the values of the parameters between the commas, but provided as
    # name value pairs
    values_found <- append(values_found, this_value)
  }
  param_found <- unlist(param_found)
  values_found <- unlist(values_found)
  return(structure(list(params = param_found, values = values_found)))
}
#######################################################################
#' Function to find the keyword for regression methods
#' @param text regression method
#' @param additional_info additional information required
#' @return the keyword that should be used in R for regression analysis
#' @examples
#' find_keyword_regression_method("linear")
#' @export
#' @details
#' This function returns the keyword to use in regression methods.
#' For example linear regression requires lm in R
#' some regression methods require additional info and it has to be
#' provided
find_keyword_regression_method <- function(text, additional_info = NA) {
  # text can not be null or NA
  if (is.null(text)) {
    stop("text should not be null")
  }else{
    if (is.na(text)) stop("text should not be NA")
  }
  text <- trimws(toupper(text))
  additional_words <- trimws(toupper(additional_info))
  keyword <- NULL
  if (text == "LINEAR REGRESSION" | text == "LINEAR") {
    keyword <- "lm"
  }
  if (text == "LOGISTIC REGRESSION" | text == "LOGISTIC") {
    keyword <- "glm"
  }
  if (text == "MULTILEVEL MODELLING" | text == "MULTILEVEL") {
    keyword <- "lmer"
  }
  if (text == "SURVIVAL" | text == "SURVIVAL ANALYSIS") {
    if (is.na(additional_info)) {
      stop("Need to provide additional info for survival analysis -
           hazard model or KM")
    }
    km_match <- c("KAPLAN-MEIER", "KM", "FLEMING-HARRINGTON", "FH2", "FH")
    index <- unlist(lapply(km_match, match, additional_words))
    if (any(!is.na(index))) {
      keyword <- "survival::survfit"
    }
    cox_match <- c("COX-PROPORTIONAL-HAZARD", "COX PROPORTIONAL HAZARD",
                   "COX-PH", "COX PH", "COXPH")
    index <- unlist(lapply(cox_match, match, additional_words))
    if (any(!is.na(index))) {
      keyword <- "survival::coxph"
    }
    pa_match <- c("PARAMETRIC REGRESSION", "PARAMETRIC")
    index <- unlist(lapply(pa_match, match, additional_words))
    if (any(!is.na(index))) {
      keyword <- "flexsurv::flexsurvreg"
    }
  }
  if (is.null(keyword)) {
    stop("No corresponding regression methods found")
  }
  return(keyword)
}
#######################################################################
#' Function to check if the parameters are sufficient to define a distribution
#' and if not see if we can estimate the parameters from the given parameters,
#' e.g. for gamma distribution, shape and rate can be estimated from mean and sd
#' @param the_expr the expression that contain the definition
#' @param distr_key the keyword used to generate random numbers in stats package
#' @return the parameters required
#' @examples
#' check_estimate_required_params("gamma(mean = 10 ,sd = 1)", "gamma")
#' @export
#' @keywords internal
#' @details
#' if the distribution is gamma and the parameters are mean and sd
#' we can estimate shape and rate from mean and sd.
#' This function is not usually needed by the user
check_estimate_required_params <- function(the_expr, distr_key) {
  # distr_key can not be null or NA
  if (is.null(distr_key)) {
    stop("distr key should not be null")
  }else{
    if (is.na(distr_key)) stop("distr key should not be NA")
  }
  distr_key <- trimws(toupper(distr_key))
  # the_expr can not be null or NA
  if (is.null(the_expr)) {
    stop("expression should not be null")
  }else{
    if (is.na(the_expr)) stop("expression should not be NA")
  }
  names_values <- get_name_value_probdistrb_def(the_expr)
  params_found <- names_values$params
  values_found <- names_values$values
  reqd_params <- find_required_parameter_combs(distr_key)
  if (distr_key == "GAMMA") {
    if (sum(params_found == c("mean", "sd")) == 2) {
      this_shape <- (values_found[1] / values_found[2])^2
      this_rate <- values_found[1] / (values_found[2]^2)
      result <- structure(list(shape = this_shape, rate = this_rate))
      return(result)
    }else{
      stop("Error - distribution is gamma, but parameters are not mean and sd")
    }
  } else {
    print("For the distributions other than gamma,the code is not equipped to
          estimate the parameters")
    return(NA)
  }
}
#' #######################################################################
#' Function to get the expression, analyse it for what distribution it is,
#' then check if it has all parameters requires,
#' if not estimate the required parameters from given parameters,
#' and substitute it for actual expression that can
#' be used with R stats package
#' @param the_expr given parameters for generating ransom numbers
#' @return the keyword that should be used in R for generating random numbers
#' @examples
#' check_estimate_substitute_proper_params("gamma(mean = 10 ,sd=1)")
#' @export
#' @keywords internal
#' @details
#' Get the expression of probability distribution with parameters, get the
#' parameters that are found, then find the required params and if they are ok
#' if not try if they can be estimated. Then it will substitute for proper
#' parameters in the way R is expecting to perform
check_estimate_substitute_proper_params <- function(the_expr) {
  # the_expr can not be null or NA
  if (is.null(the_expr)) {
    stop("expression should not be null")
  }else{
    if (is.na(the_expr)) stop("expression should not be NA")
  }

  location <- stringr::str_locate(the_expr, "\\(")
  if (sum(!is.na(location) == 0))
    stop("Error - expression is expected to have open brackets")
  distr_key <- substr(the_expr, 1, location[1] - 1)
  rand_key <- find_keyword_rand_generation(distr_key)
  required_params <- find_required_parameter_combs(distr_key)
  params_found <- get_name_value_probdistrb_def(the_expr)$params
  index <- 0
  for (i in length(required_params)) {
    index <- match(params_found, unlist(required_params))
    if (sum(!is.na(index)) == length(index)) {
      i <- length(required_params)
    }
  }
  if (sum(is.na(index)) == length(index)) {
    required_params <- unlist(required_params)
    estim_params <- check_estimate_required_params(the_expr, distr_key)
    if (sum(is.na(estim_params)) != 0) {
      stop("Check parameters that define the probability distribution")
    }
    create_expr <- ""
    for (i in seq_len(length(required_params))) {
      create_expr <- paste(create_expr, " ", required_params[i], " = ",
                           estim_params[i], sep = "")
      if (i != length(required_params)) {
        create_expr <- paste(create_expr, ",", sep = "")
      }
    }
    the_real_expr <- paste(rand_key, "(1,", create_expr, ")", sep = "")
  } else {
    remaining <- stringr::str_split(the_expr, "\\(")[[1]][2]
    the_real_expr <- paste(rand_key, "(1, ", remaining, sep = "")
  }
  return(the_real_expr)
}
#######################################################################
#' Function to get extension of a file name
#' @param filename name of a file
#' @return the extension
#' @examples
#' get_extension_file("data.txt")
#' @export
#' @details
#' if there is no "." character returns error
#' else returns last characters those after string split using "."
get_extension_file <- function(filename) {
  ex <- strsplit(basename(filename), split = "\\.")[[1]]
  if (length(ex[-1]) == 0) {
    stop("The character '.'  was not found ")
  } else {
    return(ex[-1])
  }
}

#######################################################################
#' Function to load the file containing trial data and return it
#' @param file, name of the file in full
#' @param sheet name of the sheet if excel work book is given
#' @return trial data if success, else -1
#' @examples
#' load_trial_data(system.file("extdata", "trial_data.csv",
#'   package = "packDAMipd"
#' ))
#' @export
load_trial_data <- function(file = NULL, sheet = NULL) {
  # Load trial data from file input or stored in package
  if (!is.null(file)) {
    if (IPDFileCheck::test_file_exist_read(file) == 0) {
      if (get_extension_file(file) == "txt") {
        df_trial_data <- read.table(
          file = file, header = TRUE, sep = "\t", quote = "\"",
          dec = ",", fill = TRUE, na.strings = c(""), as.is = 1:4,
          stringsAsFactors = FALSE
        )
      }
      if (get_extension_file(file) == "csv") {
        df_trial_data <- read.csv(file = file, header = TRUE,
                                  stringsAsFactors = FALSE,
                                  fileEncoding = "latin1")
      }
      if (get_extension_file(file) == "dta") {
        df_trial_data <- haven::read_dta(file = file)
      }
      if (get_extension_file(file) == "xls" |
          get_extension_file(file) == "xlsx") {
        if (is.null(sheet))
          df_trial_data <- readxl::read_excel(file)
        else
          df_trial_data <- readxl::read_excel(file, sheet = sheet)
      }
      if (get_extension_file(file) == "RDS" |
            get_extension_file(file) == "Rds" |
            get_extension_file(file) == "rds") {
        df_trial_data <- readRDS(file)
      }
      if (get_extension_file(file) == "Rdata" |
          get_extension_file(file) == "rdata" |
          get_extension_file(file) == "RDATA" |
          get_extension_file(file) == "rda") {
        stop("Please use the data in the form rds, excel, txt, csv, or dta")
      }
    } else {
      stop("Error in reading given file")
    }
  } else {
    df_trial_data <- trial_data
  }
  return(df_trial_data)
}
#######################################################################
#' Make a string of covariates using addition sign
#' @param covariates covariates
#' @return expression of covariates
#' @keywords internal
#' @examples
#' make_string_covariates(c("open", "grade"))
#' @export
make_string_covariates <- function(covariates) {
  if (is.null(covariates)) stop("Error - covariates can not be NULL")
  covariates_list <- list()
  if (sum(is.na(covariates)) == 0) {
    for (i in seq_len(length(covariates))) {
      if (i == length(covariates)) {
        covariates_list <- paste(covariates_list, paste(covariates[i],
                                                        sep = ""))
      } else {
        covariates_list <- paste(covariates_list, paste(covariates[i], " +",
                                                        sep = ""))
      }
    }
  } else {
    covariates_list <- NA
  }
  covariates_list <- trimws(covariates_list)
  return(covariates_list)
}
#######################################################################
#' Form expression to use with glm()
#' @param param_to_be_estimated  parameter of interest
#' @param indep_var the independent variable (column name in data file)
#' @param family distribution name  eg. for logistic regression -binomial
#' @param covariates list of covariates
#' @param interaction boolean value to indicate interaction in the case of
#' generalised linear models, false by default
#' @param naaction action to be taken with the missing values
#' @param link link function if not the default for each family
#' @return the formula for glm
#' @examples
#' formula <- form_expression_glm("admit",
#'   indep_var = "gre", family = "binomial",
#'   covariates = c("gpa", "rank"), interaction = FALSE, naaction = "na.omit",
#'   link = NA)
#' @export
#' @details
#' Form expression for the method glm
form_expression_glm <- function(param_to_be_estimated, indep_var, family,
                                covariates, interaction, naaction, link) {
  check_list <- list(param_to_be_estimated, indep_var, interaction)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  if (is.na(family)) {
    stop("Error - information on distribution is missing")
  } else {
    this_dist <- find_glm_distribution(family)
  }
  if (!is.na(link)) {
    link <- check_link_glm(this_dist, link)
    family_def <- paste(this_dist, "(link = ", link, ")", sep = "")
  } else {
    family_def <- paste(this_dist, sep = "")
  }
  if (length(covariates) == 0 | sum(is.na(covariates)) == length(covariates)) {
    # no need to check for interaction
    fmla <- paste("glm(", param_to_be_estimated, " ~ ", indep_var, ", family = ", family_def, ", data = dataset, na.action =", naaction, ")", sep = "")
    short_fmla <- paste(" ~ ", indep_var, sep = "")
  } else {
    expre <- paste(covariates[1], sep = "")
    i <- 2
    while (i <= length(covariates)) {
      this <- paste(covariates[i], sep = "")
      if (interaction) {
        expre <- paste(expre, this, sep = " * ")
      } else {
        expre <- paste(expre, this, sep = " + ")
      }
      i <- i + 1
    }
    fmla <- paste("glm(", param_to_be_estimated, " ~ ", expre, " + ", indep_var, ", family = ", family_def, ", data = dataset, na.action = ", naaction, ")", sep = "")
    short_fmla <- paste(" ~ ", expre, " + ", indep_var, sep = "")
  }
  expressions <- list(formula = fmla, short_formula = short_fmla)
  return(expressions)
}
#######################################################################
#' Function to find the keyword for family of distribution in glm
#' @param text distribution
#' @return the keyword - the name of distribution
#' @examples
#' find_glm_distribution("gamma")
#' @export
#' @details
#' Find the family for glm method
find_glm_distribution <- function(text) {
  # checking if test is NULL or NA
  if (is.null(text)) {
    stop("Error - text can not be null")
  }else{
    if (is.na(text))
      stop("Error - text can not be null or NA")
  }
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "BINOMIAL" | text == "BI NOMIAL") {
    keyword <- "binomial"
  }
  if (text == "GAUSSIAN" | text == "NORMAL") {
    keyword <- "gaussian"
  }
  if (text == "GAMMA") {
    keyword <- "Gamma"
  }
  if (text == "INVERSE.GAUSSIAN" | text == "INVERSE GAUSSIAN" |
      text == "INVERSE_GAUSSIAN") {
    keyword <- "inverse.gaussian"
  }
  if (text == "POISSON") {
    keyword <- "poisson"
  }
  if (text == "QUASI") {
    keyword <- "quasi"
  }
  if (text == "QUASI BINOMIAL" | text == "QUASI_BINOMIAL") {
    keyword <- "quasibinomial"
  }
  if (text == "QUASI POISSON" | text == "QUASI_POISSON") {
    keyword <- "quasipoisson"
  }
  if (is.null(keyword)) {
    stop("Error - glm - family of distribution not found  ")
  }
  return(keyword)
}
#######################################################################
#' Function to find the keyword for family of distribution in glm
#' @param family family of distribution
#' @param link function to be used
#' @return the link if they can be accepted else error
#' @examples
#' check_link_glm("gaussian", "identity")
#' @export
#' @details
#' Check and get the link function for the method glm
check_link_glm <- function(family, link) {
  #testing for null or NA
  if (is.null(family)) {
    stop("Error - Family can not be null ")
  }
  if (is.na(family)) {
    stop("Error - Family can not be NA ")
  }
  if (is.null(link)) {
    stop("Error - link can not be null")
  }
  if (is.na(link)) {
    stop("Error - link can not be NA")
  }
  family <- tolower(trimws(toupper(family)))
  link <- tolower(trimws(toupper(link)))
  link_accept = NULL
  if (family == "gaussian") {
    link_accept <- c("identity", "log", "inverse")
  }
  if (family == "binomial") {
    link_accept <- c("logit", "probit", "cauchit", "log", "loglog")
  }
  if (family == "gamma") {
    link_accept <- c("inverse", "identity", "log")
  }
  if (family == "poisson") {
    link_accept <- c("sqrt", "identity", "log")
  }
  if (family == "inverse.gaussian") {
    link_accept <- c("1/mu^2", "identity", "log", "inverse")
  }
  if (family == "quasi") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse",
                     "log", "1/mu^2", "sqrt")
  }
  if (family == "quasibinomial") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse",
                     "log", "1/mu^2", "sqrt")
  }
  if (family == "quasipoisson") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse",
                     "log", "1/mu^2", "sqrt")
  }
  if (is.null(link_accept)) {
    stop("Error - link is not found")
  }
  matching <- match(link, link_accept)
  #if not matching throw error or return the link
  if (is.na(matching)) {
    stop(paste("Error - link given- ", link, " can not be accepted by family-",
               family, sep = ""))
  } else {
    return(link)
  }
}
#######################################################################
#' Do the diagnostic test for glm model assumption
#' @param method param describing the methods, expects glm
#' @param expression_recreated the expression recreated for calling lm
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before
#' passing
#' @param interaction boolean value to indicate interaction in the case
#' of linear regression, false by default
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
#' mydata <- read.csv(datafile)
#' results_logit <- use_generalised_linear_model("admit",dataset = mydata,
#' indep_var = "gre", family = "binomial", covariates = NA,
#' interaction = FALSE,naaction = "na.omit", link = NA)
#' do_diagnostic_glm("glm", results_logit$fit, results_logit$fit$call,
#' "admit",mydata, "gre", covariates = NA, interaction = FALSE)
#' }
#' @importFrom MASS studres
#' @importFrom lmtest dwtest
#' @export
#' @keywords internal
do_diagnostic_glm <- function(method = "glm", fit, expression_recreated,
                              param_to_be_estimated,
                              dataset, indep_var, covariates, interaction) {

  check_list <- list(param_to_be_estimated, indep_var, interaction,
                     expression_recreated)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  if (is.null(method)) {
    stop("Error - method should not be NULL")
  }else{
    if (method != "glm") stop("Error - method should be glm")
  }
  if (!("glm" %in% class(fit)))
    stop("Error- Fit object should be of type glm")

  # checking if dataset is is NULL
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  # Fit R2, AIC and BIC
  R2 <- 1 - (fit$deviance / fit$null.deviance)
  AIC <- stats::AIC(fit)
  BIC <- stats::BIC(fit)
  fit_diagnostics <- data.frame(cbind(R2, AIC, BIC))
  colnames(fit_diagnostics) <- c("R2", "AIC", "BIC")

  #  auto correlation test
  autocorr_error_test <- lmtest::dwtest(fit)

  #  outlier test
  outlier_test <- car::outlierTest(fit)

  # Anova table
  anova_table <- stats::anova(fit) # anova table

  # influence fit
  influence_fit <- stats::influence(fit) # regression diagnostics

  # Evaluate homoscedasticity
  # non-constant error variance test
  # No need to check this in glm
  # Ref:https://online.stat.psu.edu/stat504/node/216/)

  name_file_plot <- paste0(method, "_Regression_diagnostics_plot_",
                           param_to_be_estimated, "_", indep_var, ".pdf",
                           sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(2, 2))

  # plot studentized residuals vs. fitted values
  suppressWarnings(car::spreadLevelPlot(fit))

  # distribution of studentized residuals
  sresid <- MASS::studres(fit)
  graphics::hist(sresid, freq = FALSE,
                 main = "Distribution of Studentized Residuals")
  xfit <- seq(min(sresid), max(sresid), length = 40)
  yfit <- stats::dnorm(xfit)
  graphics::lines(xfit, yfit)
  # Evaluate Nonlinearity component + residual plot
  if (!interaction) {
    car::crPlots(fit)
  }
  car::influencePlot(fit, main = "Influence Plot",
                     sub = "Circle size is proportial to Cook's Distance")
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()

  name_file_plot <- paste0(method, "_Residuals_", param_to_be_estimated, "_",
                           indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_diagnostics <- graphics::plot(fit)
  grDevices::dev.off()
  # wald_test_results <- c()
  # for (i in 1:length(coef(fit))) {
  #   this_res <- aod::wald.test(b = coef(fit), Sigma = vcov(fit), Terms = coef(fit)[i])
  #   wald_test_results <- append(wald_test_results, this_res)
  # }

  results <- list(
    autocorr_error_test = autocorr_error_test,
    outlier_test = outlier_test,
    anova_table = anova_table,
    influence_fit = influence_fit,
    fit_diagnostics = fit_diagnostics,
    plot_diagnostics = plot_diagnostics
  )
  return(results)
}

#######################################################################
#' Function to find the keyword for survreg distribution
#' @param text distribution
#' @return the keyword - the name of distribution
#' @examples
#' find_survreg_distribution("weibull")
#' @export
#' @details
#' For surveg method, find the distribution
find_survreg_distribution <- function(text) {
  if (is.null(text)) {
    stop("Error - text can not be null")
  }else{
    if (is.na(text))
      stop("Error - text can not be null or NA")
  }
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "EXPONENTIAL" | text == "EXPO") {
    keyword <- "exponential"
  }
  if (text == "WEIBULL") {
    keyword <- "weibull"
  }
  if (text == "GAUSSIAN") {
    keyword <- "gaussian"
  }
  if (text == "LOGGAUSSIAN" | text == "LOG GAUSSIAN") {
    keyword <- "loggaussian"
  }
  if (text == "RAYLEIGH") {
    keyword <- "rayleigh"
  }
  if (text == "LOGISTIC") {
    keyword <- "logistic"
  }
  if (text == "LOGNORMAL" | text == "LOG NORMAL") {
    keyword <- "lognormal"
  }
  if (text == "LOG LOGISTIC" | text == "LOGLOGISTIC") {
    keyword <- "loglogistic"
  }
  if (is.null(keyword)) {
    stop("Error - Survreg - family of distribution not matching  ")
  }
  return(keyword)
}
#######################################################################
#' Form expression to use with lm()
#' @param param_to_be_estimated  parameter of interest
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates
#' @param interaction boolean value to indicate interaction
#' in the case of linear regression, false by default
#' @return the formula for lm
#' @examples
#' formula <- form_expression_lm("gre", indep_var = "gpa", covariates = NA,
#' interaction = FALSE)
#' @export
#' @details
#' This function helps to create the expression for liner regression model
#' it takes care of covariates and interaction
form_expression_lm <- function(param_to_be_estimated, indep_var, covariates,
                               interaction) {
  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  if (length(covariates) == 0 | sum(is.na(covariates)) == length(covariates)) {
    # no need to check for interaction
    fmla <- paste("lm(", param_to_be_estimated, " ~ ", indep_var, ",
                  data = dataset )", sep = "")
    short_fmla <- paste(" ~ ", indep_var, sep = "")
  } else {
    expre <- paste(covariates[1], sep = "")
    i <- 2
    while (i <= length(covariates)) {
      this <- paste(covariates[i], sep = "")
      if (interaction) {
        expre <- paste(expre, this, sep = " * ")
      } else {
        expre <- paste(expre, this, sep = " + ")
      }
      i <- i + 1
    }
    fmla <- paste("lm(", param_to_be_estimated, " ~ ", expre, " + ",
                  indep_var, ", data = dataset )", sep = "")
    short_fmla <- paste(" ~ ", expre, " + ", indep_var, sep = "")
  }
  expressions <- list(formula = fmla, short_formula = short_fmla)
  return(expressions)
}

#######################################################################
#' Do the diagnostic test for lm model assumption
#' @param method param describing the methods, expects lm
#' @param expression_recreated the expression recreated for calling lm
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before
#' passing
#' @param interaction boolean value to indicate interaction in the case of
#' linear regression, false by default
#' @return the results of the regression analysis
#' @keywords internal
#' @examples
#'\donttest{
#' datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
#' mydata <- read.csv(datafile)
#' results_logit <- use_linear_regression("admit",dataset = mydata,
#' indep_var = "gre",covariates = NA, interaction = FALSE)
#' do_diagnostic_linear_regression("lm", results_logit$fit,
#' results_logit$fit$call,
#' "admit", mydata, "gre", covariates = NA , interaction= FALSE)
#' }
#' @importFrom gvlma gvlma
#' @export
do_diagnostic_linear_regression <- function(method, fit, expression_recreated,
                                            param_to_be_estimated,
                                            dataset, indep_var, covariates,
                                            interaction) {
  if (is.null(method)) {
    stop("Error - method should not be NULL")
  }else{
    if (method != "lm") stop("Error - method should be lm")
  }
  if (is.null(expression_recreated))
    stop("Error - expression recreated should not be NULL")

  if (!("lm" %in% class(fit)))
    stop("Error- Fit object should be of type lm")

  check_list <- list(param_to_be_estimated, indep_var,
                    interaction)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")


  # checking if dataset is is NULL
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  # Fit R2, AIC and BIC
  Adj_R2 <- summary(fit)$adj.r.squared
  AIC <- stats::AIC(fit)
  BIC <- stats::BIC(fit)
  fit_diagnostics <- data.frame(cbind(Adj_R2, AIC, BIC))
  colnames(fit_diagnostics) <- c("Adj_R2", "AIC", "BIC")

  #  autocorrelation test
  autocorr_error_test <- lmtest::dwtest(fit)

  #  outlier test
  outlier_test <- car::outlierTest(fit)

  # Anova table
  anova_table <- stats::anova(fit) # anova table

  # influence fit
  influence_fit <- stats::influence(fit) # regression diagnostics

  # Evaluate homoscedasticity
  # non-constant error variance test
  non_constant_error_test <- car::ncvTest(fit)

  # Model fit assumption test
  model_fit_assumptions_test <- summary(gvlma::gvlma(fit))


  name_file_plot <- paste0(method, "_Regression_diagnostics_plot_",
                           param_to_be_estimated, "_", indep_var, ".pdf",
                           sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(2, 2))

  # plot studentized residuals vs. fitted values
  suppressWarnings(car::spreadLevelPlot(fit))

  # distribution of studentized residuals
  sresid <- MASS::studres(fit)
  graphics::hist(sresid, freq = FALSE,
                 main = "Distribution of Studentized Residuals")
  xfit <- seq(min(sresid), max(sresid), length = 40)
  yfit <- stats::dnorm(xfit)
  graphics::lines(xfit, yfit)

  # Evaluate Nonlinearity component + residual plot
  if (interaction == FALSE) {
    car::crPlots(fit)
  }
  car::influencePlot(fit, main = "Influence Plot",
                     sub = "Circle size is proportial to Cook's Distance")
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()


  name_file_plot <- paste0(method, "_Residuals_", param_to_be_estimated, "_",
                           indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_diagnostics <- ggplot2::autoplot(fit, toPdf = TRUE,
                                        file = name_file_plot)
  grDevices::dev.off()

  results <- list(
    autocorr_error_test = autocorr_error_test,
    outlier_test = outlier_test,
    anova_table = anova_table,
    non_constant_error_test = non_constant_error_test,
    influence_fit = influence_fit,
    model_fit_assumptions_test = model_fit_assumptions_test,
    fit_diagnostics = fit_diagnostics,
    plot_diagnostics = plot_diagnostics
  )
  return(results)
}

#######################################################################
#' Do the prediction for regression
#' @param method param describing the methods, expects lm
#' @param expression_recreated the expression recreated for calling lm
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before
#' passing
#' @param interaction boolean value to indicate interaction in the case of
#' linear regression, false by default
#' @return the results of the regression analysis
#' @keywords internal
#' @examples
#' \donttest{
#' datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
#' mydata <- read.csv(datafile)
#' results_logit <- use_linear_regression("admit", dataset = mydata,
#' indep_var = "gre",covariates = NA,interaction = FALSE)
#' predict = prediction_regression("lm",results_logit$fit,
#' results_logit$fit$call, "admit",covariates = NA,"gre", FALSE )
#'}
#' @importFrom effects predictorEffects
#' @export
prediction_regression <- function(method, fit, expression_recreated,
                                  param_to_be_estimated, indep_var,
                                  covariates, interaction) {

  if (is.null(method)) {
    stop("Error - method should not be NULL")
  }else{
    if (method != "lm" & method != "glm")
      stop("Error - method should be lm or glm")
  }
  if (!("lm" %in% class(fit))  & !("glm" %in% class(fit)))
    stop("Error- Fit object should be of type lm or glm")
  # checking if expression_created is NULL
  if (is.null(expression_recreated))
    stop("Error - expression recreated is NULL")
  check_list <- list(param_to_be_estimated, indep_var, interaction)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  predictor_effect <- effects::predictorEffects(fit)
  leng <- length(predictor_effect)
  prediction_all <- c()
  this_names <- c()
  for (i in 1:leng) {
    this <- (predictor_effect[[i]]$model.matrix[, ])
    prediction_all <- append(prediction_all, list(this))
    if (i == leng) {
      this_names <- append(this_names, paste(indep_var, sep = ""))
    } else {
      this_names <- append(this_names, paste(covariates[i], sep = ""))
    }
  }
  prediction_all <- append(prediction_all, list(predictor_effect[[1]]$fit))
  names(prediction_all) <- c(this_names, param_to_be_estimated)
  name_file_plot <- paste0(method, "_Prediction_", param_to_be_estimated, "_",
                           indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_prediction <- graphics::plot(predictor_effect, lines = list(multiline =
                                                                     TRUE))
  grDevices::dev.off()
  results <- list(
    plot_prediction = plot_prediction,
    prediction_all = prediction_all
  )
}
#######################################################################
#' help function to keep slope and intercept portion ready in mixed model
#' expression
#' @param expression expression created so far
#' @param random_intercept_vars, names of variables for random intercept
#' @param intercept_vars_pairs, those of the random intercept variables
#' with nested effect
#' @param random_slope_intercept_pairs, random slopes intercept pairs
#' this is a list of paired variables
#' @param uncorrel_slope_intercept_pairs, variables with correlated intercepts
#' @return expression expression created
#' @export
get_slope_intercept_cross <- function(expression, random_intercept_vars,
                                intercept_vars_pairs,
                                random_slope_intercept_pairs,
                                uncorrel_slope_intercept_pairs) {
  # checking if parameter to be estimated is NULL or NA
  check_list <- list(expression, random_intercept_vars, intercept_vars_pairs)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Random effect /expression/ intercept variables
                             can not be null or na")
  all_interact_vars <- " "
  char <- "+ "
      len_ran_eff <- length(random_intercept_vars)
      len_inte_pairs <- length(unlist(intercept_vars_pairs))
      if (len_ran_eff > len_inte_pairs) {
        mem_checks <- !(random_intercept_vars %in%
                          unlist(intercept_vars_pairs))
        notin_intercept <- random_intercept_vars[mem_checks]
        m <- 1
        for (m in seq_len(length(notin_intercept))) {
          this <- paste("(", 1, "|", notin_intercept[m], ")", char)
          m <- m + 1
          all_interact_vars <- paste(all_interact_vars, this)
        }
      }
      consists <- sum(unlist(intercept_vars_pairs) %in% random_intercept_vars)
      len_all_cross <- length(unlist(intercept_vars_pairs))
      if (consists != len_all_cross) stop("Cross intercept pairs should be
                                          members of the random intercept
                                          variables")
      if (!is.null(random_slope_intercept_pairs)) {
        intercepts <- unlist(intercept_vars_pairs)
        slope_intercepts <- unlist(random_slope_intercept_pairs)
        mem_checks <- !(intercepts %in% slope_intercepts)
        notin_intercept <- intercepts[mem_checks]
        m <- 1
        for (m in seq_len(length(notin_intercept))) {
          this <- paste("(", 1, "|", notin_intercept[m], ")", char)
          m <- m + 1
          all_interact_vars <- paste(all_interact_vars, this)
        }
      }

      if (!is.null(random_slope_intercept_pairs)) {
        j <- 1
        while (j <= length(random_slope_intercept_pairs)) {
          this_pair <- random_slope_intercept_pairs[[j]]
          if (!is.null(uncorrel_slope_intercept_pairs)) {
              index_uncorrel <- match(this_pair,
                                      unlist(uncorrel_slope_intercept_pairs))
          } else {
              index_uncorrel <- NA
          }
          if (length(index_uncorrel) != 0 & sum(is.na(index_uncorrel)) > 0) {
            uncorrel_ind <- "1 + "
          } else {
            uncorrel_ind <- "0 + "
          }

          slope <- random_slope_intercept_pairs[[j]][1]
          intercept <-  random_slope_intercept_pairs[[j]][2]
          if (j == length(random_slope_intercept_pairs)) {
               this <- paste("(", uncorrel_ind, slope, "|", intercept, ") ")
          } else {
              this <- paste("(", uncorrel_ind, slope, "|",
                             intercept, ")", char)
          }
          all_interact_vars <- paste(all_interact_vars, this)
          j <- j + 1
        }
      } else {
        i <- 1
        no_intercepts <- length(unlist(intercept_vars_pairs))
        while (i <= no_intercepts) {
            intercept <-  unlist(intercept_vars_pairs)[i]
            if (i == length(unlist(intercept_vars_pairs))) {
              this <- paste("(", 1, "|", intercept, ") ")
            } else {
              this <- paste("(", 1, "|",
                            intercept, ")", char)
            }
            i <- i + 1
            all_interact_vars <- paste(all_interact_vars, this)
        }
      }
      expression <- paste(expression, all_interact_vars, sep = "")
  return(expression)
}
#######################################################################
#' help function to keep slope and intercept portion ready in mixed model
#'  expression
#' @param expression expression created so far
#' @param random_intercept_vars, names of variables for random intercept
#' @param random_slope_intercept_pairs, random slopes intercept pairs
#' this is a list of paired variables
#' @param uncorrel_slope_intercept_pairs, variables with correlated intercepts
#' @return expression expression created
#' @export
get_slope_intercept <- function(expression, random_intercept_vars,
                                      random_slope_intercept_pairs,
                                      uncorrel_slope_intercept_pairs) {
  # checking if parameter to be estimated is NULL or NA
  check_list <- list(expression, random_intercept_vars)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Random effect / expression can not be
                             NULL or NA")
  all_interact_vars <- " "
  char <- "+ "
  if (!is.null(random_slope_intercept_pairs)) {
      intercepts <- unlist(random_intercept_vars)
      slope_intercepts <- unlist(random_slope_intercept_pairs)
      mem_checks <- !(intercepts %in% slope_intercepts)
      notin_intercept <- intercepts[mem_checks]
      m <- 1
      for (m in seq_len(length(notin_intercept))) {
        this <- paste("(", 1, "|", notin_intercept[m], ")", char)
        m <- m + 1
        all_interact_vars <- paste(all_interact_vars, this)
      }
      j <- 1
      while (j <= length(random_slope_intercept_pairs)) {
        this_pair <- random_slope_intercept_pairs[[j]]
        if (!is.null(uncorrel_slope_intercept_pairs)) {
          index_uncorrel <- match(this_pair,
                                  unlist(uncorrel_slope_intercept_pairs))
        } else {
          index_uncorrel <- NA
        }
        if (length(index_uncorrel) != 0 & sum(is.na(index_uncorrel)) > 0) {
          uncorrel_ind <- "1 + "
        } else {
          uncorrel_ind <- "0 + "
        }
        slope <- random_slope_intercept_pairs[[j]][1]
        intercept <-  random_slope_intercept_pairs[[j]][2]
        if (j == length(random_slope_intercept_pairs)) {
          this <- paste("(", uncorrel_ind, slope, "|", intercept, ") ")
        } else {
          this <- paste("(", uncorrel_ind, slope, "|",
                        intercept, ")", char)
        }
        all_interact_vars <- paste(all_interact_vars, this)
        j <- j + 1
      }
      expression <- paste(expression, all_interact_vars, sep = "")
  } else {
    m <- 1
    while (m <= length(random_intercept_vars)) {
      if (m == length(random_intercept_vars)) {
        this <- paste("(", 1, "|", random_intercept_vars[m], ") ")
      } else {
        this <- paste("(", 1, "|", random_intercept_vars[m], ")", char)
      }
      m <- m + 1
      all_interact_vars <- paste(all_interact_vars, this)
    }
    expression <- paste(expression, all_interact_vars, sep = "")
  }
  return(expression)
}
#######################################################################
#' help function to keep slope and intercept portion ready in mixed
#' model expression
#' @param expression expression created so far
#' @param random_intercept_vars, names of variables for random intercept
#' @param intercept_vars_pairs, those of the random intercept variables
#' with nested effect
#' @param random_slope_intercept_pairs, random slopes intercept pairs
#' this is a list of paired variables
#' @param uncorrel_slope_intercept_pairs, variables with correlated intercepts
#' @return expression expression created
#' @export
get_slope_intercept_nested <- function(expression, random_intercept_vars,
                                      intercept_vars_pairs,
                                      random_slope_intercept_pairs,
                                      uncorrel_slope_intercept_pairs) {
  # checking if parameter to be estimated is NULL or NA
  check_list <- list(expression, random_intercept_vars, intercept_vars_pairs)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Random effect /expression/ intercept variables
                             can not be null or na")
  all_interact_vars <- " "
  char <- "+ "
  if (!is.null(intercept_vars_pairs)) {
    len_ran_eff <- length(random_intercept_vars)
    len_inte_pairs <- length(unlist(intercept_vars_pairs))
    if (len_ran_eff > len_inte_pairs) {
      mem_checks <- !(random_intercept_vars %in% unlist(intercept_vars_pairs))
      notin_intercept <- random_intercept_vars[mem_checks]
      m <- 1
      for (m in seq_len(length(notin_intercept))) {
        this <- paste("(", 1, "|", notin_intercept[m], ")", char)
        m <- m + 1
        all_interact_vars <- paste(all_interact_vars, this)
      }
    }
    consists <- sum(unlist(intercept_vars_pairs) %in% random_intercept_vars)
    len_all_cross <- length(unlist(intercept_vars_pairs))
    if (consists != len_all_cross) stop("Nested intercept pairs should be
                                  members of the random intercept variables")

    if (!is.null(random_slope_intercept_pairs)) {
      len_ran_eff <- length((random_slope_intercept_pairs))
      len_inte_pairs <- length((intercept_vars_pairs))
      for (i in 1:len_ran_eff) {
        member <- FALSE
        current_intercept <- random_slope_intercept_pairs[[i]][2]
        j <- 1
        while (j <= len_inte_pairs) {
          interceptpairs <- intercept_vars_pairs[[j]]
          member <- current_intercept %in% interceptpairs
          if (member) {
            j <- len_inte_pairs + 1
          }
          j <- j + 1
        }
        if (!member) {
          stop("intercept in slope intercept pairs should be a member of
                 intercept variables")
        }
      }
      inte_list <- unlist(intercept_vars_pairs)
      slope_int_list <- unlist(random_slope_intercept_pairs)
      # index of  intercept from slop-intercept pair in intercept pair list
      index <- which(slope_int_list %in% inte_list)
      # get the slope that connected with the previous intercept
      slopes_with_same_int_pairs <- slope_int_list[index - 1]

      # pick those intercepts that are not in slope intercepts list
      # get the index intercept that is connected with a slope from intercept
      # list.
      intercept_index <- which(inte_list %in% slope_int_list)
      len_intercepts <- length(intercept_index)
      # get the index that is paired with the slope
      pair_index <- rep(0, len_intercepts)
      for (kk in seq_len(len_intercepts)) {
        if (intercept_index[kk] %% 2 == 0)
          pair_index[kk] <- intercept_index[kk] - 1
        if (intercept_index[kk] %% 2 == 1)
          pair_index[kk] <- intercept_index[kk] + 1
      }
      # now pick all the other intercepts except this intercept and its pair
      temp1 <- inte_list[intercept_index]
      temp2 <- inte_list[sort(pair_index)]
      same_check  <- sum(temp1 == temp2)
      if (same_check == length(pair_index)) {
        other_int_pairs <- inte_list[-c(intercept_index)]
      } else {
        other_int_pairs <- inte_list[-c(intercept_index, pair_index)]
      }
       m <- 1
       while (m <= length(other_int_pairs)) {
        intercept1 <- other_int_pairs[m]
        intercept2 <- other_int_pairs[m + 1]
        if (is.na(intercept1) | is.na(intercept2))
          stop("Error - intercepts should be in pairs")
        this <- paste("(", 1, "|", intercept2,
                        "/", intercept1, ")", char)
        m <- m + 2
        all_interact_vars <- paste(all_interact_vars, this)
      }
      for (i in seq_len(length(slopes_with_same_int_pairs))) {
        slope <- slopes_with_same_int_pairs[i]
        slope_index <- which(slope_int_list == slope)
        this_pair <- c(slope_int_list[slope_index],
                       slope_int_list[slope_index + 1])
        if (!is.null(uncorrel_slope_intercept_pairs)) {
          index_uncorrel <- match(this_pair,
                                  unlist(uncorrel_slope_intercept_pairs))
        } else {
          index_uncorrel <- NA
        }
        if (length(index_uncorrel) != 0 & sum(is.na(index_uncorrel)) > 0) {
          uncorrel_ind <- "1 + "
        } else {
          uncorrel_ind <- "0 + "
        }
        intercept1 <- slope_int_list[slope_index + 1]
        pair_index <- which(inte_list == intercept1)
        if (pair_index %% 2 == 0) {
          intercept2 <- intercept1
          intercept1 <-  inte_list[pair_index - 1]
        }
        if (pair_index %% 2 == 1) intercept2 <-  inte_list[pair_index + 1]

        if (i == length(slopes_with_same_int_pairs)) {
          this <- paste("(", uncorrel_ind, slope, "|", intercept2,
                        "/", intercept1, ")")
        } else {
          this <- paste("(", uncorrel_ind, slope, "|", intercept2,
                        "/", intercept1, ")", char)
        }
        all_interact_vars <- paste(all_interact_vars, this)
      }
    } else {
      i <- 1
      no_intercepts <- length(unlist(intercept_vars_pairs))
      while (i <= no_intercepts) {
        intercept1 <-  unlist(intercept_vars_pairs)[i]
        intercept2 <-  unlist(intercept_vars_pairs)[i + 1]
        if (is.na(intercept1) | is.na(intercept2))
          stop("Error: intercepts should be in pairs")
        if (i == no_intercepts - 1) {
            this <- paste("(", 1, "|", intercept2, "/", intercept1, ") ")
        } else {
            this <- paste("(", 1, "|", intercept2, "/",
                          intercept1, ")", char)
        }
        i <- i + 2
        all_interact_vars <- paste(all_interact_vars, this)
      }
    }
    expression <- paste(expression, all_interact_vars, sep = "")
  }
  return(expression)
}
#######################################################################
#' Form expression to use with mixed models
#' @param param_to_be_estimated column name of dependent variable
#' @param dataset a dataframe
#' @param fix_eff names of variables as fixed effect predictors
#' @param fix_eff_interact_vars, if interaction -true
#' @param random_intercept_vars, names of variables for random intercept
#' @param nested_intercept_vars_pairs, those of the random intercept variables
#' with nested effect
#' @param cross_intercept_vars_pairs, those of the random intercept variables
#'  with crossed effect
#' @param uncorrel_slope_intercept_pairs, variables with correlated intercepts
#' @param random_slope_intercept_pairs, random slopes intercept pairs -
#'  this is a list of paired variables
#' @param family, family of distribution for non gaussian distribution of
#' predicted variable
#' @param link, link function for the variance
#' @return result regression result with plot if success and -1, if failure
#' @examples
#' \donttest{
#' datafile <- system.file("extdata", "data_linear_mixed_model.csv",
#' package = "packDAMipd")
#' dt = utils::read.csv(datafile, header = TRUE)
#' formula <- form_expression_mixed_model_lme4("extro",
#'   dataset = dt,
#'   fix_eff = c("open", "agree", "social"),
#'   fix_eff_interact_vars = NULL,
#'   random_intercept_vars = c("school", "class"),
#'   nested_intercept_vars_pairs = list(c("school", "class")),
#'   cross_intercept_vars_pairs = NULL,
#'   uncorrel_slope_intercept_pairs = NULL,
#'   random_slope_intercept_pairs = NULL, family = "binomial", link = NA
#' )
#' }
#' @export
#' @details
#' Form the expression for mixed model
form_expression_mixed_model_lme4 <- function(param_to_be_estimated, dataset,
                                        fix_eff,
                                        fix_eff_interact_vars,
                                        random_intercept_vars,
                                        nested_intercept_vars_pairs,
                                        cross_intercept_vars_pairs,
                                        uncorrel_slope_intercept_pairs,
                                        random_slope_intercept_pairs,
                                        family, link) {

  # checking if parameter to be estimated is NULL or NA
  check_list <- list(param_to_be_estimated, random_intercept_vars)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - param to be estimated or random intercepts is NULL or NA")
  # checking if dataset is is NULL
  if (is.null(dataset)) stop("Error - dataset can not be null")
  if (is.null(family)) stop("Error - family can not be null")
  if (!is.na(family)) {
    this_dist <- find_glm_distribution(family)
    if (!is.na(link)) {
      link <- check_link_glm(this_dist, link)
      family_def <- paste(this_dist, "(link = ", link, ")", sep = "")
    } else {
      family_def <- paste(this_dist, sep = "")
    }
  }
  if (!is.null(nested_intercept_vars_pairs) &
      !is.null(cross_intercept_vars_pairs)) {
      equ <- sum(unlist(nested_intercept_vars_pairs)
                 == unlist(cross_intercept_vars_pairs))
      diff <- min(length(unlist(nested_intercept_vars_pairs)),
                 length(unlist(cross_intercept_vars_pairs)))
      if (equ == diff)
         stop("Random intercepts should not be in both nested or crossed")
  }
  if (!is.null(nested_intercept_vars_pairs)) {
      cross <- FALSE
      len <- length(nested_intercept_vars_pairs)
      for (i in 1:len) {
        if (length(nested_intercept_vars_pairs[[i]]) != 2)
          stop("Nested intercepts have to be given as a list of pairs")
      }
      intercept_vars_pairs <- nested_intercept_vars_pairs
    }
  if (!is.null(cross_intercept_vars_pairs)) {
      cross <- TRUE
      len <- length(cross_intercept_vars_pairs)
      for (i in 1:len) {
        if (length(cross_intercept_vars_pairs[[i]]) != 2)
          stop("Cross intercepts have to be given as a list of pairs")
      }
      intercept_vars_pairs <- cross_intercept_vars_pairs
    }
  if (is.null(cross_intercept_vars_pairs) &
      is.null(nested_intercept_vars_pairs)) {
      cross <- NA
  }
  expression <- paste(param_to_be_estimated, "~")
  if (is.null(fix_eff)) {
    fix_eff <- 1
    expression <- paste(expression, fix_eff, "+")
  } else {
    no_interaction_var <- setdiff(fix_eff, fix_eff_interact_vars)
    i <- 1
    all_vars <- ""
    while (i <= length(no_interaction_var)) {
      this <- paste(no_interaction_var[i], "+ ")
      all_vars <- paste(all_vars, this)
      i <- i + 1
    }
    expression <- paste(expression, all_vars, sep = "")
    i <- 1
    all_interact_vars <- ""
    while (i <= length(fix_eff_interact_vars)) {
      if (i == length(fix_eff_interact_vars)) {
        this <- paste(fix_eff_interact_vars[i], "+ ")
      } else {
        this <- paste(fix_eff_interact_vars[i], "* ")
      }
      all_interact_vars <- paste(all_interact_vars, this)
      i <- i + 1
    }
    expression <- paste(expression, all_interact_vars, sep = "")
  }
  if (is.na(cross)) {
    expression <- get_slope_intercept(expression, random_intercept_vars,
                                      random_slope_intercept_pairs,
                                      uncorrel_slope_intercept_pairs)
  } else {
    if (cross)
      expression <- get_slope_intercept_cross(expression,
                                              random_intercept_vars,
                                          intercept_vars_pairs,
                                          random_slope_intercept_pairs,
                                          uncorrel_slope_intercept_pairs)
    if (!cross)
      expression <- get_slope_intercept_nested(expression,
                                               random_intercept_vars,
                                            intercept_vars_pairs,
                                            random_slope_intercept_pairs,
                                            uncorrel_slope_intercept_pairs)
  }
  if (!is.na(family)) {
    expression <- paste(expression, ", family = ", family_def, sep = "")
  }
  if (sys.nframe() > 1) {
    caller <- deparse(sys.calls()[[sys.nframe() - 1]])
    this_caller <- caller[1]
    index <- stringr::str_locate(this_caller, "\\(")[1]
    method_name <- substr(this_caller, 1, index[1] - 1)
    if (method_name == "use_linear_mixed_model") {
      expression <- paste("lme4::lmer(", expression, ", data = dataset)",
                          sep = "")
    } else {
         if (method_name == "use_generalised_linear_mixed_model") {
              expression <- paste("lme4::glmer(", expression, ", data = dataset, control = lme4::glmerControl(optimizer = \"bobyqa\"), nAGQ = 10)", sep = "")
         } else {
              expression <- paste("lme4::lmer(", expression, ", data = dataset)", sep = "")
          }
    }

  } else {
    expression <- paste("lme4::lmer(", expression, ", data = dataset)",
                        sep = "")
  }
  return(expression)
}
