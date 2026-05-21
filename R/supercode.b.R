supercodeClass <- R6::R6Class(
  "supercodeClass",
  inherit = supercodeBase,
  private = list(

    .init = function() {
      vars <- self$options$vars
      if (length(vars) == 0) return()
      if (is.null(self$data)) return()

      varOpts   <- self$options$varOptions
      tables    <- self$results$preview

      for (i in seq_along(vars)) {
        v      <- vars[[i]]
        if (! v %in% names(self$data)) next
        col <- self$data[[v]]
        if (is.null(col)) next
        
        opts   <- private$.findOpts(v, varOpts)
        o <- private$.readOpts(opts)
        coding     <- o$coding
        stdz       <- o$stdz
        integerize <- o$integerize
        ref        <- o$ref

        fac <- as.factor(col)
        lvls <- levels(fac)
        
        usesRef <- coding %in% c("dummy", "simple", "deviation")
        if (usesRef && !is.null(ref) && ref != "" && ref %in% lvls) {
          if (coding == "deviation") {
            lvls <- c(setdiff(lvls, ref), ref)
          } else {
            lvls <- c(ref, setdiff(lvls, ref))
          }
        }

        k  <- length(lvls)
        if (k < 2) next

        table <- tables$get(key = v)

        # Mapping for full coding names in table titles
        codingNames <- list(
          dummy = "Dummy (Treatment) Coding",
          simple = "Simple Coding",
          deviation = "Deviation (Sum) Coding",
          poly = "Orthogonal Polynomial Coding",
          helmert = "Helmert Coding",
          revhelmert = "Reverse Helmert Coding",
          forward = "Forward Difference Coding",
          backward = "Backward Difference Coding"
        )
        codingName <- codingNames[[coding]]
        if (is.null(codingName))
          codingName <- coding

        # Set title dynamically to include coding name
        table$setTitle(paste0("<strong>", v, "</strong>", " · ", codingName))

        # Add Comparison/Contrast column
        table$addColumn(name = "contrast", title = "", type = "text")

        # Add Description column
        table$addColumn(name = "description", title = "Contrast / Comparison Task", type = "text")

        # Add level columns
        for (j in seq_len(k)) {
          colName <- paste0("lvlCol", j)
          colTitle <- lvls[j]
          table$addColumn(name = colName, title = colTitle, type = "number")
        }

        # Get contrast labels
        labels <- private$.getContrastLabels(coding, lvls)

        # Add rows
        for (rowIdx in seq_len(k - 1)) {
          table$addRow(rowKey = rowIdx, values = list(
            contrast = as.character(rowIdx),
            description = labels[rowIdx]
          ))
        }

        # Clear any legacy footnotes
        table$setNote(key = "focus", note = NULL)
        table$setNote(key = "beta", note = NULL)
        table$setNote(key = "intercept", note = NULL)

        # Set footnote explaining focus, beta interpretation, and intercept in a single formatted block
        noteText <- private$.getFootnoteText(coding, integerize, stdz)
        table$setNote(key = "explanation", note = noteText)
      }
    },

    .readOpts = function(opts) {
      if (is.null(opts)) {
        return(list(coding = "dummy", stdz = FALSE, integerize = FALSE, ref = NULL))
      }
      coding     <- if (is.null(opts$coding)) "dummy" else opts$coding
      stdz       <- isTRUE(opts$standardize)
      integerize <- isTRUE(opts$integerize)
      if (stdz && integerize) integerize <- FALSE
      list(coding = coding, stdz = stdz, integerize = integerize, ref = opts$ref)
    },

    .run = function() {
      vars <- self$options$vars
      if (length(vars) == 0) return()
      if (is.null(self$data)) return()

      varOpts   <- self$options$varOptions
      keys_out  <- c()
      titles_out <- c()
      descs_out <- c()
      mtypes    <- c()
      allvals   <- list()
      tables    <- self$results$preview

      current_keys <- NULL
      if (!is.null(self$results$outputCols)) {
        current_keys <- self$results$outputCols$.__enclos_env__$private$.keys
      }
      if (is.null(current_keys)) {
        current_keys <- character()
      } else {
        current_keys <- as.character(current_keys)
      }
      other_columns <- setdiff(names(self$data), current_keys)

      for (i in seq_along(vars)) {
        v      <- vars[[i]]
        if (! v %in% names(self$data)) next
        col <- self$data[[v]]
        if (is.null(col)) next

        opts   <- private$.findOpts(v, varOpts)
        
        o <- private$.readOpts(opts)
        coding     <- o$coding
        stdz       <- o$stdz
        integerize <- o$integerize
        ref        <- o$ref

        col <- self$data[[v]]
        fac <- as.factor(col)

        lvls <- levels(fac)
        usesRef <- coding %in% c("dummy", "simple", "deviation")
        if (usesRef && !is.null(ref) && ref != "" && ref %in% lvls) {
          if (coding == "deviation") {
            # Deviation uses the last level as the reference level
            lvls <- c(setdiff(lvls, ref), ref)
          } else {
            # Dummy and Simple use the selected level as the first/reference level
            lvls <- c(ref, setdiff(lvls, ref))
          }
          fac  <- factor(fac, levels = lvls)
        }

        k  <- length(lvls)
        if (k < 2) next

        cm <- private$.buildCM(coding, k, integerize = integerize)
        preview_cm <- cm
        if (stdz)
          preview_cm <- scale(preview_cm)

        # Populate the table cells
        table <- tables$get(key = v)
        for (rowIdx in seq_len(k - 1)) {
          rowVals <- list()
          for (j in seq_len(k)) {
            colName <- paste0("lvlCol", j)
            rowVals[[colName]] <- preview_cm[j, rowIdx]
          }
          table$setRow(rowNo = rowIdx, values = rowVals)
        }

        # The rest of .run (creating output columns)
        coded <- cm[as.integer(fac), , drop = FALSE]
        if (stdz) coded <- scale(coded)

        # Find a collision-free suffix for this variable
        suffix_str <- ""
        counter <- 1
        while (TRUE) {
          col_keys <- paste0(v, ".c", seq_len(k - 1), suffix_str)
          if (!any(col_keys %in% other_columns)) {
            break
          }
          counter <- counter + 1
          suffix_str <- paste0("_", counter)
        }

        for (j in seq_len(k - 1)) {
          keys_out   <- c(keys_out, col_keys[j])
          titles_out <- c(titles_out, col_keys[j])
          descs_out  <- c(descs_out, paste0(v, " [", coding, j, "]"))
          mtypes     <- c(mtypes, "continuous")
          allvals[[col_keys[j]]] <- as.numeric(coded[, j])
        }
      }

      if (self$options$outputCols) {
        if (length(keys_out) == 0) return()

        self$results$outputCols$set(
          keys         = keys_out,
          titles       = titles_out,
          descriptions = descs_out,
          measureTypes = mtypes
        )
        self$results$outputCols$setRowNums(rownames(self$data))
        for (i in seq_along(keys_out)) {
          self$results$outputCols$setValues(
            index  = i,
            values = allvals[[keys_out[i]]])
        }
      } else {
        self$results$outputCols$set(
          keys         = character(),
          titles       = character(),
          descriptions = character(),
          measureTypes = character()
        )
      }
    },

    .getContrastLabels = function(coding, lvls) {
      k <- length(lvls)
      labels <- character(k - 1)
      if (k < 2) return(labels)

      switch(coding,
        dummy = {
          for (j in seq_len(k - 1)) {
            labels[j] <- paste0(lvls[j + 1], " - ", lvls[1])
          }
        },
        simple = {
          for (j in seq_len(k - 1)) {
            labels[j] <- paste0(lvls[j + 1], " - ", lvls[1])
          }
        },
        deviation = {
          all_lvls <- paste(lvls, collapse = ", ")
          for (j in seq_len(k - 1)) {
            labels[j] <- paste0(lvls[j], " - (", all_lvls, ")")
          }
        },
        poly = {
          names_poly <- c("linear", "quadratic", "cubic", "quartic", "quintic", "sextic", "septic", "octic")
          for (j in seq_len(k - 1)) {
            if (j <= length(names_poly)) {
              labels[j] <- names_poly[j]
            } else {
              labels[j] <- paste0("degree ", j, " polynomial")
            }
          }
        },
        helmert = {
          for (j in seq_len(k - 1)) {
            rhs <- paste(lvls[(j + 1):k], collapse = ", ")
            if (length((j + 1):k) > 1) {
              rhs <- paste0("(", rhs, ")")
            }
            labels[j] <- paste0(lvls[j], " - ", rhs)
          }
        },
        revhelmert = {
          for (j in seq_len(k - 1)) {
            rhs <- paste(lvls[1:j], collapse = ", ")
            if (j > 1) {
              rhs <- paste0("(", rhs, ")")
            }
            labels[j] <- paste0(lvls[j + 1], " - ", rhs)
          }
        },
        forward = {
          for (j in seq_len(k - 1)) {
            labels[j] <- paste0(lvls[j], " - ", lvls[j + 1])
          }
        },
        backward = {
          for (j in seq_len(k - 1)) {
            labels[j] <- paste0(lvls[j + 1], " - ", lvls[j])
          }
        }
      )
      labels
    },

    .getFootnoteText = function(coding, integerize, standardize) {

      intro <- switch(coding,
        dummy = paste0(
          "Each contrast tests one level against a single reference level. R's default scheme."
        ),
        simple = paste0(
          "Same per-level comparisons as dummy, but the intercept is the grand mean rather than the reference mean."
        ),
        deviation = paste0(
          "Each contrast tests one level against the grand mean of all groups."
        ),
        poly = paste0(
          "Each contrast tests for a specific polynomial trend across ordered, evenly spaced levels."
        ),
        helmert = paste0(
          "Each contrast tests one level against the mean of all levels that come after it."
        ),
        revhelmert = paste0(
          "Each contrast tests one level against the mean of all levels that come before it."
        ),
        forward = paste0(
          "Each contrast tests one level against the next adjacent level."
        ),
        backward = paste0(
          "Each contrast tests one level against the previous adjacent level."
        ),
        ""
      )

      if (standardize) {
        interp <- paste0(
          "Each contrast column has been z-scored, so betas express change in Y per one ",
          "standard deviation of the contrast column. They are no longer raw mean differences."
        )
      } else if (integerize || coding %in% c("dummy", "deviation", "poly")) {
        interp <- switch(coding,
          dummy = paste0(
            "The intercept is the reference group's mean. ",
            "Each beta is this level's mean minus the reference mean."
          ),
          simple = paste0(
            "Using the integer matrix common in textbooks. ",
            "The intercept is the grand mean. ",
            "Each beta is (this level's mean minus the reference mean) divided by k, the number of levels."
          ),
          deviation = paste0(
            "The intercept is the grand mean. ",
            "Each beta is this level's mean minus the grand mean."
          ),
          poly = paste0(
            "The intercept is the grand mean. ",
            "Each beta is the coefficient of one polynomial term (linear, quadratic, cubic, and so on). ",
            "Sign and significance are interpretable; magnitude depends on how the contrast column is scaled."
          ),
          helmert = paste0(
            "Using the integer matrix common in textbooks. ",
            "The intercept is the grand mean. ",
            "The j-th beta is (mean of level j minus mean of all subsequent levels) divided by (k − j + 1)."
          ),
          revhelmert = paste0(
            "Using the integer matrix common in textbooks. ",
            "The intercept is the grand mean. ",
            "The j-th beta is (mean of level j+1 minus mean of all prior levels) divided by (j + 1)."
          ),
          forward = paste0(
            "Using the integer matrix common in textbooks. ",
            "The intercept is the grand mean. ",
            "The j-th beta is (mean of level j minus mean of level j+1) divided by k."
          ),
          backward = paste0(
            "Using the integer matrix common in textbooks. ",
            "The intercept is the grand mean. ",
            "The j-th beta is (mean of level j+1 minus mean of level j) divided by k."
          )
        )
      } else {
        interp <- switch(coding,
          simple = paste0(
            "The intercept is the grand mean. ",
            "Each beta is this level's mean minus the reference mean."
          ),
          helmert = paste0(
            "The intercept is the grand mean. ",
            "The j-th beta is the mean of level j minus the mean of all subsequent levels."
          ),
          revhelmert = paste0(
            "The intercept is the grand mean. ",
            "The j-th beta is the mean of level j+1 minus the mean of all prior levels."
          ),
          forward = paste0(
            "The intercept is the grand mean. ",
            "The j-th beta is the mean of level j minus the mean of level j+1."
          ),
          backward = paste0(
            "The intercept is the grand mean. ",
            "The j-th beta is the mean of level j+1 minus the mean of level j."
          )
        )
      }

      paste(intro, interp)
    },

    .buildCM = function(coding, k, integerize = FALSE) {
      switch(coding,
        dummy      = contr.treatment(k),
        deviation  = contr.sum(k),
        poly       = private$.buildIntegerPolyCM(k),
        simple     = if (integerize) private$.buildIntegerSimpleCM(k)     else private$.buildCleanSimpleCM(k),
        helmert    = if (integerize) private$.buildIntegerHelmertCM(k)    else private$.buildCleanHelmertCM(k),
        revhelmert = if (integerize) private$.buildIntegerRevHelmertCM(k) else private$.buildCleanRevHelmertCM(k),
        forward    = if (integerize) private$.buildIntegerForwardCM(k)    else private$.buildCleanForwardCM(k),
        backward   = if (integerize) private$.buildIntegerBackwardCM(k)   else private$.buildCleanBackwardCM(k)
      )
    },

    .buildIntegerSimpleCM = function(k) {
      k * contr.treatment(k) - 1
    },

    .buildIntegerPolyCM = function(k) {
      x <- seq_len(k)
      powers <- sapply(seq_len(k - 1), function(degree) x ^ degree)

      if (k - 1 == 1)
        powers <- matrix(powers, ncol = 1)

      centered <- scale(powers, center = TRUE, scale = FALSE)
      qr_q <- qr.Q(qr(centered))
      raw <- matrix(0, nrow = k, ncol = k - 1)

      for (j in seq_len(k - 1)) {
        col <- qr_q[, j]
        non_zero <- which(abs(col) > sqrt(.Machine$double.eps))
        first_idx <- non_zero[1]
        desired_sign <- if (j %% 2 == 1) -1 else 1
        if (!is.na(first_idx) && sign(col[first_idx]) != desired_sign)
          col <- -col

        min_abs <- min(abs(col[non_zero]))
        scaled <- col / min_abs
        rounded <- round(scaled)
        gcd_value <- Reduce(private$.gcd, abs(rounded[rounded != 0]))
        raw[, j] <- rounded / gcd_value
      }

      raw
    },

    .buildIntegerHelmertCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[j, j] <- k - j
        m[(j + 1):k, j] <- -1
      }
      m
    },

    .buildIntegerRevHelmertCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[1:j, j] <- -1
        m[j + 1, j] <- j
      }
      m
    },

    .buildIntegerForwardCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[1:j, j] <- k - j
        m[(j + 1):k, j] <- -j
      }
      m
    },

    .buildIntegerBackwardCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[1:j, j] <- -(k - j)
        m[(j + 1):k, j] <- j
      }
      m
    },

    .buildCleanSimpleCM = function(k) {
      contr.treatment(k) - 1 / k
    },

    .buildCleanHelmertCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        n_sub          <- k - j
        m[j, j]        <- n_sub / (n_sub + 1)
        m[(j + 1):k, j] <- -1 / (n_sub + 1)
      }
      m
    },

    .buildCleanRevHelmertCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[1:j, j]    <- -1 / (j + 1)
        m[j + 1, j]  <-  j / (j + 1)
      }
      m
    },

    .buildCleanForwardCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[1:j, j]       <-  (k - j) / k
        m[(j + 1):k, j] <- -j / k
      }
      m
    },

    .buildCleanBackwardCM = function(k) {
      -1 * private$.buildCleanForwardCM(k)
    },

    .gcd = function(a, b) {
      a <- as.integer(abs(a))
      b <- as.integer(abs(b))
      while (b != 0) {
        tmp <- b
        b <- a %% b
        a <- tmp
      }
      a
    },

    .findOpts = function(varName, varOpts) {
      if (is.null(varOpts)) return(NULL)
      for (opts in varOpts) {
        if (!is.null(opts$var) && opts$var == varName) {
          return(opts)
        }
      }
      return(NULL)
    }
  )
)