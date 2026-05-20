supercodeClass <- R6::R6Class(
  "supercodeClass",
  inherit = supercodeBase,
  private = list(

    .init = function() {
      vars <- self$options$vars
      if (length(vars) == 0) return()

      varOpts   <- self$options$varOptions
      tables    <- self$results$preview

      for (i in seq_along(vars)) {
        v      <- vars[[i]]
        opts   <- if (v %in% names(varOpts)) varOpts[[v]] else varOpts[[i]]
        
        if (is.null(opts)) {
          coding <- "dummy"
          ref    <- NULL
        } else {
          coding <- if (is.null(opts$coding)) "dummy" else opts$coding
          ref    <- opts$ref
        }

        col <- self$data[[v]]
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

        # Set title dynamically to include coding name
        table$setTitle(paste0("Contrast Matrix - ", v, " (", coding, ")"))

        # Add Level column
        table$addColumn(name = "level", title = "Level", type = "text")

        # Add coding columns
        for (j in seq_len(k - 1)) {
          colName <- paste0("col", j)
          colTitle <- paste0(coding, j)
          table$addColumn(name = colName, title = colTitle, type = "number")
        }

        # Add rows
        for (rowIdx in seq_len(k)) {
          table$addRow(rowKey = rowIdx, values = list(level = lvls[rowIdx]))
        }
      }
    },

    .run = function() {
      vars <- self$options$vars
      if (length(vars) == 0) return()

      varOpts   <- self$options$varOptions
      keys_out  <- c()
      titles_out <- c()
      mtypes    <- c()
      allvals   <- list()
      tables    <- self$results$preview

      for (i in seq_along(vars)) {
        v      <- vars[[i]]
        opts   <- if (v %in% names(varOpts)) varOpts[[v]] else varOpts[[i]]
        
        if (is.null(opts)) {
          coding <- "dummy"
          stdz   <- FALSE
          ref    <- NULL
        } else {
          coding <- if (is.null(opts$coding)) "dummy" else opts$coding
          stdz   <- if (is.null(opts$standardize)) FALSE else opts$standardize
          ref    <- opts$ref
        }

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

        cm <- private$.buildCM(coding, k)
        preview_cm <- cm
        if (stdz)
          preview_cm <- scale(preview_cm)

        # Populate the table cells
        table <- tables$get(key = v)
        for (rowIdx in seq_len(k)) {
          rowVals <- list()
          for (j in seq_len(k - 1)) {
            colName <- paste0("col", j)
            rowVals[[colName]] <- preview_cm[rowIdx, j]
          }
          table$setRow(rowNo = rowIdx, values = rowVals)
        }

        # The rest of .run (creating output columns)
        coded <- cm[as.integer(fac), , drop = FALSE]
        if (stdz) coded <- scale(coded)

        suffix <- coding
        col_keys <- paste0(v, "_", suffix, seq_len(k - 1))

        for (j in seq_len(k - 1)) {
          keys_out   <- c(keys_out, col_keys[j])
          titles_out <- c(titles_out, paste0(v, " [", suffix, j, "]"))
          mtypes     <- c(mtypes, "continuous")
          allvals[[col_keys[j]]] <- as.numeric(coded[, j])
        }
      }

      if (self$options$runButton) {
        if (length(keys_out) == 0) return()

        self$results$outputCols$set(
          keys         = keys_out,
          titles       = titles_out,
          descriptions = titles_out,
          measureTypes = mtypes
        )
        self$results$outputCols$setRowNums(rownames(self$data))
        for (i in seq_along(keys_out)) {
          self$results$outputCols$setValues(
            index  = i,
            values = allvals[[keys_out[i]]])
        }
      }
    },

    .buildCM = function(coding, k) {
      switch(coding,
        dummy = {
          contr.treatment(k)
        },
        simple = {
          private$.buildRawSimpleCM(k)
        },
        deviation = {
          contr.sum(k)
        },
        poly = {
          private$.buildRawPolyCM(k)
        },
        helmert = {
          private$.buildRawHelmertCM(k)
        },
        revhelmert = {
          private$.buildRawRevHelmertCM(k)
        },
        forward = {
          private$.buildRawForwardCM(k)
        },
        backward = {
          private$.buildRawBackwardCM(k)
        }
      )
    },

    .buildRawSimpleCM = function(k) {
      k * contr.treatment(k) - 1
    },

    .buildRawPolyCM = function(k) {
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

    .buildRawHelmertCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[j, j] <- k - j
        m[(j + 1):k, j] <- -1
      }
      m
    },

    .buildRawRevHelmertCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[1:j, j] <- -1
        m[j + 1, j] <- j
      }
      m
    },

    .buildRawForwardCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[1:j, j] <- k - j
        m[(j + 1):k, j] <- -j
      }
      m
    },

    .buildRawBackwardCM = function(k) {
      m <- matrix(0, nrow = k, ncol = k - 1)
      for (j in seq_len(k - 1)) {
        m[1:j, j] <- -(k - j)
        m[(j + 1):k, j] <- j
      }
      m
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
    }
  )
)