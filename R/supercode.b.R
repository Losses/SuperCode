supercodeClass <- R6::R6Class(
  "supercodeClass",
  inherit = supercodeBase,
  private = list(

    .run = function() {
      vars <- self$options$vars
      if (length(vars) == 0) return()

      varOpts   <- self$options$varOptions
      keys_out  <- c()
      titles_out <- c()
      mtypes    <- c()
      allvals   <- list()
      html_parts <- c()

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
          ref    <- opts$refLevel
        }

        col <- self$data[[v]]
        fac <- as.factor(col)

        lvls <- levels(fac)
        if (!is.null(ref) && ref != "" && ref %in% lvls) {
          if (coding == "deviation") {
            # Deviation uses the last level as the reference level
            lvls <- c(setdiff(lvls, ref), ref)
          } else {
            # Dummy, Simple, and Helmert/Difference codings put the reference/first level first
            lvls <- c(ref, setdiff(lvls, ref))
          }
          fac  <- factor(fac, levels = lvls)
        }

        k  <- length(lvls)
        if (k < 2) next

        cm <- private$.buildCM(coding, k)

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

        html_parts <- c(html_parts,
          private$.buildPreviewHtml(v, coding, lvls, cm))
      }

      self$results$preview$setContent(
        paste(html_parts, collapse = "<hr/>"))

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
    },

    .buildCM = function(coding, k) {
      switch(coding,
        dummy = {
          contr.treatment(k)
        },
        simple = {
          c_mat      <- contr.treatment(k)
          my_coding  <- matrix(rep(1/k, k * (k-1)), ncol = k-1)
          c_mat - my_coding
        },
        deviation = {
          contr.sum(k)
        },
        poly = {
          contr.poly(k)
        },
        helmert = {
          m <- matrix(0, nrow = k, ncol = k-1)
          for (j in seq_len(k-1)) {
            m[j, j]          <-  (k - j) / (k - j + 1)
            m[(j+1):k, j]    <- -1 / (k - j + 1)
          }
          m
        },
        revhelmert = {
          contr.helmert(k)
        },
        forward = {
          m <- matrix(0, nrow = k, ncol = k-1)
          for (j in seq_len(k-1)) {
            m[1:j, j]        <-  (k - j) / k
            m[(j+1):k, j]    <- -j / k
          }
          m
        },
        backward = {
          m <- matrix(0, nrow = k, ncol = k-1)
          for (j in seq_len(k-1)) {
            m[1:j, j]        <- -( k - j) / k
            m[(j+1):k, j]    <-  j / k
          }
          m
        }
      )
    },

    .buildPreviewHtml = function(varName, coding, lvls, cm) {
      k    <- length(lvls)
      cols <- paste0(coding, seq_len(k-1))

      header <- paste0(
        "<th>Level</th>",
        paste(sprintf("<th>%s</th>", cols), collapse = ""))

      rows <- vapply(seq_len(k), function(i) {
        cells <- paste(sprintf("<td>%.3f</td>", cm[i, ]), collapse = "")
        sprintf("<tr><td><b>%s</b></td>%s</tr>", lvls[i], cells)
      }, character(1))

      sprintf(
        "<p><b>%s</b> &mdash; %s</p>
         <table border='1' cellpadding='4' style='border-collapse:collapse'>
           <thead><tr>%s</tr></thead>
           <tbody>%s</tbody>
         </table>",
        varName, coding, header, paste(rows, collapse = ""))
    }
  )
)