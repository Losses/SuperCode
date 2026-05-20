supercodeClass <- R6::R6Class(
  "supercodeClass",
  inherit = supercodeBase,
  private = list(
    .run = function() {
      vars   <- self$options$vars
      coding <- self$options$coding
      stdz   <- self$options$standardize

      if (length(vars) == 0) return()

      keys <- c()
      titles <- c()
      mtypes <- c()
      allvals <- list()

      for (v in vars) {
        col  <- self$data[[v]]
        fac  <- as.factor(col)
        lvls <- levels(fac)
        k    <- length(lvls)
        if (k < 2) next

        cm <- switch(coding,
          dummy     = contr.treatment(k),
          helmert   = contr.helmert(k),
          poly      = contr.poly(k),
          deviation = contr.sum(k)
        )

        coded <- cm[as.integer(fac), , drop = FALSE]

        if (stdz) {
          coded <- scale(coded)
        }

        suffix <- switch(coding,
          dummy     = "dum",
          helmert   = "helm",
          poly      = "poly",
          deviation = "dev"
        )

        for (i in seq_len(k - 1)) {
          key <- paste0(v, "_", suffix, i)
          keys   <- c(keys, key)
          titles <- c(titles, paste0(v, " [", suffix, i, "]"))
          mtypes <- c(mtypes, "continuous")
          allvals[[key]] <- as.numeric(coded[, i])
        }
      }

      if (length(keys) == 0) return()

      self$results$outputCols$set(
        keys         = keys,
        titles       = titles,
        descriptions = titles,
        measureTypes = mtypes
      )

      for (i in seq_along(keys)) {
        self$results$outputCols$setValues(index = i, allvals[[keys[i]]])
      }
    }
  )
)
