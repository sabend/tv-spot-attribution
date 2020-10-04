library("R6")

AttributionResult <- R6Class("AttributionResult",
  # PRIVATE
  private = list(
    .ids = NULL,
    .results = NULL,
    .baselines = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize @overwrite
    initialize = function(ids) {
      stopifnot(is.character(ids))

      result_list <- vector("list", length(ids))
      names(result_list) <- ids

      private$.ids <- ids
      private$.results <- result_list
      private$.baselines <- result_list
    },

    # FUN: ids
    ids = function() {
      return(private$.ids)
    },

    # FUN: add
    insert = function(id, res) {
      private$.results[[id]] <- res$attribution
      private$.baselines[[id]] <- res$baseline
    },

    # FUN: get_result
    get_result = function(id = NULL) {
      if (!is.null(id)) {
        return(private$.results[[id]])
      } else {
        return(data.frame(
          id    = private$.ids,
          value = as.vector(unlist(private$.results))
        ))
      }
    },

    # FUN: get_baseline
    get_baseline = function(id = NULL) {
      if (!is.null(id)) {
        return(private$.baselines[[id]])
      } else {
        return(private$.baselines)
      }
    },

    # FUN: print
    print = function(...) {
      print(self$get_result())
      invisible(self)
    }
  )
)