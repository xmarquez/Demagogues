#' Build a HathiTrust workset with a Bookworm fallback
#'
#' Attempts the legacy HTRC Solr workset builder or, when requested, uses
#' HathiTrust+Bookworm search results to retrieve HTIDs by term and publication
#' period. The Bookworm path is useful when the retired Solr Proxy or Workset
#' Builder Solr endpoint is unavailable.
#'
#' @param token Character vector of search terms.
#' @param pub_date Numeric vector of publication years to search.
#' @param lang Language code. The Solr path passes this to
#'   `hathiTools::workset_builder()`. The Bookworm path is filtered downstream
#'   against the local Hathi catalog metadata.
#' @param volumes_only Logical passed to the Solr builder.
#' @param token_join Boolean join for multiple terms, either `"AND"` or `"OR"`.
#' @param max_vols Maximum number of HTIDs to return.
#' @param method Workset source: `"auto"` tries Solr and falls back to Bookworm,
#'   `"solr"` requires Solr, and `"bookworm"` skips Solr.
#' @param verbose Logical; if `TRUE`, emit progress messages.
#'
#' @return A tibble with at least `htid`, `title`, and `url`, with class
#'   `hathi_workset`.
workset_builder_resilient <- function(token,
                                      pub_date,
                                      lang = "eng",
                                      volumes_only = TRUE,
                                      token_join = c("AND", "OR"),
                                      max_vols = Inf,
                                      method = c("auto", "solr", "bookworm"),
                                      verbose = FALSE) {
  method <- match.arg(method)
  token_join <- match.arg(token_join)

  if (method %in% c("auto", "solr")) {
    solr_result <- tryCatch(
      hathiTools::workset_builder(
        token = token,
        pub_date = pub_date,
        lang = lang,
        volumes_only = volumes_only,
        token_join = token_join,
        max_vols = max_vols,
        verbose = verbose
      ),
      error = function(e) e
    )

    if (!inherits(solr_result, "error")) {
      return(solr_result)
    }

    if (identical(method, "solr")) {
      stop(conditionMessage(solr_result), call. = FALSE)
    }

    warning(
      "Solr workset query failed; falling back to Bookworm search_results. ",
      conditionMessage(solr_result),
      call. = FALSE
    )
  }

  bookworm_workset_builder(
    token = token,
    pub_date = pub_date,
    token_join = token_join,
    max_vols = max_vols,
    verbose = verbose
  )
}

#' Build a workset from HathiTrust+Bookworm search results
#'
#' Queries Bookworm once per term for the supplied publication-year range, then
#' unions or intersects the HTIDs according to `token_join`.
#'
#' @param token Character vector of terms to search.
#' @param pub_date Numeric vector of publication years.
#' @param token_join Boolean join for multiple terms, either `"AND"` or `"OR"`.
#' @param max_vols Maximum number of HTIDs to return.
#' @param verbose Logical; if `TRUE`, emit progress messages.
#'
#' @return A `hathi_workset` tibble.
#' @keywords internal
bookworm_workset_builder <- function(token,
                                     pub_date,
                                     token_join = c("AND", "OR"),
                                     max_vols = Inf,
                                     verbose = FALSE) {
  token_join <- match.arg(token_join)
  token <- unique(as.character(token))
  lims <- range(pub_date, na.rm = TRUE)

  if (verbose) {
    message(
      "Bookworm workset query: ",
      paste(token, collapse = paste0(" ", token_join, " ")),
      " (", lims[1], "-", lims[2], ")"
    )
  }

  results <- purrr::map(token, function(term) {
    tryCatch(
      hathiTools::query_bookworm(
        term,
        method = "search_results",
        lims = lims,
        ignore_case = TRUE
      ),
      error = function(e) {
        warning(
          "Bookworm query failed for term `", term, "`: ",
          conditionMessage(e),
          call. = FALSE
        )
        tibble::tibble(htid = character(), title = character(), url = character())
      }
    )
  })

  if (identical(token_join, "AND") && length(results) > 1) {
    keep_ids <- purrr::map(results, "htid") |>
      purrr::reduce(intersect)
    out <- dplyr::bind_rows(results) |>
      dplyr::filter(htid %in% keep_ids)
  } else {
    out <- dplyr::bind_rows(results)
  }

  out <- out |>
    dplyr::filter(!is.na(htid), htid != "") |>
    dplyr::distinct(htid, .keep_all = TRUE)

  if (!is.infinite(max_vols)) {
    out <- dplyr::slice_head(out, n = max_vols)
  }

  attr(out, "query") <- paste(token, collapse = paste0(" ", token_join, " "))
  attr(out, "source") <- "bookworm_search_results"
  class(out) <- c("hathi_workset", class(out))
  out
}
