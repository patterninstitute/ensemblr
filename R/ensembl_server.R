#' Is the Ensembl REST API server reachable?
#'
#' Check if the Ensembl server where REST API service is running is reachable.
#' This function attempts to connect to
#' \href{https://rest.ensembl.org}{https://rest.ensembl.org}, returning
#' \code{TRUE} on success, and \code{FALSE} otherwise. Set \code{chatty = TRUE}
#' for a step by step description of the connection attempt.
#'
#' @param url Ensembl REST API server URL. Default is
#'   \url{https://rest.ensembl.org}. You should not need to change this parameter.
#' @param port Network port on which to ping the server. You should not need to
#'   change this parameter.
#' @param chatty Whether to be verbose (\code{TRUE}) or not (\code{FALSE}).
#'
#' @return A logical value: \code{TRUE} if EBI server is reachable, \code{FALSE}
#'   otherwise.
#'
#' @examples
#' # Check if the Ensembl Server is reachable
#' is_ensembl_reachable() # Returns TRUE or FALSE.
#'
#' # Check if the GWAS Catalog Server is reachable
#' # and show exactly at what step is it failing (if that is the case)
#' is_ensembl_reachable(chatty = TRUE)
#'
#' @export
is_ensembl_reachable <-
  function(url = ensembl_server(), port = 443L, chatty = FALSE) {
    am_i_online <- suppressMessages(pingr::is_online())

    # Check whether I am online
    if (is.logical(am_i_online) && !am_i_online) {
      rlang::inform('It seems you are not online.')
      return(FALSE)
    } else {
      if (chatty)
        rlang::inform('Am I online? Yes.')
    }

    # Check whether there is a DNS service running
    if (is.character(am_i_online) &&
        identical(am_i_online, "nodns")) {
      rlang::inform('It seems there is no Domain Name Server (DNS) service running.')
      return(FALSE)
    } else {
      if (chatty)
        rlang::inform('Is there a DNS service running? Yes.')
    }

    # If we were not caught up in one of the stop()s earlier then we do have a
    # connection to the internet.
    if (chatty)
      rlang::inform('Connected to the internet? Yes.')

    domain <- urltools::domain(url)
    if (chatty)
      rlang::inform('Ping\'ing {domain} on port {port}...')

    ping_response <-
      pingr::ping_port(domain,
                       port = port,
                       count = 1L,
                       timeout = 2)
    if (is.na(ping_response)) {
      rlang::inform('{domain} is not replying to ping requests on port {port}.')
      return(FALSE)
    } else {
      if (chatty)
        rlang::inform(glue::glue('{domain} successfully replied in {ping_response} ms.'))
      return(TRUE)
    }
  }
