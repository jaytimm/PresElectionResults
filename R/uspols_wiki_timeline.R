#'
#' @name uspols_wiki_timeline
#' @import data.table
#'
#' @export
#' @rdname uspols_wiki_timeline
uspols_wiki_timeline <- function() {

  qs <- c('_Q1', '_Q2', '_Q3', '_Q4')
  ys <- c(2017:2020)
  allqs <- do.call(paste0, expand.grid(ys, qs))

  allqs <- gsub('2020_Q4', '2020_Q4–January_2021', allqs)

  timeline <- lapply(1:length(allqs), function(x) {

    url1 <- paste0('https://en.wikipedia.org/wiki/Timeline_of_the_Donald_Trump_presidency_(',
                   allqs[x], ')')

    ###
    tq_list <- xml2::read_html(url1)
    tq_list1 <- rvest::html_nodes(tq_list, "table")
    tq_list2 <- rvest::html_table(tq_list1, fill = TRUE)

    new <- lapply(1:length(tq_list2), function(z) {

      if ( all(c('Date', 'Events') %in% colnames(tq_list2[[z]]))  ) {

        tq_list2[[z]][, c('Date', 'Events')]

      } else{ data.frame(Date = '', Events = '')
      }
    })

    out <- data.table::rbindlist(Filter(function(i) nrow(i) >= 3, new))
    out$quarter <- allqs[x]
    out <- subset(out, !grepl('\\[edit', Date))
    out$Events <- gsub('\\[[0-9]+\\]', '', out$Events) ## Citations
    out$Events <- gsub('\\[citation needed\\]', '', out$Events)

    out$Date <- gsub('\n', ' ', out$Date)
    out$Date <- gsub(',', '', out$Date)

    out$dow <- gsub(' .*$', '', out$Date)

    out$date <-  as.Date(paste0(gsub('_..', '', out$quarter),
                                gsub('^.*day ', '', out$Date)),
                         "%Y %B %d")
    out[, c('quarter', 'date', 'dow', 'Events')]
  })


  ####
  y <- data.table::rbindlist(timeline)
  y <- y[order(y$date),]

  ## add `week of` and day #
  y$weekof <- lubridate::floor_date(as.Date(y$date, "%Y-%m-%d"),
                                 unit = 'week')
  y$daypres <- 1:nrow(y)


  ## by bullet point -- BREAKS NOW --
  y1 <- tidyr::separate_rows(y, Events, sep = '\n')
  y1$bullet <- sequence(rle(as.character(y1$date))$lengths)
  zz <- y1[, c('quarter', 'weekof', 'daypres', 'date',
               'dow', 'bullet', 'Events')]
  return(zz)
}
