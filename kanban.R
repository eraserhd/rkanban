
# flow is a data.frame containing your system's flow data.  The frame should
# have the following layout:
#
#  Date      | Category | Done | QA | Dev | Backlog ...
# -----------+----------+------+----+-----+-------------
# 2010-01-01 | Project  | 10   | 2  | 4   | 96
#
# PlotCFD() does not currently use Category (it aggregates the data by date,
# so we are only plotting a cumulative flow of the accumulation of all
# categories).  The names from `Done' onward can be changed, and we assume
# that all columns after and including the third are flow data in reverse
# chronological order.  These are expected to mirror manual board counts,
# and so they should not include the prior categories.

CFDBins <- function(flow) {
  names(flow)[3:length(flow)]
}

CFDCategorySum <- function(flow) {
  flow.all <- rowsum(flow[CFDBins(flow)], group = flow$Date)
  flow.all$Date <- as.Date(row.names(flow.all))

  totals <- NULL
  for (bin in CFDBins(flow)) {
    if (is.null(totals))
      totals <- flow.all[bin]
    else
      totals <- totals + flow.all[bin]
    flow.all[bin] <- totals
  }

  flow.all
}

PlotCFD <- function(flow) {
  require(lattice)

  bins <- CFDBins(flow)
  bin.colors <- rainbow(length(bins)) 

  flow.all <- CFDCategorySum(flow)

  x.ticks.at <- seq(as.Date(format(flow$Date[1], format="%Y-%m-01")),
                    flow$Date[length(flow$Date)],
                    "month")
  x.ticks.labels <- strftime(x.ticks.at, "%b %Y")

  with(flow.all,
       xyplot(as.formula(paste(paste(bins, collapse="+"), "~", names(flow)[1])),
              data=flow.all,
              panel=function(x, y, groups, ...) {
                 for (i in length(levels(groups)):1) {
                     lvl <- levels(groups)[i]
                     gx <- x[groups == lvl]
                     gy <- y[groups == lvl]
                     panel.polygon(append(gx, c(gx[length(gx)], gx[1])),
                                   append(gy, c(0, 0)),
                                   col=bin.colors[i])
                 }
              },
              main="Cumulative Flow",
              scales=list(x=list(at=x.ticks.at, labels=x.ticks.labels)),
              ylab="Items",
              xlab=NULL,
              key=list(corner=c(0,1),
                       rectangles=list(col=rev(bin.colors)),
                       text=list(rev(bins))),
              type="l")) 
}

# vi:set sts=2 sw=2 ft=r ai et:
