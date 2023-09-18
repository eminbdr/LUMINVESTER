library(ggplot2)
library(zoo)
library(ggthemes)
library(ggpubr)

mean_form <- function(dt, n = 3)round(mean(dt),n)

var_form <- function(dt, n = 3) round(var(dt), n)

std_form <- function(dt, n = 3) round(sqrt(var_form(dt)), n)

t_score_calc <- function(x, y, n = 3) {
  x <- as.double(x)
  y <- as.double(y)
  if (length(y) == 1) {
    y <- rep(y, length(x))
  }
  result <-
    abs((mean(x) - mean(y)) / sqrt(var(x) / length(x) + var(y) / length(y)))
  round(result, n)
}

# Function to calculate paired t-score
paired_t_score_calc <- function(x, y, n = 3) {
  x <- as.double(x)
  y <- as.double(y)
  d <- (x - y)
  result <- abs(mean(d) * sqrt(length(d) / var(d)))
  round(result, n)
}

p_value_cal <- function(t_score, df, two_tail) {
  if (two_tail) {
    (1 - pt(q = t_score, df = df)) * 2
  } else {
    1 - pt(q = t_score, df = df)
  }
}

t_cric_calc <- function(alpha, df) {
  qt(alpha / 2, df, lower.tail = FALSE)
}

which.median <- function(x) {
  med <- median(x, na.rm = TRUE)
  result <- which(x == med)
  if (length(result) == 0)
    round(approx(seq_along(x), x, n = 1, method = "linear")$x)
  else
    result[1]
}
desc_finder <-function(sectorsp, which_one, digits = 3) {
  mean_df <-
    data.frame(mean = sapply(province, function(prov)
      mean(as.numeric(
        subset(df, sector == sectorsp &
                 province == prov)[, 3:length(df)]
      ))))
  index <- which_one(mean_df$mean)
  c(rownames(mean_df)[index], round(mean_df[index, ], digits = digits))
}

desc_sorter <-function(sectorsp,decreasing = TRUE,digits = 3) {
  mean_df <-
    data.frame(mean = sapply(province, function(prov)
      mean(as.numeric(
        subset(df, sector == sectorsp &
                 province == prov)[, 3:length(df)]
      ))))
  round(mean_df[order(mean_df$mean, decreasing = decreasing), , drop = FALSE], digits = digits)
}

first_kth <-function(nums,collapser = "\n",k = 3) {
  paste(sapply(1:k, function(x)
    paste(row.names(nums)[x], nums[x, ], sep = ":")), collapse = collapser)
}


average_funcs <- list(
  "roll_mean" = zoo::rollmean,
  "roll_max" = zoo::rollmax,
  "roll_med" = zoo::rollmedian
)

# Function to calculate rolling average
rollers <- function(series,param,k = 5,color = "blue") {
  if (param[[1]] == "0") {
    geom_blank()

  } else {
    ma_series <- average_funcs[[param[[1]]]](series,k = ifelse(is.null(k),5,k),na.pad = TRUE,align = "center")
    ma_series <- zoo::na.trim(ma_series, sides = "right")
    pad_length <- length(series) - length(ma_series)
    ma_series <- c(rep(NA, pad_length), ma_series)
    geom_line(aes(y = ma_series), color = color)
  }
}

#Graphing the Plots
analyze_graph <- function(y1, y2, colnamey1, colnamey2, ...) {
  main_plot <- ggplot()
  dates <- as.Date(paste(names(y1),"-01",sep=""))
  breaking <- 3
  scale <- scale_x_continuous(breaks = dates[seq(1,length(dates),breaking)], labels = format(dates[seq(1,length(dates),breaking)], "%Y-%m"))
  data <- data.frame(date = dates,
                     first = y1,
                     second = y2)  
  
  data<-data[order(data$date),]
  ovar <- list(...)
  
  p <- ggplot(data, aes(x = date, y = first)) + geom_line() +
    ylab(colnamey1) + scale + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("MONTHS")+geom_smooth(method="loess",colour = "purple")
  if (!is.null(ovar[["wroller"]])) {
    p <- p + rollers(data$first, ovar[["wroller"]], ovar[["k"]])
  }
  
  q <- ggplot(data, aes(x = date, y = second)) + geom_line() +
    ylab(colnamey2) + xlab("MONTHS") + geom_smooth(method="loess",colour = "purple")+
    scale + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  if (!is.null(ovar[["wroller"]])) {
    q <- q + rollers(data$second, ovar[["wroller"]], ovar[["k"]])
  }
  if(colnamey1 != colnamey2){main_plot <- ggarrange(p, q)}
  else{main_plot<-p
    }
  
  
  main_plot + ggtitle("Export Performance between 2019-January to 2023-March ($1000)") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

year_comp <- function(sector,year1,year2,decreasing=TRUE,digits=3){
  col1 <-data.frame(subset(df, sector == sector)[c("province",year1)])
  col2 <-data.frame(subset(df, sector == sector)[c("province",year2)])
  deg <- col1
  deg[,2] <- (col1[,2]/(col2[,2]+.00000001)-1)*100
  View(deg)
  round(deg[order(deg[,2], decreasing = decreasing,drop = FALSE)], digits = digits)
}
