#Image to Knit
##Travis Zalesky
##5/4/2024
##V1.0.0

##Objective: To convert black and white imagery to a three tone knitting pattern.
###   Output visual guide and pattern text.

#packages
##install.packages("terra")
library(terra)
##install.packages("progress")
library(progress)

img <- file.choose()

rast <- rast(img)
#plotRGB(rast)

tryCatch({plotRGB(rast)},
          error = function(e) {plot(rast)})

rast

#Adjust scale factor for desired resolution
scl_fct <- 10

agg <- aggregate(rast, fact = scl_fct, fun = "mean")

agg

tryCatch({plotRGB(agg)},
         error = function(e) {plot(agg)})

names(agg)[1] <- "R"

#for black and white images, select red band
plot(agg)

agg[agg <= 83] <- 1
agg[agg > 83 & agg <= 166] <- 2
agg[agg > 166] <- 3

plot(agg)

#Select default band, default = Red
band <- "R"

plot(subset(agg, names(agg) == band))

jpeg(file = "pattern.jpeg")
plot(subset(agg, names(agg) == band))
dev.off()

total <- dim(agg$R)[1]*dim(agg$R)[2]
pb <- progress_bar$new(format = "  scanning image [:bar] :percent eta: :eta",
                       total = total, width = 60)
pattern_text <- function(agg) {
  
  pb$tick(0)
  
  n <- 0
  row_sum <- 0
  n_row <- 0
  n_cols <- dim(agg)[2]
  col <- NA
  lst_col <- NA
  txt <- ""
  
  for (i in c(1:length(matrix(subset(agg, names(agg) == band))))) {
    #print(i)
    pb$tick()
    pix <- matrix(subset(agg, names(agg) == band))[i]
    n <- n + 1
    row_sum <- row_sum + 1
    
    if (i == 1) {
      if (is.nan(pix)) {
        next
      }
      if (pix == 1) {
        col <- "white"
      } 
      if (pix == 2) {
        col <- "grey"
      }
      if (pix == 3) {
        col <- "black"
      }
      lst_col <- col
      next
    }
    
    #Diagnostics
    #print(paste(i, col, n, row_sum))
    #Sys.sleep(0.2)
    
    if (i%%n_cols != 0) {
      if (is.nan(pix)) {
        next
      }
      if (pix == 1) {
        col <- "white"
      } 
      if (pix == 2) {
        col <- "grey"
      }
      if (pix == 3) {
        col <- "black"
      }
      if (is.na(lst_col)) {
        txt <- paste(txt, n, " ", lst_col, "\n", sep = "")
      } else { #if !is.na(lst_col)
        if (col == lst_col) {
        } else { #if col != lst_col
          txt <- paste(txt, n, " ", lst_col, "\n", sep = "")
          n <- 0
        }
      }
    }
    if (i%%n_cols == 0) {
      if (is.nan(pix)) {
        txt <- paste(txt, n, " ", lst_col, "\n", sep = "")
        txt <- paste(txt, "New line (row sum = ", row_sum, ")", "\n", sep = "")
        ##reset count
        n <- 0
        row_sum <- 0
        next
      }
      if (pix == 1) {
        col <- "white"
      } 
      if (pix == 2) {
        col <- "grey"
      }
      if (pix == 3) {
        col <- "black"
      }
      if (is.na(lst_col)) {
        txt <- paste(txt, n, " ", lst_col, "\n", sep = "")
      } else { #if !is.na(lst_col)
        if (col == lst_col) {
        } else { #if col != lst_col
          txt <- paste(txt, n, " ", lst_col, "\n", sep = "")
          n <- 0
        }
      }
      n_row <- n_row + 1
      txt <- paste(txt, n, " ", lst_col, "\n", sep = "")
      txt <- paste(txt, "New line (row sum = ", row_sum, ")", "\n", sep = "")
      ##reset count
      n <- 0
      row_sum <- 0
    }
    lst_col <- col
    
  }
  writeLines(txt, con = "pattern.txt")
  
}

pattern_text(agg)
