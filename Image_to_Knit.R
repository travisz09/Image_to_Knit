#Image to Knit
##Travis Zalesky
##5/4/2024
##V1.0.2

##Objective: To convert black and white imagery to a three tone knitting pattern.
###   Output visual guide and pattern text.

#Version History:
##1.0.0 - Initial Commit
##1.0.1 - Bug Fix: Maintain original image size for resacled image.
##1.1.0 - Feat: Add support for more than 3 colors (i.e. multiple gray tones)

#packages
##install.packages("terra")
library(terra)
##install.packages("progress")
library(progress)
#install.packages("grDevices")
library(grDevices)

img <- file.choose()

rast <- rast(img)
#plotRGB(rast)

tryCatch({plotRGB(rast)},
          error = function(e) {plot(rast)})

rast

orig_h <- dim(rast)[1]
orig_w <- dim(rast)[2]

#Adjust scale factor for desired resolution
scl_fct <- 10

#Adjust n colors
n_col <- 4

agg <- aggregate(rast, fact = scl_fct, fun = "mean")

agg

tryCatch({plotRGB(agg)},
         error = function(e) {plot(agg)})

names(agg)[1] <- "R"

#for black and white images, select red band
plot(agg$R)

div <- 255/(n_col+1)
for (i in c(1:(n_col+1))) {
  agg[agg <= (div*i) & agg >= (div*(i-1))] <- i
}

agg[agg > n_col] <- n_col

plot(agg$R)

#Select default band, default = Red
band <- "R"

plot(subset(agg, names(agg) == band),
     legend = F, axes = F, col = gray.colors(n = 4, start = 0, end = 1))

jpeg(file = "pattern.jpeg", width = orig_w, height = orig_h)
plot(subset(agg, names(agg) == band),
     legend = F, axes = F, col = gray.colors(n = 4, start = 0, end = 1))
dev.off()

get_cols <- function(val, n_col) {
  if (val == 1) {
      col <- "white"
    }
    if (val == max(n_col)) {
      col <- "black"
    }
    if (val > 1 & val < max(n_col)) {
      col <- paste("gray", val-1, sep = "_")
    }
  return(col)
}

#get_cols(4, 4)

total <- dim(agg$R)[1]*dim(agg$R)[2]
pb <- progress_bar$new(format = "  scanning image [:bar] :percent eta: :eta",
                       total = total, width = 60)
pattern_text <- function(img, n_col) {
  
  pb$tick(0)
  
  n <- 0
  row_sum <- 0
  n_row <- 0
  n_cols <- dim(img)[2]
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
      if (is.nan(pix) | is.na(pix)) {
        next
      }
      col <- get_cols(as.numeric(pix), n_col)
      lst_col <- col
      next
    }
    
    #Diagnostics
    #print(paste(i, col, n, row_sum))
    #Sys.sleep(0.2)
    
    if (i%%n_cols != 0) {
      if (is.nan(pix) | is.na(pix)) {
        next
      }
      col <- get_cols(pix, n_cols)
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
      if (is.nan(pix) | is.na(pix)) {
        txt <- paste(txt, n, " ", lst_col, "\n", sep = "")
        txt <- paste(txt, "New line (row sum = ", row_sum, ")", "\n", sep = "")
        ##reset count
        n <- 0
        row_sum <- 0
        next
      }
      col <- get_cols(pix, n_cols)
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

pattern_text(agg, n_col)
