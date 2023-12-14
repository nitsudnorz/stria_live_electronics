# TODO:
# - correct Axis Labels to CC numbers
# - events 4-7 are one slot to low
# - make more beautiful cluster markers


# set your working directory to the location of this File
# "your_harddrive_location/stria_live_electronics/tools/R-code"

path <- "../../data/score_as_csv/Stria_score_asNumbers_FaderCC.csv"
stria_score <- read.csv(path, sep = ",", dec = ".") # load file

stria_score$End = stria_score$Start+stria_score$Dur # calc Ends
col_scale_fact = 1.0/max(stria_score$Freq[1:383]) # scaling factors
Amp_scale_fact = 0.5/max(stria_score$Amp[1:383])
number_distance = 1.4
quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", "Avenir Black Oblique"))
par(family = 'avenir')

# identify overlapping clusters
str(stria_score)

stria_score$isCluster <- FALSE
for (k in stria_score$Event_num) { # k event number, that refers to the line in full sheet
	if (k > 1 && stria_score$MidiFaderCC[k] == stria_score$MidiFaderCC[k-1])
	{
		#print(k)
		#print("same as last")
		stria_score$isCluster[k] = TRUE
	}
}

#export lists for PD
for (fader_CC in 0:7){
	file.create(paste("exportPD/fader",fader_CC,".txt", sep="")) # empty old files
	lines_in_txt <- list()
	line_list<-c()
	events <- stria_score[stria_score$MidiFaderCC==fader_CC, ]
  	for (e in 1:length(events$isCluster)){
  		if (events$isCluster[e] == FALSE) { # for each individual event make a new line
  			if(e > 1){lines_in_txt <- append(lines_in_txt, list(line_list))} # append latest line_list, before making a new one		
  			line_list <- c(events$Event_num[e])
  			} else { # if its a cluster, add the other events to the same line
  			line_list <- c(line_list, events$Event_num[e])
  			}
  	}
	lines_in_txt <- append(lines_in_txt, list(line_list)) # append very latest line_list
	lapply(lines_in_txt, cat, "; \n", file=paste("exportPD/fader",fader_CC,".txt", sep=""), append=TRUE) # write line by line
}


# shapes of Buffers # todo check these hardcoded numbers!
getx <- function (x)
{
  switch(x, 
         {},
         {xx <- c(0, 4096, 8192, 12288, 16384, 0)}, 
         {xx <- c(0, 2094, 5246, 12620, 13913, 16384, 0)},
         {xx <- c(0, 1500, 2500, 3220, 4220, 5464, 16384, 0)},
         {xx <- c(0, 2525, 5990, 9994, 11345, 16384, 0)},
         {xx <- c(0, 3276, 6552, 9828, 16384, 0)},
         {xx <- c(0, 3000, 3010, 3100, 6034, 7415, 8967, 11296, 16384, 0)},
         {xx <- c(0, 2071, 3279, 3712, 3889, 4580, 5270, 6478, 7253, 8113, 10093, 11557, 16384, 0)}
  )
}

gety <- function (y)
{
  switch(y, 
         {},
         {yy <- c(0, 0.33,  1,  0.33, 0, 0)},
         {yy <- c(1, 0.543, 0.367, 0.2037, 0.0845, 0, 0)},
         {yy <- c(1, 0.4, 0.2, 0.15, 0.08, 0, 0, 0)},
         {yy <- c(0.6904, 1, 0.71, 0.255, 0.15, 0, 0)},
         {yy <- c(-0.5, 0.04, 0.08, 0.01, 0.03, 0)},
         {yy <- c(0, 0.35, 0.85, 1, 0.857, 0.44, 0.214, 0.107, 0, 0)},
         {yy <- c(0, 0.135, 0.27, 0.416, 0.827, 0.949, 0.998, 1, 0.916, 0.7, 0.365, 0.234, 0, 0)}
  )
}

f <- function (l, r , x, y, a, o, c)# left right start, end, amplitude, offset=freq, c=color
{
  polygon((x+getx(l)*(y-x)/16384),(gety(l)*a+o), col = grey(c))
  polygon((x+getx(r)*(y-x)/16384),(gety(r)*-1*a+o), col = grey(c))
}


#x="Fader Pl 1"
plotter <- function(start, end){
  plot(x = 1,  
       xlab = "", # axis lable 
       ylab = "",
       xlim = c(start, end), 
       ylim = c(0, 10), # axis size
       xaxs = "i",
       yaxs = "i",
       main = "",
       type = "n",
       yaxt = "n",
       xaxt = "n")
  
  axis(2, at = c( 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5), las = 2, labels = c("", "1", "", "2", "", "3", "", "4", "", "", "1", "", "2", "", "3", "", "4"), tick = FALSE)
  
  fader_lines <- c(1, 2, 3, 4, 6, 7, 8, 9)
  abline(h = fader_lines, col = "grey", lty = "dashed")
  abline(h = 5, col = "black")
  mtext("Fbank1",                     # Add title manually
        side = 2,
        line = 2,
        las = 2,
        at = 3,
        cex = 0.8,
        font = 3)
  mtext("Fbank2",                     # Add title manually
        side = 2,
        line = 2,
        las = 2,
        at = 8,
        cex = 0.8,
        font = 3)
  
  #steps = c(0:95)*10
  #for (l in steps){
  #  abline(v = steps, col = "grey", lty = "dashed")
  #	}
}



drawfader <- function (y){ # input is fader number
    for (k in stria_score$Event_num) { # k event number, that refers to the line in full sheet
      if (!is.na(k) && stria_score$MidiFaderCC[k] == y){
        #segments(stria_score$Start[k], y+stria_score$Freq[k]*0.0001, stria_score$End[k], y+stria_score$Freq[k]*0.0001) # 
        col_r <- col_scale_fact*stria_score$Freq[k]
        f(stria_score$ampF[k],stria_score$IAF[k] ,stria_score$Start[k], stria_score$End[k], stria_score$Amp[k]*Amp_scale_fact, y+1+(12*log2(stria_score$Freq[k]/440)+69)*0.005, col_r)
       if(!stria_score$isCluster[k]) { # event is in cluster, omit event number
	        text(stria_score$Start[k]-number_distance,
	             y+1+(12*log2(stria_score$Freq[k]/440)+69)*0.005,
	             labels = k, cex = 1.5)
	             } else { # mark events in cluster
	             	segments(stria_score$Start[k], y+1+stria_score$Freq[k]*0.0001, stria_score$End[k], y+1+stria_score$Freq[k]*0.0001, col = "red") #
	             }
	        text(stria_score$Start[k] + stria_score$Dur[k]*0.5,
	             y+1+(12*log2(stria_score$Freq[k]/440)+69)*0.005,
	             labels = paste(round(stria_score$Dur[k], 0), "s"), cex = 1.5, col = "white")
      }
    }
}


drawpage <- function (nr,beg, end)
{
    par(mfcol=c(1,1), mai = c(0.7, 1, 0, 0.4), omi = c(0.2, 0, 0.5, 0), cex = 0.5) # papiergröße
  plotter(beg, end)
  mtext("Stria by John Chowning Performancescore",                     # Add Titel manually
        side = 3,
        line = 2,
        cex = 0.5
        )
  mtext(nr,                     # Add Pagenumber manually
        side = 3,
        line = 2,
        cex = 0.5,
        adj=1
        )
  for (fader_CC in 0:7){
  	drawfader(fader_CC)
    }

}

# while debugging Export goes to Desktop
pdf("~/Desktop/Stria_ahnew_v1.pdf", width = 10.0, height = 7,
    onefile = TRUE, encoding = "TeXtext.enc")


drawpage(1, 0, 95)
drawpage(2, 95, 190)
drawpage(3, 190, 285)
drawpage(4, 285, 380)
drawpage(5, 380, 475)
drawpage(6, 475, 570)
drawpage(7, 570, 665)
drawpage(8, 665, 760)
drawpage(9, 760, 855)
drawpage(10, 855, 950)


dev.off()