####### MAIN
#' extracts hr for each color band from avg pixel value per frame of video (processed hr) JSON data file
#'
#'
#' @param hrJsonFileLoc path to hr json file
#' @return list containing hr and confidence of the estimate for each color (red, green, blue)
#' @export
#' @examples
#' @author Meghasyam Tummalacherla 


#############################################################
# Wrapper function to take in json and give HR per color channel
#############################################################

getHrFromJson <- function(hrJsonFileLoc, windowLen = 10){

  #############################################################
  # Main Code Block
  #############################################################
  
  # If no json file exists
  if(is.na(hrJsonFileLoc)){return('No JSON file') }
  
  # Get HR data
  dat = tryCatch({ jsonlite::fromJSON(as.character(hrJsonFileLoc)) }, 
                 error = function(e){ NA })
  if(all(is.na(dat))){return('JSON file read error') }
  
  # Get sampling rate
  samplingRate = length(dat$timestamp)/(dat$timestamp[length(dat$timestamp)] - dat$timestamp[1])
  
  # Convert window length from seconds to samples
  windowLen = round(samplingRate*windowLen)
  
  # Apply pre processing filter signal between freqRange
  freqRange = c(1,25) # Bandpass frequency range (1-25Hz)
  bpforder = 128 # Bandpass filter order
  mforder = 2*round(60*samplingRate/220) + 1 # order for the running mean based filter
  
  # Split each color into segments based on windowLen
  dat = dat %>% dplyr::select(red, green, blue) %>% lapply(mpowertools:::windowSignal, windowLen, ovlp=0.5)
  
  # Apply filter to each segment of each color
  dat <- dat %>% lapply(function(dfl){
    dfl[is.na(dfl)] <- 0
    dfl = tryCatch({
      apply(dfl,2,getfilteredsignal,mforder,bpforder, freqRange,samplingRate)}, error = function(e){ NA })
  })
  if(all(is.na(dat))){return('filtering error') }
  
  # Get HR for each filtered segment of each color
  dat <- dat %>% lapply(function(dfl){
    dfl = tryCatch({
      apply(dfl,2,getHR,samplingRate)}, error = function(e){ NA })
    dfl = as.data.frame(t(dfl))
    colnames(dfl) = c('hr','confidence')
    return(dfl)
  })
  if(all(is.na(dat))){return('HR calculation error') }
  
  return(dat)
  
}

#############################################################
# Required Sub Functions
#############################################################

# Bandpass and sorted mean filter the given signal
  
 getfilteredsignal <- function(x, mforder = 33, bpforder = 128, freqRange=c(2,25), samplingRate){
  
    # Defaults are set for 60Hz sampling rate
    x[is.na(x)]<-0
    x = x-mean(x) #Mean centering the signal
    
    # Bandpass filter the given time series data
    bandPassFilt = signal::fir1(bpforder-1, c(freqRange[1] * 2/samplingRate, freqRange[2] * 2/samplingRate),
                                type="pass", 
                                window = seewave::hamming.w(bpforder))
    x = signal::filtfilt(bandPassFilt, x)
    x = x[((bpforder/2)+1):(length(x)-(bpforder/2)+1)]
    
    # Sorted Mean filter the given signal
    y = 0*x
    for (i in seq((mforder+1)/2, length(x)-(mforder-1)/2,1)){
      tempseq <- sort(x[(i-((mforder-1)/2)):((i+((mforder-1)/2)))])
      y[i] = x[i]-mean(tempseq[1:(length(tempseq)-1)])
    }
    return(y)
 }
  
  # Given a processed time series find its period using autocorrelation and then convert it to HR (bpm)
  
 getHR <- function(x, samplingRate, minHR = 40, maxHR=200){
    x[is.na(x)] <- 0
    x <- stats::acf(x,lag.max = 1000, plot=F)$acf
    y <- 0*x
    y[round(60*samplingRate/maxHR):round(60*samplingRate/minHR)] = x[round(60*samplingRate/maxHR):round(60*samplingRate/minHR)]
    confidence = max(y)/max(x)
    hr = 60*samplingRate/(which(y==max(y))-1)
    
    # If hr or condidence is NaN, then return hr = 0 and confidence = 0
    if(is.na(confidence) || is.na(hr)){
      confidence = NA
      hr = NA
    }
    
    return(c(hr, confidence))
 }
  
 
