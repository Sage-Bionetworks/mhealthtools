#' Extract heart rate for each color band from avg pixel value per frame of video (processed hr)
#'
#' @param dat A data frame with columns timestamp, red, green and blue
#' @param windowLen Length of the time window in seconds, to be considered
#' while calculating the heart rate for each channel
#' @param freqRange Frequency range in Hz for the bandpass filter parameters
#' @param bpforder Order (length) of the bandpass filter to be used for filtering
#' @return list containing heart rate and confidence of the estimate for each color (red, green, blue)
#' @export
#' @author Meghasyam Tummalacherla 

#############################################################
# Wrapper function to take in json and give HR per color channel
#############################################################

get_heartrate <- function(dat, windowLen = 10, freqRange = c(1,25), bpforder = 128){
  
  #############################################################
  # Main Code Block
  #############################################################
  
  dat1 = data.frame(red = NA, green = NA, blue = NA, error = NA, samplingRate = NA)
  
  # Get sampling rate
  samplingRate = tryCatch({ length(dat$timestamp)/(dat$timestamp[length(dat$timestamp)] - dat$timestamp[1]) }, 
                          error = function(e){ NA })
  if(!is.finite(samplingRate)){dat1$error = 'Sampling Rate calculated from timestamp is Inf or NaN / timestamp not found in json'; return(dat1) }
  
  if(samplingRate < 55){if(samplingRate > 22){bpforder = 64}else{bpforder = 32}}
  
  # Convert window length from seconds to samples
  windowLen = round(samplingRate*windowLen)
  
  # Apply pre processing filter signal between freqRange
  mforder = 2*round(60*samplingRate/220) + 1 # order for the running mean based filter
  
  # Split each color into segments based on windowLen
  dat = tryCatch({ dat %>% dplyr::select(red, green, blue) %>% na.omit() %>% 
      lapply(mhealthtools:::windowSignal, windowLen, 0.5) }, 
      error = function(e){ NA })
  if(all(is.na(dat))){dat1$error = 'red, green, blue cannot be read from JSON'; return(dat1) }
  
  # Apply filter to each segment of each color
  dat <- dat %>% lapply(function(dfl){
    dfl[is.na(dfl)] <- 0
    dfl = tryCatch({
      apply(dfl,2,mhealthtools:::getfilteredsignal,mforder,bpforder, freqRange,samplingRate)}, error = function(e){ NA })
  })
  if(all(is.na(dat))){dat1$error = 'filtering error'; return(dat1) }
  
  # Get HR for each filtered segment of each color
  dat <- dat %>% lapply(function(dfl){
    dfl = tryCatch({
      apply(dfl,2,mhealthtools:::getHrFromTimeSeries,samplingRate)}, error = function(e){ NA })
    dfl = as.data.frame(t(dfl))
    colnames(dfl) = c('hr','confidence')
    return(dfl)
  })
  if(all(is.na(dat))){dat1$error = 'HR calculation error'; return(dat1) }
  
  dat$error = 'none'
  if(samplingRate < 55){dat$error = 'Low Sampling Rate,atleast 55FPS needed'}
  dat$samplingRate = samplingRate
  return(dat)
  
}

#############################################################
# Required Sub Functions
#############################################################
#' Bandpass and sorted mean filter the given signal
#'
#' @param x A time series numeric data
#' @param mforder Length of the sorted mean filter window
#' @param freqRange Frequency range in Hz for the bandpass filter parameters
#' @param bpforder Order (length) of the bandpass filter to be used for filtering
#' @param samplingRate The sampling rate (fs) of the time series data
#' @return The filtered time series data

getfilteredsignal <- function(x, mforder = 33, bpforder = 128, freqRange=c(2,25), samplingRate){
  
  # Defaults are set for 60Hz sampling rate
  x[is.na(x)]<-0
  x = x-mean(x) #Mean centering the signal
  
  # Bandpass filter the given time series data
  if(samplingRate > 55){ 
    bandPassFilt = signal::fir1(bpforder-1, c(freqRange[1] * 2/samplingRate, freqRange[2] * 2/samplingRate),
                                type="pass", 
                                window = seewave::hamming.w(bpforder))
  }else{
    bandPassFilt = signal::fir1(bpforder, freqRange[1] * 2/samplingRate, # the order for a high pass filter needs to be even
                                type="high", 
                                window = seewave::hamming.w(bpforder+1))   
  }
  
  x = signal::filtfilt(bandPassFilt, x)
  x = x[((bpforder/2)+1):(length(x)-(bpforder/2)+1)]
  
  # Sorted Mean filter the given signal
  y = 0*x
  for (i in seq((mforder+1)/2, length(x)-(mforder-1)/2,1)){
    tempseq <- (x[(i-((mforder-1)/2)):((i+((mforder-1)/2)))])
    # y[i] = x[i]-(sum(tempseq)-max(tempseq))/(mforder-1)
    
    tempseq <- tempseq - mean(tempseq)
    y[i] = (((x[i] - max(tempseq) - min(tempseq))-(sum(tempseq)-max(tempseq))/(mforder-1))/(max(tempseq)-min(tempseq) + 0.0000001))
  }
  return(y)
}

#' Given a processed time series find its period using autocorrelation and then convert it to heart rate (bpm)
#'
#' @param x A time series numeric data
#' @param samplingRate The sampling rate (fs) of the time series data
#' @param minHR Minimum expected heart rate
#' @param maxHR Maximum expected heart rate
#' @return A named vector containing heart rate and the confidence of the result 

getHrFromTimeSeries <- function(x, samplingRate, minHR = 40, maxHR=200){
  x[is.na(x)] <- 0
  x <- stats::acf(x,lag.max = 1000, plot=F)$acf
  y <- 0*x
  y[round(60*samplingRate/maxHR):round(60*samplingRate/minHR)] = x[round(60*samplingRate/maxHR):round(60*samplingRate/minHR)]
  confidence = max(y)/max(x)
  hr = 60*samplingRate/(which.max(y)-1)
  
  # If hr or condidence is NaN, then return hr = 0 and confidence = 0
  if(is.na(confidence) || is.na(hr)){
    confidence = NA
    hr = NA
  }
  
  return(c(hr, confidence))
}


