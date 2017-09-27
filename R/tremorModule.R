#####
## Given a quaternion - the following function will return a rotation matrix
####
QuaternionRotationMatrix <- function(q) {
  r11 <- q["w"]^2 + q["x"]^2 - q["y"]^2 - q["z"]^2
  r12 <- 2 * q["x"] * q["y"] + 2 * q["w"] * q["z"]
  r13 <- 2 * q["x"] * q["z"] - 2 * q["w"] * q["y"]
  r21 <- 2 * q["x"] * q["y"] - 2 * q["w"] * q["z"]
  r22 <- q["w"]^2 - q["x"]^2 + q["y"]^2 - q["z"]^2
  r23 <- 2 * q["y"] * q["z"] + 2 * q["w"] * q["x"]
  r31 <- 2 * q["x"] * q["z"] + 2 * q["w"] * q["y"]
  r32 <- 2 * q["y"] * q["z"] - 2 * q["w"] * q["x"]
  r33 <- q["w"]^2 - q["x"]^2 - q["y"]^2 + q["z"]^2
  matrix(as.numeric(c(r11, r12, r13,r21, r22, r23, r31, r32, r33)),
         nrow = 3,ncol = 3, byrow = TRUE)
}

#######
# Function to rotate the user acceleration
#######
get_quaternary_rotated_userAccel <- function(dat){
  attitude <- dat$attitude
  userAccel <- dat$userAcceleration
  colnames(userAccel) <- paste0('a_',colnames(userAccel))
  tmp_dat <- data.frame(attitude, userAccel)
  tmp_res <- apply(tmp_dat, 1, function(row){
    tmp_userAccel <- row[c('a_x', 'a_y', 'a_z')]
    tmp_userAccel <- as.matrix(tmp_userAccel, nrow=3)
    quaternion  <- row[c('x', 'y', 'z', 'w')]
    
    #create a rotation matrix from quaternion
    rotMatrix <- QuaternionRotationMatrix(row)
    
    #translate the user acceleration to uniform reference frame
    crossprod(rotMatrix, tmp_userAccel)
  })
  
  userAccelerationRotated <- t(tmp_res)
  colnames(userAccelerationRotated) <- c('x', 'y', 'z')
  userAccelerationRotated
}

#######
# Function to extract single axis features in time domain
#######
getTimeDomainFeatures <- function(accel, samplingRate, windowLen, ovlp){
  ftrs = data.frame(mean.tm = mean(accel, na.rm = TRUE),
                    median.tm = quantile(accel, probs = c(0.5), na.rm = TRUE),
                    mode.tm = pracma::Mode(accel),
                    sd.tm = sd(accel, na.rm = TRUE),
                    skewness.tm = e1071::skewness(accel),
                    kurtosis.tm = e1071::kurtosis(accel),
                    Q25.tm = quantile(accel, probs = c(0.25), na.rm = TRUE),
                    Q75.tm = quantile(accel, probs = c(0.75), na.rm = TRUE),
                    range.tm = max(accel, na.rm = T) - min(accel, na.rm = T),
                    rough.tm = seewave::roughness(accel),
                    rugo.tm = seewave::rugo(accel),
                    acf.tm = (stats::acf(accel, lag.max = 1, plot = FALSE)$acf[2, 1, 1]),
                    zcr.tm = mean(seewave::zcr(accel, f = samplingRate, wl = windowLen, ovlp = ovlp, plot = FALSE)[,'zcr'], na.rm = T),
                    mtkeo.tm = mean(seewave::TKEO(accel, f = samplingRate, plot = F)[,2], na.rm = T),
                    dfa.tm = fractal::DFA(accel, sum.order = 1)[[1]] ) %>%
    dplyr::mutate(IQR.tm = Q25.tm - Q75.tm)
  
  return(ftrs)
}

#######
# Function to extract single axis features in frequency domain
#######
getFreqDomainFeatures <- function(accel, samplingRate, windowLen, ovlp){
  
  # Sub-function to interpolate spectrogram at given frequencies
  interpolateFreq <- function(originalSpecDensity, originalFreq, interpolatedFreq){
    fn = stats::approxfun(originalFreq, originalSpecDensity)
    return(fn(interpolatedFreq))
  }
  
  # Split signal in to overlapping windows
  st.ind = seq(1,length(timestamp), by = ovlp*windowLen)
  end.ind = seq(windowLen,length(timestamp), by = ovlp*windowLen)
  st.ind = st.ind[1:length(end.ind)]
  
  wave = mapply(function(st, end, accel){
    seewave::hamming.w(windowLen) * accel[st:end]
  }, st.ind, end.ind, MoreArgs = list(accel), SIMPLIFY = T)
  
  # Find properties of spectrogram
  specGram = seewave::meanspec(accel, f = samplingRate, wl = windowLen, wn = 'hamming')
  ftrs =  specGram %>%
    seewave::specprop(f = samplingRate, wl = windowLen, wn = 'hamming') %>%
    as.data.frame()
  colnames(ftrs) = paste0(colnames(ftrs), '.freq')
  
  # Estimate fundamental frequency for each time slice
  tmp = seewave::fund(accel, f = samplingRate, wl = windowLen, ovlp = ovlp, fmax = freqRange[2], plot = FALSE)
  ftrs$mean.freq0 = mean(tmp[,'y']*1000, na.rm = T); ftrs$sd.freq0 = sd(tmp[,'y']*1000)
  
  # Estimate Lomb-scargle periodogram peaks with each time slice
  tmp = apply(wave, 2, function(x){
    tmp = lomb::lsp(x, times = (1:windowLen)/samplingRate, plot = FALSE)
    return(c(lsp.amp = tmp$peak, lsp.freq = tmp$peak.at[1]))
  }) %>% t
  ftrs$mean.lsp.amp = mean(tmp[,'lsp.amp'], na.rm = T);
  ftrs$sd.lsp.amp = sd(tmp[,'lsp.amp'])
  ftrs$mean.lsp.freq = mean(tmp[,'lsp.freq'], na.rm = T)
  ftrs$sd.lsp.freq = sd(tmp[,'lsp.freq'])
  
  # Energy estimation in a spetra
  ftrs$csh = mean(seewave::csh(accel, f = samplingRate, wl = windowLen, ovlp = ovlp, plot = FALSE)[,'h'], na.rm = T)
  
  return(ftrs)
}
  
  
####### MAIN
#' extracts features from tremor task accelerometer JSON data file
#'
#' @param tremor_task_json_file path to tremor accelerometer json file
#' @return data frame of tremor features with one column
#' @export
#' @examples
#' library(synapseClient)
#' synapseLogin()
#' sample_tremor_File <-'syn10790889'
#' walkingJsonFile <- synGet(sample_walking_File)@filePath
#' getWalkFeatures(walkingJsonFile)

getTremorFeatures <- function(tremor.json.file, windowLen = 256, freqRange = c(0.1, 25)) {
  # Get data from json file
  dat =jsonlite::fromJSON(as.character(tremor.json.file))
  
  # Rotate userAcceleration (x,y,z)
  timestamp = dat$timestamp
  userAcceleration = data.frame(get_quaternary_rotated_userAccel(dat))
  
  # Compute average userAcceleration
  userAcceleration$a = sqrt(rowSums(dat$userAcceleration^2))
  
  # Compute sampling rate
  samplingRate = length(timestamp)/(timestamp[length(timestamp)] - timestamp[1])
  
  # Order userAcceleration
  ind = order(timestamp)
  timestamp = timestamp[ind]
  userAcceleration = userAcceleration[ind, ]
  
  # Remove first and last second of data
  ind = round(samplingRate)+1:(length(timestamp)-round(samplingRate)-1)
  userAcceleration = userAcceleration[ind,]
  timestamp = timestamp[ind]
  
  # Remove data offset
  userAcceleration = apply(userAcceleration, 2, seewave::rmoffset, samplingRate)
  
  # Pass band filter between freqRange[1] and freqRange[2] Hz
  bandPassFilt = signal::fir1(windowLen-1, 
                              c(freqRange[1] * 2/samplingRate, freqRange[2] * 2/samplingRate), 
                              type="pass", 
                              window = hamming(windowLen))
  userAcceleration = apply(userAcceleration, 2, function(x, bandPassFilt) signal::filtfilt(bandPassFilt, x), bandPassFilt)
  
  # Get time domain features
  tmFeatures = apply(userAcceleration, 2, 
                     getTimeDomainFeatures, samplingRate, windowLen, ovlp) %>%
    data.table::rbindlist(idcol = 'axis')
  
  # Get freq domain features
  freqFeatures = apply(userAcceleration, 2, 
                       getFreqDomainFeatures, samplingRate, windowLen, ovlp) %>%
    data.table::rbindlist(idcol = 'axis')
  
  ftrs = plyr::join_all(list(tmFeatures, freqFeatures))
  
  return(ftrs)
}