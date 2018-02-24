#####
## Given a quaternion the following function will return a rotation matrix
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
## Function to rotate the user acceleration to earth co-ordinates
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

#####
## Window signal - Given a acceleration vector this function will return a windowed signal with hamming window
# accel - timeseries vector of length n
# windowLen - window length
# ovlp - window overlap

# a - returns a matrix of windowLen x nwindows windowed acceleration matrix
#####
windowSignal <- function(accel, windowLen = 256, ovlp = 0.5){
  nlen = length(accel)
  
  # If length of signal is less than window length
  if (nlen < windowLen){
    windowLen = nlen; ovlp = 1;
  }
    
  nstart = seq(1, nlen, windowLen*ovlp)
  nend = seq(windowLen, nlen, windowLen*ovlp)
  nstart = nstart[1:length(nend)]
  wn = seewave::hamming.w(windowLen)
  
  a = apply(cbind(nstart,nend), 1, function(x, a, wn){
    a[seq(x[1],x[2],1)]*wn
  }, accel, wn)
  colnames(a) = paste0('Window',1:dim(a)[2])
  return(a)
}

#####
## Get AR spectrum - Given a acceleration vector this function will return a spectrum with all pole AR model
# accel - timeseries vector of length n
# samplingRate - samplingRate of the signal (by default it is 100 Hz)
# n.freq - Number of frequecy points to be interpolated

# spect - Returns an AR spectrum
#####
getSpectrum <- function(accel, samplingRate = 100, n.freq = 500){
  tmp = stats::spec.ar(accel, n.freq = n.freq, plot = F)
  spect = data.frame(freq = tmp$freq * samplingRate, pdf = tmp$spec)
  return(spect)
}

#####
## Get time domain features - Given a acceleration vector this function will return features characterising the time series in time domain
# accel - timeseries vector of length n
# samplingRate - samplingRate of the signal (by default it is 100 Hz)

# ftrs - A features data frame of dimension 1 x 19
#####
getTimeDomainSummary <- function(accel, samplingRate = 100){
  ftrs = data.frame(mean = mean(accel, na.rm = TRUE),
                    median = quantile(accel, probs = c(0.5), na.rm = TRUE),
                    mode = pracma::Mode(accel),
                    mx =  max(accel, na.rm = T),
                    mn = min(accel, na.rm = T),
                    sd = sd(accel, na.rm = TRUE),
                    skewness = e1071::skewness(accel),
                    kurtosis = e1071::kurtosis(accel),
                    Q25 = quantile(accel, probs = c(0.25), na.rm = TRUE),
                    Q75 = quantile(accel, probs = c(0.75), na.rm = TRUE),
                    range = max(accel, na.rm = T) - min(accel, na.rm = T),
                    rough = seewave::roughness(accel),
                    rugo = seewave::rugo(accel),
                    energy = sum(accel^2),
                    mobility = sqrt(var(diff(accel)*samplingRate)/var(accel)),
                    mtkeo = mean(seewave::TKEO(accel, f = samplingRate, plot = F)[,2], na.rm = T),
                    dfa = fractal::DFA(accel, sum.order = 1)[[1]],
                    rmsmag = sqrt(sum(accel^2)/length(accel))) %>%  # Root Mean Square magnitude
    dplyr::mutate(IQR = Q25 - Q75,
                  complexity = sqrt(var(diff(diff(accel)*samplingRate)*samplingRate)/var(diff(accel)*samplingRate)))
  names(ftrs) = paste0(names(ftrs),'.tm')
  return(ftrs)
}

#####
## Get frequency domain features - Given a acceleration vector this function will return features characterising the time series in frequency domain
# accel - timeseries vector of length n
# samplingRate - samplingRate of the signal (by default it is 100 Hz)
# npeaks - Number of peaks to be computed in EWT

# ftrs - A features data frame of dimension 1 x 13
#####
getFrequencyDomainSummary <- function(accel, samplingRate = 100, npeaks = 3){
  spect = getSpectrum(accel, samplingRate)
  freq = spect$freq
  pdf = spect$pdf/sum(spect$pdf, na.rm = T)
  cdf = cumsum(pdf)
  
  # Get STFT spectrum based features
  ftrs = list()
  ftrs$mn = sum(freq * pdf)
  ftrs$mx = max(pdf)
  ftrs$sd = sqrt(sum(pdf * ((freq - ftrs$mn)^2)))
  ftrs$sem = ftrs$sd/sqrt(dim(spect)[1])
  ftrs$md = freq[length(cdf[cdf <= 0.5]) + 1]
  ftrs$mod = freq[which.max(pdf)]
  ftrs$Q25 = freq[length(cdf[cdf <= 0.25]) + 1]
  ftrs$Q75 = freq[length(cdf[cdf <= 0.75]) + 1]
  ftrs$IQR = ftrs$Q75 - ftrs$Q25
  ftrs$cent = sum(freq * pdf)
  w = sd(pdf)
  ftrs$skew = (sum((pdf - mean(pdf))^3)/(dim(spect)[1] - 1))/w^3
  ftrs$kurt = (sum((pdf - mean(pdf))^4)/(dim(spect)[1] - 1))/w^4
  ftrs$sfm = seewave::sfm(pdf)
  ftrs$sh = seewave::sh(pdf)
  
  # Get EWT spectrum
  ewSpect = data.frame(freq = freq, pdf = pdf) %>%
    getEWTspectrum(samplingRate = samplingRate, npeaks = npeaks)
  
  # Compute normalised point energies of each EW spctrum
  ewEnergy = colSums(ewSpect^2, na.rm = T)
  ewEnergy = ewEnergy/sum(ewEnergy, na.rm = T)
  
  # Compute entropy with EWT approximated energies
  ftrs$ewt.permEnt = statcomp::permutation_entropy(ewEnergy)
  ftrs$ewt.shannonEnt = seewave::sh(ewEnergy, alpha = 'shannon')
  ftrs$ewt.simpsonEnt = seewave::sh(ewEnergy, alpha = 'simpson')
  ftrs$ewt.renyiEnt = seewave::sh(ewEnergy, alpha = 2) # alpha is hardcoded to be 2
  ftrs$ewt.tsallisEnt = (1-sum(ewEnergy^0.1))/(0.1-1) # q is hardcoded to be 0.1
  
  names(ftrs) = paste0(names(ftrs),'.fr')

  return(data.frame(ftrs))
}

#####
## Get frequency domain features - Given a acceleration vector this function will return features characterising the time series in frequency domain
# accel - timeseries vector of length n
# samplingRate - samplingRate of the signal (by default it is 100 Hz)

# ftrs - A features data frame of dimension 1 x 48
#####
getFrequencyDomainEnergy <- function(accel, samplingRate = 100){
  spect = getSpectrum(accel, samplingRate)
  freq = spect$freq
  pdf = spect$pdf/sum(spect$pdf, na.rm = T)
  cdf = cumsum(pdf)
  
  st = seq(1,24.5,0.5)
  en = seq(1.5,25,0.5)
  
  ftrs = mapply(function(indStr, indEn){
    ind = which(freq >= indStr & freq <= indEn)
    pracma::trapz(freq[ind], pdf[ind])
  }, st, en) %>% t %>% data.frame()
  colnames(ftrs) = paste0('EnergyInBand',gsub('\\.','_',st))
  
  return(ftrs)
}

#####
## Get log spectral distance - Given two acceleration vector this function will return features characterising the distance between two spectrum
# accel.ref - reference timeseries vector of length n
# accel.agt - against timeseries vector of length n
# samplingRate.ref - samplingRate of the reference signal (by default it is 100 Hz)
# samplingRate.agt - samplingRate of the against signal (by default it is 100 Hz)

# lsd - A features data frame of dimension 1 x 1
#####
getLogSpectralDistance <- function(accel.ref, accel.agt, samplingRate.ref = 100, samplingRate.agt = 100){
  # Get spectrogram
  spect.ref = mpowertools:::getSpectrum(accel.ref, samplingRate = samplingRate.ref)
  spect.ref = splinefun(spect.ref$freq, spect.ref$pdf)
  spect.agt = mpowertools:::getSpectrum(accel.agt, samplingRate = samplingRate.agt)
  spect.agt = splinefun(spect.agt$freq, spect.agt$pdf)
  
  lsd = data.frame(freq = seq(0,25,0.1)) %>%
    dplyr::mutate(ref = spect.ref(freq),
                  agt = spect.agt(freq),
                  lsd = 10*log10(ref/agt)^2)
  lsd = data.frame(lsd.fr = sqrt(pracma::trapz(lsd$freq, lsd$lsd)))
  return(lsd)
}

#####
## Get EWT spectrum - Given a time series vector this function will return Emprical Wavelet Transformed spectrum
# spect - FFT spectrum as a two dimensional data frame with columns names as freq and pdf respectively n.freq x 2
# npeaks - Number of peaks to be captured 
# fractionMinPeakHeight - minimum height (relative to maximum peak height) a peak has to have.Specified as fraction between 0 and 1.
# minPeakDistance - the minimum distance (in indices) peaks have to have to be counted 
# samplingRate - samplingRate of the signal (by default it is 100 Hz)

# ewSpect - Emprical wavelet transformed spectrum of dimension n.freq x (npeaks + 1)
#####
getEWTspectrum <- function(spect, npeaks = 3, fractionMinPeakHeight = 0.1, minPeakDistance = 1, samplingRate = 100){
  
  # Find top peaks for EWT calculation
  peakFreqs = pracma::findpeaks(spect$pdf, 
                                minpeakheight = fractionMinPeakHeight * max(spect$pdf, na.rm = T),
                                minpeakdistance = minPeakDistance, 
                                npeaks = npeaks,
                                sortstr = TRUE)
  
  # Convert peak frequency to radians and find mid points
  peakFreqs = spect$freq[sort(peakFreqs[,2])] * pi * 2 / samplingRate
  peakFreqs = unique(c(0,peakFreqs,pi))
  midPeakFreqs = c(0, peakFreqs[-length(peakFreqs)] + diff(peakFreqs)/2, pi)
  
  # Choose optimal scaling operator for the transition widths
  numeratorvec = midPeakFreqs[2:(length(midPeakFreqs)+2)] - midPeakFreqs[1:(length(midPeakFreqs)+1)]
  denominatorvec = midPeakFreqs[2:(length(midPeakFreqs)+2)] + midPeakFreqs[1:(length(midPeakFreqs)+1)]
  optimalGamma = min(numeratorvec/denominatorvec, na.rm = TRUE)
  
  # Compute emprical scaling and wavelets
  empricalWavelets = purrr::map2(midPeakFreqs[1:(length(midPeakFreqs)-1)], 
                                 midPeakFreqs[2:length(midPeakFreqs)],
                                 .f = function(wn1, wn2, n.freq, optimalGamma){
                                   # Compute emprical scaling function for the first peak
                                   phi.sy = rep(0, n.freq)
                                   w = seq(0, pi, len = n.freq)
                                   
                                   # Compute beta (an arbitary coefficient)
                                   x = (1/(2*optimalGamma*wn1)) * (abs(w) - (1-optimalGamma) * wn1)
                                   beta1 = x^4*(35-84*x+70*x^2-20*x^3)
                                   
                                   x = (1/(2*optimalGamma*wn2)) * (abs(w) - (1-optimalGamma) * wn2)
                                   beta2 = x^4*(35-84*x+70*x^2-20*x^3)
                                   
                                   if(wn2 != pi){
                                     # Compute scaling/wavelets for different conditions
                                     ind = ((1+optimalGamma)*wn1 <= abs(w)) & (abs(w) <= (1-optimalGamma) * wn2)
                                     phi.sy[ind] = 1
                                     ind = ((1 - optimalGamma) * wn2 <= abs(w)) & (abs(w) <= (1 + optimalGamma) * wn2)
                                     phi.sy[ind] = cos(pi*beta2[ind]/2)
                                     ind = ((1 - optimalGamma) * wn1 <= abs(w)) & (abs(w) <= (1 + optimalGamma) * wn1)
                                     phi.sy[ind] = sin(pi*beta1[ind]/2)
                                   } else {
                                     # Compute scaling/wavelets for different conditions
                                     ind = abs(w) <= (1-optimalGamma) * wn1
                                     phi.sy[ind] = 1
                                     ind = ((1 - optimalGamma) * wn1 <= abs(w)) & (abs(w) <= (1 + optimalGamma) * wn1)
                                     phi.sy[ind] = cos(pi*beta1[ind]/2)
                                     phi.sy = 1 - phi.sy
                                   }
                                   
                                   return(phi.sy)
                                 }, 
                                 dim(spect)[1], optimalGamma)
  
  # Compute EW modified spectrum
  ewSpect = sapply(empricalWavelets, function(x, spect){spect$pdf*x}, spect)
  
  return(ewSpect)
}