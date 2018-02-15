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
# wl - window length
# ovlp - window overlap

# a - returns a matrix of wl x nwindows windowed acceleration matrix
#####
windowSignal <- function(accel, wl = 256, ovlp = 0.5){
  nlen = length(accel)
  nstart = seq(1, nlen, wl*ovlp)
  nend = seq(wl, nlen, wl*ovlp)
  nstart = nstart[1:length(nend)]
  wn = seewave::hamming.w(wl)
  
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
                    dfa = fractal::DFA(accel, sum.order = 1)[[1]]) %>%
    dplyr::mutate(IQR = Q25 - Q75,
                  complexity = sqrt(var(diff(diff(accel)*samplingRate)*samplingRate)/var(diff(accel)*samplingRate)))
  names(ftrs) = paste0(names(ftrs),'.tm')
  return(ftrs)
}

#####
## Get frequency domain features - Given a acceleration vector this function will return features characterising the time series in frequency domain
# accel - timeseries vector of length n
# samplingRate - samplingRate of the signal (by default it is 100 Hz)

# ftrs - A features data frame of dimension 1 x 13
#####
getFrequencyDomainSummary <- function(accel, samplingRate = 100){
  spect = getSpectrum(accel, samplingRate)
  freq = spect$freq
  pdf = spect$pdf/sum(spect$pdf, na.rm = T)
  cdf = cumsum(pdf)
  
  ftrs = list()
  ftrs$mn = sum(freq * pdf)
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