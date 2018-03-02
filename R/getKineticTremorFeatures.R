####### MAIN
#' extracts features from tremor task handToNose accelerometer and gyroscope JSON data file
#'
#'
#' @param tremorJsonFileLoc path to tremor accelerometer json file
#' @return data frame of tremor features
#' @export
#' @examples
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla 
getKineticTremorFeatures <- function(tremorJsonFileLoc, windowLen = 256, freqRange = c(1, 25), ovlp = 0.5) {
  
  # If no json file exists
  ftrs = data.frame(Window = NA, error = NA)
  if(all(is.na(tremorJsonFileLoc))){ ftrs$error = 'No JSON file'; return(ftrs) }
  
  # Read contents of JSON file
  dat = tryCatch({ jsonlite::fromJSON(as.character(tremorJsonFileLoc)) }, 
                 error = function(e){ NA })
  if(all(is.na(dat))){ ftrs$error = 'JSON file read error'; return(ftrs) }
  
  # Get sampling rate
  samplingRate = length(dat$timestamp)/(dat$timestamp[length(dat$timestamp)] - dat$timestamp[1])
  
  # Get accelerometer features
  ftrs.acc = getKineticTremorFeatures.userAccel(dat, samplingRate, windowLen = windowLen, freqRange = freqRange, ovlp = ovlp)
  
  # Get accelerometer features
  ftrs.gyro = getKineticTremorFeatures.rotRate(dat, samplingRate, windowLen = windowLen, freqRange = freqRange, ovlp = ovlp)
  
  # Return if processing is errored
  if(!is.na(ftrs.acc$error) || !is.na(ftrs.gyro$error)){
    return(list(accelerometer = ftrs.acc, gyroscope = ftrs.gyro) %>%
             data.table::rbindlist(use.names = TRUE, fill = T, idcol = 'sensor'))
  }
  
  # Tag outliers windows based on phone rotation
  gr.error = lapply(dat$gravity, function(x) {
    accel = mpowertools:::windowSignal(x) %>%
      as.data.frame() %>%
      tidyr::gather(Window, value) %>%
      dplyr::group_by(Window) %>%
      dplyr::summarise(mx = max(value, na.rm = T),
                       mn = min(value, na.rm = T))
  }) %>%
    data.table::rbindlist(use.names = T, fill = T, idcol = 'axis') %>%
    dplyr::mutate(error = sign(mx) != sign(mn)) %>% 
    dplyr::group_by(Window) %>% 
    dplyr::summarise(error = any(error, na.rm = T))
  gr.error$error[gr.error$error == TRUE] = 'Phone rotated within window'
  gr.error$error[gr.error$error == FALSE] = 'None'
  
  # Combine all features
  ftrs = list(accelerometer = ftrs.acc, gyroscope = ftrs.gyro) %>%
    data.table::rbindlist(use.names = TRUE, fill = T, idcol = 'sensor') %>%
    tidyr::separate(Window, c('IMF','Window'), sep = '\\.') %>%
    dplyr::mutate(Window = as.character(Window)) %>%
    dplyr::select(-error) %>%
    dplyr::left_join(gr.error, by = 'Window')

  return(ftrs)
}  
  
# Function to extract kinetic tremor features from user acceleration from accelerometer
getKineticTremorFeatures.userAccel <- function(dat, samplingRate, windowLen = 256, freqRange = c(1, 25), ovlp = 0.5){  
  
  ftrs = data.frame(Window = NA, error = NA)

  # Get user acceleration data
  userAccel = tryCatch({
    userAccel = cbind(timestamp = dat$timestamp-dat$timestamp[1],
                      dat$userAcceleration) %>%
      as.data.frame()
    ind = order(userAccel$timestamp)
    userAccel = userAccel[ind, ] %>%
      tidyr::gather(axis, accel, -timestamp)
  }, error = function(e){ NA })
  if(all(is.na(userAccel))){ ftrs$error = 'userAccel extraction error'; return(ftrs) }
  
  # Detrend data
  userAccel = tryCatch({
    userAccel %>%
      plyr::ddply(.variables = 'axis', .fun = function(x){
        x$accel = loess(x$accel~x$timestamp)$residual
        return(x)
      })
  }, error = function(e){ NA })
  if(all(is.na(userAccel))){ ftrs$error = 'Detrend error'; return(ftrs) }
  
  # Band pass filter signal between freqRange
  userAccel = tryCatch({
    userAccel %>% 
      plyr::ddply(.variables = 'axis', .fun = function(x, windowLen, sl, freqRange){
        bandPassFilt = signal::fir1(windowLen-1, c(freqRange[1] * 2/sl, freqRange[2] * 2/sl),
                                    type="pass", 
                                    window = seewave::hamming.w(windowLen))
        x$accel = signal::filtfilt(bandPassFilt, x$accel)
        return(x)
      }, windowLen, samplingRate, freqRange)
  }, error = function(e){ NA })
  if(all(is.na(userAccel))){ ftrs$error = 'Band pass filter error'; return(ftrs) }
  
  # Filter signal between 1 and 9 sec
  userAccel = tryCatch({
    userAccel %>%
      dplyr::filter(timestamp >= 1, timestamp <= 9)
  }, error = function(e){ NA })
  if(all(is.na(userAccel))){ ftrs$error = 'Not enough time samples'; return(ftrs) }
  
  # Split user acceleration into EMDs and window
  userAccel = userAccel %>%
    plyr::dlply(.variables = 'axis', .fun = function(accel, windowLen, ovlp){
      imf =  EMD::emd(accel$accel, accel$timestamp, max.imf = 4)$imf %>%
        as.data.frame()
      colnames(imf) = paste0('IMF',1:dim(imf)[2])
      a = lapply(imf, mpowertools:::windowSignal, windowLen = windowLen, ovlp = ovlp)
      a = mapply(function(x,y){ 
        colnames(x) = paste(y,colnames(x),sep  = '.'); return(x)
      }, a, names(a), SIMPLIFY = F) %>%
        do.call(cbind,.)
    }, windowLen, ovlp)
  
  # Get user jerk
  userJerk = userAccel %>%
    lapply(function(accel, sl){
      apply(accel,2, diff)*sl
    }, samplingRate)
  
  # Get user velocity
  userVel = userAccel %>%
    lapply(function(accel, sl){
      apply(accel,2, diffinv)*sl
    }, samplingRate)
  
  # Get user displacement
  userDisp = userVel %>%
    lapply(function(accel, sl){
      apply(accel,2, diffinv)*sl
    }, samplingRate)
  
  # Get acf of user accel
  userACF = userAccel %>%
    lapply(function(accel, sl){
      apply(accel,2, function(x){acf(x, plot = F)$acf})
    }, samplingRate)
  
  # Get time and frequency domain features for angular velocity, acceleration, displacement, auto correlation of velocity
  ftrs = list(ua = userAccel, uj = userJerk, uv = userVel,  ud = userDisp, uaacf = userACF) %>%
    plyr::ldply(.fun = function(userAccel){
      plyr::ldply(userAccel, .fun = function(accel){
        list(apply(accel, 2, getTimeDomainSummary, samplingRate) %>%
               data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
             apply(accel, 2, getFrequencyDomainSummary, samplingRate = samplingRate, npeaks = 3) %>%
               data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
             apply(accel, 2, getFrequencyDomainEnergy, samplingRate) %>%
               data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')) %>%
          plyr::join_all(by = 'Window')
      }, .id = 'axis')
    }, .id = 'measurementType')%>%
    dplyr::mutate(error = NA)
  
  return(ftrs)
}

# Function to extract tremor features from user angular velocity from gyroscope
getKineticTremorFeatures.rotRate <- function(dat, samplingRate, windowLen = 256, freqRange = c(1, 25), ovlp = 0.5) {
  
  ftrs = data.frame(Window = NA, error = NA)
  
  # Get user angular velocity from gyro data
  userAngVel = tryCatch({
    userAngVel = cbind(timestamp = dat$timestamp-dat$timestamp[1],
                       dat$rotationRate) %>% 
      as.data.frame()
    ind = order(userAngVel$timestamp)
    userAngVel = userAngVel[ind, ] %>%
      tidyr::gather(axis, angvel, -timestamp)
  }, error = function(e){ NA })
  if(all(is.na(userAngVel))){ ftrs$error = 'userAccel rotation error'; return(ftrs) }
  
  # Detrend data
  userAngVel = tryCatch({
    userAngVel %>%
      plyr::ddply(.variables = 'axis', .fun = function(x){
        x$angvel = loess(x$angvel~x$timestamp)$residual
        x <- return(x)
      })
  }, error = function(e){ NA })
  if(all(is.na(userAngVel))){ ftrs$error = 'Detrend error'; return(ftrs) }
  
  # Band pass filter signal between freqRange
  userAngVel = tryCatch({
    userAngVel %>% 
      plyr::ddply(.variables = 'axis', .fun = function(x, windowLen, sl, freqRange){
        bandPassFilt = signal::fir1(windowLen-1, c(freqRange[1] * 2/sl, freqRange[2] * 2/sl),
                                    type="pass", 
                                    window = seewave::hamming.w(windowLen))
        x$angvel = signal::filtfilt(bandPassFilt, x$angvel)
        return(x)
      }, windowLen, samplingRate, freqRange)
  }, error = function(e){ NA })
  if(all(is.na(userAngVel))){ ftrs$error = 'Band pass filter error'; return(ftrs) }
  
  # Filter signal between 1 and 9 sec
  userAngVel = tryCatch({
    userAngVel %>%
      dplyr::filter(timestamp >= 1, timestamp <= 9)
  }, error = function(e){ NA })
  if(all(is.na(userAngVel))){ ftrs$error = 'Not enough time samples'; return(ftrs) }
  
  # Split user acceleration into EMDs and window
  userAngVel = userAngVel %>%
    plyr::dlply(.variables = 'axis', .fun = function(accel, windowLen, ovlp){
      imf =  EMD::emd(accel$angvel, accel$timestamp, max.imf = 4)$imf %>%
        as.data.frame()
      colnames(imf) = paste0('IMF',1:dim(imf)[2])
      a = lapply(imf, mpowertools:::windowSignal, windowLen = windowLen, ovlp = ovlp)
      a = mapply(function(x,y){ 
        colnames(x) = paste(y,colnames(x),sep  = '.'); return(x)
      }, a, names(a), SIMPLIFY = F) %>%
        do.call(cbind,.)
    }, windowLen, ovlp)
  
  # Get user angular acceleration
  userAngAcc = userAngVel %>%
    lapply(function(angvel, sl){
      apply(angvel, 2, diff)*sl
    }, samplingRate)
  
  # Get user angular displacement
  userAngDis = userAngVel %>%
    lapply(function(angvel, sl){
      apply(angvel,2, diffinv)*sl
    }, samplingRate)
  
  # Get user acf (ACF of user angular velocity)
  userACF = userAngVel %>%
    lapply(function(angvel, sl){
      apply(angvel,2, function(x){acf(x, plot = F)$acf})
    }, samplingRate)
  
  # Get time and frequency domain features for angular velocity, acceleration, displacement, auto correlation of velocity
  ftrs = list(uav = userAngVel, uaa = userAngAcc, uad = userAngDis,  uavacf = userACF) %>%
    plyr::ldply(.fun = function(userAccel){
      plyr::ldply(userAccel, .fun = function(accel){
        list(apply(accel, 2, getTimeDomainSummary, samplingRate) %>%
               data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
             apply(accel, 2, getFrequencyDomainSummary, samplingRate = samplingRate, npeaks = 3) %>%
               data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
             apply(accel, 2, getFrequencyDomainEnergy, samplingRate) %>%
               data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')) %>%
          plyr::join_all(by = 'Window')
      }, .id = 'axis')
    }, .id = 'measurementType') %>%
    dplyr::mutate(error = NA)
  
  return(ftrs)
}