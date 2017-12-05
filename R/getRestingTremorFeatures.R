####### MAIN
#' extracts features from tremor task handInLap or handAtShoulderLength accelerometer JSON data file
#'
#'
#' @param tremorJsonFileLoc path to tremor accelerometer json file
#' @return data frame of tremor features
#' @export
#' @examples
getRestingTremorFeatures <- function(tremorJsonFileLoc, windowLen = 256, freqRange = c(1, 25), ovlp = 0.5) {
  
  # If no json file exists
  ftrs = data.frame(error = NA)
  if(is.na(tremorJsonFileLoc)){
    ftrs$error = 'No JSON file'; return(ftrs) 
  }
  
  # Get accelerometer data
  tryCatch({
    dat = jsonlite::fromJSON(as.character(tremorJsonFileLoc))
    samplingRate = length(dat$timestamp)/(dat$timestamp[length(dat$timestamp)] - dat$timestamp[1])
  }, error = function(e){
    ftrs$error = 'JSON file read error'; return(ftrs) 
  })
  
  # Rotate acceleration data to earth co-ordinates
  tryCatch({
    userAccel = cbind(timestamp = dat$timestamp-dat$timestamp[1],
                      mpowertools:::get_quaternary_rotated_userAccel(dat)) %>%
      as.data.frame()
    ind = order(userAccel$timestamp)
    userAccel = userAccel[ind, ] %>%
      tidyr::gather(axis, accel, -timestamp)
  }, error = function(e){
    ftrs$error = 'userAccel Rotation error'; return(ftrs) 
  })
  
  # Detrend data
  tryCatch({
    userAccel = userAccel %>%
      plyr::ddply(.(axis), .fun = function(x){
        x$accel = loess(x$accel~x$timestamp)$residual
        return(x)
      })
  }, error = function(e){
    ftrs$error = 'Detrend error'; return(ftrs) 
  })
  
  # Band pass filter signal between freqRange
  tryCatch({
    userAccel = userAccel %>%
      plyr::ddply(.(axis), .fun = function(x, wl, sl, freqRange){
        bandPassFilt = signal::fir1(wl-1, c(freqRange[1] * 2/sl, freqRange[2] * 2/sl),
                                    type="pass", 
                                    window = seewave::hamming.w(wl))
        x$accel = signal::filtfilt(bandPassFilt, x$accel)
        return(x)
      }, windowLen, samplingRate, freqRange)
  }, error = function(e){
    ftrs$error = 'Band pass filter error'; return(ftrs) 
  })
  
  # Filter signal between 1 and 9 sec
  tryCatch({
    userAccel = userAccel %>%
      dplyr::filter(timestamp >= 1, timestamp <= 9)
  }, error = function(e){
    ftrs$error = 'Not enough time samples'; return(ftrs) 
  })
  
  # Split user acceleration into windows
  userAccel  = userAccel %>%
    plyr::dlply(.(axis), .fun = function(accel, wl, ovlp){
      a = windowSignal(accel$accel, wl = wl, ovlp = ovlp) 
    }, windowLen, ovlp)
  
  # Get time and frequency domain features for userAcceleration
  ftrs.accel = userAccel %>%
    lapply(function(accel){
      list(apply(accel, 2, getTimeDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, getFrequencyDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, getFrequencyDomainEnergy) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')) %>%
        plyr::join_all()
    }) %>%
    data.table::rbindlist(use.names = T, fill = T, idcol = 'axis')
  colnames(ftrs.accel)[-(1:2)] = paste0(colnames(ftrs.accel)[-(1:2)], '.ua')
  
  # Get user jerk
  userJerk = userAccel %>%
    lapply(function(accel, sl){
      apply(accel,2, diff)*sl
    }, samplingRate)
  
  # Get time and frequency domain features for jerk
  ftrs.jerk = userJerk %>%
    lapply(function(accel){
      list(apply(accel, 2, getTimeDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, getFrequencyDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, getFrequencyDomainEnergy) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')) %>%
        plyr::join_all()
    }) %>%
    data.table::rbindlist(use.names = T, fill = T, idcol = 'axis')
  colnames(ftrs.jerk)[-(1:2)] = paste0(colnames(ftrs.jerk)[-(1:2)], '.uj')
  
  # Get user acf
  userACF = userAccel %>%
    lapply(function(accel, sl){
      apply(accel,2, function(x){acf(x)$acf})
    }, samplingRate)
  
  # Get time and frequency domain features for acf
  ftrs.acf = userACF %>%
    lapply(function(accel){
      list(apply(accel, 2, getTimeDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, getFrequencyDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, getFrequencyDomainEnergy) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')) %>%
        plyr::join_all()
    }) %>%
    data.table::rbindlist(use.names = T, fill = T, idcol = 'axis')
  colnames(ftrs.acf)[-(1:2)] = paste0(colnames(ftrs.acf)[-(1:2)], '.acf')
  
  # Combine all features
  ftrs = list(ftrs.accel, ftrs.jerk, ftrs.acf) %>%
    plyr::join_all()
  
  return(ftrs)
}