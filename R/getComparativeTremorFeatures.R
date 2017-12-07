####### MAIN
#' extracts features from tremor task handInLap or handAtShoulderLength accelerometer JSON data file by comparing left against right
#'
#'
#' @param tremorRefJsonFileLoc and tremorAgtJsonFileLoc path to tremor accelerometer json file
#' @return data frame of tremor features
#' @export
#' @examples
getComparativeTremorFeatures <- function(tremorRefJsonFileLoc, tremorAgtJsonFileLoc, windowLen = 256, freqRange = c(1, 25), ovlp = 0.5) {
  
  # If no json file exists
  ftrs = data.frame(error = NA)
  if(is.na(tremorRefJsonFileLoc)){ 
    ftrs$error = 'No reference JSON file'; return(ftrs) 
  }
  if(is.na(tremorAgtJsonFileLoc)){ 
    ftrs$error = 'No against JSON file'; return(ftrs) 
  }
  
  # Get accelerometer data
  dat.ref = tryCatch({ 
    jsonlite::fromJSON(as.character(tremorRefJsonFileLoc)) 
    }, error = function(e){ NA })
  if(is.na(dat.ref)){ ftrs$error = 'Reference JSON file read error'; return(ftrs) }
  
  dat.agt = tryCatch({ 
    jsonlite::fromJSON(as.character(tremorAgtJsonFileLoc)) 
  }, error = function(e){ NA })
  if(is.na(dat.ref)){ ftrs$error = 'Against JSON file read error'; return(ftrs) }
  
  # Get sampling rate
  samplingRate.ref = length(dat.ref$timestamp)/(dat.ref$timestamp[length(dat.ref$timestamp)] - dat.ref$timestamp[1])
  samplingRate.agt = length(dat.agt$timestamp)/(dat.agt$timestamp[length(dat.agt$timestamp)] - dat.agt$timestamp[1])
  
  # Rotate acceleration data to earth co-ordinates
  userAccel.ref = tryCatch({
    userAccel = cbind(timestamp = dat.ref$timestamp-dat.ref$timestamp[1],
                      mpowertools:::get_quaternary_rotated_userAccel(dat.ref)) %>%
      as.data.frame()
    ind = order(userAccel$timestamp)
    userAccel = userAccel[ind, ] %>%
      tidyr::gather(axis, accel, -timestamp)
  }, error = function(e){ NA })
  if(is.na(userAccel.ref)){ ftrs$error = 'Reference userAccel rotation error'; return(ftrs) }
  
  userAccel.agt = tryCatch({
    userAccel = cbind(timestamp = dat.agt$timestamp-dat.agt$timestamp[1],
                      mpowertools:::get_quaternary_rotated_userAccel(dat.agt)) %>%
      as.data.frame()
    ind = order(userAccel$timestamp)
    userAccel = userAccel[ind, ] %>%
      tidyr::gather(axis, accel, -timestamp)
  }, error = function(e){ NA })
  if(is.na(userAccel.agt)){ ftrs$error = 'Against userAccel rotation error'; return(ftrs) }
  
  # Detrend data
  userAccel.ref = tryCatch({
    userAccel.ref %>%
      plyr::ddply(.(axis), .fun = function(x){
        x$accel = loess(x$accel~x$timestamp)$residual
        return(x)
      })
  }, error = function(e){ NA })
  if(is.na(userAccel.ref)){ ftrs$error = 'Reference detrend error'; return(ftrs) }
  
  userAccel.agt = tryCatch({
    userAccel.agt %>%
      plyr::ddply(.(axis), .fun = function(x){
        x$accel = loess(x$accel~x$timestamp)$residual
        return(x)
      })
  }, error = function(e){ NA })
  if(is.na(userAccel.agt)){ ftrs$error = 'Against detrend error'; return(ftrs) }
  
  # Band pass filter signal between freqRange
  userAccel.ref = tryCatch({
    userAccel.ref %>% 
      plyr::ddply(.(axis), .fun = function(x, wl, sl, freqRange){
        bandPassFilt = signal::fir1(wl-1, c(freqRange[1] * 2/sl, freqRange[2] * 2/sl),
                                    type="pass", 
                                    window = seewave::hamming.w(wl))
        x$accel = signal::filtfilt(bandPassFilt, x$accel)
        return(x)
      }, windowLen, samplingRate.ref, freqRange)
  }, error = function(e){ NA })
  if(is.na(userAccel.ref)){ ftrs$error = 'Reference band pass filter error'; return(ftrs) }
  
  userAccel.agt = tryCatch({
    userAccel.agt %>% 
      plyr::ddply(.(axis), .fun = function(x, wl, sl, freqRange){
        bandPassFilt = signal::fir1(wl-1, c(freqRange[1] * 2/sl, freqRange[2] * 2/sl),
                                    type="pass", 
                                    window = seewave::hamming.w(wl))
        x$accel = signal::filtfilt(bandPassFilt, x$accel)
        return(x)
      }, windowLen, samplingRate.agt, freqRange)
  }, error = function(e){ NA })
  if(is.na(userAccel.agt)){ ftrs$error = 'Against band pass filter error'; return(ftrs) }
  
  # Filter signal between 1 and 9 sec
  userAccel.ref = tryCatch({
    userAccel.ref %>%
      dplyr::filter(timestamp >= 1, timestamp <= 9)
  }, error = function(e){ NA })
  if(is.na(userAccel.ref)){ ftrs$error = 'Reference not enough time samples'; return(ftrs) }
  
  userAccel.agt = tryCatch({
    userAccel.agt %>%
      dplyr::filter(timestamp >= 1, timestamp <= 9)
  }, error = function(e){ NA })
  if(is.na(userAccel.agt)){ ftrs$error = 'Reference not enough time samples'; return(ftrs) }
  
  # Split user acceleration into windows
  userAccel.ref  = userAccel.ref %>%
    plyr::dlply(.(axis), .fun = function(accel, wl, ovlp){
      a = mpowertools:::windowSignal(accel$accel, wl = wl, ovlp = ovlp) 
    }, windowLen, ovlp)
  
  userAccel.agt  = userAccel.agt %>%
    plyr::dlply(.(axis), .fun = function(accel, wl, ovlp){
      a = mpowertools:::windowSignal(accel$accel, wl = wl, ovlp = ovlp) 
    }, windowLen, ovlp)
  
  # Get log spectral distance
  ftrs.accel = userAccel.ref %>%
    plyr::ldply(.fun = function(accel.ref, userAccelAgt, samplingRate.ref, samplingRate.agt){
      apply(accel.ref, 2, function(accelRef, userAccelAgt, samplingRate.ref, samplingRate.agt){
        lsd.fr = do.call(cbind, userAccelAgt) %>%
          apply(2, function(accelAgt, accelRef, samplingRate.ref, samplingRate.agt){
            getLogSpectralDistance(accelRef, accelAgt, samplingRate.ref, samplingRate.agt)
          }, accelRef, samplingRate.ref, samplingRate.agt) %>%
          unlist() %>% median(na.rm = T)
        return(data.frame(lsd.fr = lsd.fr))
      }, userAccelAgt, samplingRate.ref, samplingRate.agt) %>%
        data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')
    }, userAccel.agt, samplingRate.ref, samplingRate.agt, .id = 'axis')
  colnames(ftrs.accel)[-(1:2)] = paste0(colnames(ftrs.accel)[-(1:2)], '.ua')
  
  # Get user jerk
  userJerk.ref = userAccel.ref %>%
    lapply(function(accel, sl){
      apply(accel,2, diff)*sl
    }, samplingRate.ref)
  
  userJerk.agt = userAccel.agt %>%
    lapply(function(accel, sl){
      apply(accel,2, diff)*sl
    }, samplingRate.agt)
  
  # Get log spectral distance
  ftrs.jerk = userJerk.ref %>%
    plyr::ldply(.fun = function(accel.ref, userAccelAgt, samplingRate.ref, samplingRate.agt){
      apply(accel.ref, 2, function(accelRef, userAccelAgt, samplingRate.ref, samplingRate.agt){
        lsd.fr = do.call(cbind, userAccelAgt) %>%
          apply(2, function(accelAgt, accelRef, samplingRate.ref, samplingRate.agt){
            getLogSpectralDistance(accelRef, accelAgt, samplingRate.ref, samplingRate.agt)
          }, accelRef, samplingRate.ref, samplingRate.agt) %>%
          unlist() %>% median(na.rm = T)
        return(data.frame(lsd.fr = lsd.fr))
      }, userAccelAgt, samplingRate.ref, samplingRate.agt) %>%
        data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')
    }, userJerk.agt, samplingRate.ref, samplingRate.agt, .id = 'axis')
  colnames(ftrs.jerk)[-(1:2)] = paste0(colnames(ftrs.jerk)[-(1:2)], '.uj')
  
  # Get user acf
  userACF.ref = userAccel.ref %>%
    lapply(function(accel, sl){
      apply(accel,2, function(x){acf(x, plot = F)$acf})
    }, samplingRate.ref)
  
  userACF.agt = userAccel.agt %>%
    lapply(function(accel, sl){
      apply(accel,2, function(x){acf(x, plot = F)$acf})
    }, samplingRate.agt)
  
  # Get time and frequency domain features for acf
  ftrs.acf = userACF.ref %>%
    plyr::ldply(.fun = function(accel.ref, userAccelAgt, samplingRate.ref, samplingRate.agt){
      apply(accel.ref, 2, function(accelRef, userAccelAgt, samplingRate.ref, samplingRate.agt){
        lsd.fr = do.call(cbind, userAccelAgt) %>%
          apply(2, function(accelAgt, accelRef, samplingRate.ref, samplingRate.agt){
            getLogSpectralDistance(accelRef, accelAgt, samplingRate.ref, samplingRate.agt)
          }, accelRef, samplingRate.ref, samplingRate.agt) %>%
          unlist() %>% median(na.rm = T)
        return(data.frame(lsd.fr = lsd.fr))
      }, userAccelAgt, samplingRate.ref, samplingRate.agt) %>%
        data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')
    }, userACF.agt, samplingRate.ref, samplingRate.agt, .id = 'axis')
  colnames(ftrs.acf)[-(1:2)] = paste0(colnames(ftrs.acf)[-(1:2)], '.acf')
  
  # Tag outliers windows based on phone rotation
  gr.error = lapply(dat.ref$gravity, function(x) {
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
  ftrs = list(ftrs.accel, ftrs.jerk, ftrs.acf) %>%
    plyr::join_all() %>%
    dplyr::left_join(gr.error)
  
  return(ftrs)
}