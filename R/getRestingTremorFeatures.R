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
  if(is.na(tremorJsonFileLoc)){ ftrs$error = 'No JSON file'; return(ftrs) }
  
  # Get accelerometer data
  dat = tryCatch({ jsonlite::fromJSON(as.character(tremorJsonFileLoc)) }, 
                 error = function(e){ NA })
  if(is.na(dat)){ ftrs$error = 'JSON file read error'; return(ftrs) }
  
  # Get sampling rate
  samplingRate = length(dat$timestamp)/(dat$timestamp[length(dat$timestamp)] - dat$timestamp[1])

  # Rotate acceleration data to earth co-ordinates
  userAccel = tryCatch({
    userAccel = cbind(timestamp = dat$timestamp-dat$timestamp[1],
                      mpowertools:::get_quaternary_rotated_userAccel(dat)) %>%
      as.data.frame()
    ind = order(userAccel$timestamp)
    userAccel = userAccel[ind, ] %>%
      tidyr::gather(axis, accel, -timestamp)
  }, error = function(e){ NA })
  if(is.na(userAccel)){ ftrs$error = 'userAccel rotation error'; return(ftrs) }
  
  # Detrend data
  userAccel = tryCatch({
    userAccel %>%
      plyr::ddply(.(axis), .fun = function(x){
        x$accel = loess(x$accel~x$timestamp)$residual
        return(x)
      })
  }, error = function(e){ NA })
  if(is.na(userAccel)){ ftrs$error = 'Detrend error'; return(ftrs) }
  
  # Band pass filter signal between freqRange
  userAccel = tryCatch({
    userAccel %>% 
      plyr::ddply(.(axis), .fun = function(x, wl, sl, freqRange){
        bandPassFilt = signal::fir1(wl-1, c(freqRange[1] * 2/sl, freqRange[2] * 2/sl),
                                    type="pass", 
                                    window = seewave::hamming.w(wl))
        x$accel = signal::filtfilt(bandPassFilt, x$accel)
        return(x)
      }, windowLen, samplingRate, freqRange)
  }, error = function(e){ NA })
  if(is.na(userAccel)){ ftrs$error = 'Band pass filter error'; return(ftrs) }
  
  # Filter signal between 1 and 9 sec
  userAccel = tryCatch({
    userAccel %>%
      dplyr::filter(timestamp >= 1, timestamp <= 9)
  }, error = function(e){ NA })
  if(is.na(userAccel)){ ftrs$error = 'Not enough time samples'; return(ftrs) }
  
  # Split user acceleration into windows
  userAccel  = userAccel %>%
    plyr::dlply(.(axis), .fun = function(accel, wl, ovlp){
      a = mpowertools:::windowSignal(accel$accel, wl = wl, ovlp = ovlp) 
    }, windowLen, ovlp)
  
  # Get time and frequency domain features for userAcceleration
  ftrs.accel = userAccel %>%
    lapply(function(accel){
      list(apply(accel, 2, mpowertools:::getTimeDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, mpowertools:::getFrequencyDomainSummary, samplingRate) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, mpowertools:::getFrequencyDomainEnergy, samplingRate) %>%
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
      list(apply(accel, 2, mpowertools:::getTimeDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, mpowertools:::getFrequencyDomainSummary, samplingRate) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, mpowertools:::getFrequencyDomainEnergy, samplingRate) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')) %>%
        plyr::join_all()
    }) %>%
    data.table::rbindlist(use.names = T, fill = T, idcol = 'axis')
  colnames(ftrs.jerk)[-(1:2)] = paste0(colnames(ftrs.jerk)[-(1:2)], '.uj')
  
  # Get user acf
  userACF = userAccel %>%
    lapply(function(accel, sl){
      apply(accel,2, function(x){acf(x, plot = F)$acf})
    }, samplingRate)
  
  # Get time and frequency domain features for acf
  ftrs.acf = userACF %>%
    lapply(function(accel){
      list(apply(accel, 2, mpowertools:::getTimeDomainSummary) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, mpowertools:::getFrequencyDomainSummary, samplingRate) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window'),
           apply(accel, 2, mpowertools:::getFrequencyDomainEnergy, samplingRate) %>%
             data.table::rbindlist(use.names = T, fill = T, idcol = 'Window')) %>%
        plyr::join_all()
    }) %>%
    data.table::rbindlist(use.names = T, fill = T, idcol = 'axis')
  colnames(ftrs.acf)[-(1:2)] = paste0(colnames(ftrs.acf)[-(1:2)], '.acf')
  
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
  ftrs = list(ftrs.accel, ftrs.jerk, ftrs.acf) %>%
    plyr::join_all() %>%
    dplyr::left_join(gr.error)
  
  return(ftrs)
}