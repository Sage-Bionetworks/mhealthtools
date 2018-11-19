---
title: "Feature Definitions"
author: "Thanneer Malai Perumal"
date: "Mon Jun 26 17:46:41 2017"
output: html_document
---

## Tapping

| Feature Name      | Definition                                                                                        |
| ----------------- | ------------------------------------------------------------------------------------------------- |
| meanTapInter      | mean tapping interval                                                                             |
| medianTapInter    | median tapping interval                                                                           |
| iqrTapInter       | interquartile range tapping interval                                                              |
| minTapInter       | minimum tapping interval                                                                          |
| maxTapInter       | maximum tapping interval                                                                          |
| skewTapInter      | skewness tapping interval                                                                         |
| kurTapInter       | kurtosis tapping interval                                                                         |
| sdTapInter        | standard deviation tapping interval                                                               |
| madTapInter       | mean absolute deviation tapping interval                                                          |
| cvTapInter        | coefficient of variation tapping interval                                                         |
| rangeTapInter     | range tapping interval                                                                            |
| tkeoTapInter      | teager-kaiser energy operator tapping interval                                                    |
| ar1TapInter       | autocorrelation (lag = 1) tapping interval                                                        |
| ar2TapInter       | autocorrelation (lag = 2) tapping interval                                                        |
| fatigue10TapInter | difference in mean tapping interval between the first and last 10% of the tapping interval series |
| fatigue25TapInter | difference in mean tapping interval between the first and last 25% of the tapping interval series |
| fatigue50TapInter | difference in mean tapping interval between the first and last 50% of the tapping interval series |
| meanDriftLeft     | mean drift in the left button                                                                     |
| medianDriftLeft   | median drift in the left button                                                                   |
| iqrDriftLeft      | interquartile range of drift in the left button                                                   |
| minDriftLeft      | minimum of drift in the left button                                                               |
| maxDriftLeft      | maximum of drift in the left button                                                               |
| skewDriftLeft     | skewness of drift in the left button                                                              |
| kurDriftLeft      | kurtosis of drift in the left button                                                              |
| sdDriftLeft       | standard deviation of drift in the left button |
| madDriftLeft      | mean absolute deviation of drift in the left button |
| cvDriftLeft       | coefficient of variation of drift in the left button |
| rangeDriftLeft    | range of drift in the left button |
| meanDriftRight    | mean drift in the right button |
| medianDriftRight  | median drift in the right button |
| iqrDriftRight     | interquartile range of drift in the right button |
| minDriftRight     | minimum of drift in the right button |
| maxDriftRight     | maximum of drift in the right button |
| skewDriftRight    | skewness of drift in the right button |
| kurDriftRight     | kurtosis of drift in the right button |
| sdDriftRight      | standard deviation of drift in the right button |
| madDriftRight     | mean absolute deviation of drift in the right button |
| cvDriftRight      | coefficient of variation of drift in the right button |
| rangeDriftRight   | range of drift in the right button |
| numberTap         | number of taps |
| buttonNoneFreq    | frequency where neither the left or right buttons were hit |
| corXY             | correlation between the X and Y coordinates of the hits |


## Acceleromter and Gyroscope
### (Walking, Tremor, Rest etc.,)

| Feature Name <br />(.tm =time <br /> .fr=frequency)      | Description   |
| ----------------- | ------------------------------------------------------------------------------------------------- |
| complexity.tm    | Complexity of a time series  $$ \sqrt{\frac{Var(f_s^2*\frac{d^2 x}{dt^2 })}{(Var(f_s* \frac{dx}{dt})}} $$ Where $Var$ means variance |
| dfa.tm           | Scaling exponent estimated after performing detrended fluctuation analysis(DFA) |
| energy.tm        | Total energy |
| IQR.tm           | Interquartile Range (Q25 - Q75) |
| Kurtosis.tm      | $$E\left[\left(\frac{X-μ}{σ}\right)^4\right]$$ Where $X$ is the signal, $μ$ is the mean and $σ$ is the standard deviation; and $E[.]$ is the expectation |
| mean.tm 		   | Mean $$E[X]$$ Where $X$ is the signal and $E[.]$ is the expectation |
| median.tm        | Median <br /> Middle value of the measurements, i.e the 50th Quartile |
| mn.tm            | Minimum $$x_{min},x_{min}≤x,∀x$$ |
| mobility.tm      | Mobility of a time series $$\sqrt{\frac{Var(f_s* \frac{dx}{dt})}{Var(x)}}$$ Where $Var$ means variance |
| mode.tm          | Mode, Most occuring measure $$x_{mode}, \#x_{mode} ≥ \#x,∀x$$ |
| mtkeo.tm         | Mean of the Teager-Kaiser energy operator |
| mx.tm            | Max $$x_{max}, x_{max} ≥x,∀x$$ |
| Q25.tm           | $25th$ Quartile | 
| Q75.tm           | $75th$ Quartile |
| range.tm         | Range: Max - Min $$x_{max} - x_{min}$$ |
| rmsmag.tm        | Root Mean Square Magnitude |
| rough.tm         | Roughness or Total Curvature of the time series |
| rugo.tm          | Rugosity of the time series |
| sd.tm            | Standard Deviation $$E[(X-μ)^2]$$ Where $X$ is the signal, $μ$ is the mean and $E[.]$ is the expectation |
| skewness.tm      | $$E\left[\left(\frac{X-μ}{σ}\right)^3\right]$$ Where $X$ is the signal, $μ$ is the mean and $σ$ is the standard deviation; and $E[.]$ is the expectation |
| cent.fr          | Centroid of the frequency spectrum, i.e the weighted mean of all frequencies present in  the Fourier Spectrum $$Centroid =  \frac{Σf.H(f)}{ΣH(f)}$$ Where $f$ is the frequency, and $H(f)$ is the spectral magnitude at that frequency |
| ewt.permEnt.fr   | Permutation entropy of the probability mass function(PMF) obtained from the energies of the modes obtained from Empirical Wavelet Transform(EWT) |
| ewt.renyiEnt.fr  | Renyi entropy of the PMF calculated using the modes obtained from EWT |
| ewt.shannonEnt.fr| Shannon entropy of the PMF calculated using the modes obtained from EWT |
| ewt.simpsonEnt.fr| Simpson entropy of the PMF calculated using the modes obtained from EWT |
| ewt.tsallisEnt.fr| Tsallis entropy of the PMF calculated using the modes obtained from EWT |
| IQR.fr           | Interquartile range (Q75-Q25) |
| kurt.fr          | Kurtosis of the frequency probability  distribution |
| md.fr            | Median frequency |
| mn.fr            | Mean frequency |
| mod.fr           | Frequency with the maximum amplitude in the frequency spectrum, i.e mode frequency |
| mx.fr            | Maximum amplitude of the frequency probability distribution |
| Q25.fr           | 25th Quartile of the frequency spectrum |
| Q75.fr           | 75th Quartile of the frequency spectrum |
| sd.fr            | Standard deviation of the frequency spectrum |
| sem.fr           | Mean standard deviation, i.e  $$\frac{sd}{\sqrt{n}}$$ |
| sfm.fr           | Spectral Flatness Measure of the frequency probability distribution |
| sh.fr            | Shannon Entropy of the frequency probability distribution |
| skew.fr          | Skewness of the frequency probability distribution |
| EnergyInBandk    | Energy present in k-(k+1) Hz band of the frequency spectrum |




# OLD FILE 

## Voice

| Feature Name                | Definition                                                 |
| --------------------------- | ---------------------------------------------------------- |
| Median F0                   | Median of all possible pitch estimates                     |
| Mean Jitter                 | Mean of all possible jitter estimates                      |
| Median Jitter               | Median of all possible jitter estimates                    |
| Mean Shimmer                | Mean of all possible shimmer estimates                     |
| Median Shimmer              | Median of all possible shimmer estimates                   |
| MFCC Band 1                 | Mel-Frequency Cepstrum Band 1                              |
| MFCC Band 2                 | Mel-Frequency Cepstrum Band 2                              |
| MFCC Band 3                 | Mel-Frequency Cepstrum Band 3                              |
| MFCC Band 4                 | Mel-Frequency Cepstrum Band 4                              |
| MFCC Jitter Band 1 Positive | Positive change in Mel-Frequency Cepstrum Band 1 over time |
| MFCC Jitter Band 2 Positive | Positive change in Mel-Frequency Cepstrum Band 2 over time |
| MFCC Jitter Band 3 Positive | Positive change in Mel-Frequency Cepstrum Band 3 over time |
| MFCC Jitter Band 4 Positive | Positive change in Mel-Frequency Cepstrum Band 4 over time |

## Walk

| Feature Name | Definition                                                                                                            |
| ------------ | --------------------------------------------------------------------------------------------------------------------- |
| meanX        | mean of the X acceleration series                                                                                     |
| sdX          | standard deviation of the X acceleration series                                                                       |
| modeX        | mode of the X acceleration series                                                                                     |
| skewX        | skewness of the X acceleration series                                                                                 |
| kurX         | kurtosis of the X acceleration series                                                                                 |
| q1X          | first quartile of the X acceleration series                                                                           |
| medianX      | median of the X acceleration series                                                                                   |
| q3X          | third quartile of the X acceleration series                                                                           |
| iqrX         | interquartile range of the X acceleration series                                                                      |
| rangeX       | range of the X acceleration series                                                                                    |
| acfX         | autocorrelation (lag = 1) of the X acceleration series                                                                |
| zcrX         | zero-crossing rate of the X acceleration series                                                                       |
| dfaX         | scaling exponent of the detrended fluctuation analysis of the X acceleration series                                   |
| cvX          | coefficient of variation of the X acceleration series                                                                 |
| tkeoX        | teager-kaiser energy operator of the X acceleration series                                                            |
| F0X          | frequency at which the maximum peak of the Lomb-Scargle periodogram occurred for the X acceleration series            |
| P0X          | maximum power in the inspected frequency interval of the Lomb-Scargle periodogram for the X acceleration series       |
| meanY        | mean of the Y acceleration series                                                                                     |
| sdY          | standard deviation of the Y acceleration series                                                                       |
| modeY        | mode of the Y acceleration series                                                                                     |
| skewY        | skewness of the Y acceleration series                                                                                 |
| kurY         | kurtosis of the Y acceleration series                                                                                 |
| q1Y          | first quartile of the Y acceleration series                                                                           |
| medianY      | median of the Y acceleration series                                                                                   |
| q3Y          | third quartile of the Y acceleration series                                                                           |
| iqrY         | interquartile range of the Y acceleration series                                                                      |
| rangeY       | range of the Y acceleration series                                                                                    |
| acfY         | autocorrelation (lag = 1) of the Y acceleration series                                                                |
| zcrY         | zero-crossing rate of the Y acceleration series                                                                       |
| dfaY         | scaling exponent of the detrended fluctuation analysis of the Y acceleration series                                   |
| cvY          | coefficient of variation of the Y acceleration series                                                                 |
| tkeoY        | teager-kaiser energy operator of the Y acceleration series                                                            |
| F0Y          | frequency at which the maximum peak of the Lomb-Scargle periodogram occurred for the Y acceleration series            |
| P0Y          | maximum power in the inspected frequency interval of the Lomb-Scargle periodogram for the Y acceleration series       |
| meanZ        | mean of the Z acceleration series                                                                                     |
| sdZ          | standard deviation of the Z acceleration series                                                                       |
| modeZ        | mode of the Z acceleration series                                                                                     |
| skewZ        | skewness of the Z acceleration series                                                                                 |
| kurZ         | kurtosis of the Z acceleration series                                                                                 |
| q1Z          | first quartile of the Z acceleration series                                                                           |
| medianZ      | median of the Z acceleration series                                                                                   |
| q3Z          | third quartile of the Z acceleration series                                                                           |
| iqrZ         | interquartile range of the Z acceleration series                                                                      |
| rangeZ       | range of the Z acceleration series                                                                                    |
| acfZ         | autocorrelation (lag = 1) of the Z acceleration series                                                                |
| zcrZ         | zero-crossing rate of the Z acceleration series                                                                       |
| dfaZ         | scaling exponent of the detrended fluctuation analysis of the Z acceleration series                                   |
| cvZ          | coefficient of variation of the Z acceleration series                                                                 |
| tkeoZ        | teager-kaiser energy operator of the Z acceleration series                                                            |
| F0Z          | frequency at which the maximum peak of the Lomb-Scargle periodogram occurred for the Z acceleration series            |
| P0Z          | maximum power in the inspected frequency interval of the Lomb-Scargle periodogram for the Z acceleration series       |
| meanAA       | mean of the average acceleration series                                                                               |
| sdAA         | standard deviation of the average acceleration series                                                                 |
| modeAA       | mode of the average acceleration series                                                                               |
| skewAA       | skewness of the average acceleration series                                                                           |
| kurAA        | kurtosis of the average acceleration series                                                                           |
| q1AA         | first quartile of the average acceleration series                                                                     |
| medianAA     | median of the average acceleration series                                                                             |
| q3AA         | third quartile of the average acceleration series                                                                     |
| iqrAA        | interquartile range of the average acceleration series                                                                |
| rangeAA      | range of the average acceleration series                                                                              |
| acfAA        | autocorrelation (lag = 1) of the average acceleration series                                                          |
| zcrAA        | zero-crossing rate of the average acceleration series                                                                 |
| dfaAA        | scaling exponent of the detrended fluctuation analysis of the average acceleration Series                             |
| cvAA         | coefficient of variation of the average acceleration series                                                           |
| tkeoAA       | teager-kaiser energy operator of the average acceleration series                                                      |
| F0AA         | frequency at which the maximum peak of the Lomb-Scargle periodogram occured for the average acceleration series       |
| P0AA         | maximum power in the inspected frequency interval of the Lomb-Scargle periodogram for the average acceleration series |
| meanAJ       | mean of the average jerk series                                                                                       |
| sdAJ         | standard deviation of the average jerk series                                                                         |
| modeAJ       | mode of the average jerk series                                                                                       |
| skewAJ       | skewness of the average jerk series                                                                                   |
| kurAJ        | kurtosis of the average jerk series                                                                                   |
| q1AJ         | first quartile of the average jerk series                                                                             |
| medianAJ     | median of the average jerk series                                                                                     |
| q3AJ         | third quartile of the average jerk series                                                                             |
| iqrAJ        | interquartile range of the average jerk series                                                                        |
| rangeAJ      | range of the average jerk series                                                                                      |
| acfAJ        | autocorrelation (lag = 1) of the average jerk series                                                                  |
| zcrAJ        | zero-crossing rate of the average jerk series                                                                         |
| dfaAJ        | scaling exponent of the detrended fluctuation analysis of the average jerk series                                     |
| cvAJ         | coefficient of variation of the average jerk series                                                                   |
| tkeoAJ       | teager-kaiser energy operator of the average jerk series                                                              |
| F0AJ         | frequency at which the maximum peak of the Lomb-Scargle periodogram occurred for the average jerk series              |
| P0AJ         | maximum power in the inspected frequency interval of the Lomb-Scargle periodogram for the average jerk series         |
| corXY        | correlation between the X and Y acceleration                                                                          |
| corXZ        | correlation between the X and Z acceleration                                                                          |
| corYZ        | correlation between the Y and Z acceleration                                                                          |

## Rest

| Feature Name | Definition                                                                                        |
| ------------ | ------------------------------------------------------------------------------------------------- |
| meanAA       |  mean of the average acceleration series                                                          |
| sdAA         |  standard deviation of the average acceleration series                                            |
| modeAA       |  mode of the average acceleration series                                                          |
| skewAA       |  skewness of the average acceleration series                                                      |
| kurAA        |  kurtosis of the average acceleration series                                                      |
| q1AA         |  first quartile of the average acceleration series                                                |
| medianAA     |  median of the average acceleration series                                                        |
| q3AA         |  third quartile of the average acceleration series                                                |
| iqrAA        |  interquartile range of the average acceleration series                                           |
| rangeAA      |  range of the average acceleration series                                                         |
| acfAA        |  autocorrelation (lag = 1) of the average acceleration series                                     |
| zcrAA        |  zero-crossing rate of the average acceleration series                                            |
| dfaAA        |  scaling exponent of the detrended fluctuation analysis of the average acceleration series        |
| turningTime  |  turning time                                                                                     |
| Postpeak     |  posture peak                                                                                     |
| Postpower    |  posture power                                                                                    |
| Alpha        |  scaling exponent of the detrended fluctuation analysis of the force vector magnitute series      |
| dVol         | displacement volume (volume of the box around the displacement across the X, Y, and Z directions) |
| ddVol        | delta displacement volume                                                                         |