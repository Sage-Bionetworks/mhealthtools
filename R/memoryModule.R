###### Memory module data processing functions
###### Abhishek Pratap Sage Bionetworks


processCoordLine <- function(x) {
    if (is.na(x) == T) {
        return(c(NA, NA))
    } else {
        r <- unlist(strsplit(x, ","))
        r <- as.numeric(gsub("[\\{|\\}]", "",
            r, perl = T))
        return(r)
    }
}

getFlowerCenters <- function(iphone_coord_list) {
    res <- plyr::ldply(iphone_coord_list, function(x) {
        r <- processCoordLine(x)
        # iphone coordinate system ref:
        # http://www.idev101.com/code/User_Interface/view_frames_bounds.html
        x_cord <- r[1]
        y_cord <- r[2]
        width <- r[3]
        height <- r[4]
        x_midpoint <- x_cord + width/2
        y_midpoint <- y_cord + height/2
        c(x_cord, y_cord, width, height, x_midpoint,
            y_midpoint)
    })
    colnames(res) <- c("x_cord", "y_cord", "width",
        "height", "x_midpoint", "y_midpoint")
    res["flower_num"] <- seq(1:length(iphone_coord_list))
    res
}

process_subseq_in_a_game <- function(game_subsequence) {
    flowerCenters <- getFlowerCenters(game_subsequence$TargetRects[[1]])
    userTouchInfo <- game_subsequence$TouchSamples[[1]]
    colnames(userTouchInfo) <- gsub("MemoryGameTouchSample","", colnames(userTouchInfo))
    userTouchInfo$TargetIndex = userTouchInfo$TargetIndex + 1  #convert C 0-based indices to R 1-based
    userTouchInfo$userSequence <- userTouchInfo$TargetIndex
    userTouchInfo$TargetIndex <- NULL
    origSeqeunce <- unlist(strsplit(game_subsequence$Sequence,","))
    # ref //
    # http://www.inside-r.org/packages/cran/qpcR/docs/cbind.na
    # and
    # https://stackoverflow.com/questions/28080881/could-not-find-function-cbind-na
    userTouchInfo <- qpcR:::cbind.na(origSeqeunce,userTouchInfo)

    temp_coords <- t(sapply(userTouchInfo$Location,processCoordLine))
    userTouchInfo["user_x_coord"] = as.numeric(temp_coords[,1])
    userTouchInfo["user_y_coord"] = as.numeric(temp_coords[,2])
    userTouchInfo["game_subseqeunce_order"] = seq(1:nrow(userTouchInfo))
    userTouchInfo <- merge(flowerCenters, userTouchInfo, by.x = "flower_num",
                           by.y = "origSeqeunce", all.y = T, sort = F) %>%
      dplyr::arrange(game_subseqeunce_order)
    userTouchInfo["distance"] = sqrt( (userTouchInfo$x_midpoint - userTouchInfo$user_x_coord)^2 + (userTouchInfo$y_midpoint -
        userTouchInfo$user_y_coord)^2)
    # calculate time delta between flower
    # touches
    oneOffTimeStamps <- c(0, userTouchInfo$Timestamp[1:nrow(userTouchInfo) - 1])
    userTouchInfo$deltaTime <- userTouchInfo$Timestamp - oneOffTimeStamps
    userTouchInfo
}

createMemoryFeaturesErrorResult <- function(error) {
  features <- c(rep(NA, 17), error)
  names(features) <- c("totalDistance", "totalTime", "totalCorrectFlowers", "avg_wrongflowerNum", "total_newFlowers_touched",
                        "varTime", "meanTime", "medTime", "meanDist", "medDist", "varDist", "flower1_meanTime", "flower1_medTime",
                        "flower1_varTime", "flower1_meanDist", "flower1_medDist", "flower1_varDist","error")
  features
}

processGame <- function(game){
  game <- game %>% dplyr::filter(!MemoryGameStatus == "MemoryGameStatusTimeout")
  if(nrow(game) == 0){
    return(NA)
  }
  colnames(game) <- gsub("MemoryGameRecord", "", colnames(game))
  colnames(game) <- gsub("MemoryGameStatus", "Status", colnames(game))
  game["flowerMatrixSize"] = game$GameSize
  game["GameSize"] = unlist(lapply(game$Sequence,
                                   length))
  game$Sequence <- unlist(lapply(game$Sequence,
                                 function(x) paste(x + 1, collapse = ",")))
  df <- plyr::ddply(.data = game, .variables = c("flowerMatrixSize", "GameSize",
                                                 "GameScore", "Seed",
                                                 "Sequence", "Status"),
                    .fun = process_subseq_in_a_game) %>%
    mutate(order = 1:length(Seed))
  return(df)
}

memoryGame_generateSummaryStats <- function(memoryGame){
  if(is.na(memoryGame) == TRUE){
    return(createMemoryFeaturesErrorResult('Got 0 rows after MemoryGameStatusTimeout filtering'))
  }

  totalDistance <- sum(memoryGame$distance, na.rm=T)
  totalTime <- sum(memoryGame$deltaTime, na.rm=T)
  totalCorrectFlowers <- sum(memoryGame$IsCorrect, na.rm=T)
  avg_wrongflowerNum <- memoryGame %>% dplyr::filter(IsCorrect == FALSE) %>% .$game_subseqeunce_order %>% mean()

  #calculating #flowers touched that were not shown to the user in each game
  total_newFlowers_touched <- memoryGame %>% dplyr::group_by(Seed) %>% dplyr::summarize(n=length(setdiff(userSequence,flower_num))) %>% .$n %>% sum

  # second flower onwards stats
  flowers_except1 <- memoryGame %>% dplyr::filter(game_subseqeunce_order != 1)
  varTime <- var(flowers_except1$deltaTime, na.rm=T)
  meanTime <- mean(flowers_except1$deltaTime, na.rm=T)
  medTime <- median(flowers_except1$deltaTime, na.rm=T)
  meanDist <- mean(flowers_except1$distance, na.rm=T)
  medDist <- median(flowers_except1$distance, na.rm=T)
  varDist <- var(flowers_except1$distance, na.rm=T)

  #flower1 stats
  flower1 <- memoryGame %>% dplyr::filter(game_subseqeunce_order == 1)
  flower1_meanTime <- mean(flower1$deltaTime, na.rm=T)
  flower1_medTime <- median(flower1$deltaTime, na.rm=T)
  flower1_varTime <- var(flower1$deltaTime, na.rm=T)
  flower1_meanDist <- mean(flower1$distance, na.rm=T)
  flower1_medDist <- median(flower1$distance, na.rm=T)
  flower1_varDist <- var(flower1$distance, na.rm=T)

  memoryGameStats <- c(totalDistance, totalTime, totalCorrectFlowers, avg_wrongflowerNum, total_newFlowers_touched,
                       varTime, meanTime, medTime, meanDist, medDist, varDist, flower1_meanTime, flower1_medTime,
                       flower1_varTime, flower1_meanDist, flower1_medDist, flower1_varDist)
  colnames(memoryGameStats) <- c("totalDistance", "totalTime", "totalCorrectFlowers", "avg_wrongflowerNum", "total_newFlowers_touched",
                              "varTime", "meanTime", "medTime", "meanDist", "medDist", "varDist", "flower1_meanTime", "flower1_medTime",
                              "flower1_varTime", "flower1_meanDist", "flower1_medDist", "flower1_varDist")
  memoryGameStats['error'] = 'None'
  memoryGameStats
}




####### MAIN
#' extracts memory game features from mPower game JSON data file
#'
#'
#' @param game_json_file path to game records json file
#' @return data frame of memory game features
#' @export
#' @examples
#' library(synapseClient)
#' synapseLogin()
#' memoryTable = synTableQuery("SELECT * FROM syn5713115")
#' memoryTable = memoryTable@values
#' sampleRow = rownames(memoryTable)[1]
#' sample_gameRecord_File <- synDownloadTableFile('syn5713115', sampleRow,
#' "MemoryGameResults.json.MemoryGameGameRecords")
#' getMemoryGameFeatures(sample_gameRecord_File)
getMemoryGameFeatures <- function(game_json_file) {

    if (is.na(game_json_file) == T) {
        null_result <- createMemoryFeaturesErrorResult("No JSON file present")
        return(null_result)
    }

    tryCatch({
        gameData <- jsonlite::fromJSON(game_json_file)
    }, error = function(err) {
      null_result = createMemoryFeaturesErrorResult('unable to read game JSON file')
      return(null_result)
    })

    tryCatch({
      memoryGame <- processGame(gameData)
      memoryGameFeatures <- memoryGame_generateSummaryStats(memoryGame)
      return(memoryGameFeatures)
    }, error = function(err) {
       null_result <- createMemoryFeaturesErrorResult("unable to process game record from JSON file")
        return(null_result)
    })
    return(df)
}


