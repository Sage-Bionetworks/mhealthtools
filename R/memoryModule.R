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
    res <- ldply(iphone_coord_list, function(x) {
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
    colnames(userTouchInfo) <- gsub("MemoryGameTouchSample",
        "", colnames(userTouchInfo))
    userTouchInfo$TargetIndex = userTouchInfo$TargetIndex +
        1  #convert C 0-based indices to R 1-based
    userTouchInfo$userSequence <- userTouchInfo$TargetIndex
    userTouchInfo$TargetIndex <- NULL
    origSeqeunce <- unlist(strsplit(game_subsequence$Sequence,
        ","))
    # ref //
    # http://www.inside-r.org/packages/cran/qpcR/docs/cbind.na
    # and
    # https://stackoverflow.com/questions/28080881/could-not-find-function-cbind-na
    userTouchInfo <- qpcR:::cbind.na(origSeqeunce,
        userTouchInfo)

    temp_coords <- t(sapply(userTouchInfo$Location,
        processCoordLine))
    userTouchInfo["user_x_coord"] = as.numeric(temp_coords[,
        1])
    userTouchInfo["user_y_coord"] = as.numeric(temp_coords[,
        2])
    userTouchInfo["game_subseqeunce_order"] = seq(1:nrow(userTouchInfo))
    userTouchInfo <- merge(flowerCenters, userTouchInfo,
        by.x = "flower_num", by.y = "origSeqeunce",
        all.y = T, sort = F) %>% arrange(game_subseqeunce_order)
    userTouchInfo["distance"] = sqrt((userTouchInfo$x_midpoint -
        userTouchInfo$user_x_coord)^2 + (userTouchInfo$y_midpoint -
        userTouchInfo$user_y_coord)^2)
    # calculate time delta between flower
    # touches
    oneOffTimeStamps <- c(0, userTouchInfo$Timestamp[1:nrow(userTouchInfo) -
        1])
    userTouchInfo$deltaTime <- userTouchInfo$Timestamp -
        oneOffTimeStamps
    userTouchInfo
}


process_memoryGame_module <- function(game_json_file) {
    return_colnames <- c("flowerMatrixSize",
        "GameSize", "GameScore", "Seed", "Sequence",
        "Status", "flower_num", "x_cord", "y_cord",
        "width", "height", "x_midpoint", "y_midpoint",
        "Timestamp", "Location", "IsCorrect",
        "userSequence", "user_x_coord", "user_y_coord",
        "game_subseqeunce_order", "distance",
        "deltaTime", "error")

    if (is.na(game_json_file) == T) {
        null_result <- c(rep(NA, 23), "No JSON file present")
        null_result <- as.data.frame(t(null_result))
        colnames(null_result) <- return_colnames
        return(null_result)
    }

    tryCatch({
        game <- jsonlite::fromJSON(game_json_file)
    }, error = function(err) {
        print(paste("Error: unable to read game JSON file: ",
            game_json_file))
        null_result <- as.data.frame(t(null_result))
        colnames(null_result) <- return_colnames
        return(null_result)
    })
    tryCatch({
        game <- game %>% filter(!MemoryGameStatus ==
            "MemoryGameStatusTimeout")
        colnames(game) <- gsub("MemoryGameRecord",
            "", colnames(game))
        colnames(game) <- gsub("MemoryGameStatus",
            "Status", colnames(game))
        game["flowerMatrixSize"] = game$GameSize
        game["GameSize"] = unlist(lapply(game$Sequence,
            length))
        game$Sequence <- unlist(lapply(game$Sequence,
            function(x) paste(x + 1, collapse = ",")))
        df <- ddply(.data = game, .variables = c("flowerMatrixSize",
            "GameSize", "GameScore", "Seed",
            "Sequence", "Status"), .fun = process_subseq_in_a_game) %>%
            arrange(flowerMatrixSize, GameSize,
                game_subseqeunce_order)
    }, error = function(err) {
        print(paste("Error: ", err))
        null_result <- c(rep(NA, 23), "unable to process game record from JSON file")
        null_result <- as.data.frame(t(null_result))
        colnames(null_result) <- return_colnames
        return(null_result)
    })
    return(df)
}


process_medicationChoiceAnswers <- function(json_file) {
    tryCatch({
        d <- jsonlite::fromJSON(json_file)
        data.frame(medication = paste(unique(d$identifier),
            collapse = "+"), medicationTime = paste(unique(d$answer),
            collapse = "+"))
    }, error = function(err) {
        data.frame(medication = "NA", medicationTime = "NA")
    })
}


