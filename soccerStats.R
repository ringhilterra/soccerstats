soccerStatsAll <- function(year) {
    control <- soccerStatsControl(year)
    discipline <- soccerStatsDiscipline(year)
    standard <- soccerStatsStandard(year)
    a <- merge(control,discipline)
    all <- merge(a, standard)
	name <- paste0(year, "_fullEplPlayerStats.csv")
    write.csv(all, name, row.names = FALSE)
    return(all)
}


soccerStatsControl <- function(year) {
    library(XML)
    library(xlsx)
    library(dplyr)

    urlPrefix <- "http://www.foxsports.com/soccer/stats?competition=1&season=0&category=CONTROL&pos=0&team=0&isOpp=0&splitType=0&sort=3&sortOrder=0&page="
    seasonString <- paste0("season=", year)
    urlPrefix <- gsub("season=", seasonString, urlPrefix )
    url <- paste0(urlPrefix,1)
    stats.table <- readHTMLTable(url, header = T, trim = T, which = 1)

    i = 2
    while(i < 12) {
        url <- paste0(urlPrefix, i)
        tempstats.table <- readHTMLTable(url, header = T, trim = T, which = 1)
        stats.table <- rbind(stats.table, tempstats.table)
        i = i + 1
    }

    stats.table <- stats.table[complete.cases(stats.table),]
    colnames(stats.table) <- c("name", "GP", "GS", "MP", "TT", "P", "INT", "BLK", "GMB", "TKL", "OFF", "C", "CK")

    stats.table[,1:13] <- sapply(stats.table[,1:13], as.character) #makes all characters

    stats.table[,2:13] <- sapply(stats.table[,2:13], as.numeric) #makes all numeric
    stats.table$name <- sapply(stats.table$name, nameClean) #clean names
    stats.table[,1] <- sapply(stats.table[1],as.character)  #make name column char type instead of factor
    stats.table <- stats.table[order(stats.table$name),]       #sort by name


    return(stats.table)

}

soccerStatsDiscipline <- function(year) {
    library(XML)
    library(xlsx)
    library(dplyr)

    urlPrefix <- "http://www.foxsports.com/soccer/stats?competition=1&season=0&category=DISCIPLINE&pos=0&team=0&isOpp=0&splitType=0&sort=3&sortOrder=0&page="
    seasonString <- paste0("season=", year)
    urlPrefix <- gsub("season=", seasonString, urlPrefix )
	url <- paste0(urlPrefix,1)
    stats.table <- readHTMLTable(url, header = T, trim = T, which = 1)

    i = 2
    while(i < 12) {
        url <- paste0(urlPrefix, i)
        tempstats.table <- readHTMLTable(url, header = T, trim = T, which = 1)
        stats.table <- rbind(stats.table, tempstats.table)
        i = i + 1
    }

    stats.table <- stats.table[complete.cases(stats.table),]
    colnames(stats.table) <- c("name", "GP", "GS", "MP", "FS", "FC", "YC", "RC", "OFF", "C", "CK", "PKG", "PK")

    stats.table[,1:13] <- sapply(stats.table[,1:13], as.character) #makes all characters

    stats.table[,2:13] <- sapply(stats.table[,2:13], as.numeric) #makes all numeric
    stats.table$name <- sapply(stats.table$name, nameClean) #clean names
    stats.table[,1] <- sapply(stats.table[1],as.character)  #make name column char type instead of factor
    stats.table <- stats.table[order(stats.table$name),]       #sort by name

    return(stats.table)

}

soccerStatsStandard <- function(year) {
    library(XML)
    library(xlsx)
    library(dplyr)

    urlPrefix <- "http://www.foxsports.com/soccer/stats?competition=1&season=0&category=STANDARD&pos=0&team=0&isOpp=0&splitType=0&sort=3&sortOrder=0&page="
    seasonString <- paste0("season=", year)
    urlPrefix <- gsub("season=", seasonString, urlPrefix )
	url <- paste0(urlPrefix,1)
    stats.table <- readHTMLTable(url, header = T, trim = T, which = 1)

    i = 2
    while(i < 12) {
        url <- paste0(urlPrefix,i)
        tempstats.table <- readHTMLTable(url, header = T, trim = T, which = 1)
        stats.table <- rbind(stats.table, tempstats.table)
        i = i + 1
    }

    stats.table <- stats.table[complete.cases(stats.table),]
    colnames(stats.table) <- c("name","GP", "GS", "MP", "G", "A", "SOG", "S", "YC", "RC")

    stats.table[,1:10] <- sapply(stats.table[,1:10], as.character) #makes all characters

    stats.table[,2:10] <- sapply(stats.table[,2:10], as.numeric) #makes all numeric
    stats.table$name <- sapply(stats.table$name, nameClean) #clean names
    stats.table[,1] <- sapply(stats.table[1],as.character)  #make name column char type instead of factor
    stats.table <- stats.table[order(stats.table$name),]       #sort by name

    return(stats.table)

}

removeTeam <- function(s) {
    b <- length(s) - 3
    substring(b, b+3)


    a
}

nameClean <- function(a) {
    a <- gsub('[[:digit:]]+', '', a)
    a <- gsub('\n', '', a)
    a <- gsub('\t', '', a)
    a <- gsub('\r', '', a)

    a
}

