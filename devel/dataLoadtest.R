# db load testing

## DETCTOR NAMES
# For seeing what detectors we have - PamguardModules table will have a thing for each detector (seems every time its run, or finds something).
# Module_Name has WhistlesMoans and Click Detector, then Module_Type has the name of the detector. These correspond to table names.
# For clicks: Name_of_detector_(Background/Clicks/OfflineClicks/OfflineEvents)
# For WM: Name_of_detector and Name_of_detector_Localised_Name_of_detector_Contours

library(RSQLite)
library(dplyr)
library(stringr)
dbfile <- './devel/WMTesting.sqlite3'
con <- dbConnect(drv=SQLite(), dbfile)
db <- sapply(dbListTables(con), function(tbl) {
  dbReadTable(con, tbl)
})

# Get all the table names for rocca, WM, click
getDetectorList <- function(con) {
  mods <- dbReadTable(con, 'PamguardModules')
  detTables <- mods %>%
    mutate(Module_Name=str_trim(Module_Name),
           Module_Name=gsub(' ', '', Module_Name),
           Module_Type=str_trim(Module_Type),
           Module_Type=gsub(' ', '_', Module_Type)) %>%
    filter(Module_Name %in% c('ClickDetector', 'WhistlesMoans', 'Rocca')) %>%
    distinct(Module_Type, Module_Name)
  detTables
}

# Add a name here? Then we can just combine all clicks whistles whatever together. transpose style if a list. and db name
dataList <- lapply(detList, function(det) {
  sapply(det, function(tbl) {
    dbReadTable(con, tbl) %>% colnames() # Just getting names for testing porpoises
  }, USE.NAMES=TRUE)
})

# rocca whistle has offlineEventID
# whistle has fucking nothing
# OfflineClicks has EventId

# Trying to match binary click sample no.s to wav file read sample nos. Doesnt work?
#####
library(tuneR)
clickTest <- './devel/Click_Detector_HaveSome_Clickies_Clicks_20040828_164312.pgdf'
ct2 <- './devel/Click_Detector_Click_Detector_Clicks_20040828_164312.pgdf'
clickBin <- loadPamguardBinaryFile(clickTest)
clickBin2 <- loadPamguardBinaryFile(ct2)
soundTest <- './devel/Recording/OrcaExample.wav'
binWav <- clickBin$data[[4]] #wave is 8bit int scaled by max amplitude
wavTest <-readWave(soundTest, from=binWav$startSample+1, to=binWav$startSample+binWav$sampleDuration-1, units='samples')
wavTest <-readWave(soundTest, from=binWav$startSample+1, to=200, units='samples')
plot(wavTest)
plot(binWav$wave[,2], type='l') + abline(h=0)
lines(binWav$wave[,1], col='red')
str(binWav$wave)
binWav$wave[,1]
wavTest@left


# PLAN: Go through tables, get detector names. Read in all detector data into dfs by detector name. Named list of detectors.
# Go through each detector and add event column based on time stamp. Split by event, then transpose. Should now have list.

# Read directory into list of detections ----------------------------------
# Formatting can be different between versions. Need to check version or in the
# readers force outputs to be the same. Or do I? Events are not across DBs

detFromDb <- function(dir) {
  dbList <- tools::list_files_with_exts(dir, exts='sqlite3')
  lapply(dbList, function(dbfile) {
    con <- dbConnect(drv=SQLite(), dbfile)
    detList <- getDetectorList(con)
    detTables <- purrr::map2(detList$Module_Name, detList$Module_Type, function(module, name) {
      switch(module,
             WhistlesMoans = wmReader(con, name),
             ClickDetector = cdReader(con, name),
             Rocca = rocReader(con, name)) %>%
        mutate(db = basename(dbfile),
               UTC = as.POSIXct(UTC, format='%Y-%m-%d %H:%M:%OS'))
    })
    names(detTables) <- detList$Module_Type
    # add basename(dbfile)
    dbDisconnect(con)
    detTables
  })
}

# Call type readers -------------------------------------------------------

wmReader <- function(con, name) {
  contours <- dbReadTable(con, paste0(name,'_Localised_', name, '_Contours'))
  whistles <- dbReadTable(con, name)
  whistles
}

cdReader <- function(con, name) {
  clicks <- dbReadTable(con, paste0(name, '_Clicks'))
  events <- dbReadTable(con, paste0(name, '_OfflineEvents')) %>%
    mutate(eventType = str_trim(eventType)) %>%
    select(Id, eventType)
  offClicks <- dbReadTable(con, paste0(name, '_OfflineClicks')) %>%
    mutate(BinaryFile = str_trim(BinaryFile)) %>%
    select(Id, UTC, EventId, BinaryFile, ClickNo) %>%
    filter(EventId %in% events$Id) %>%
    left_join(events, by = c('EventId' = 'Id'))
}

rocReader <- function(con, name) {
  encounter <- dbReadTable(con, paste0(name, '_Encounter_Stats'))
  whistles <- dbReadTable(con, paste0(name, '_Whistle_Stats'))
  # data.frame(hey='im here', this='seems to work')
  whistles
}

# DCL Logs ----------------------------------------------------------------
dclLogs <- do.call(rbind, lapply(list.files('./devel/DCL', full.names = TRUE), function(f) {
  read.csv(f, stringsAsFactors = FALSE, header=FALSE)
  })) %>%
  rename(Location=V1, Site=V2, Species=V3, Start=V4, End=V5) %>%
  mutate(Start = as.POSIXct(Start, format='%Y-%m-%dT%H:%M:%OS'),
         End = as.POSIXct(End, format='%Y-%m-%dT%H:%M:%OS'),
         Location = paste0(Location, Site)) %>%
  arrange(Start) %>% mutate(Id=1:n())

dclLogs %>%
  # filter(Id > 260, Id < 700) %>%
  ggplot(aes(color=Location)) +
  geom_segment(aes(x=Start, xend=End, y=Id, yend=Id), size=5)


td <- data.frame(a=1:6, b=rep(c('a', 'b'), 3))
td2 <- data.frame(a=10:15, b=c(rep('a', 2), rep('b', 4)))
test <- list(list(c=td), list(c=td2), list(z=td))

after <- sapply(test, function(x) {
  for(d in names(x)) {
    x[[d]] <- mutate(x[[d]], b=paste0(d, b))
  }
  lapply(x, function(a) split(a, a$b))
})

unlist(
  sapply(test, function(x) {
    lapply(x, function(a) split(a, a$b, drop=T))
  }), recursive = F)

test <- bind_rows(lapply(bbtest, function(x) {
  distinct(x, eventType, EventId) %>%
    rename(species=eventType, event.id=EventId)
}))

# banter debug ------------------------------------------------------------
id <- 1

data <- train.data$detectors[[1]]
evTest <- train.data$events

evTest <- bbwEvents %>% distinct()
data <- bbwDetections[[1]]

df <- evTest %>%
  dplyr::select(.data$event.id, .data$species) %>%
  dplyr::inner_join(data, by = "event.id") %>%
  dplyr::mutate(species = as.character(.data$species)) %>%
  as.data.frame

sampsize <- .getSampsize(
  df$species,
  2,
  paste0("Detector model (", names(bbwDetections)[id], ")")
)

df2 <- df %>%
  dplyr::filter(.data$species %in% names(sampsize)) %>%
  dplyr::mutate(species = factor(.data$species)) %>%
  dplyr::mutate(id = paste0(.data$event.id, ".", .data$call.id)) %>%
  tibble::column_to_rownames("id") %>%
  as.data.frame() %>%
  droplevels()


.getSampsize <- function(x, n, warn.label) {
  if(n < 0) n <- 1
  freq <- if(is.table(x)) x else table(x)
  n <- if(n >= 1) n else round(min(freq) * n, 0)
  if(n == 0) n <- 1

  bad.freq <- freq[freq <= n]
  good.freq <- freq[freq > n]

  if(length(good.freq) < 2) {
    stop(paste0(
      warn.label, ":\n",
      "sampsize = ", n,
      " is >= too many species frequencies (need at least 2 for model):\n",
      paste0("  ", names(freq), ": ", freq, "\n", collapse = "")
    ))
  } else if(length(bad.freq) > 0) {
    warning(paste0(
      warn.label, ": sampsize = ", n, " is >= species frequencies:\n",
      paste0("  ", names(bad.freq), ": ", bad.freq, "\n", collapse = ""),
      "These species will be used in the model:\n",
      paste0("  ", names(good.freq), ": ", good.freq, "\n", collapse = "")
    ), call. = FALSE, immediate. = TRUE)
    setNames(rep(n, length(good.freq)), names(good.freq))
  } else {
    setNames(rep(n, length(freq)), names(freq))
  }
}
###
mdl <- initBanterModel(bbwEvents) %>%
  addBanterDetector(bbwDetections, ntree=500, sampsize=1) %>%
  runBanterModel(ntree=1000, sampsize=1)
summary(mdl)

sa <- db$Sound_Acquisition %>%
  select(Id, UTC, sampleRate, Status) %>%
  mutate(Status=str_trim(Status),
         SaUTC=UTC,
         UTC=as.POSIXct(UTC, format='%Y-%m-%d %H:%M:%OS')) %>%
  arrange(UTC) %>% filter(Status=='Start') %>%
  data.table() %>% setkey(UTC)

clicks <- db$Click_Detector_OfflineClicks %>%
  mutate(CUTC=UTC, UTC=as.POSIXct(UTC, format='%Y-%m-%d %H:%M:%OS')) %>%
  data.table() %>% setkey(UTC)

# data table time join
jnd <- sa[clicks, roll=T]
