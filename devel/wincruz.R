# wincruz
# Event 'S' is first sight. Spps have species, summarise below. Sight should be number.
windas <- swfscMisc::das.read('./devel/wincruz/CalC1647.das')
swfscMisc::das.spp.freq(windas)
# spcode .dat file is a fwf
spp <- read.fwf('./devel/wincruz/SpCodes_2013.dat',
                widths=c(4, 11, 39), stringsAsFactors=FALSE)
colnames(spp) <- c('Code', 'ShortName', 'ScientificName')
spp <- mutate(spp, Code = str_trim(Code),
              ShortName = str_trim(ShortName),
              ScientificName = str_trim(ScientificName))

windas %>% filter(!is.na(Sight) & !is.na(Spp1)) %>% head() %>%
    mutate(test=paste0(Spp1, Spp2, Spp3, sep=','), test=gsub('NA', '', test)) %>%
    str()
win <- windas %>% filter(!is.na(Sight) & !is.na(Spp1)) %>%
    select(Code = Spp1, SightingId = Sight) %>% distinct()