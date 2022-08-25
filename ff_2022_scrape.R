#Initial Yahoo Data
yh_urls <- paste0("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=AD&pos=ALL&sort=DA_AP&count=",seq(0,200, by=50))

yh_list <- list()
j <- 1
for (j in seq_along(yh_urls)) {
  yh_list[[j]] <- yh_urls[[j]] %>%   
    read_html() %>% 
    html_table(fill = TRUE)
  Sys.sleep(1)
  j <- j+1                  
}

yh_df <- yh_list %>%
  map(as.data.frame) %>%
  bind_rows()

#Initial Number Fire Data
nf_content <- read_html("https://www.numberfire.com/nfl/fantasy/remaining-projections")
nf_lists <- nf_content %>% 
  html_table(fill = TRUE)

nf_table <- do.call(rbind, Map(data.frame, Player = nf_lists[1], Stats=nf_lists[2]))

#Initial Fantasy Football Pros
fp_content <- read_html("https://draftwizard.fantasypros.com/auction/fp_nfl.jsp?tab=tabO&C=0&1B=0&2B=0&SS=0&3B=0&OF=0&SP=0&RP=0&BN=6&Util=0&P=0&CI=0&MI=0&IF=0&LF=0&CF=0&RF=0&scoring=HALF&teams=12&tb=200")
fp_lists <- fp_content %>% 
  html_table(fill = TRUE)

fp_df <- fp_lists[[1]][c(2,4)]

#Initial Beer Sheets data
#Input your league settings in Inputs, using the row.names below as a guide
Inputs <- as.character(c(12, 1, 2, 2, 1, 1,
            0, 0, 0,
            4, 6, 6,
            0.04, 0.1, 0.1,
            0, 0, -2, 
            0, 0.5, 0,
            0, 200))

BS_Inputs <- as.data.frame(Inputs)

row.names(BS_Inputs) <- c("Teams", "QBs", "RBs", "WRs", "TEs", "RB/WR/TE flex",
                          "RB/WR flex", "WR/TE flex", "QB/RB/WR/TE super-flex",
                          "points per passing TD", "points per rushing TD", "points per receiving TD",
                          "points per passing yard", "points per rushing yard", "points per receiving yard",
                          "points per completion", "points per incompletion", "points per interception",
                          "points per carry", "points per reception (PPR)", "TE PPR Premium (added to PPR)",
                          "points per first down (PPFD)", "auction money")

bs_table <- read.csv(file = paste0("https://footballabsurdity.com//wp-content/plugins/BeerSheetRequests/sheets/", paste(Inputs, collapse=","),".csv"),
                     stringsAsFactors = FALSE,
                     header = TRUE)

