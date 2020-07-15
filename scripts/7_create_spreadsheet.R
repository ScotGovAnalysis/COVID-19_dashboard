# Downloadable datasets ----------------------------------------------

#set up default formats
options("openxlsx.dateFormat" = "dd/mm/yyyy")
options("openxlsx.datetimeFormat" = "dd/mm/yyyy")
options("openxlsx.minWidth" = 10)

#corrections to spreadsheets - e.g. making percentage formatting consistent
download_data <- map(datasets[["sg_template"]], ~ drop_na(.x))
download_data[["H3_transport"]][["%"]] <- download_data[["H3_transport"]][["%"]] * 100
download_data[["H4_unemployment"]][["rate"]] <- download_data[["H4_unemployment"]][["rate"]] * 100
download_data[["H4_claimants"]][["% change in claimant counts (RHS)"]] <- download_data[["H4_claimants"]][["% change in claimant counts (RHS)"]] * 100

download_wb <- createWorkbook()

#table of contents
addWorksheet(download_wb, sheetName = "TOC")
setColWidths(download_wb, "TOC", width = c(20, "auto"), cols = 1:2, ignoreMergedCells = TRUE)
addStyle(download_wb, "TOC", rows = 1, cols = 1, style = createStyle(fontSize = 14, textDecoration = "bold"))
writeData(download_wb, "TOC", x = "Table of Contents") #title
datasets[["sg_template"]][["TEXT"]] %>%
  filter(worksheet_name %in% names(download_data)) %>%
  arrange(table_no) %>%
  select(worksheet_name, table_no, TITLE_max_35_characters) %>%
  mutate(r = row_number()) %>%
  pwalk(~ {
    sheet_name <- sub("^H\\d", ..2, ..1)
    writeFormula(download_wb, "TOC", startRow = 1 + ..4,
                 x = makeHyperlinkString(sheet = sheet_name, row = 1, col = 1, text = sheet_name))
    writeData(download_wb, "TOC", startRow = 1 + ..4, startCol = 2, x = ..3) #table
  })

#add tables
datasets[["sg_template"]][["TEXT"]] %>%
  filter(worksheet_name %in% names(download_data)) %>%
  arrange(table_no) %>%
 select(position, worksheet_name, TITLE_max_35_characters, source, methodology, table_no) %>% #..1 is position, ..2 worksheet_name, and so on
pwalk(~ {
        data_table <- download_data[[..2]] %>% 
          select(matches("^year$"), contains("month"), contains("week"), contains("date"), everything())
        sheet_name <- sub("^H\\d", ..6, ..2)
        
        #set up sheet
        ncols <- length(data_table)
        nrows <- nrow(data_table)
        addWorksheet(download_wb, sheetName = sheet_name)
        setColWidths(download_wb, sheet_name, width = c(10, rep("auto", ncols - 1)), cols = 1:ncols, ignoreMergedCells = TRUE)
        
        #set up links (convert markdown format to excel links format)
        link <- ..4
        if(grepl("^.*\\[(.*)\\]\\((.*)\\).*$", link)) {
          names(link) <- sub("^.*\\[(.*)\\]\\((.*)\\).*$", "\\1", link)
          class(link) <- "hyperlink"
          link <- sub("^.*\\[(.*)\\]\\((.*)\\).*$", "\\2", link)
        }
        
        #add formatting
        addStyle(download_wb, sheet_name, rows = 1, cols = 1, style = createStyle(fontSize = 14, textDecoration = "bold"))
        addStyle(download_wb, sheet_name, rows = 4, cols = 1:ncols, style = createStyle(textDecoration = "bold", border = c("top", "bottom")))
        addStyle(download_wb, sheet_name, rows = nrows + 5, cols = 1:ncols, style = createStyle(border = c("top")))
        pwalk(list(data_table, names(data_table), seq_along(data_table)),
              function(x, name, id, sheet = sheet_name) {
                if(is.numeric(x) & !grepl("^year$", name, ignore.case = TRUE)) {
                  addStyle(download_wb, sheet, rows = 4, col = id, 
                           style = createStyle(halign = "right", textDecoration = "bold", border = c("top", "bottom"))) #right align column headings for numbers
                  x <- replace_na(x, 0)
                  if(any(x > 1000)) { #add comma formatting. Number formatting with 1 or 2 d.p. if data contains unrounded values
                    addStyle(download_wb, sheet, rows = (1:nrows) + 4, col = id, style = createStyle(numFmt = "#,##0"))
                  } else if(all(x == floor(x))) {
                    addStyle(download_wb, sheet, rows = (1:nrows) + 4, col = id, style = createStyle(numFmt = "#,##0"))
                  } else {
                    addStyle(download_wb, sheet, rows = (1:nrows) + 4, col = id, style = createStyle(numFmt = "0.0#"))
                  }
                } 
              }
        )
        
        #write spreadsheet
        writeData(download_wb, sheet_name, x = ..3) #title
        writeData(download_wb, sheet_name, x = link, startRow = 2) #source/link
        writeData(download_wb, sheet_name, x = data_table, startRow = 4) #table
        writeData(download_wb, sheet_name, x = "Notes:", startRow = nrows + 6)
        writeData(download_wb, sheet_name, x =  ..5, startRow = nrows + 7) #methodology
      })
#saveWorkbook(download_wb, paste0("download/Scottish Government COVID-19 data (", format(Sys.Date(), "%d-%m-%Y"), ").xlsx"), overwrite = TRUE)
saveWorkbook(download_wb, paste0("download/Scottish Government COVID-19 data (", last_updated$data, ").xlsx"), overwrite = TRUE)
