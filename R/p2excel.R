#' The excel output of a dataset
#'@param tabname  The name of the tab,
#'@param datastable  The output dataset
#'@param tablename The name of the table as the frist row
#'@param filename The name of the excel file
#'@return An excel sheet that ouputs the table
#'@examples
#'p2excel(tabname = "Uni PFS", datastable = uni_out, tablename = "Table 2, Univariable PFS", filename = "Table2.xlsx" )
#'p2excel(datastable = uni_out)
#'@export
#'@name p2excel
#'

p2excel <-function(tabname = "Default", datastable, tablename = "Default", filename = "Default.xlsx")
{
  wb <- createWorkbook()
  addWorksheet(wb,tabname)
  writeData(wb, tabname,data.frame( tablename), startRow=1,colNames=F)
  hs1=createStyle(fgFill="#DCE6F1",halign="CENTER",textDecoration="bold")
  writeData(wb,tabname, datastable, startRow=2,headerStyle = hs1)
  setColWidths(wb,tabname, cols = 1:(dim(datastable)[2]), widths = "auto")
  freezePane(wb,tabname,firstActiveRow = 3, firstCol = TRUE)
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}
