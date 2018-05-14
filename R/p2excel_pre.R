#' The excel output of a dataset prepration
#'@param tabname  The name of the tab,
#'@param datastable  The output dataset
#'@param tablename The name of the table as the frist row
#'@param filename The name of the excel file
#'@return An excel sheet that ouputs the table
#'@examples
#'wb <- createWorkbook()
#'wb <- p2excel_pre(tabname = "Uni PFS", datastable = uni_out, tablename = "Table 2, Univariable PFS", filesave = wb)
#'saveWorkbook(wb, file = "filename.xlsx", overwrite = TRUE)
#'@export
#'@name p2excel_pre
#'

p2excel_pre <-function(tabname = "Default", datastable, tablename = "Default", filesave)
{
  addWorksheet(filesave,tabname)
  writeData(filesave, tabname,data.frame( tablename), startRow=1,colNames=F)
  hs1=createStyle(fgFill="#DCE6F1",halign="CENTER",textDecoration="bold")
  writeData(filesave,tabname, datastable, startRow=2,headerStyle = hs1)
  setColWidths(filesave,tabname, cols = 1:(dim(datastable)[2]), widths = "auto")
  freezePane(filesave,tabname,firstActiveRow = 3, firstCol = TRUE)
  return(filesave)
}
