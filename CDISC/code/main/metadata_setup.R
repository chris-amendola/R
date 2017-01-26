# Create Metadata objects from excel file
code_lists<-access_excel_metadata( sheet_name="CODELISTS"
                                  ,path=paste(install_head,'\\metadata\\',sep=''))
dataset_metadata<-access_excel_metadata( sheet_name="TOC_METADATA"
                                        ,path=paste(install_head,'\\metadata\\',sep=''))
variable_metadata<-access_excel_metadata( sheet_name="VARIABLE_METADATA"
                                          ,path=paste(install_head,'\\metadata\\',sep=''))