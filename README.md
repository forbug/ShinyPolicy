Download folder and place in your working directory (check with getwd() in R).

Add a folder titled *data* and download the following files from SQL Developer in .csv format:
  -DW_CPP_MONOLN_POL_D
  -DW_CPP_MONOLN_POL_F
  -DW_CPP_MONOLN_POL_LOC_D
  -DW_CPP_MONOLN_POL_LOC_F_STG
  
In a separate R file, load each of the files using the following code:
  ```
  obj <- load.csv("PATH\TO\FILE.csv")
  saveRDS(obj,"PATH\TO\data\pol_RESPECTIVE_EXTENSION.rds")
  ```
  (Complete 4 times) -- find the proper rds file names to save them as at the top of *app.R*
  
 
