# This is the repository for all the materials used to create the manuscript. Welcome, please see below for how to generate reproducable tables and figures from the R codes and data files.

# 'C:\Tile-Drainage-Flow-and-P-in-VT\Data\Data for thesis - AHS swap' folder: All the event hydrograph and nutrient data stored as csv's in this folder, separated by either '- Flow -' or '- EC,TP -', respectively. These are the only csv's that do not start with a period ("."). This is because the R code used to import the event flow and nutrient data looks for csv's in the working directory, and omits files with leading periods (leading periods are deemed 'hidden' in windows file explorer). This keeps the R code that reads in the event data from the entire folder simple.

# 'C:\Tile-Drainage-Flow-and-P-in-VT\Codes\Workspace X - Full Source AHS swap' file: This is the source code that proccess the data in the folder mentioned above. Various dataframes and lists are created in the global enviornment and then saved as '.Rdata' file (workspaces) in the working directory. There are three .Rdata files genrated from this source code, 'work1.RData', 'work2.RData', and 'work3.RData'. These are inputs into the RMarkdown files that generate the figures and tables in the manuscript and supplemental materials (see below). 

# 'C:\Tile-Drainage-Flow-and-P-in-VT\Codes\Markdown' folder: The Rmarkdown files in this folder are used to generate html files in the directory where the code is located (does not save the .html to the working directory). 

# Please note:

# If errors occur, some of the library code chunks at the top of the file are commented out. To uncomment 'Ctrl+Shift+C', you only need to run the library functions once per R session.

# Warning messages are okay

# Each R code file uses the 'setwd()' function to set the working directory to the 'Data for thesis' folder, you should change this to the path you have the folder saved to, or use the keyboard shortcut 'Ctrl+Shift+H' to choose in file explorer.

# 'AHS Swap' are duplicates of the orginal files with additional chunks of code to address concerns about the name of the the field. For the tables in the manuscript, the Rmarkdown file did not need to be updated since we could manually edit the tables in Word. 
