# 'main/RAR-Uvm/Data/Data for thesis': All the data for R codes is kept here. All the event hydrograph and nutrient data are in csv's with the names of the site and event, separated by either '- Flow -' or '- EC,TP -'. These are the only csv's that do not start with a period ("."). This is because the R code used to import the event flow and nutrient data looks for csv's in the working directory, and omits files with leading periods (leading periods are deemed 'hidden' in windows file explorer). This keeps the R code that reads in the event dat from the entire folder simple.

# 'main/RAR-Uvm/Code': 'WorkspaceX.R' is used to process data into various dataframes  and lists and then saves then as .Rdata workspaces in the working directory. There are three .Rdata files genrated from 'WorkspaceX.R'. These are inputs into some of the other R files in this folder. I recommend running 'WorkspaceX.R' first, then the others.

# Please note:

# If errors occur, some of the library code chunks at the top of the file are commented out. To uncomment 'Ctrl+Shift+C', you only need to run the library functions once per R session.

# Warning messages are okay

# each file uses the 'setwd()' function to set the working directory to the 'Data for thesis' folder, you should change this to the path you have the folder saved to, or use the keyboard shortcut 'Ctrl+Shift+H' to choose in file explorer.
