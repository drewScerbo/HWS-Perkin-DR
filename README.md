# HWS-Perkin-DR
These are our Programs used to Process data for the HWS Perkin Observatory.

Instructions for students to run these programs:

1. Download and set up R with the RStudio IDE
-This will be the environment to run these programs
2. Make sure the 'FITSio' package is installed.
3. Download two files; 'Filter_Image_Processing_Script.R' and 'Image_Processing_Functions.R'
-'Filter_Image_Processing_Script.R' is a script to that actually takes the images in it's directory and calibrates them. It will write the calibrated images into a subfolder called 'modified images'. During this it will create several master calibration images which are averages of the given calibration images. It will write the masters into a subfolder called 'calbration images'.
-'Image_Processing_Functions.R' is sourced in the beginning of and holds all of the functions used in the processing script. It is vital that this is also downloaded into the same directory.
4. Open 'Filter_Image_Processing_Script.R' in RStudio. Click the Source button.
-RStudio has two ways of running code. 'Run' will run the whole script or a highlighted area in interactive mode while sourcing will run without extra clutter in the console. IT IS VITAL THAT THIS IS SOURCED NOT RUN. Some aspects of the script will act differently when run than when sourced. These differences will cause errors.
5. In the console, a question of whether to run the script in interactive mode will apear. Input either a 'y' or 'n'.
-Interactive mode will ask the user for directory paths to missing calibration images. If not enough are found, it will look in directories that are closest chronologically for the images until some are found or there are no more. 
