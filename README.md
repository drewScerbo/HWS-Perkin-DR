# HWS-Perkin-DR
These are our Programs used to Process data for the HWS Perkin Observatory.

## Image Processing

Instructions for students to run these programs:

1. Download and set up R with the RStudio IDE
-This will be the environment to run these programs
2. Make sure the 'FITSio' package is installed.
3. Download two files into the directory of the images; 'Filter_Image_Processing_Script.R' and 'Image_Processing_Functions.R'
-'Filter_Image_Processing_Script.R' is a script to that actually takes the images in it's directory and calibrates them. It will write the calibrated images into a subfolder called 'modified images'. During this it will create several master calibration images which are averages of the given calibration images. It will write the masters into a subfolder called 'calbration images'.
-'Image_Processing_Functions.R' is sourced in the beginning of and holds all of the functions used in the processing script. It is vital that this is also downloaded into the same directory.
4. Open 'Filter_Image_Processing_Script.R' in RStudio. Click the Source button.
-RStudio has two ways of running code. 'Run' will run the whole script or a highlighted area in interactive mode while sourcing will run without extra clutter in the console. IT IS VITAL THAT THIS IS SOURCED NOT RUN. Some aspects of the script will act differently when run than when sourced. These differences will cause errors.
5. In the console, a question of whether to run the script in interactive mode will apear. Input either a 'y' or 'n'.
-Interactive mode will ask the user for directory paths to missing calibration images. If not enough are found, it will look in directories that are closest chronologically for the images until some are found or there are no more. 

If anlyzing of flat fields is wanted:
6. Download 'FlatField_fitting.R' into the same directory.
-'FlatField_fitting.R' analyzes and plots the normalized. pdf's are saved into the directory
-This plots the master flat fields, so 'Filter_Image_Processing_Script.R' must be sourced first to create them.

## Exposure Time Calculator

An online widget was created to calculate three values, signal-to-noise ratio, magnitude of the star, and exposure time. Two of these values in the upper section must be provided to calculate the third. 

The other inputs in the middle section also affect the calculated values. 
1. 'Telescope/Instrument' is the telescope and instrument used to capture the image. We only have one combination. 
2. 'Filter' is the filter of the images captured; g, r, i, Y, or z. As of right now we only have values for the g filter.
3. 'Moon Phase' is self-explanatory. The moon phase (New, Half, or Full) affects the brightness of the sky and therefore, affects the background sky.
4. 'Airmass' is the calculated airmass of the star.

Once all input values are set, the 'Calculate' button will display the calculated values in the bottom section. 
