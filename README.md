nPAsym

About

nPAsym is a user-friendly ImageJ plugin allowing to quantify nuclear shape asymmetry in digital images captured 
from cytologic and histologic preparations. It works with 8-bit grayscale images segmented into black (0) nuclear 
masks and white (255) background. The plugin is written in Skala and packaged in a single JAR file containing 
the plugin code and Scala libraries. 

Availability

As a JAR file, the plugin can be downloaded from (target folder): https://github.com/ ...  After download it has 
to be placed in the Plugins subfolder (./plugins – in Linux) within the ImageJ application folder. 
Next time ImageJ starts, nPAsym will be available in the Plugins menu. 
Demo images can be downloaded from: https://github.com/ ...

Building an executable JAR file

To compile nPAsym plugin one needs to install and configure Scala (https://www.scala-lang.org/download/2.12.8.html), 
sbt (an open-source build tool) (https://www.scala-sbt.org/release/docs/Setup.html), and sbt assembly plugin (https://github/sbt/sbt-assembly). 
Then, from the command line, a user has to run sbt assembly in the top directory of the project. 
After that, one should copy target/scala-2.12/nPAsym-assembly-0.1.jar to the Plugins directory of the ImageJ installation. 

Short user guidelines

1. Open in ImageJ a raw image of interest.
2. Segment it using either a raster graphics editor or built-in ImageJ functions.
3. Run nPAsym from the Plugins submenu of ImageJ.
4. If necessary, change the value of a lower size threshold (in pixels) in a pop-up box that comes up. 
5. Save PA quantities to a cvs or text file by clicking the Save as button in the menu of the Results table. 
6. If necessary, save the output image by going to the File menu and selecting the Save option.

Contributors

Oleg Golovko, Yurij M.Bozhok, Alexander G.Nikonenko

