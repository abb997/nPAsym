nPAsym

About

nPAsym is a user-friendly ImageJ plugin allowing to quantify nuclear shape 
asymmetry in digital images captured from cytologic and histologic preparations. 
It works with 8-bit grayscale images segmented into black (0) nuclear masks and white (255) background. 
The plugin is written in Scala and packaged in a single .jar file containing the plugin code and Scala libraries. 

Availability

As a .jar file, the plugin can be downloaded from: https://github.com/abb997/nPAsym/tree/master/bld . 
After download it has to be placed in the Plugins subfolder (./plugins – in Linux) 
within the ImageJ application folder. Please, be sure that the name of a .jar file contains an underscore sign. 
If not, change its name to, e.g., nPAsym_.jar. Next time ImageJ starts, nPAsym will be available in the Plugins menu. 
Demo images can be downloaded from: https://github.com/abb997/nPAsym/tree/master/img .

Building an executable JAR file

To compile nPAsym plugin one needs to install and configure sbt (an open-source build tool) 
(https://www.scala-sbt.org/release/docs/Setup.html), sbt assembly plugin (https://github/sbt/sbt-assembly), 
and copy ./jars/ij.jar from ImageJ installation directory to the ./lib directory of the project. 
Then, from the command line, a user has to run sbt assembly in the top directory of the project. 
During first compilation sbt installs Scala of a required version with all dependencies, 
then compiles the code and assembles it into a single .jar file. After that, one should copy 
target/scala-2.12/nPAsym-assembly-0.1.jar to the Plugins directory of the ImageJ installation. 
Don’t forget to change the name of a .jar file as indicated above.

Short user guidelines

1. In ImageJ, open a raw image of interest.
2. Segment it using built-in ImageJ functions.
3. Run nPAsym from the Plugins submenu of ImageJ.
4. If necessary, change the value of a lower size threshold (in pixels) in a pop-up box that comes up. 
5. Save PA quantities to a .csv or .txt file by clicking the Save as button in the menu of the Results table. 
6. If necessary, save the output image by selecting the Save option in the File menu.

Contributors

Oleg Golovko, Yurij M.Bozhok, Alexander G.Nikonenko
