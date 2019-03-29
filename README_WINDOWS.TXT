README.TXT                                           @VER_DATE@

                                 VS2DRTI

        A Graphical Software Package for Simulating Water Flow and Heat
        and Reactive Solute Transport in Variably Saturated Porous Media

                    VS2DRTI - Version @MAJOR_MINOR@  @BDATE@

                                  for
                           Microsoft Windows
                                 



                          TABLE OF CONTENTS

                          A. SYSTEM REQUIREMENTS
                          B. DISTRIBUTION FILES
                          C. DOCUMENTATION
                          D. INSTALLING
                          E. RUNNING THE SOFTWARE
                          F. UNINSTALLING
                          G. TUTORIALS AND EXAMPLE PROBLEMS 


A. SYSTEM REQUIREMENTS

For installation of VS2DRTI, 75 megabytes of free disk space is needed.

To run VS2DRTI, the following are necessary:

  - Windows 7 or higher
  - 100 MB free disk space or more, depending on size of problem
  - display resolution of 800x600 or higher
  - display color of 16 bit or higher


B. DISTRIBUTION FILES

The following distribution files are currently available for
Windows:

  vs2drti-@VERSION@-@REVISION@-x64.msi  -  VS2DRTI version @MAJOR_MINOR@ installation program

  vs2drti@MAJOR_MINOR@_examples.zip  -  Zip file containing
                             examples and tutorial


C. DOCUMENTATION

This software package includes on-line help manuals that provide
detailed information on how to set up and run a simulation. To access
the on-line help manual, run the software and select "Contents" from 
the "Help" menu.

In addition, the following reports are available:

Parkhurst, D.L., and Wissmeier, L. 2015. PhreeqcRM: A reaction
   module for transport simulators based on the geochemical model PHREEQC.
   Advances in Water Resources 83: 176–189. DOI: 10.1016/j.advwatres.2015.06.001.

Haile, Sosina S. 2013. VS2DRT: Variable saturated two dimensional reactive 
   transport modeling in the vadose zone. (Doctoral dissertation) Freiberg Online 
   Geoscience 34: 1–152.

Parkhurst, D.L., and Appelo, C.A.J. 2013. Description of input and
   examples for PHREEQC Version 3—a computer program for speciation, batch-reaction,
   one-dimensional transport, and inverse geochemical calculations. U.S. Geological
   Survey Techniques and Methods 6-A43.
   An electronic version of this report can be downloaded from:
   https://pubs.er.usgs.gov/publication/tm6A43
   
Charlton, S.R., and Parkhurst, D.L. 2002. PHREEQCI—A graphical user
   interface to the geochemical model PHREEQC. U.S. Geological Survey
   Fact Sheet 031–02.
   An electronic version of this report can be downloaded from:
   https://pubs.er.usgs.gov/publication/fs03102  <p>


Hsieh, P.A., Wingle, William, and Healy, R.W., 1999, VS2DI--a graphical 
   software package for simulating fluid flow and solute or energy 
   transport in variably saturated porous media: U.S. Geological Survey 
   Water-Resources Investigations Report 99-4130, 16 p.
   An electronic version of this report can be downloaded from
   http://pubs.er.usgs.gov/pubs/wri/wri994130

Healy, R.W., and Ronan, A.D., 1996, Documentation of computer program
   VS2DH for simulation of energy transport in variably saturated porous
   media--modification of the U.S. Geological Survey's computer program
   VS2DT: U.S. Geological Survey Water-Resources Investigations Report
   96-4230, 36 p.
   An electronic version of this report can be downloaded from
   http://pubs.er.usgs.gov/pubs/wri/wri964230

Healy, R.W., 1990, Simulation of solute transport in variably saturated
   porous media with supplemental information on modifications to the U.S.
   Geological Survey's Computer Program VS2D: U.S. Geological Survey
   Water-Resources Investigations Report 90-4025, 125 p.
   An electronic version of this report can be downloaded from
   http://pubs.er.usgs.gov/pubs/wri/wri904025

Lappala, E.G., Healy, R.W., and Weeks, E.P., 1983, Documentation of
   computer program VS2D to solve the equations of fluid flow in variably
   saturated porous media: U.S. Geological Survey Water-Resources
   Investigations Report 83-4099, 184 p.
   An electronic version of this report can be downloaded from
   http://pubs.er.usgs.gov/pubs/wri/wri834099

D. INSTALLING

To install VS2DRTI, double-click vs2drti-1.4.0-12981-x64.msi to execute the 
installation program, and follow the directions on the screen.

By default, the software is installed in the directory
C:\Program Files\USGS\vs2drti-1.4.0.  This is the recommended installation 
directory, but you may use other directories or disk drives.

The following directory structure will be created (the contents of each 
directory are shown to the right):

      vs2drti-@VERSION@    ; README_WINDOWS.TXT
        -----bin       ; executables and DLLs
        -----database  ; Phreeqc database files
        -----doc       ; model reports and input instructions
        -----examples  ; VS2DRTI examples
 


E. RUNNING THE SOFTWARE

After the VS2DRTI package is properly installed as described above, 
the software can be run as follows:

1. Click the "Start" button
2. Select "Programs"
3. Select "VS2DRTI @MAJOR_MINOR@"
4  Click:
     VS2DRTI -- to simulate water flow and heat and solute transport 
     Postprocessor -- to run the postprocessor in standalone mode

It is anticipated that most user will not need to run the
numerical model VS2DRT independently from the graphical
user interface. However, if you need to run this model 
separately (from a DOS command prompt window), see the
file readme.txt in the "bin" directory.


F. UNINSTALLING

The following procedure is used to uninstall the VS2DRTI package:

  1. Click the "Start" menu, then click "Settings", and finally 
     click "Control Panel". The Control Panel is displayed.
  2. Double Click on "Add/Remove Programs". 
     The "Add/Remove Programs Properties" dialog box is displayed.
  3. From the list of installed programs, select "VS2DRTI @MAJOR_MINOR@".
  4. Click "Add/Remove". Windows will uninstall the application.

Note that user data will not be removed by the uninstallation
process.


G. TUTORIALS AND EXAMPLE PROBLEMS

Tutorials and examples for VS2DRTI are included in the download package.
These are designed to help the user learn to use the software 
and to allow testing of the software after installation. Each 
tutorial or problem is in its own working directory. 
For the example problems, the main output files are included 
(renamed as exampleN.out) for users to verify that the software 
is running correctly.


