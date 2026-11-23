# General data: Logging

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

OrcaFlex stores the results of a simulation by sampling at regular intervals and storing the samples in a temporary [log file](Logfiles.htm). When you save the simulation OrcaFlex writes the data to the simulation file, followed by a copy of the log file, so that the sampled values can be read back in again at a later date. The [location of any log files](Logfiles.htm#LogFileLocation) used by OrcaFlex is given in the [general data properties report](Generaldata,Propertiesreport.htm) and the [general data results](Generaldata,Results.htm).

### Sample interval

You can control the time interval between log samples by setting the **target sample interval** on the general data form. The **actual sample interval** will be the nearest whole multiple of the dynamic time step. You can obtain more information about the logging from the [general data properties report](Generaldata,Propertiesreport.htm). This reports the number of log samples that will be taken and the size of the resulting simulation file.

### Logging precision

You can also control the **precision** with which samples are logged.

Single precision uses 4 bytes to represent each value and gives about 7 significant figures, which is quite accurate enough for almost all applications. Double precision uses 8 bytes per value, giving about 16 significant figures but uses twice as much disk space.

Double precision logging is usually only needed in very particular cases. We therefore recommend that you use single precision logging unless you see signs of precision problems in the results. The typical signs of precision problems are that the curvature or bend moment time histories for a line look more like a step function than a smooth curve. If you see such results then try using double-precision logging.

A typical case giving rise to precision problems is one containing a pipe or riser which has an extremely high bend stiffness *and* which experiences large displacements during the simulation. Why? Well, OrcaFlex logs the positions of each node but, to save space in the simulation file, it does not log the curvature, bend moment etc but instead recalculates these results from the node positions whenever you request them. If both the bend stiffness and the node displacements are very large then this calculation can greatly amplify the small steps in node position (8th significant figure) that are present in a single precision log, giving a bend moment graph that has steps rather than being smooth.

### Start time

Specifies the simulation time at which logging commences. At simulation times before this value, no samples are recorded. If a value of ~ is specified then logging begins at the start of the simulation.

Simulation files can be large and this has implications for both storage space and performance of read/write file operations. Typically, the logged values are the largest section of the simulation file, and it may be desirable to log only the latter part of the simulation file in order to reduce the size of the file. When a simulation is only partially logged, results and replays are not available for those parts of the simulation that were not logged.
