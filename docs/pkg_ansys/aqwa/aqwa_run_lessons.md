## Key Lessons

The key lessons learnt for use of AQWA are given in this document.

### Summary

### Share Topology Usage

- For grouping structures as single entity so they can more together in AQWA

### Internal Lid

- DECK2
- format

<code>

</code>

### Starting position

- Stability run
- run dynamic run

Either positions can be

- Explicityly defined
- From a previous run (RESTART)

### Working in Workbench

### References

<https://www.inas.ro/en/ansys-structures-aqwa>

 ANSYS AQWA Documentation link

<https://ansyshelp.ansys.com/account/secured?returnurl=/Views/Secured/prod_page.html?pn=Aqwa&pid=Aqwa&lang=en>

#### Communications - Shared Topology Usage

From: Alex Austin <aaustin@drd.com>
Sent: Thursday, May 9, 2024 1:27 PM
To: Bram Weisman <bramw@acma-inc.com>; Vamsee Achanta <vamseea@acma-inc.com>
Subject: RE: ANSYS AQWA | Ship Pier Tutorial

Aqwa does not have this Prime Mesh method functionality.

Shared Topology is a way to share nodes across geometric topology that is coincident, rather than using contact or joints. This is separate from physics and applies to all physics. In the case Bram is talking about, ST is provided by the Prime Mesh method, so you don?t use ST in SpaceClaim.

Alex Austin
DRD Technology
Technical Support: 918.743.3013 x 1
Office: 918.743.3013 x 612

From: Bram Weisman <bramw@acma-inc.com>
Sent: Thursday, May 9, 2024 1:13 PM
To: Vamsee Achanta <vamseea@acma-inc.com>; Alex Austin <aaustin@drd.com>
Subject: RE: ANSYS AQWA | Ship Pier Tutorial

Vamsee,

If you are meshing in ANSYS Mechanical, there is another solution that can overcome lack of shared topology and (perhaps more importantly) inaccuracies (e.g. gaps or overlaps) in the geometry: ?Batch Connections? (pre 2024) or ?Automatic (PrimeMesh)? in 2024.  Shared topology isn?t necessary anymore.

Alex,

Do you see a clear advantage to Shared Topology?

Thanks,

Bram

From: Vamsee Achanta <vamseea@acma-inc.com>
Sent: Thursday, May 9, 2024 1:07 PM
To: Alex Austin <aaustin@drd.com>
Cc: Bram Weisman <bramw@acma-inc.com>
Subject: RE: ANSYS AQWA | Ship Pier Tutorial

Thank you ? like the confidence of the single line answer.

What does Share Topology really mean? Where do we get these high-level lessons from a  physics/fundamental point of view from SpaceClaim geometry. Was thinking SpaceClaim is a pure geometry thing but some physics is already happening there.

From: Alex Austin <aaustin@drd.com>
Sent: Thursday, May 9, 2024 1:04 PM
To: Vamsee Achanta <vamseea@acma-inc.com>
Subject: RE: ANSYS AQWA | Ship Pier Tutorial

In the pier component entity in SpaceClaim, go to the properties panel and set Share Topology to Share. I will be in meetings through 2:30.

Alex Austin
DRD Technology
Technical Support: 918.743.3013 x 1
Office: 918.743.3013 x 612

From: Vamsee Achanta <vamseea@acma-inc.com>
Sent: Thursday, May 9, 2024 1:02 PM
To: Alex Austin <aaustin@drd.com>
Subject: RE: ANSYS AQWA | Ship Pier Tutorial

Alex,

My SpaceClaim model has a single component for Pier/Lid/Base. But I could not get it import into a dedicated multibody part in Workbench. This possibly led to multiple problems.

Please can you share the .dat file or workbench file for the recreated tutorial? Alternatively, can we have a brief meeting before 2:30 PM today if possible to help review where I went totally wrong.

Thank you,
Vamsee

From: Alex Austin <aaustin@drd.com>
Sent: Thursday, May 9, 2024 11:49 AM
To: Vamsee Achanta <vamseea@acma-inc.com>
Subject: RE: ANSYS AQWA | Ship Pier Tutorial

Vamsee,

As a start, I recreated the tutorial as it outlines and was able to generate the same output on the last slide for global position RX/Y/Z.

The biggest difference I see is that the Pier bodies, including the base and lid, are not in a multibody part. If you look at the Pressure and Motions output, it appears the pier is bobbing in the water, which I don?t think is what you expect. I think this is simply leading to further issues downstream (hah!).

#### Reference - Working in Workbench, .dat files

Alex,

Thank you very much again for the prompt response. Never even realized till your email that there are 3 AQW folders in hr_wave run.

This is the exact information I was looking for. Thank you again for the taking time to write the details.

Vamsee

From: Alex Austin <aaustin@drd.com>
Sent: Friday, May 10, 2024 8:32 AM
To: Vamsee Achanta <vamseea@acma-inc.com>
Subject: RE: ANSYS AQWA | Ship Pier Tutorial

Vamsee,

For the full project:

For each system, if you right click on Solution > General Solver Input File, you can then navigate down the project folders to find the .dat files. Note that each system has its own directory, i.e., AQW, AQW-1, AQW-2, etc. Example:

\02_s03_hr_wave_files\dp0\AQW\AQW\AQ\Analysis\Analysis.dat
\02_s03_hr_wave_files\dp0\AQW\AQW\AQ\Analysis\Equilibrium.dat
\02_s03_hr_wave_files\dp0\AQW-2\AQW\AQ\Analysis\TimeResponse.dat

Those are the three inputs files for the diffraction, stability, and time response, in that order. These files have differences and reference the one before it for the latter two if you look at the RESTART line.

For each project:

If you compare the Analysis.dat between the s01 and s03 projects, you should see small differences. For something like this, I use Notepad++ and a plugin called Compare to compare files against each other. This pointed out several differences:

#### Communications - Slow Drift Problem
