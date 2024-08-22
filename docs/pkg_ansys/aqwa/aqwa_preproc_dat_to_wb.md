
If the customer is looking to create a single input file containing all of these structures, the best approach is to use the Aqwa Workbench editor, which allows you to import existing Aqwa dat files into the model. To do this:

    - Create a Hydrodynamic Diffraction system on the Workbench Project Schematic;
    - Create or import some simple geometry (just so that we can open the Aqwa editor);
    - Once the editor has opened, right-click on the Model object in the Outline tree and 'Insert from Aqwa Model Data File';
    - Once you have selected the first dat file, you will be asked 'Do you want to add another Aqwa Model Data File?' - click No to this, as the editor would be expecting the same geometry but with some additional data (e.g. environment definition);
    - Right-click on Model and 'Insert from Aqwa Model Data File' for each of the remaining Aqwa input files, until all of the geometries are included in the Aqwa editor;
    - Suppress the original simple geometry, assuming you do not want to include it in the final model.

Note that you are not able to move the structures once they have been imported, and MSTR cards are not considered in the data import (i.e. Deck 1 node positions are unmodified). However, you can use Starting Positions objects to re-position the structures in a Hydrodynamic Response analysis. Also note you are not able to modify the mesh that was defined in each Aqwa dat file. You should check that all of the relevant data has been imported; some data (e.g. Wave Directions, Wave Frequencies) are not included in the import. The structure numbering will not be preserved, but you can see the new structure numbers in the Hydrodynamic Diffraction Structure Selection details.

Alternatively, if the customer is intending to combine all of the existing hydrodynamic databases into one analysis, this can be done using the FILE/CPDB/CSTR data records from the FDR data category (Deck 6), see the Aqwa Reference Manual or more info. The dat files will need to be combined into a single definition of nodes, elements etc., either by manually copying/pasting the data or by using the Aqwa Workbench method described above. Then the FILE/CPDB/CSTR commands are used to merge the hydrodynamic databases (Aqwa hyd files) from each previous analysis into a single database for subsequent use in response calculations.
Let me know if you have any follow-up questions on this, or if this covers what you wanted to do.

Best regards,
Duncan Staggs
DRD Technology
ANSYS Elite Channel Partner
Support: (918) 743-3013 x1 / <support@drd.com>
Desk: (918) 743-3013 x602

From: Vamsee Achanta <vamseea@acma-inc.com>
Sent: Friday, July 26, 2024 8:47 AM
To: Duncan Staggs <dstaggs@drd.com>
Cc: Scott McClure <scottm@acma-inc.com>
Subject: RE: AQWA | Multiple Structure files imported into 1 model

Thank you for the prompt responses Duncan.

Practical modular usage examples are few and far between anywhere in the AQWA documentation.

From: Duncan Staggs <dstaggs@drd.com>
Sent: Friday, July 26, 2024 8:36 AM
To: Vamsee Achanta <vamseea@acma-inc.com>
Cc: Scott McClure <scottm@acma-inc.com>
Subject: RE: AQWA | Multiple Structure files imported into 1 model

(FYI, I just opened the new case with Ansys ? case #00093514 for reference. I?ll share what I get from Ansys on that as I receive it, of course.)

Best regards,
Duncan Staggs
DRD Technology
ANSYS Elite Channel Partner
Support: (918) 743-3013 x1 / <support@drd.com>
Desk: (918) 743-3013 x602

From: Duncan Staggs
Sent: Friday, July 26, 2024 8:25 AM
To: Vamsee Achanta <vamseea@acma-inc.com>
Cc: Scott McClure <scottm@acma-inc.com>
Subject: RE: AQWA | Multiple Structure files imported into 1 model

Hi Vamsee,

I?ll pose this question to the AQWA folks at Ansys.

Does this mean you had no further questions on the previous topic? (See attached.)

Best regards,
Duncan Staggs
DRD Technology
ANSYS Elite Channel Partner
Support: (918) 743-3013 x1 / <support@drd.com>
Desk: (918) 743-3013 x602

From: Vamsee Achanta <vamseea@acma-inc.com>
Sent: Thursday, July 25, 2024 6:12 PM
To: support <support@drd.com>
Cc: Scott McClure <scottm@acma-inc.com>
Subject: AQWA | Multiple Structure files imported into 1 model

Dear Customer Service,

I have run different Diffraction runs in individual files:
FST1 Structure: Draft 1.dat, Draft 2.dat, Draft 3.dat (3 RAO files)
FST2 Structure: Draft 1, Draft 2, Draft 3 (3 RAO files)
Connecting structures: 12 off non diffracting.
LNGC Structure: Draft 1, Draft 2 ( 2 files)

I would like to be able to import these like a lego into a single file to prepare my main model and run.

a/ Any easy way to import the above to construct my model from individual files above (from geometry and all the way through) without changing the structure numbering etc.
b/ A secondary requirement is that if I can offset each individual model files. I will at least take care of the approximate right positions as I know them upfront.
c/ I can combine in Workbench (or in .dat file) as  you see fit.

Thank you,
Vamsee
