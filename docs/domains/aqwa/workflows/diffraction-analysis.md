## Layout

### General Modelling

Need a flowchart here

- Geometry Mesh
- Define properities (For mass , cog, gyradii, )
- Scripts

ANSYS. REad one file from another. CHain them?
<https://www.mm.bme.hu/~gyebro/files/ans_help_v182/ans_cmd/Hlp_C_INPUT.html>

<https://www.boatdesign.net/threads/ansys-aqwa-automation.46251/>

Check AQWA Reference, chapter 5. There it is explained under 'Running AQWA using command file'

Old AQWA manual:
<http://oss.jishulink.com/caenet/forums/upload/2008/03/11/14408771118338.pdf>

## General Notes

AQWA is a "script friendly" product. Some tools to use are:
a/ journaling
b/ JScript programming
c/ utilities like Aqwa reader.

# Questions

- Is there python interface available for AQWA to get results?

# Calculate RAOs

NRNM - Calculates Nodal RAOs With No Moorings

(L) This option is used to output in Aqwa-Line run RAOs at particular nodes defined in Data Cat-
egory 18. The run stages should be from 1 to 5.

- An experiment is performed with an example RUn. Check it works or otherwise?


### Preparing .dat file

- Always check that the TCG may need to be zero (minor requirement)
- THe mass based on displaced volume and given mass should not differ by more than 10%. Always correct the mass based on displaced volume. Utilize density of water to back calculate the mass and enter in input file
