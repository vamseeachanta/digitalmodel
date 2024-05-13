---

```json
{
    "settings": {
        "markdown.marp.themes": [
            "./css/schema.css"
            "./css/structure.css"
            "./css/leibniz.css"
        ]
    }
}
```


title       : AQWA External Force Function
author      : Vamsee Achanta
description : ANSYS AQWA Tutorial
keywords    : ANSYS, AQWA, External Force, Debug
marp        : true
paginate    : true
theme       : leibniz
math        : katex

---

<!-- _class: titlepage -->

# External Force Function

## ANSYS AQWA Tutorial

### Vamsee Achanta

#### 13.May.2024

##### ANSYS Utils Example Explanation

---

## Introduction and Objectives

**Agenda**

- Understand and run ANSYS AQWA Utils example
- Plot the Nodal force timetrace (and verify change with time)

- Understand the Existing example
- How to work with python in following methods
  - Command line
  - IDE (VS Code)
- Summary and Conclusions

---

## Running Process

External Force (EF) Server:

- External Force (EF) server
- Uses Python to open a port and Socket
- Utilizes Python 2 or 3 code as suitable
  - Any ANSYS commonfile Python version works
  - virtual environment (unverified)

AQWA Client:

- The AQWA .DAT Run that uses data from EF Server

- Run modes:
  - .dat or command mode (Supported)
  - Workbench
    - No external force specification given in Workbench (ANSYS/AQWA 2022R1)
  - Potentially can read back .dat file (not verfied?)

Steps to run:

1. Start EF server
1. Run AQWA client

```markdown
Best mode : using .DAT AQWA file in Command mode
```

---

## References

- ANSYS AQWA Reference Manual, 2024R1, Section 30.3. External Server for User-Defined Force Calculation
- [external Force Notes](docs\ansys\aqwa\external_force\external_force.md)
- ANSYS AQWA Utils folder:
<code>

ANSYS_INSTALL_DIR\v241\aqwa\utils\ExternalForceCalculation\
</code>

```markdown
Key Referenes
```

---

## .Dat file : Options

<code>

OPTIONS PPEL REST SUFC PBIS END
RESTART   1  5

</code>

<br>
<br>

PPEL - Print Properties of Each Element
REST - Restart ()A restart data record must follow the options list when the restart option is used.
SUFC - Look for External User-Defined Force Server
PBIS - Print Force Components at Each Iteration Step

```markdown
The Option to look for External User-Defined Force Server is **SUFC**
```

---

## Geometry

<img src="v222\101_s01_geometry_schematic.PNG" alt="geometry_schematic" width="400"/>

<br>

<code>
    01   1                0.00    -17.53     0.000 <br>
    01   2                0.00     0.000     0.000 <br>
    01   3                0.00     17.53     0.000 <br>
    01 101             -115.00     0.000   -50.000 <br>
    01 102             -115.00    -17.53   -50.000 <br>
    01 103             -115.00     17.53   -50.000 <br>
    01 104              115.00    -17.53   -50.000 <br>
    01 105              115.00     17.53   -50.000 <br>
    01 204              135.00    -17.53   -50.000 <br>
    01 205              135.00     17.53   -50.000 <br>
    01 303                0.00     17.53   -50.000 <br>
 END01 999                0.00     0.000     0.000 <br>
</code>

```markdown
Tube is defined
```

---

## IUFC/RUFC Records

- Control parameters can be used to configure the External Force server.
- Alternatively, use a suitable configuration (.INI or .YML or .PY etc.) to directly configure the python server.
<br>

<code>
    10    HLD1 <br>
    10RUFC    1    6        24     1.5e3    1.5e4      1.5e6    1.5e6        3e4  <br>
 END10
</code>

HLD1: Structure 1 related
IUFC - Up to 6 Interger (control) parameters. Also known as I_Control parameters
RUFC - Up to 6 Real (control) parameters. Also known as R_Control parameters

For above data:
1st Parameter number: 1
Last Parmeter number: 6
RUFC paramters: 24     1.5e3    1.5e4      1.5e6    1.5e6        3e4

```markdown
Utilize suitable configuration (AQWA .DAT, .INI, .YML or .PY etc.) to get maximum flexibility for the project.
```

---

## Environmental Loading

<img src="v222\101_s01_hr_load_summary.PNG" alt="geometry_schematic" width="400"/>

<br>

<code>
    13    SPEC <br>
    13SPDN                 0.0 <br>
    13CURR               8.000     300.0<br>
    13WIND              30.000     120.0<br>
 END13PSMZ                 0.3       2.0         5      11.0<br>

</code>

SPDN - Wave Spectral Direction

```markdown
Environmental loads defined.
```

## Nodal Force in .DAT File

<img src="v222\101_s01_hr_load_summary.PNG" alt="geometry_schematic" width="400"/>

<br>

<code>
    14    MOOR <br>
 END14FORC    1    3    0  303     30000  <br>

</code>

The LINE/WNCH/FORC Data Records - Linear Cables

- FORC - A constant force
- Structure 1, Node 3, Structure 0, Node 303, Force of 30 kN

```markdown
Based on Python Server User Function (UF) used, A linear force can be accessed and also overidden (eg. in UF2). 

This force can also be directly controlled in the external python program without .DAT definition.
```

---

## EF Server | Inputs

The key inputs are:

- Analysis: Analysis Class Object for data
- Mode:
- Stage:
- Pos: Position of structures
- Vel: Velocity of structures
- Time:
- TImeStep:
<br>

<code>

def UF3(Analysis,Mode,Stage,Time,TimeStep,Pos,Vel)

</code>
Using structure ID, the analysis outputs can be accessed and used for calculations.

```markdown
The analysis outputs can be accessed and utilized as EF server inputs to perform calculations.
```

---

## EF Server | Calculation - Node Current Position

- Use initial position (X, Y, Z) of a node (on a structure)
- Get current position using the Rotation matrix (based on rotation of relevant structure) and the initial position.

<br>

<code>
        RotMat=RotationMatrix(self.Pos[Struct][3:]) # Pos[Struct][3:] are the rotation angles

        # Calculating offset from COGs in definition axes system
        COGOffsetX = DefAxesX - self.COGs[Struct][0]
        COGOffsetY = DefAxesY - self.COGs[Struct][1]
        COGOffsetZ = DefAxesZ - self.COGs[Struct][2]

        ...

        # Need to apply rotation on that vector
        RotatedX,RotatedY,RotatedZ = RotMat.Apply(COGOffsetX,COGOffsetY,COGOffsetZ)

        # Now we just need to add the current COG position to the rotated vector
        FinalX = RotatedX + self.Pos[Struct][0]
        FinalY = RotatedY + self.Pos[Struct][1]
        FinalZ = RotatedZ + self.Pos[Struct][2]

</code>

```markdown
Calculation to get Node Current position
```

---

## EF Server | Calculation - Apply Force on Structure

- Transform the Force components at point of application and into Force and Torque components at the Structure CoG.
  - Use Point of force application and CoG coordinates for the transformation
- These forces with appropriate structure index is returned to apply in AQWA analysis

<br>

<code>
    def ApplyForceOnStructureAtPoint(self,Struct,FX,FY,FZ,AppPtX,AppPtY,AppPtZ):
        Force = BlankForce(self.NOfStruct)

        DX = AppPtX - self.Pos[Struct][0]
        DY = AppPtY - self.Pos[Struct][1]
        DZ = AppPtZ - self.Pos[Struct][2]
        
        Tx,Ty,Tz = ForceTorque(FX,FY,FZ,DX,DY,DZ)

        Force[Struct] = [FX,FY,FZ,Tx,Ty,Tz]

        return Force
</code>

```markdown
Calculation to apply force on Structure at a defined location
```

---

## EF Server | Outputs

- Force: Force to be applied on the structure at a given timestep
- AddMass: AddMass to be applied on the structure at a given timestep
-Error: If Error=1, the AQWA Client program will stop and analysis is aborted
<br>

<code>
return Force,AddMass,Error
</code>

```markdown
EF server outputs are used as AQWA client analysis inputs
```

---

## EF Server | Run in VS Code

Use this method to throughly understand how AQWA exposes its analysis data to Python.

- Open VS Code
- Set interpreter to ANSYS common files (or other suitable) python executable
- Run AqwaSocketUserForceExample.py
- "Socket now listening" message will appear as below (If no errors)
Running in VS Code no-debug (or in-debug) mode, the AQWA_SocketUserForceServerDetails.cfg file is not updated with appropriate port number.

Manually update AQWA_SocketUserForceServerDetails.cfg file with correct port number before running AQWAClient i.e. AQWA run.

<img src="v222\101_s01_hr_vscode_socket_running.PNG" alt="geometry_schematic" width="300"/>

<br>

**AQWA Runtime Analysis Error below**
<br>

<code>
 **** SYSTEM ERROR **** PROBLEM USING REMOTE USER FORCE. CONNECTION TIME OUT. IS SERVER RUNNING ?
 ------------- <br>
**** ANALYSIS ERROR **** USER_FORCE DLL ERROR NUMBER 1
 --------------------------------------------------------------------------------

</code>

```markdown
Manual edits are required to run with EF Server and AQWA client in debug mode.
```

---

## Utils | EF Server Example Functions

- 3 example functions i.e. UF1, UF2, UF3 are provided by AQWA
- UF1:
  - Takes the node position
  - calculates force and added mass
- UF2:
  - A custom calculation of force to be applied on strcture
- UF3:
  - A couple force applied on structure

```markdown
Summary of AQWA Utils
```

---

## Output Options

<img src="v222\101_s01_hr_load_summary.PNG" alt="geometry_schematic" width="400"/>

<br>

<code>
    18    PROP <br>
    18PRNT    1   34 <br>
    18NODE    1    1    0  102 <br>
    18NODE    1    3    0  103 <br>
    18NODE    1    1    0  104 <br>
    18NODE    1    3    0  105 <br>
 END18NODE    1    2            <br>

</code>

PRNT: For structure 1, print External force (34th Option)
LIST (.LIS file) Output for nodes 1, 3 and 2 on structure 1

```markdown
Output of External force defined.
```

---

<!-- _class: transition -->

# Summary and Conclusions

---

# Way Forward

- Utilize VS Code for custom EF Server programming/development

```markdown
  Proceed to impement in projects
```
