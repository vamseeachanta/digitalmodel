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


title       : Jira Adoption for ACMA
author      : Vamsee Achanta
description : Technology Enabled Engineering
keywords    : Technology, Engineering, Manufacturing.
marp        : true
paginate    : true
theme       : leibniz
math        : katex

---

<!-- _class: titlepage -->

# External Force Function - Typical Process

## ANSYS AQWA Tutorial

### Vamsee Achanta

#### 13.May.2024

##### Typical Procedure

---

## Introduction and Objectives

- Set-up and Run an ANSYS AQWA External Force loading conditon
- Benchmark Run:
  - with Fender properties out of box AQWA
  - With fender forces applied using EF server
- Define a new function to provide the force to simulate
- Plot the Nodal force timetrace (and verify change with time)

---

## References

[An AQWA Utils Example explained](docs\ansys\aqwa\external_force\external_force_util_example.md)

---

## Terminology & Running Process

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

## File Preperation | Files

Copy the following files from utils folder

- AqwaServerMgr.py
- AQWA_SocketUserForceServerDetails.cfg (If using VS code or other IDE)
- StartAqwaPythonUserForceServer.bat
- AqwaSocketUserForceExample.py (EF Server driver file)
- Appropriate .dat (eg: 02_s04_hr_ext_force.dat) file (For AQWA client analysis run)

```markdown
TBA
```

---

## File Preperation | Dat file (Client Run File)

- to the .dat (i.e. 02_s04_hr_ext_force.dat) file, add the following opton:
  - SUFC

- Add NODE to track external force applied
  - ?

```markdown
TBA
```

---

## File Preperation | EF Server Starter Batch

- StartAqwaPythonUserForceServer.bat
- The batch file uses latest ANSYS python release
- This can be changed to access another python release if required.

```markdown
TBA
```

---

## File Preperation | EF Server Socket Config File

- AQWA_SocketUserForceServerDetails.cfg
- In command mode run, this file is automatically created and updated by EF Server driver upon start
- In case of using VS Code (or other IDEs),
  - this file need to be created
  - port number needs to match the EF Server run. See screenshot below.

<img src="v222\101_s01_hr_vscode_socket_running.PNG" alt="geometry_schematic" width="300"/>

```markdown
# Server waiting for a connection from an AqwaClient on
ACMA-ANSYS03:52642

```

---

## Geometry

```markdown
Make necessary edits to the geometry in Workbench prior to saving the .dat file for EF Server runs.
```

---

## File Preperation | EF Server Driver File

- Rename "AqwaSocketUserForceExample.py" to appropriate file ie. (eg: 02_s04_hr_ext_force.py)
- Changes to EF Server Driver File
  - The ExpectedFileName parameters needs to be changed to the AQWA client .dat file. See below
    - ExpectedFileName = '02_s04_hr_ext_force'
- Add required functions to either of the following files:
  - AqwaServerMgr.py (EF Server manager)
  - 02_s04_hr_ext_force.py (EF Server driver file)

```markdown
TBA
```

---

## File Preperation | EF Server Driver File ... contd

- Identify the node location
- Utilize VS Code (or any text editor) to add required custom functions.
-
- Need additional logging to track External forces used to prove the case?

<img src="..\..\tutorials\03_dat\002_ship_with_pier\fender_as_ext_force.PNG" alt="geometry_schematic" width="300"/>

```markdown
TBA
```

---

## Configuration

??
---
