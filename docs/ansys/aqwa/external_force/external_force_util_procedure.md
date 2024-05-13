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

# External Force Function

## ANSYS AQWA Tutorial

### Vamsee Achanta

#### 11.May.2024

##### Understand Details

---

## Introduction and Objectives

- Understand the ANSYS AQWA Utils example
- Define a new function to provide the force to simulate
- Plot the Nodal force timetrace (and verify change with time)

**Agenda**

- Understand the Existing example
- Define new example
- Verify output
- Summary and Conclusions

---

## Options

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
TBA
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
TBA
```

---

## IUFC/RUFC Records

<img src="v222\101_s01_geometry_schematic.PNG" alt="geometry_schematic" width="400"/>

<br>

<code>
    10    HLD1 <br>
    10RUFC    1    6        24     1.5e3    1.5e4      1.5e6    1.5e6        3e4  <br>
 END10
</code>

HLD1: Structure 1 related
IUFC - Up to 6 Interger (control) parameters
RUFC - Up to 6 Real (control) parameters

For above data:
1st Parameter number: 1
Last Parmeter number: 6
RUFC paramters: 24     1.5e3    1.5e4      1.5e6    1.5e6        3e4

```markdown
What do the control parameters defined above mean?
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

## Nodal Force

<img src="v222\101_s01_hr_load_summary.PNG" alt="geometry_schematic" width="400"/>

<br>

<code>
    14    MOOR <br>
 END14FORC    1    3    0  303     30000  <br>

</code>

The LINE/WNCH/FORC Data Records - Linear Cables

FORC - A constant force
Structure 1, Node 3, Structure 0, Node 303, Force of 30 kN

```markdown
Linear Constant Force
```

---

## Python Function | Inputs

Analysis,Mode,Stage,Time,TimeStep,Pos,Vel

<br>

<code>
??
</code>

??

```markdown
Based on structures etc., One can access inputs.
```

---

## Python Function | Run in VS Code

- Open VS COde
- Set interpreter to ANSYS common files python executable
- Run AqwaSocketUserForceExample.py
- "Socket now listening" message will appear as below (If no errors)

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

Use this method to throughly understand how AQWA exposes its analysis data to Python.

Running in VS Code no-debug (or in-debug) mode, the AQWA_SocketUserForceServerDetails.cfg file is not updated with appropriate port number.

Manually update AQWA_SocketUserForceServerDetails.cfg file with correct port number before running AQWAClient i.e. AQWA run.

```markdown
Successful run with external client in debug mode.
```

---

## Structure Force and AddMass | UF1 Outputs

Force,AddMass,Error

<br>

<code>
??
</code>

??

```markdown
The Force,AddMass are provided to a structure
```

---

## Node Force | UF2 Outputs

Force,AddMass,Error

<br>

<code>
??
</code>

??

```markdown
The Force are provided to a node of a structure
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

- Utilize VS Code to keep the external server running
- When using UF1, understand what are the inputs and outputs
- Also understand what the key inputs that we can access
- What are the key outputs/external Forces we can set?

```markdown
  Decide Yes or No to Jira now.
```
