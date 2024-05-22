## Introduction

Process to define external force

## External Force

Node position, velocity etc. in X and Y Direction
Calculate the forces in X and Y (Using script)
Apply back to the node (Using script)

- Run using a regular wave input to the ship
  - Check results (i.e. X,Y force on the mooring via X,Y velocity of the node)

External Force Example

### Develop Code

- Utilize virtual environment to test the python code
- Test the code with another function.
- Wireup AQWA node/variables to the forces

### Digital Model - Error Troubleshooting

Ensure the more than zero files are found by the program.

<code>
Exception ignored in: <function AqwaServerMgr.__del__ at 0x00000198EE4FD1C0>
Traceback (most recent call last):
  File "K:\python\digitalmodel\src\digitalmodel\custom\aqwa\ef_server\AqwaServerMgr.py", line 196, in __del__
    self.Connection.shutdown(socket.SHUT_RDWR)
</code>

### Way Forward

- Create a Python3 server (more flexibility). But many things have to be changed.
  - I am assuming we have to use Python2 server (i.e. Python.exe) for our AqwaServerMgr is primary Python 2. Going to Python 3 requires some handling of another python 3 virtual environment/server etc.? However, wanted to hear from the professionals.

-

### Python 3 vs. Python 2

Python Interpreter Versions

| ANSYS Version | CPython Version | Notes |
| --- | --- | --- |
| 2024 R1 | 3.8.5 | |
| 2022 R2 | 3.7 | |
| 18.1 | 2.7 | |

### Reference - Python 3 vs. Python 2

Alex,

The driving example files did not change between versions and I assumed that the Cpython is still old. I see that the newer versions are using Python3 interpreter.  Thank you for making me investigate this aspect further. I will start running in 2022 and take it further.

You can close this ticket for now.

Thank you,
Vamsee

From: Alex Austin <aaustin@drd.com>
Sent: Friday, May 10, 2024 10:11 AM
To: Vamsee Achanta <vamseea@acma-inc.com>
Subject: RE: ANSYS AQWA | External Force Calculation Python2 vs. Python 3

Well, you are running an old version of the software. ?? R18.1 is considered old at this point? not quite ancient but getting there. ??

Alex Austin
DRD Technology
Technical Support: 918.743.3013 x 1
Office: 918.743.3013 x 612

From: Vamsee Achanta <vamseea@acma-inc.com>
Sent: Friday, May 10, 2024 10:10 AM
To: Alex Austin <aaustin@drd.com>
Subject: RE: ANSYS AQWA | External Force Calculation Python2 vs. Python 3

The print statements in the AqwaServerMgr.py are my cue. I will try running today/Monday and will reach out for any help.

Programming in Py3 is relatively easier as I am well versed with the syntax. Py2 is old and even python organization does not support it.

Vamsee

From: Alex Austin <aaustin@drd.com>
Sent: Thursday, May 9, 2024 2:07 PM
To: Vamsee Achanta <vamseea@acma-inc.com>
Subject: RE: ANSYS AQWA | External Force Calculation Python2 vs. Python 3

Vamsee,

I do not see an issue with using Python 3? and I?m not sure how you?ve determined the AqwaServerMgr.py is Python 2. In any case, you can choose to use your own Python installation with this external force module, per the docs:

<https://ansyshelp.ansys.com/account/Secured?returnurl=/Views/Secured/corp/v241/en/aqwa_ref/aqwaref_ext_forces_server_pythonmod.html>

Frankly, I would simply use what is built into the installation, rather than jumping through many hoops to make something work, unless the built in functionality of Python does not work for some reason.

Alex Austin
DRD Technology
Technical Support: 918.743.3013 x 1
Office: 918.743.3013 x 612

From: Vamsee Achanta <vamseea@acma-inc.com>
Sent: Thursday, May 9, 2024 1:03 PM
To: support <support@drd.com>
Subject: ANSYS AQWA | External Force Calculation Python2 vs. Python 3

Dear Support Representative,

I am currently using the External Force calculation module given in AQWA Utils.

C:\Program Files\ANSYS Inc\v181\aqwa\utils\ExternalForceCalculation

Please clarify if we can Python 3 programming language? My thoughts are below but wanted to hear from the professionals before I develop bunch of code.

a/ I am assuming we have to use Python2 server (i.e. Python.exe) as our AqwaServerMgr.py is primary Python 2.
b/ I am also assuming that going to Python 3 requires some handling of another python 3 virtual environment/server etc.? However, wanted to hear from the professionals.

Thank you,
Vamsee
