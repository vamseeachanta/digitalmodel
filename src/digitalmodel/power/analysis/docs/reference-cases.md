# Independent power-flow reference cases

These synthetic cases qualify only the stated circuit convention and numerical
outputs. They do not establish limits, reactive controls or compliance with IEEE
Std 399. Tolerances are numerical regression criteria, rather than design limits.

## Tap and charging convention

A real ideal transformer of ratio t is placed at the sending bus ahead of the
entire pi section. With y=1/(r+jx) and h=jb/2:

- T1: Yff=(y+h)/t², Yft=Ytf=-y/t, Ytt=y+h.
- T2: U=Vf/t; Iseries=(U-Vt)y; If=(Iseries+Uh)/t; It=-Iseries+Vt h.
- T3: Sf=Vf conjugate(If)=U conjugate(Iseries+Uh),
  St=Vt conjugate(It), Sloss=Sf+St.

These equations follow from Kirchhoff's current law and ideal-transformer power
conservation. The branch convention is also given in the
[MATPOWER manual, section 3.2, equations (3.1)–(3.2)](https://matpower.app/manual/matpower/Branches.html),
with zero phase shift. No MATPOWER execution is used.

For r=0.01, x=0.1 and b=0.2 pu, hand division gives y=(100-j1000)/101
and h=j0.1 pu. At t=0.95, T1 gives Yff=1.097062614848743-j10.85982282438770 pu.
The test checks every Y entry and calculates both terminal powers directly from
T2–T3, independently of build_ybus. The slack is 1∠0 pu and the PQ demand is
0.5+j0.2 pu. Both t=0.95 and t=1.05 are tested. Absolute criteria are 1e-12 pu
admittance and 1e-8 pu power; relative tolerance is zero. Both charging halves
are on the pi side of the ideal tap; the old unscaled sending shunt is excluded.

## Solved three-bus reference

The network is lossless and radial: 1—2—3, x12=0.5 pu, x23=0.25 pu,
zero resistance and charging, unit taps, 100 MVA base. Bus 1 is slack, bus 2
is PV and bus 3 is PQ. The independently selected rectangular voltages are:

R1: V1=1+j0, V2=0.96+j0.28, V3=0.90-j0.12 pu.

R2: I12=(V1-V2)/(j0.5)=-0.56-j0.08;
I23=(V2-V3)/(j0.25)=1.60-j0.24 pu.
Bus currents are I1=I12, I2=-I12+I23=2.16-j0.16, I3=-I23.

R3: S=V conjugate(I) gives S1=-0.56+j0.08,
S2=2.0288+j0.7584, S3=-1.4688-j0.024 pu.
These literal scheduled injections specify the test network; no package result
is used to derive them. The negative slack P is intentional import at the slack.

R4: |V2|=sqrt(0.96²+0.28²)=1 pu;
|V3|=sqrt(0.90²+0.12²)=sqrt(0.8244)=0.907964757025293 pu.
Angles from atan2 are 0, 16.26020470831196 and -7.594643368591445 degrees.

The solve starts from flat voltages, uses tol=1e-12 pu mismatch and compares all
three bus voltages, P and Q injections with absolute tolerance 1e-8 pu and angles
with absolute tolerance 1e-6 degree, with zero relative tolerance. The criteria
are tighter than the one-iteration error and looser than floating-point roundoff.
The identical numerical assertions reject max_iter=1 without consulting the
convergence flag. In particular, PV reactive output is 0.7584 pu at the solution
and about 0.3669443414 pu after one iteration. This is a local algebra check,
not a published benchmark or an additional engineering standard.
