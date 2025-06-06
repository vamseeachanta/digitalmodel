## Introduction

## Summary

### ToDo

- Try changing mesh
- Try changing number of CPUs
- Try changing geometry?
- <https://www.cfd-online.com/Forums/openfoam/>

### Helyx vs. OpenFOAM

For the pre and post processing layers for OpenFOAM
<https://engys.com/products/helyx>

| Feature | HELYX |  OpenFOAM |  Additional comments |
|---|---|---|---|
| Preprocessing | HELYX-GUI | CAD software <br> Blender for Engineers <br> FreeCAD | n/a |
| CORE | HELYX-Core | openFOAM | HELYX-Core is built on top of OpenFOAM |
| Hardware | Windows, Linux | Windows, Linux, MacOS | n/a |
| Operating System | Windows, Linux | Windows, Linux, MacOS | n/a |
| User Support | Help-line | - |  |
| Resources | Documentation | Documentation | n/a |
| Maintenance | Enterprise - more responsive | Open-source | n/a |
| Solver Technology | standard finite-volume approach <br> Generalised Internal Boundaries (GIB) method| standard finite-volume approach | n/a |
| Add-Ons | Marine Ship Hull <br> Adjoint <br> Others | standard finite-volume approach | n/a |

### Workflow Paths

- Example workflow paths:
  - CAD software -> Blender -> OpenFOAM -> Blender -> Outputs
  - CAD software -> Helyx -> OpenFOAM -> Helyx -> Outputs

### Installation

- Follow the instructions in the [OpenFOAM Installation Guide](https://github.com/vamseeachanta/energy/blob/61c3bb3bf50beeaac8f6cc2d4c9e143f91ff3083/eng/openfoam/InstallationOpenFoam-2.pdf). Replace "openfoam8" with appropriate openfoam version (i.e. "openfoam2212")

### Running

Typical steps for running a case study:

- Meshing
- Boundary Conditons
- Solver
- Paraview, see [paraview help guide](https://github.com/vamseeachanta/energy/blob/master/eng/paraview.md)

## Common Errors

### OpenFOAM

- The filepath name should not contain spaces
-

## Case Studies

### Summary

| Case Study | Source Zip Size |  Compiled Size |  Model Used |    Runtime (min) | Additional comments |  Learnings |  
|---|---|---|---|---|---|---|
| Venturi | 23 kB | 930 MB |  RAS <br> (Reynolds Averaging Simulation) | < 10  with 2 cpus | post-process | n/a |
| Wigley Hull | 80 MB | ? |  RAS | 6 hrs  with 4 cpus | post-process | n/a |

### Venturi

### Wigley Hull

## Clarifications

**HELYX Marine/AdJoint Add-On Capabilities**

- What are the capabilities? Is it just GUI abstraction or additional core solver capabilities?

**Naval Hydro Pack**

- Who is the owner/maintainer of Hydro Pack? OpenFOAM Foundation or someone else?
- What are the capabilities of Naval Hydro Pack?
- How to install and use Naval Hydro Pack?

### References

**Introduction to OpenFOAM**

[Introduction to CFD using OpenFOAM](https://www.youtube.com/watch?v=mGSUIXye9j4&list=PLcOe4WUSsMkH6DLHpsYyveaqjKxnEnQqB)
  
[First Simulation in OpenFOAM - part-1](https://www.youtube.com/watch?v=KznljrgWSvo)

[First Simulation in OpenFOAM - part-2](https://www.youtube.com/watch?v=bJTSKaxG58Y)

**Marine Hydrodynamics - Meshing**

- [CFD Geometry WOrkflow Using Blender](https://www.youtube.com/watch?v=Asalec1dFR4)

- [Meshing STL Using snappyHexMesh in OpenFOAM
](https://www.youtube.com/watch?v=ehDEcCVN2MI)

- [Quick and dirty introduction to overset meshes in OpenFOAM-V2312
](https://www.youtube.com/watch?v=jD5YkqCJajs)

- [18th OpenFOAM Workshop - Naval hydrodynamics 1](https://www.youtube.com/watch?v=etn3Yi8BGV8&t=2389s&pp=ygUPb3BlbmZvYW0gbmF2YWwg)

**Marine Hydrodynamics - Hull Form Design**

- [Ship resistance prediction (Luofeng Huang, UCL)](https://www.youtube.com/watch?v=xcdq4CAw7BM)

- [Wigley hull - VOF with free surface](<http://www.wolfdynamics.com/tutorials.html?id=149>)
  - [Wigley hull - VOF with free surface | input files](http://www.wolfdynamics.com/validations/wigleyhull/wigleyHull_LTS.tar.gz)
- [Flow about the classical Ahmed body](http://www.wolfdynamics.com/tutorials.html?id=146)
- [Two ahmed bodies in platoon](http://www.wolfdynamics.com/tutorials.html?id=147)
- [3D dam-break simulation](http://www.wolfdynamics.com/tutorials.html?id=95)

- [SIG_Ship_Hydrodynamics](<https://openfoamwiki.net/index.php/SIG_Ship_Hydrodynamics>)
- [Estimation of hydrodynamic derivatives of an appended KCS model in open and restricted waters](https://www.sciencedirect.com/science/article/pii/S0029801822022302)

- [Joint Research Project | Ship Energy Efficiency Solutions](<https://www.jores.net/>)

- [Seakeeping (head wave) using OpenFOAM](<https://www.youtube.com/watch?v=k_qtcKA_nYQ&list=PLt4NwTlK-IVNogd4noiSHmlqmnJ16zGcJ>)

- [Ship Hydrodynamics Lecture 16](https://youtu.be/uWcPtCTEI3c)

**Marine Hydrodynamics - Diffraction Analysis**

- [Floating Object Stability Modeling with OpenFOAM,  Tutorial Part 1](https://www.youtube.com/watch?v=4A2aWzRy1j4)

- [Floating Object Stability Modeling with OpenFOAM,  Tutorial Part 2](https://www.youtube.com/watch?v=DvWNpVYx5xo)

- [Floating Object Stability Modeling with OpenFOAM Tutorial - Animation](https://www.youtube.com/watch?app=desktop&v=QWCmDi1fw20)

**OpenFOAM Workshop Videos**

- [18th OpenFoam Workshop | Naval Hydrodynamics I](<https://youtu.be/etn3Yi8BGV8>)

- [17th OpenFoam Workshop | Naval Hydrodynamics I](<https://www.youtube.com/watch?v=PDDRRz478fs>)
- [17th OpenFoam Workshop | Naval Hydrodynamics I](<https://www.youtube.com/watch?v=Nr7tMtII-DU>)

**OpenFOAM Videos**

- [OpenFOAM® validation cases ready to use](http://www.wolfdynamics.com/tutorials.html?id=94)
- [openfoam Youtube Channel](<https://www.youtube.com/@openfoamjournal6606>)
- [Fluid Structure Interactions](<https://www.youtube.com/watch?v=Lnu4muOXV0Q>)

**Postprocessing**

- [Using blender to visualize OpenFOAM outputs?](<https://www.youtube.com/watch?v=yp9khQtP1g8>)

- [How to open OpenFOAM� results in ParaView](https://www.youtube.com/watch?v=8J59CpaYnVc)

- [OpenFOAM Documentation | Getting Started](<https://www.openfoam.com/documentation/tutorial-guide/1-introduction/1.1-getting-started#x4-30001.1>)
- [learn-openfoam](<https://holzmann-cfd.com/community/learn-openfoam>)

**Course Work**

- [MIT | numerical-marine-hydrodynamics](<https://ocw.mit.edu/courses/2-29-numerical-marine-hydrodynamics-13-024-spring-2003/>)
- [MIT | design-principles-for-ocean-vehicles](<https://ocw.mit.edu/courses/2-22-design-principles-for-ocean-vehicles-13-42-spring-2005/pages/readings/>)
- [MIT | Numerical Fluid Dynamics](<https://ocw.mit.edu/courses/2-29-numerical-fluid-mechanics-spring-2015/pages/lecture-notes-and-references/>)
- [MIT | ocean-wave-interaction-with-ships-and-offshore-energy-systems](<https://ocw.mit.edu/courses/2-24-ocean-wave-interaction-with-ships-and-offshore-energy-systems-13-022-spring-2002/>)

**FoamPython**

- [FoamPython, OpenFOAM utilizing python](<https://www.youtube.com/watch?v=EDAn2uFJ6jU>)

- [FoamPython, Link not working](<https://gitlab.com/share-renderluh/foampython-1.0>)

**UDEMY courses**

<https://www.udemy.com/course/learn-openfoam-cfd-level-3/?couponCode=ST2MT110724ANEW>
<https://www.udemy.com/course/learn-openfoam-cfd-level-2/?couponCode=ST2MT110724ANEW>
<https://www.udemy.com/course/learn-openfoam-cfd-level-1/?couponCode=ST2MT110724ANEW>
