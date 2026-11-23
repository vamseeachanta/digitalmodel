# Introduction

FreeCAD is a computer aided design (CAD) software. The capabilities are:


Create objects in freeCAD? 
https://www.youtube.com/@FreeCADAcademy

Example wind turbine animation
https://www.linkedin.com/posts/barry-young-63b529230_floatingwind23-renewableenergy-floatingoffshorewind-activity-7115343878034964480-gxJj?utm_source=share&utm_medium=member_desktop


## Summary



### Workflows

Trying to create the best workflow is a challenge as tasks overlap among various softwares.

- Start with 3D CAD tool and then export STP file to blender if:
    - Accurate design and detailed drawings are required
    - An existing design already exists
- Start with blender if: 
    - Validity (and not accuracy) is the objective of the design


### Blender Capabilities

The high-level capabilities of blender are:
- Rendering
- Modeling and Sculpting
- Animation and Rigging
- Story artistry
- VFX
- Simulation
- Pipeline
- Video editing
- Scripting (Python API)
- Interface (UI, Window layout etc.)


Some detailed level capabilities of blender for mechanical engineering are:
- Blender can create technical drawings for your models i.e. It allows for 2D orthographic views and measurements of your objects within the viewport.
- Design and create models to 10th of mm. 
- Assembly of parts is extremely fast

Downsides for engineering:
- The object integrity is also difficult to maintain
    - With click of a mouse, the dimensions of a part can be altered. Blender is not good at keeping the dimensions accurate
    - Faces can be removed thus making it an empty or open vessel in a non-engineering way
    - The faces need wall thickness but there is not concept of wall thickness



https://workwut.com/mechanical-engineers-blender/

https://www.blenderbasecamp.com/home/can-you-use-blender-for-mechanical-engineering/


### Subsea UseCases


https://www.linkedin.com/posts/vincentmullenders_blender3d-visualengineering-ugcPost-7016417396265484288-gbjo/?utm_source=share&utm_medium=member_ios

https://www.heerema.com/heerema-marine-contractors/simulation-center
Vincent Mullenders

## Hardware Rerquirements



### Courses

### Blender

Starting tutorials:
https://www.youtube.com/watch?v=TPrnSACiTJ4
https://www.youtube.com/watch?v=nIoXOplUvAw&list=PLjEaoINr3zgFX8ZsChQVQsuDSjEqdWMAD



#### Blender for Engineers

[Blender for Engineers](https://www.skillshare.com/en/classes/Blender-for-Engineers-step-by-step-Guide/1343651764) course work consists of the following learnings:
- you will learn how to import cad files in blender (.stl file?)
- how to make animation using free cad models
- how to render step files in blender
- how to make mechanical drawings
- optimize your models for 3d printing

A usecase of [blender for large scale projects](https://blendergrid.com/learn/articles/tyler-disney-interview) where visualizations are performed using blender  by importing CAD drawings from revit (.fbx format).

[Precision modelling in Blender Full Course](https://www.youtube.com/watch?v=83yNYScsRPI)


#### FreeCAD + Blender

A [freecad and blender](https://www.blendernation.com/2020/08/31/mechanical-engineering-tutorial-with-freecad-and-blender/) is a tutorial for use.

### Extensions

#### .fbx 

- A closed format - usable with only certain software
- Not easily transferrable between systems
- Not optimized for transmission between applications (AR, animation etc.)

#### .gITF

- A closed format - usable with only certain software
- Not easily transferrable between systems
- Not optimized for transmission between applications (AR, animation etc.)


#### .OBJ



### Free Resources


| Name | Description  | Comments |
|---|---|---|
|[Mirage 3D Models](https://github.com/MirageYM/3DModels) | Sports car & Road Bike |  free resource |
|[3D Models](https://github.com/sparkfun/3D_Models) | Details unknown |  free resource |
|[3D Models](https://github.com/jaanga/3d-models) | Aircrafts, Saab, Blackjack etc. |  free resource |
|[free3d models](https://free3d.com/3d-models/furniture) | Architecture, vehicles, characters, aircraft, furniture, electronics, animals, plants, weapons, sports, food, anatomy |  free & paid resources |
|[blender add-ons](https://github.com/agmmnn/awesome-blender) | simulation/physics, Generator/builder, modeling, animation, render engine, texture/UV, GameDev, Misc, Space, sound/Music, free stocks,  |  add-on guidance |
|[blender starter by Ron Caster](https://github.com/Ron-Caster/blender) | unknown |  starter tutorials |
|[blender starter by jame](https://github.com/jamel931/jamel931.github.io) | unknown |  A website of tutorials? |

## References

https://www.udemy.com/course/python-scripting-in-blender-with-practical-projects

https://docs.blender.org/api/current/info_quickstart.html

https://www.youtube.com/watch?v=rHzf3Dku_cE (Python + Blender a very basic primer; 20 lines)

https://www.youtube.com/watch?v=XqX5wh4YeRw (Python + Blender a very crash course; 60 lines)

https://k3no.medium.com/the-basics-of-using-python-in-blender-46831fd094e6

https://demando.se/blogg/post/dev-generating-a-procedural-solar-system-with-blenders-python-api/

[IEEE article for using blender for engineering work](https://tryengineering.org/game/blender/)


[Year 2017: The pros and cons of using blender for engineering](https://blender.stackexchange.com/questions/53293/is-blender-actually-useable-for-engineering)

[Mechanical Engineering with Blender and FreeCad Tutorial](https://www.youtube.com/watch?v=AD_jyBN09jA)