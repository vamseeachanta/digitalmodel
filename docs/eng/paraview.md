## Introduction

## Summary

### Installation - Ubuntu Software Store(s)

**Search in Ubuntu Application software or Snap Store and install Paraview.**

### Saving Animation

saving the animation
<code>
ffmpeg -i animation/animation.%04d.png venturi.mp4
</code>

### OpenFoam Tricks

[OpenFoam - Paraview: Extract Data for All Time Steps](https://www.youtube.com/watch?v=84-aJ3wLDgM&t=27s)

### References

<https://youtu.be/tWEGjWD8d2M>

### Installation - From Source Code

**Use Software store unless latest version is absolutely necessary**

- Remove old version (if any)
- Download the latest version of Paraview from [here](https://www.paraview.org/download/)

- Extract and copy
  - sudo gedit ~/.bashrc
  - Add the following line(s) to the end of the file:
    - export PATH=$PATH:/opt/ParaView-5.11/bin/
- Compile
- Terminal access
- Create a lancher icon
  - sudo gedit /usr/share/applications/paraview.desktop
  - Add the following line(s) to the end of the file:
<code>
[Desktop Entry]
Version=1.0
Name=ParaView 5.11
Exec=paraview
Terminal=false
Icon=/opt/ParaView-5.11/share/icons/hicolor/96X96/apps/paraview.png
Type=Application
x-Ayatana-Desktop-Shortcuts=NewWindow
[NewWindow Shortcut Group]
Name=New Window
Exec=paraview
TargetEnvironment=Unity
</code>
