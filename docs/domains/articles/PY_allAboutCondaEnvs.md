title: Conda Environments for Non-programmer
date: 2020-07-25
category: python

## Introduction
After getting started in Python and writing a few programs or applications, the inevitable moment arrives. With passage of time, the first program code does not work anymore.
Setting up the 3rd program has broken the back of the first program. However, investigation (may or may not) reveal the root cause.


Researching may reveal that the first program command works in previous version libarary and not the latest (eg. Pandas 0.21 vs 0.23).
However the end-clients always expect a program to work rain or shine at the drop of the hat. Frustration ensues possibly due to lack of advanced programming experience.

Virtual environments or virtualization offers a solution. Learn more about < blog/virutalization.html> here. Conda not only helps manage python dependencies but also help creates virtual environments so programs work in their own self-contained environments (silos)

Conda has become an increasingly reliable package to create, maintain and update projects for python and data science projects.
Conda, Miniconda and Anaconda are environments in the order of increasing weight or packages. The commands (and results) are very similar while using any of these environments.

## Conda Create Environment
The step by step instructions to create are:



## Summary of Key Commands
TBA



## Transitioning from Anaconda to Miniconda
If programming practice is to create environment at project start, transitioning to Miniconda is very natural.

Install miniconda in a new (adjacent) folder and keep it ready for next project. The key features or advantages are:

* latest conda (which is less error prone to work with)
* Light weight base environment so does not break easily (compared with Anaconda).

If anaconda already exists:

* Environments exist and are working, continue using them as usual.
* If conda version is old and can not be updated. Transition over these old environments when project or package refresh is required.
* Easier to refresh them in latest Miniconda version than old Anaconda version.
* Latest Miniconda version will find right packages and install them for you with less hassles.

## Migrating Environments
TBA

## Conda Packages
* If practical, define all version for all packages in the virtual envrionment file (environment.yaml or requirements.txt)
* If packages are not generic, fixing the version with "==" (not "">="") is recommended. Upgrades also bring in application specific problems (eg. an application may run 100% slower when numba is upgraded from 0.50 to 0.51).
In this scenario, planned (with testing) version upgrade of a package is required
* Generic packages may use ">=" if absolutely necessary for the application ecosystem.


## References
1. <https://medium.com/knerd/best-practices-for-python-dependency-management-cc8d1913db82>
1. <https://medium.com/knerd/best-practices-for-python-dependency-management-cc8d1913db82>
