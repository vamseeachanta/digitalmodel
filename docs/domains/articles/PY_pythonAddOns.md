title: Python AddOns for Non-Programmer
date: 2020-07-25
category: python

## Introduction
After getting started in Python and writing a few programs or applications, we spend time on the some repeat yet time consuming tasks.
It sometimes feel like these time consuming tasks are defeating the main purpose (Saving Time) of writing code in the first place.

We bring a few tools that help increase efficiencies.

## Summary



## Autoformat


If anaconda already exists:

* Environments exist and are working, continue using them as usual.
* If conda version is old and can not be updated. Transition over these old environments when project or package refresh is required.
* Easier to refresh them in latest Miniconda version than old Anaconda version.
* Latest Miniconda version will find right packages and install them for you with less hassles.

## YAPF in Pycharm
Setting up Yapf on Pycharm:
Click on Settings
Tools
Click on YAPF
Plugin settings:
Format on save
YAPF settings
Executable path: C:\Data\Continuum\Anaconda3\envs\mole_368\Scripts\yapf.exe
Style filename: .style.yapf
Contents of style file:
[style]
based_on_style = google
column_limit=119

Location of style filename is :

## References
1. <https://github.com/google/yapf>
1. <>
