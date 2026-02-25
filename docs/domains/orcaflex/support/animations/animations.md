## Response 1

Dear Vamsee,

For attached files see:
docs\pkg_orcaflex\support\animations\response_1

A workspace file will display a series of windows together, including running the replay of animation alongside graphs and tables.  It is possible to export a video (.avi), or snapshots (.pdf) of an animated replay in a window, if that window is the active window.  However, the .avi/.pdf file produced will show only what is in that one window.  It will not record all of what is shown on screen.  When using workspaces, I tend to avoid using more than 6 windows on display at once if it needs to be shown to a large room.  The detail in the images and size of text (axis and tick labels) tends to be too small to see from the back you see.

I?m afraid that there is no means of recording what is on the screen (in the g.u.i.) with regard to multiple views (as an animation is being replayed), along with graphs etc. being visible.  Using the a.p.i. won?t help either I?m afraid, if I?ve understood correctly, since you would be requesting a recording of the way the g.u.i. is set up (with a workspace).  The nearest thing to that is a Python script which will, in turn, load each .sim file which is in a folder, then produce a snapshot of the model at a particular time defined by a user.  That is the archive attached ? I?ve given in the name Screenshots_Multiple Files.txt to try to avoid it being stripped from this e-mail.  Please change the extension to .7z, then unpack it.  You may modify it to use different times etc.  should it be of some use.

Using a suite of software from another company is the only way to do a recording of all of what is shown on a monitor screen and export it in a format which can be played outside OrcaFlex.  The videos which are available on Orcina?s website: either (1) the tutorials, or (2) introductions to the OrcaFlex objects were created using a suite of software known as Camtasia.  That is what is termed 3rd-party software, so we wouldn?t offer support for it.  I mention it only to give you an example of what will do the job.  In use, it simply records what it sees, mouse clicks and all.  

There is a custom-replay wizard within OrcaFlex though, to control the field of view (see the help page User interface | Replays | Custom replay wizard).  A user would create a series of instructions to display the replay for a simulation file, the replay being discretised in sections.  Each section of replay can be shown in either shaded- or wire-frame graphics mode and a user may set up view points at the start of a period of time and end of a period of time.  The size of the field of view and angle of view can be set at the start and end of a section of replay too.  It gives the effect of a camera panning, zooming & flying around the objects being modelled.  

To find the custom replay wizard, I?ve attached the file named how-to-find-custom-replay.png.  In addition, I have some attached images annotated with instructions about how to use it (please see the file read-me.txt for the specific names of files).  Thirdly, I have an example of a .txt file containing the information to produce one such custom replay.  That is the file Ch-lantern_pan&zoom.txt, which is for use with one of Orcina?s examples H01.  To help in that respect, I have attached the .dat file for H01 (H01 Chinese Lantern.dat) to this e-mail.  Please save it and run the simulation, then save the .sim file for it.  You should then be able to use the ??pan&zoom? file.

If a graph is open already, as well as a 3-d view, then the graph will remain visible if a custom replay is used.  Said graph can be edited (by double-clicking it), however it will not be possible to open a new graph through the button select results in the g.u.i.  If you have more than one window with a 3-d view open, please consider the option all views on the data form replay parameters too.  If that option is ticked, a custom replay will be applied to all windows of 3-d views showing the model.  If the option is not ticked, then a 3-d view window will continue to display what was contained beforehand, but will not show the animated replay.   In summary, a workspace file can be loaded to display a 3-d view and various graphs, then a custom replay can be opened afterwards to show a particular sequence of views flying around a model and zooming in/out, with any graphs being visible still.

I hope something in the discussion above will help you?
Yours sincerely,
Alistair Arnott

## Response 2

Hello again Vamsee,

For attached files see:
docs\pkg_orcaflex\support\animations\response_2


In addition to my previous e-mail, perhaps the following information will be useful it you do decide to use OrcaFlex?s custom replays.  The info. and files attached were created some years ago, but I?ve checked they will run in O/F 11.5.  The model attached is of a vessel near to a shape representing a quayside.  Six line objects act as Yokohama-style fenders and there are two simple moorings.  Please save the file Yokohama fender.dat and run the simulation, then save the .sim file.  In addition, please save the file Yoko-fender_flyaroundbow_zoominonfender.txt in the same folder.

In the normal replay, you'll see the environmental forces will push the vessel onto the fenders.  In turn, the vessel rebounds, etc.  Please stop the normal replay and open the data form to edit the replay's parameters.  In the upper-left corner, you'll see two options for replay type.  Normally, the active simulation would be the subject of the replay, using whatever 3-d. view is in use at the time.  However, please select custom replay.  The form will change to allow a user to create his/her own custom way in which a replay is shown, or load a file created previously.  Please browse for and load the .txt file Yoko-fender_flyaroundbow_zoominonfender.txt .

That animation is fairly straightforward.  If you want to perform a full 360° fly-by around a vessel for example though, then the following advice should be useful.

Beware when setting up the ?from? and ?to? parameters (part1)

If the settings from view parameters are as follows:
View size    View centre    View angles
                        X      Y    Z           Az    Elev   Gamma
130                70    0    3           0      10       0

and the settings to view parameters are:
View size    View centre    View angles
                        X      Y    Z           Az    Elev   Gamma
130               70    0    3           360    10     0

then the custom replay will appear to do nothing.  The viewpoint is the same: 0° is treated as the same angle as 360°.  I may as well have used 0° azimuth for the to view parameters.  If a user sets up a section of replay for which the to view parameters are the same as the from view parameters, that's how a user gets the replay to stay with one viewpoint for a number of seconds.  

Beware when setting up the ?from? and ?to? parameters (part2)

Keeping the same from view parameters as above, but changing the to view parameters to:
View size    View centre    View angles
                        X      Y    Z           Az    Elev   Gamma
130               70    0    3           359    10     0

one might hope a flight around the vessel by 359° would occur.  In the shaded graphics view it would not be noticeable if one ?loses? 1° from the rotation.  However, the viewpoint might not appear to change at all.  Instead of rotating 359° in one sense, it merely rotates 1° in the opposite sense.  The solver has treated 359° as a -1° rotation instead.  

Beware when setting up the ?from? and ?to? parameters (part3)

One might think of using two sections in the replay file, (1) to rotate the view from bow view to stern view, then (2) from stern to bow.   However, it is possible to get ?tripped up? here also.  The text file attached as customreplayA_2sections_0-180_180-0.txt is attached to show the problem.  The two sections are set up as 0° ? 180°, then 180° ? 0° again.  However, what happens is that the rotation around, from looking at the bow to looking at the stern, is merely reversed in the second half of the replay.

At this point, it's pertinent to mention the option use smoothed panning ... effects on the data form for the custom replay wizard.  If a user ticks that option, then the 'camera's motion' in a section of replay will slow down and stop when it comes to the end of that section.  The motion in the subsequent section will then accelerate again to fly around ... in this case back to looking at the bow of the vessel.  If that option is not ticked, then the join from section to section is not smooth and the reversal of the direction of rotation of the 'camera' is instantaneous.

What should work to give a full 360° flight around

Please load the custom replay file customreplayB_2sections_0-180_180-360.txt.  The angles of view are 0° ? 180°, then 180° ? 360° .  That file will produce a (seemingly) continuous full rotation around the model.  The option use smoothed panning ... effects is not ticked in order to achieve a flight around the vessel which resembles one continuous movement, instead of it slowing to a stop halfway around.

I hope that?s helpful?

Alistair



## Question 1


Dear Support Representative,

Are there any good generic resources for learning how to create animations in OrcaFlex? 

We ventured into opening .wrk files in ascii format. However, some questions (below) are lingering:

- Any starter python scripts for creating animations? Even if you can provide a dumb starting script (such as 58 OrcFxAPI example files) that you have, we will be able leverage it
- ReplayTime=6988.6 (start? or end?)
- Taking video of the tiled outputs (5-10) windows from OrcaFlex itself (or do we need to rely on the screen recording software?) 
- Any screen recording option in OrcaFlex directly or using python rather than leveraging external software  i.e. driven by command rather than manual steps

Thank you for your help in advance.

Best Regards,
Vamsee

