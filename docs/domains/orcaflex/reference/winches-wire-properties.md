# Winches: Wire properties

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

### Wire stiffness

The elastic stiffness $k$ of the winch wire. The [winch tension](Winchtheory.htm) contribution from wire stiffness is given by
\begin{equation}
t = k \epsilon
\end{equation}
where $\epsilon =$ wire strain.

### Wire damping

A dimensional stiffness-proportional material damping factor, $c$, for the winch wire. The [winch tension](Winchtheory.htm) contribution from wire material damping is given by
\begin{equation}
t = c\, k \ODt{\epsilon}
\end{equation}
where $\ODt{\epsilon} =$ wire strain rate.

|  |  |
| --- | --- |
| Note: | The mass of the winch wire is not modelled. |

### Winch inertia (detailed winches only)

The [inertia of the winch drive](Winchtheory.htm#Inertia), which resists changes in the rate of pay out or haul in of the winch wire, if the winch is in **force control** mode. The winch inertia has no effect if the winch is in **length control** mode.

This is a linear, rather than rotational, inertia. To represent the rotational inertia of a winch drum, set the value for winch inertia to
\begin{equation}
m = \frac{I}{r^2}
\end{equation}
where

$I =$ drum rotational inertia

$r =$ radius at which the wire is fed.

|  |  |
| --- | --- |
| Notes: | The winch inertia does **not** contribute to the mass of any objects to which the winch is connected and so does not **directly** resist acceleration of any of the connection points. (Such accelerations are resisted indirectly, of course, through the changes they cause to the winch wire path length and hence to the winch wire tension.) To include the true translational inertia of the winch drive, drum and wire it is necessary to suitably increase the masses of the objects to which it is connected. |
|  | Setting the winch inertia to a small value, to model a low inertia winch, can lead to very short natural periods for the winch system. These then require very short time steps for the simulation, extending the calculation time. To avoid this, the winch inertia can be [set to zero](Winchtheory.htm#Inertia), rather than to a small value; the winch system inertia is then not modelled at all and the short natural periods are avoided. |
