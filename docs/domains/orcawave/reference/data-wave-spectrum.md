# Data: Wave spectrum

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\rmin}{r\_\textrm{min}}$
$\newcommand{\rmax}{r\_\textrm{max}}$
$\newcommand{\f}[2]{f\_{\mathrm{#1}{\mathrm{#2}}}}\!$
$\newcommand{\NDir}{N\_\textrm{Dir}}$

The wave spectrum defines a sea state that is used for [stochastic linearisation](Theory,Hydrodynamicdrag.htm#DragLinearisation) of hydrodynamic drag.

In most cases you define an energy spectrum, together with data for how OrcaWave will discretise the spectrum into a finite set of wave components for the calculation. Various types of spectrum are available via the **wave type** data item. We refer to these wave types as [random waves](#DataForRandomWaves):

* [JONSWAP spectrum](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Environment,DataforJONSWAPandISSCspectra.htm)
* [ISSC spectrum](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Environment,DataforJONSWAPandISSCspectra.htm) (also known as Bretschneider or modified Pierson-Moskowitz)
* [Ochi-Hubble spectrum](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Environment,DataforOchi-Hubblespectrum.htm)
* [Torsethaugen spectrum](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Environment,DataforTorsethaugenspectrum.htm)
* [Gaussian swell](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Environment,DataforGaussianswellspectrum.htm)
* [User-defined spectrum](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Environment,Dataforuserdefinedwavespectrum.htm)

Alternatively, you can provide [user-specified components](#DataForUserSpecifiedComponents). This wave type gives you complete control over the wave components used in the calculation. It is typically used when comparing OrcaWave results with those produced by a different program, to ensure that the sea state is identical in the two programs.

|  |  |
| --- | --- |
| Notes: | The wave spectrum [has no direction](Theory,Hydrodynamicdrag.htm#ValidityOfL1) of its own. For each [wave heading](Data,Environment.htm#WaveHeadings), $\beta$, in the environment data, OrcaWave performs a separate drag linearisation in which the **sea state direction** is $\beta$. |
|  | The wave spectrum has no phase data. This is because it is only used to evaluate [expectations](Theory,Hydrodynamicdrag.htm#DragLinearisation), for which phase is irrelevant. |

## Data for random waves

Data common to all random waves are described below.

### Hs, Tz

$\Hs$ is the significant wave height. $\Tz$ is the mean zero up-crossing period.

### Number of wave directions, $\NDir$

This allows you to model a directional spread spectrum. If $\NDir=1$ (the default), then no spreading is applied.

If $\NDir>1$, a spreading function proportional to $\cos^n$ is used, where $n$ is the **spreading exponent**. This directional spectrum is discretised using an equal-energy approach.

|  |  |
| --- | --- |
| Note: | If $\NDir>1$, OrcaWave will [interpolate over heading](Data,Environment.htm#WaveSpectrumForDragLinearisation) during drag linearisation. In this case the [wave headings](Data,Environment.htm#WaveHeadings) must cover the full 360° range of directions. |

### Number of components

The number of wave components that will represent the random wave in the calculation. If $\NDir>1$, then you specify the number of wave components *per direction*. Otherwise, it is the total number of wave components.

|  |  |
| --- | --- |
| Note: | If the [maximum component frequency range](#MaximumComponentFrequencyRange) is not '~', then the actual number of components used can be greater than the number specified here. |

### Frequency spectrum discretisation method

This gives you control over the algorithm used to choose the wave components:

* Arithmetic progression: wave components equally-spaced in frequency
* Geometric progression: the ratio of successive wave component frequencies is constant
* Equal energy: each component represents an equal amount of spectral energy (sometimes described as the *equal area approach*)

### Relative frequency range

The **minimum** $(r\!\_\mathrm{min})$ and **maximum** $(r\!\_\mathrm{max})$ **relative frequency** determine the range of frequencies used for the frequency spectrum discretisation algorithm. Specifically, the overall frequency range considered is $\big[\rmin\f{m}{-},\ \rmax\f{m}{+}\big]$ where $\f{m}{-}$ is the frequency of the spectral peak with the lowest frequency and $\f{m}{+}$ is the frequency of the spectral peak with the highest frequency (for single-peaked spectra $\f{m}{-}{=}\f{m}{+}$).

The default values of $r\!\_\mathrm{min}{=}0.5$ and $r\!\_\mathrm{max}{=}10$ are usually sufficient to produce a good representation of the spectrum.

### Maximum component frequency range

Available only when the equal energy frequency spectrum discretisation method is selected. The **maximum component frequency range** $\delta\!f\_\mathrm{max}$ places an upper limit on the width of the frequency range represented by each wave component: each is constrained to cover a frequency range no greater than $\delta\!f\_\mathrm{max}$. A value of '~' means that the component frequency ranges are not constrained in this way.

This data item is provided because an equal energy discretisation can result in some wave components (e.g. those in the low and high frequency tails of the spectrum) covering a wide frequency range. If a single component represents a wide frequency range, this may give a poor model of system responses. We recommend that you use $\delta\!f\_\mathrm{max}$ to avoid such problems. If any of the wave components generated by the equal energy discretisation covers a frequency range wider than $\delta\!f\_\mathrm{max}$, then that component is repeatedly subdivided into components (which now each have lower energy) until all have a frequency range less than $\delta\!f\_\mathrm{max}$.

## Spectrum graphs and tables

The **view frequency spectrum** button displays a graph of the energy spectrum.

The **view direction spectrum** button similarly displays a graph of the direction spectrum. This button is only available if $\NDir>1$.

The **view wave components** button displays a spreadsheet giving details of the wave components that OrcaWave has chosen to represent a random wave.

|  |  |
| --- | --- |
| Note: | The direction spectrum graph and the wave components spreadsheet both report directions **relative** to the [sea state direction](#SeaStateDirection). |

### Other spectrum properties

The wave components spreadsheet also reports the following properties of the spectrum:

* Spectral moments $m\_0, m\_1, \ldots, m\_4$
* Spectral bandwidth parameter $\epsilon = \left[ 1 - m\_2^2/\left( m\_0 m\_4 \right) \right]^{1/2}$
* Mean period $T\_1 = m\_0/m\_1$. Note that $T\_1$ is sometimes denoted by $T\_m$.
* Peak period and frequency $\Tp$ and $\fm$. These are the period and frequency at which the spectrum has the greatest spectral density.

The $i^\textrm{th}$ spectral moment, $m\_i$, of a process with spectrum $S(f)$ is defined to be
\begin{equation}
m\_i = \int\_0^\infty f^i\ S(f)\,\ud f
\end{equation}
where $f$ denotes temporal frequency.

## Data for user-specified components

The *user specified components* wave type allows you to directly specify the wave components used in the calculation. For each component you give:

### Frequency or period

You may specify either one of these – the other is automatically updated using the relationship period = 1 / frequency.

### Amplitude

The single amplitude of the component – that is, half the peak to trough height.
