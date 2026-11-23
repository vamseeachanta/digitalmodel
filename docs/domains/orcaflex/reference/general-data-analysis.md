# General data: Analysis

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

### Statics enabled

Specifies whether to run a [static analysis](Staticanalysis.htm) for this model. This is most useful for a [restart analysis](Restartanalyses.htm), when restarting from an existing static state or time domain simulation; however, it can also be used to disable statics for a standard OrcaFlex model, in which case dynamics will begin with the various model objects in their [reset state](Modelstates.htm#ModelStatesReset) positions.

### Dynamics enabled

Specifies whether to run a [dynamic analysis](Dynamicanalysis.htm) for this model.

### Start time/actual start time

The *start time* specifies the [simulation time](Dynamicanalysis.htm) at which the simulation will begin. A value of ~ for the start time means that the *actual start time* (the read-only value actually used in the analysis) is:

* Minus the duration of the first stage, if the model **is not** a [restart analysis](Restartanalyses.htm). This means that the first stage will end at a simulation time of 0s.
* The simulation time at the end of the [parent model](Variationmodels.htm#VariationModelsParentModels), if the model **is** a [restart analysis](Restartanalyses.htm). Sometimes, this can differ from the end time specified by the parent model's data because the duration of the parent model was not an exact multiple of its [time step](Generaldata,Dynamics.htm).

Otherwise, the start time and the actual start time are the same.

### First stage/actual first stage

The *first stage* is a number labelling the first stage of the simulation; each subsequent stage has a stage number one higher than its predecessor. A value of ~ for the first stage means that the *actual first stage* (the read-only value actually used in the analysis) is:

* Zero, if the model **is not** a [restart analysis](Restartanalyses.htm).
* One greater than the final stage of the [parent model](Variationmodels.htm#VariationModelsParentModels), if the model **is** a [restart analysis](Restartanalyses.htm).

Otherwise, the first stage and the actual first stage are the same. The stage number is used when defining [release at stage](#AnalysisStageRelease) data for objects and when selecting a [results period](Results,Producingresults.htm#ResultsPeriod).

### Ramp start time/actual ramp start time

The *ramp start time* is the time at which the [ramp function](Dynamicanalysis,Ramping.htm) first becomes non-zero. A value of ~ for the ramp start time means that the *actual ramp start time* (the read-only value actually in the analysis) is:

* Equal to the [actual start time](#AnalysisStartTime), if the model **is not** a [restart analysis](Restartanalyses.htm).
* Equal to the actual ramp start time defined by the [parent model](Variationmodels.htm#VariationModelsParentModels), if the model **is** a [restart analysis](Restartanalyses.htm).

Otherwise, the ramp start time and the actual ramp start time are the same.

### Ramp finish time/actual ramp finish time

The *ramp finish time* is the time at which the [ramp function](Dynamicanalysis,Ramping.htm) first becomes equal to one. A value of ~ for the ramp finish time means that the *actual ramp finish time* (the read-only value actually in the analysis) is:

* Equal to the simulation time at the end of the first [stage](#AnalysisStages) (or the [actual start time](#AnalysisStartTime) if no stages are defined), if the model **is not** a [restart analysis](Restartanalyses.htm).
* Equal to the actual ramp finish time defined by the [parent model](Variationmodels.htm#VariationModelsParentModels), if the model **is** a [restart analysis](Restartanalyses.htm).

Otherwise, the ramp finish time and the actual ramp finish time are the same. It is possible to define the ramp data so that the ramp start time is greater than the ramp finish time. If this happens, then OrcaFlex will interpret this as ramping down from one before the ramp finish time to zero after the ramp start time. This might be useful as a way of avoiding unwanted transients when redefining wave conditions in a [restart analysis](Restartanalyses.htm), by ramping down the old wave conditions at the end of the parent model and then ramping up the new wave conditions at the start of the child model. The ramp can effectively be turned off altogether by simply defining a ramp that is already fully built-up by the time the simulation starts.

### Time history import range

These data determine the range of [simulation times](Dynamicanalysis.htm) that will be imported for time history data ([wave elevation](Environment,Datafortimehistorywaves.htm#TimeHistoryWaveData), [wind speed and direction](Environment,Winddata.htm), [vessel motion](Vesseldata,Timehistory.htm) and [constraint imposed motion](Constraints,Imposedmotion.htm)).

Time history data must cover at least the range of times $[T\_{\rm start}, T\_{\rm finish}]$, where $T\_{\rm start}$ and $T\_{\rm finish}$ are the simulation start and finish times, respectively. However, the time history data may cover a much greater range of times. It is usually desirable not to import the entire time history data because this can lead to excessive memory use and can result in slower performance due to the increased cost of interpolating into a larger set of data.

If the time import range from time is set to ~ then $T\_{\rm start}$ will be used. Similarly, if the time import range to time is ~ then $T\_{\rm finish}$ will be used.

Using ~ for both from and to import range time leads to the minimum amount of data being imported and used. For many situations this is exactly what is required. However, for time domain dynamic [restart analyses](Restartanalyses.htm), and for [extended simulations](Calculationmenu.htm#MenuExtendSimulation), it is sometimes necessary to specify the import time range explicitly, in order to user a wider range of times.

Consider a dynamic restart analysis where the child has a simulation start time equal to the finish time of the parent. If the model uses time history data then it would be expected that these time histories would transition continuously from parent analysis to child analysis. However, if ~ is used for the import range times then the parent and child analyses will import different ranges of times. This may result in a discontinuity at the parent to child transition.

The time history import range data can be used to overcome this problem. If an explicit range of times is specified that covers both the parent and child simulations, then both parent and child analyses will import the same ranges of times, and therefore the transition from parent to child will be continuous.

The above example considers a restart involving two analyses, parent and child. But in general there may be a longer chain of restarts. If you wish to have continuity of time history data for the entire restart chain, then you need to specify an import range that covers the entire chain. That is, the import from time should be the simulation start time of the first model in the chain, and the import to time should be the simulation finish time of the last model in the chain.

For [extended simulations](Calculationmenu.htm#MenuExtendSimulation) the considerations are a little different. For a restarts, each analysis in the restart chain imports the time history data at the beginning of the calculation. For extended simulations the time history data is not re-imported when the simulation is extended. This means that the entire range of times for the original simulation, and the extension, must be imported at the very beginning of the original calculation. So if ~ is used for the import to time then only times to the end of the original simulation will be imported. This means that extending the simulation will not be possible. To avoid this limitation you should specify an import to time that extends beyond the extended simulation finish time. Of course, this does require you to anticipate the possibility of extending the simulation before you start running the original simulation.

### Solution method for rigid connections

Specifies whether rigid connections between objects should be enforced ([wherever possible](Objectconnections.htm#SolutionMethod)) via the **direct** or **indirect**  [solution method](Objectconnections.htm#SolutionMethod).

## Stages

Time domain simulations proceed in a number of **stages**, each of a given **duration**. By default, the first stage of the simulation is a [build-up period](Dynamicanalysis,Ramping.htm), during which the sea conditions are slowly ramped up from zero in order to avoid sudden transients when starting a simulation. The default build-up stage is numbered 0, and time during this build-up stage is reported as negative, so that the first stage proper is stage 1 and starts at time $t{=}0$. This default behaviour can be overridden by defining a custom ramp using the above data.

If dynamics is [disabled](#AnalysisDynamicsEnabled) or a [frequency domain](Dynamicanalysis,Frequencydomainsolution.htm) dynamic analysis is being performed, then the stage duration data is not relevant and is unavailable. We nevertheless consider that the static analysis takes place immediately *before* the [actual first stage](#AnalysisStartTime) defined by the [data](#AnalysisFirstStage) (but *after* any preceding stage in a chain of [restarts](Restartanalyses.htm)). This is mainly for conceptual clarity when considering [restart analyses](Restartanalyses.htm) that can have several sequential static and dynamic analyses defined over a chain of models. Similarly, the static analysis conceptually occurs at precisely the [actual start time](#AnalysisStartTime). This time is used to define the behaviour of those aspects of the model that are active in statics but are usually considered to have time-dependence, such as the value of the [ramp](Dynamicanalysis,Ramping.htm) factor when scaling the [current velocity](Environment,Currentdata.htm#SeaCurrentRamp).

Conversely, frequency domain dynamic analyses are not considered to 'take place' at a particular stage or simulation time: whilst time is still relevant in the frequency domain, in the sense that we can [synthesise time histories](Dynamicanalysis,Timehistorysynthesis.htm) after the analysis is complete, this time is an idealised time that is based on infinitesimal periodic motions with no well-defined beginning or end.

Whilst the stage numbering is generally arbitrary, one context where it can be significant is connection release for [lines](Linedata,Endconnections.htm#LineRelease), [links](Links,Data.htm#LinkRelease), [winches](Winches,Data.htm#WinchRelease), and [constraints](Constraints,CalculatedDOFs.htm#ConstraintOutFrameRelease). Release is defined to take place at the beginning of a given stage. For example, if a link is set to release at the start of stage 0 and the actual first stage is also stage 0, then the link will remain connected during the static analysis (which is deemed to take place immediately *before* stage 0), but then disconnect once the time-domain simulation begins. However, if the actual first stage were instead set to stage 1, then the link would be disconnected in the static analysis as well because we would consider the static analysis to take place *before* stage 1 but *after* stage 0. For most standard models, this distinction is not particularly relevant; however, for [restart analyses](Restartanalyses.htm) this is important to maintain continuity and causality within a chain of models.

When using [regular waves](Environment,Dataforregularwaves.htm), it is usual to define the whole simulation as a single stage (subsequent to the build-up); results are then presented on a cycle-by-cycle basis. In [random waves](Environment,Dataforrandomwaves.htm) there is no meaningful wave cycle. By dividing the simulation time into stages you are free to collect results for specific time periods of interest.

Stages are also used to specify discrete events such as

* changes in [winch control mode](Winches,Control.htm#ByStage)
* vessel [prescribed motion](Vesseldata,Prescribedmotion.htm).
