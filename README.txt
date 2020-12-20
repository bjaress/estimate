Scrum-style project estimation

Usage: estimate DEFINED_WORK PAST_VELOCITIES [--targetVelocity INTEGER] 
                [--focus PERCENT_SEPARATED_FRACTION] [--undefinedWork INTEGER] 
                [--simulationTrials INTEGER] [--simulationSeed INTEGER] 
                [-v|--verbose]
  Estimate how many time intervals are needed for an amount of work. This
  program assumes that time intervals are shared with other projects. Because of
  that, velocity inputs are adjusted by a focus amount.
  
  For example, suppose you always do 100 units of work in every time interval,
  and you plan to spend half the work in each time interval on a project.
  Estimate that project with a velocity of 100 and a focus of one half. If the
  answer is 3, it's an estimate the project will last for three time intervals.

Available options:
  DEFINED_WORK             Amount of well-defined work as an integer.
  PAST_VELOCITIES          Examples of past velocities as a bracketed,
                           comma-separated list of integers. For example, "[30,
                           50, 10]" is an input of three past velocities. Each
                           number represents the total work done in a time
                           interval across all projects.
  --targetVelocity INTEGER Typical or target amount of work done per time
                           interval across all projects. By default, inferred
                           from past velocities using a rule of thumb.
  --focus PERCENT_SEPARATED_FRACTION
                           Fraction of velocity (and past velocities) to apply
                           to the specific work being estimated. Default is
                           based on a rule of thumb for teams "dedicated to the
                           top priority." (default: 2 % 3)
  --undefinedWork INTEGER  Amount of work that is expected but not fully
                           defined. For example, your process might include a
                           final best-practices review that seems to request a
                           consistent amount of additional work, no matter what
                           practices were followed.
  --simulationTrials INTEGER
                           Number of times to simulate completing the project
  --simulationSeed INTEGER Seed for random number generator used in simulations
  -v,--verbose             Repeat input and intermediate values in output
  -h,--help                Show this help text

https://github.com/bjaress/estimate
