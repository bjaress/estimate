Scrum-style project estimation

This command-line application accepts as input:

*   the total amount of work done in past time intervals (for example,
    story points per sprint)
*   the amount of work left in a project
*   optionally, an intended rate of work (velocity)

and produces as output:

*   the number of time intervals until completion at the intended rate
    (if given) or at a reasonable rate inferred from past intervals
*   the percentage chance of actually achieving that, based on past
    intervals


# How The Program Computes The Output And Why It's Useful

The percentage is based on simple simulation: randomly picking past
intervals until they add up to project completion.  After simulating the
project many times, the program calculates the percentage of times the
target duration was met.  This is based on ideas from [Evidence-Based
Scheduling][], but instead of reporting a distribution it reports the
success rate for a specific target.

[Evidence-Based Scheduling]: https://en.wikipedia.org/wiki/Evidence-based_Scheduling

Everything else, such as picking a work rate if none is provided or
converting a work rate into a number of time intervals until completion,
is based on rule-of-thumb assumptions that can be overridden with
optional input.  For example, most agile tracking systems track all work
done across all projects, so when a past interval is randomly selected,
only a fraction of the work it represents is credited to the project.
The fraction defaults to a rule of thumb for "the big project we're
focused on" but can be overridden.

This approach provides the sort of evidence-based second opinion that
most occasional estimators are looking for.  The inputs and outputs are
tailored for someone who might be asked early on what a reasonable
timeline is, but who's more likely to be asked later on, "Can you meet
the deadline?" and "What if you worked faster?"


# Installation

If you have Docker and can run amd64 images, that's the easiest way to
run the program:

    $ docker run bjaress/estimate --help

If an updated version is released, you can bring it into your Docker
cache with:

    $ docker pull bjaress/estimate


If you can't do that (or would rather not) you can install [The Haskell
Tool Stack][], then build and install the program from source:

    $ stack install
    $ estimate --help

[install Docker]: https://www.docker.com/get-started
[The Haskell Tool Stack]: https://docs.haskellstack.org/en/stable/README/#how-to-install


# Examples

## Estimating a project with 100 units of work based on a record of six past time intervals

    $ docker run bjaress/estimate 100 '[65, 30, 50, 70, 40, 90]'
    success: 97%
    target: 4

Meaning that you should target four sprints for the whole project to
have a 97% chance of success.


## Setting a more ambitious intention

    $ docker run bjaress/estimate --target-velocity=50 100 '[65, 30, 50, 70, 40, 90]'
    success: 70%
    target: 3

Meaning that if you do fifty points per sprint, you will finish the
project in three sprints, but there's a 70% chance of achieving that.
Both `--target-velocity` and the list of six past sprints refer to all
the work you accomplish, but the program assumes by default that only
most of that work will be on the project being estimated.


## Keeping the ambitious intention to work fast and vowing to spend literally all of your work time on the project

    $ docker run bjaress/estimate --target-velocity=50 --focus='1%1' 100 '[65, 30, 50, 70, 40, 90]'
    success: 72%
    target: 2

Meaning that if you work on nothing else and do fifty points per sprint,
you will finish the one hundred points in two sprints, but your past
history suggests only a 72% chance of achieving that.

The fraction library used by the program separates the numerator and
denominator with a percent sign, so `--focus=1%1` is how you specify the
assumption that all your work will go to the project being estimated.


## Dropping that ambition and also realizing that only half your work can go toward this project

    $ docker run bjaress/estimate --focus='1%2' 100 '[65, 30, 50, 70, 40, 90]'
    success: 98%
    target: 5

Meaning that if you spend half your effort working on the project, you
have a 98% chance of being done if five sprints.


## Achieving higher velocity before attempting another project

    $ docker run bjaress/estimate 100 '[70, 60, 50, 100, 88, 120]'
    success: 99%
    target: 3

Meaning that with a history of higher past velocities, you can target
finishing 100 points in three sprints, with a 99% chance of success.
