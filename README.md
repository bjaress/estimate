Scrum-style project estimation

Estimate how many time intervals are needed for an amount of work. The
estimate is expressed as a target number of time intervals and a success rate.
It's based on the amount of work to do and examples of how much work was done
in past time intervals.

For planning purposes, you can also input the amount of work you intend to do
per time interval and the fraction of that work you intend to spend on the
project being estimated.

Whether you believe the estimates is up to you.


# Installation

If you have Docker and can run amd64 images, it's the easiest way to run
the program:

    $ docker run bjaress/estimate --help

If an updated version is released, you can bring it into your Docker
cache with:

    $ docker pull bjaress/estimate


If you can't do that (or would rather not) you can install [The Haskell
Tool Stack][], then build and install the program from source:

    $ stack install

[install Docker]: https://www.docker.com/get-started
[The Haskell Tool Stack]: https://docs.haskellstack.org/en/stable/README/#how-to-install


# Examples

## Estimating a project with 100 units of work based on a record of six past time
intervals

    $ docker run bjaress/estimate 100 '[65, 30, 50, 70, 40, 90]'
    success: 97%
    target: 4

Meaning that you should target four time periods for the whole project
to have a 97% chance of success.


## Setting a more ambitious intention

    $ docker run bjaress/estimate --target-velocity=50 100 '[65, 30, 50, 70, 40, 90]'
    success: 70%
    target: 3

Meaning that if you attempt to do fifty units of work per time interval,
you will finish the project in three time intervals (if successful) but
there's now only a 70% chance of success.  Both `--target-velocity` and
the list of six past intervals refer to all the work you accomplish, but
the program assumes by default that only most of that work will be on
the project being estimated.


## Keeping the ambitious intention to work fast and vowing to spend
literally all of your work time on the project

    $ docker run bjaress/estimate --target-velocity=50 --focus='1%1' 100 '[65, 30, 50, 70, 40, 90]'
    success: 72%
    target: 2

Meaning that if you work on nothing else and do fifty units of work per
time interval, you will finish the one hundred units in two time
intervals, but your past history suggests only a 72% chance of actually
achieving that.

The fraction library uses a percent sign to separate the numerator and
denominator, so `--focus=1%1` is how you specify the (possibly
unrealistic) assumption that all your work can go to the project being
estimated.


# Dropping that ambition and also realizing that only half your work can
go toward this project

    $ docker run bjaress/estimate --focus='1%2' 100 '[65, 30, 50, 70, 40, 90]'
    success: 98%
    target: 5

Meaning that if you spend half your time working on the project, you can
be done in five time intervals with a 98% chance of success.

The `--focus` argument is trusted as true, since the program has no
information on it, so use it wisely.  The success percentages are based
on comparing the historical record to the target.


# Explanation

The program uses ideas from [Evidence-Based Scheduling][], but instead
of reporting a distribution it uses a rule of thumb (or the
`--target-velocity` option) to pick a target project duration and give
the success rate for that target.

[Evidence-Based Scheduling]: https://en.wikipedia.org/wiki/Evidence-based_Scheduling
