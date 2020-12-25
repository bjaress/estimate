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

If you have Docker, it's the easiest way to run the program:

    docker run bjaress/estimate --help

If an updated version is released, you can bring it into your Docker
cache with:

    docker pull bjaress/estimate

If you don't have Docker, the easiest way is to [install Docker][] and then
follow the instructions above.

You can also install [The Haskell Tool Stack][] to build and run the
program from source.

[install Docker]: https://www.docker.com/get-started
[The Haskell Tool Stack]: https://docs.haskellstack.org/en/stable/README/#how-to-install


# Examples

Estimating 100 units of work based on six past time intervals:

    $ docker run bjaress/estimate 100 '[65, 30, 50, 70, 40, 90]'
    success: 97%
    target: 4


Setting a more ambitious intention:

    $ docker run bjaress/estimate --targetVelocity=50 100 '[65, 30, 50, 70, 40, 90]'
    success: 70%
    target: 3


Dropping that ambition and also realizing that only half your work can
go toward this project:

    $ docker run bjaress/estimate --focus='1%2' 100 '[65, 30, 50, 70, 40, 90]'
    success: 98%
    target: 5

(The default focus is already less than one.)
