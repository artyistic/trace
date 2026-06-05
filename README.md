![final render, 800 width, 5000 spp, 40 bounces](finalScene_800w_5000spp_40.jpg "final render")


Yet another raytracer written in haskell

Hardware: ryzen 4700u, gpu not used
Final render stats: check executionStats.txt

To run:
cabal build && cabal run tracerays -- 7 800 5000 40 +RTS -s -N8 -RTS

I took some inspiration with the smoke effect and the idea of using a "deterministic random" in the smoke effect

by this project. It was either that or use monad transformers which is excruciating slow
[_icrainbow's rtow_](https://gitlab.com/dpwiz/rtow/)

referenced
 
[_Ray Tracing: The Next Week_](https://raytracing.github.io/books/RayTracingTheNextWeek.html)

[_Ray Tracing: The Next Week_](https://raytracing.github.io/books/RayTracingTheNextWeek.html)


