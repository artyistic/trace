![final render, 800 width, 10000 spp, 40 bounces](finalScene.jpg "final render")


Yet another raytracer written in haskell

Hardware: ryzen 4700u, integrated graphics not used
Final render stats: check executionStats.txt

Final scene ran with:
<br>cabal build && cabal run tracerays -- 7 -w 800 -n 10000 -b 40 +RTS -s -N -RTS
<br>Explanation:
<br>7th scene (final scene from book)
<br>-w width 800 pixels, height is chosen according to 16:9 scale
<br>-n number of samples, 10000
<br>-b number of bounces before terminating, 40
<br>+RTS -s -N -RTS, this is for parallel execution, my hardware has 8 cores, -N uses all cores, -s is for runtime statistics, see executionStats.txt

I took some inspiration with the smoke effect and the idea of using a "deterministic random" in the smoke effect
from this project. It was either that or use monad transformers which is excruciating slow
<br>[_icrainbow's rtow github link_](https://gitlab.com/dpwiz/rtow/)

referenced
<br>[_Ray Tracing in One Weekend_](https://raytracing.github.io/books/RayTracingInOneWeekend.html)
<br>[_Ray Tracing: The Next Week_](https://raytracing.github.io/books/RayTracingTheNextWeek.html)


