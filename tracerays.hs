{-# LINE 1 "tracerays.prof" #-}
	Fri Jul 11 19:19 2025 Time and Allocation Profiling Report  (Final)

	   tracerays +RTS -p -RTS

	total time  =       29.13 secs   (29132 ticks @ 1000 us, 1 processor)
	total alloc = 66,944,389,320 bytes  (excludes profiling overheads)

COST CENTRE           MODULE                          SRC                                             %time %alloc

rayColor              Camera                          src/Camera.hs:(162,1)-(176,72)                   37.3   36.2
evalPoint             Graphics.Point                  src/Graphics/Point.hs:23:1-35                    14.2   17.6
evalRandT             Control.Monad.Trans.Random.Lazy Control/Monad/Trans/Random/Lazy.hs:184:1-42      12.2   13.3
getRandomVec          Camera                          src/Camera.hs:196:1-104                           8.7    8.2
iterateUntilM         Control.Monad.Loops             src/Control/Monad/Loops.hs:(171,1)-(173,43)       7.0    2.7
getRandomOnHemisphere Camera                          src/Camera.hs:(209,1)-(211,56)                    4.8    4.3
averageColor          Graphics.Pixel                  src/Graphics/Pixel.hs:(37,1)-(38,64)              3.9    4.7
getSampleSquare       Camera                          src/Camera.hs:192:1-86                            3.1    3.2
getRandomUnitBallVec  Camera                          src/Camera.hs:(201,1)-(203,65)                    2.0    1.4
iterateUntil          Control.Monad.Loops             src/Control/Monad/Loops.hs:178:1-50               1.9    0.8
nextWord64            System.Random.SplitMix          src/System/Random/SplitMix.hs:(115,1)-(117,29)    1.9    5.8
generateHitRecord     Hittable                        src/Hittable.hs:(26,1)-(30,72)                    1.1    1.3


                                                                                                                                            individual      inherited
COST CENTRE                      MODULE                            SRC                                                   no.     entries  %time %alloc   %time %alloc

MAIN                             MAIN                              <built-in>                                            305           0    0.0    0.0   100.0  100.0
 CAF                             GHC.Conc.Signal                   <entire-module>                                       545           0    0.0    0.0     0.0    0.0
 CAF                             GHC.IO.Encoding                   <entire-module>                                       509           0    0.0    0.0     0.0    0.0
 CAF                             GHC.IO.Encoding.Iconv             <entire-module>                                       507           0    0.0    0.0     0.0    0.0
 CAF                             GHC.IO.FD                         <entire-module>                                       500           0    0.0    0.0     0.0    0.0
 CAF                             GHC.IO.Handle.FD                  <entire-module>                                       498           0    0.0    0.0     0.0    0.0
 CAF                             Main                              <entire-module>                                       361           0    0.0    0.0     0.0    0.0
  main                           Main                              app/Main.hs:(10,1)-(15,56)                            610           1    0.0    0.0     0.0    0.0
   camera                        Camera                            src/Camera.hs:(37,1)-(71,79)                          611           1    0.0    0.0     0.0    0.0
    evalPoint                    Graphics.Point                    src/Graphics/Point.hs:23:1-35                         649           2    0.0    0.0     0.0    0.0
     toV3                        Graphics.Point                    src/Graphics/Point.hs:16:1-18                         650           2    0.0    0.0     0.0    0.0
   makeHittableList              Hittables                         src/Hittables.hs:12:1-54                              644           1    0.0    0.0     0.0    0.0
   render                        Camera                            src/Camera.hs:(74,1)-(128,92)                         612           1    0.0    0.0     0.0    0.0
   mkStdGen                      System.Random.Internal            src/System/Random/Internal.hs:787:1-36                639           0    0.0    0.0     0.0    0.0
    mkStdGen64                   System.Random.Internal            src/System/Random/Internal.hs:797:1-32                641           0    0.0    0.0     0.0    0.0
     mkSMGen                     System.Random.SplitMix            src/System/Random/SplitMix.hs:371:1-61                642           1    0.0    0.0     0.0    0.0
 CAF                             Graphics.Point                    <entire-module>                                       360           0    0.0    0.0     0.0    0.0
  fromCoord                      Graphics.Point                    src/Graphics/Point.hs:12:1-30                         652           1    0.0    0.0     0.0    0.0
  fromV3                         Graphics.Point                    src/Graphics/Point.hs:19:1-14                         647           1    0.0    0.0     0.0    0.0
 CAF                             Camera                            <entire-module>                                       356           0    0.0    0.0     0.0    0.0
  getRandomUnitBallVec           Camera                            src/Camera.hs:(201,1)-(203,65)                        662           1    0.0    0.0     0.0    0.0
   getRandomVec                  Camera                            src/Camera.hs:196:1-104                               667           1    0.0    0.0     0.0    0.0
   iterateUntil                  Control.Monad.Loops               src/Control/Monad/Loops.hs:178:1-50                   664           1    0.0    0.0     0.0    0.0
  getSampleSquare                Camera                            src/Camera.hs:192:1-86                                635           1    0.0    0.0     0.0    0.0
  lightBlue                      Camera                            src/Camera.hs:184:1-29                                657           1    0.0    0.0     0.0    0.0
   color                         Graphics.Pixel                    src/Graphics/Pixel.hs:(17,1)-(18,35)                  658           1    0.0    0.0     0.0    0.0
  white                          Camera                            src/Camera.hs:181:1-19                                655           1    0.0    0.0     0.0    0.0
   color                         Graphics.Pixel                    src/Graphics/Pixel.hs:(17,1)-(18,35)                  656           1    0.0    0.0     0.0    0.0
  camera                         Camera                            src/Camera.hs:(37,1)-(71,79)                          651           0    0.0    0.0     0.0    0.0
  rayColor                       Camera                            src/Camera.hs:(162,1)-(176,72)                        660           0    0.0    0.0     0.0    0.0
   color                         Graphics.Pixel                    src/Graphics/Pixel.hs:(17,1)-(18,35)                  678           1    0.0    0.0     0.0    0.0
 CAF                             Graphics.Pixel                    <entire-module>                                       354           0    0.0    0.0     0.0    0.0
  gammaCorrected                 Graphics.Pixel                    src/Graphics/Pixel.hs:48:1-62                         630           1    0.0    0.0     0.0    0.0
  averageColor                   Graphics.Pixel                    src/Graphics/Pixel.hs:(37,1)-(38,64)                  633           0    0.0    0.0     0.0    0.0
   color                         Graphics.Pixel                    src/Graphics/Pixel.hs:(17,1)-(18,35)                  634           1    0.0    0.0     0.0    0.0
 CAF                             Control.Concurrent.Async.Internal <entire-module>                                       350           0    0.0    0.0     0.0    0.0
  async                          Control.Concurrent.Async.Internal Control/Concurrent/Async/Internal.hs:99:1-35          619           1    0.0    0.0     0.0    0.0
   asyncUsing                    Control.Concurrent.Async.Internal Control/Concurrent/Async/Internal.hs:(124,1)-(130,35) 620           1    0.0    0.0     0.0    0.0
 CAF                             System.Random.Internal            <entire-module>                                       335           0    0.0    0.0     0.0    0.0
  mkStdGen                       System.Random.Internal            src/System/Random/Internal.hs:787:1-36                638           1    0.0    0.0     0.0    0.0
  mkStdGen64                     System.Random.Internal            src/System/Random/Internal.hs:797:1-32                640           1    0.0    0.0     0.0    0.0
 main                            Main                              app/Main.hs:(10,1)-(15,56)                            613           0    0.0    0.0   100.0  100.0
  render                         Camera                            src/Camera.hs:(74,1)-(128,92)                         614           0    0.3    0.1   100.0  100.0
   colorToRGBString              Graphics.Pixel                    src/Graphics/Pixel.hs:(41,1)-(45,49)                  659       90000    0.1    0.1     0.1    0.1
   evalImageConcurrently         Camera                            src/Camera.hs:(140,1)-(145,44)                        615           1    0.0    0.0    99.6   99.7
    chunkList                    Camera                            src/Camera.hs:(158,1)-(159,77)                        616       22501    0.0    0.0     0.0    0.0
    evaluateChunk                Camera                            src/Camera.hs:(149,1)-(154,19)                        618       22500    0.1    0.0    99.6   99.7
     runRandColor                Camera                            src/Camera.hs:136:1-44                                623       90000    0.0    0.0     0.0    0.0
     waitSTM                     Control.Concurrent.Async.Internal Control/Concurrent/Async/Internal.hs:(229,1)-(231,27) 624       90000    0.0    0.0     0.2    0.0
      async                      Control.Concurrent.Async.Internal Control/Concurrent/Async/Internal.hs:99:1-35          625           0    0.0    0.0     0.2    0.0
       asyncUsing                Control.Concurrent.Async.Internal Control/Concurrent/Async/Internal.hs:(124,1)-(130,35) 626           0    0.2    0.0     0.2    0.0
     async                       Control.Concurrent.Async.Internal Control/Concurrent/Async/Internal.hs:99:1-35          621           0    0.0    0.0    99.3   99.7
      asyncUsing                 Control.Concurrent.Async.Internal Control/Concurrent/Async/Internal.hs:(124,1)-(130,35) 622           0    0.1    0.2    99.3   99.7
       runRandColor              Camera                            src/Camera.hs:136:1-44                                627           0    0.0    0.0    99.2   99.5
        evalRand                 Control.Monad.Trans.Random.Lazy   Control/Monad/Trans/Random/Lazy.hs:119:1-38           628       90000    0.0    0.0    99.2   99.5
         evalRandT               Control.Monad.Trans.Random.Lazy   Control/Monad/Trans/Random/Lazy.hs:184:1-42           629       90000   12.2   13.3    99.2   99.5
          rayColor               Camera                            src/Camera.hs:(162,1)-(176,72)                        643    16165035   37.3   36.2    70.0   64.1
           toV3                  Graphics.Point                    src/Graphics/Point.hs:16:1-18                         653    39494934    0.0    0.0     0.0    0.0
           surrounds             Interval                          src/Interval.hs:29:1-47                               654    31550289    0.0    0.0     0.0    0.0
           getRandomOnHemisphere Camera                            src/Camera.hs:(209,1)-(211,56)                        661     7165034    4.8    4.3    26.1   21.5
            getRandomUnitBallVec Camera                            src/Camera.hs:(201,1)-(203,65)                        663           0    2.0    1.4    21.3   17.1
             getRandomVec        Camera                            src/Camera.hs:196:1-104                               668           0    8.7    8.2    10.0   12.3
              nextWord64         System.Random.SplitMix            src/System/Random/SplitMix.hs:(115,1)-(117,29)        669    42726345    1.3    4.1     1.3    4.1
             iterateUntil        Control.Monad.Loops               src/Control/Monad/Loops.hs:178:1-50                   665           0    1.9    0.8     9.4    3.5
              iterateUntilM      Control.Monad.Loops               src/Control/Monad/Loops.hs:(171,1)-(173,43)           666    14242115    7.0    2.7     7.5    2.7
               contains          Interval                          src/Interval.hs:25:1-48                               670    14242115    0.4    0.0     0.4    0.0
           at                    Graphics.Ray                      src/Graphics/Ray.hs:11:1-41                           673     7164978    0.1    0.0     4.9    5.1
            evalPoint            Graphics.Point                    src/Graphics/Point.hs:23:1-35                         674     7164978    4.8    5.1     4.8    5.1
             toV3                Graphics.Point                    src/Graphics/Point.hs:16:1-18                         675     7164978    0.0    0.0     0.0    0.0
           generateHitRecord     Hittable                          src/Hittable.hs:(26,1)-(30,72)                        672     7164978    1.1    1.3     1.1    1.3
           hitNormal             Hittable                          src/Hittable.hs:20:5-13                               671     7164978    0.2    0.0     0.2    0.0
           hitP                  Hittable                          src/Hittable.hs:19:5-8                                676     7164978    0.3    0.0     0.3    0.0
           hitT                  Hittable                          src/Hittable.hs:21:5-8                                677      523267    0.0    0.0     0.0    0.0
          evalPoint              Graphics.Point                    src/Graphics/Point.hs:23:1-35                         646     9000000    9.5   12.5     9.5   12.5
           toV3                  Graphics.Point                    src/Graphics/Point.hs:16:1-18                         648     9000000    0.0    0.0     0.0    0.0
          toV3                   Graphics.Point                    src/Graphics/Point.hs:16:1-18                         645     9000000    0.0    0.0     0.0    0.0
          averageColor           Graphics.Pixel                    src/Graphics/Pixel.hs:(37,1)-(38,64)                  632       90000    3.9    4.7     3.9    4.7
          gammaCorrected         Graphics.Pixel                    src/Graphics/Pixel.hs:48:1-62                         631           0    0.0    0.0     0.0    0.0
          getSampleSquare        Camera                            src/Camera.hs:192:1-86                                636           0    3.1    3.2     3.6    4.9
           nextWord64            System.Random.SplitMix            src/System/Random/SplitMix.hs:(115,1)-(117,29)        637    18000000    0.5    1.7     0.5    1.7
   evalPoint                     Graphics.Point                    src/Graphics/Point.hs:23:1-35                         617           0    0.0    0.0     0.0    0.0
