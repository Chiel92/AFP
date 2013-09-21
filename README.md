AFP
===

This is an awesome tutorial: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html#monads


How to create a cabal library
-----------------------------
Create a file _package_.cabal and include something like this:

```
Name:               BeimersBrinke
Version:            1.0
Cabal-Version:      >= 1.2
Author:             Chiel ten Brinke and Mattias Beimers
License:            GPL
License-file:       LICENSE
Category:           Educational Assignment
Description:        This is the first set of assignments
Build-Type:         Simple
data-files:         README.md,
                    profiling_smoothPerms.ps,
                    profiling_smoothPerms'.ps

Library
  Build-Depends:    base,
                    criterion,
                    QuickCheck
  Exposed-modules:  A2_5,
                    A7_1,
                    A8_1,
                    Profiling
```

Then run `cabal sdist` to produce a compressed package that can be easily distributed.
