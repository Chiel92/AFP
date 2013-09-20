First set of assignments
========================

_Chiel ten Brinke 3677133_
_Mattias Beimers 3672565_

The code for assignment __7.1__ is included in `A7_1.hs`.
The function which generates all smooth permutations using a tree is called `smoothPerms'`.

Exercise __8.1__ also goes with some code, included in `A8_1.hs`.
The QuickCheck property is called `AllSmoothPerms`, as required.
The benchmarking is done twice on a fairly random array with a different distance.
The function that has been used for this is also contained in `A8_1.hs`, and is called `test`.
The results are below:

Benchmarking results for smoothperms
------------------------------------
```
benchmarking smoothPerms/1
mean: 23.20979 us, lb 23.07006 us, ub 23.41027 us, ci 0.950
std dev: 844.1786 ns, lb 636.5858 ns, ub 1.135237 us, ci 0.950
found 7 outliers among 100 samples (7.0%)
  2 (2.0%) high mild
  5 (5.0%) high severe
variance introduced by outliers: 32.635%
variance is moderately inflated by outliers

benchmarking smoothPerms/2
mean: 23.06453 us, lb 22.96225 us, ub 23.28419 us, ci 0.950
std dev: 734.4305 ns, lb 425.9676 ns, ub 1.455542 us, ci 0.950
found 3 outliers among 100 samples (3.0%)
  2 (2.0%) high mild
  1 (1.0%) high severe
variance introduced by outliers: 26.792%
variance is moderately inflated by outliers
```

Benchmarking results for smoothperms'
-------------------------------------
```
benchmarking smoothPerms/1
mean: 34.58379 us, lb 33.06098 us, ub 40.79017 us, ci 0.950
std dev: 13.75148 us, lb 2.557850 us, ub 32.16632 us, ci 0.950
found 13 outliers among 100 samples (13.0%)
  3 (3.0%) high mild
  10 (10.0%) high severe
variance introduced by outliers: 98.903%
variance is severely inflated by outliers

benchmarking smoothPerms/2
mean: 32.41907 us, lb 31.99825 us, ub 33.52664 us, ci 0.950
std dev: 3.224501 us, lb 1.012719 us, ub 6.372595 us, ci 0.950
found 5 outliers among 100 samples (5.0%)
  3 (3.0%) high mild
  2 (2.0%) high severe
variance introduced by outliers: 78.989%
variance is severely inflated by outliers
```

As one can see from the mean duration, the function smoothPerms' is faster, which is to be expected.
When more larger testcases are provided, the difference in performance will probably grow bigger.

For analyzing the space consumption, two heap profiles graphs have been included named `profiling_smoothPerms.ps` and `profiling_smoothPerms'.ps` which profile the functions smoothPerms and smoothPerms' respectively.
The profiling script is _not_ included in `A8_1`, but put into a seperate file called `profile.hs`.
As one can see from the graphs, smoothPerms does not have a big heap graphs, while smoothPerms' does.
This is explained fairly intuitively by noticing that the first function can just generate each permutation and discard or print it immediately, while the latter function has to maintain a big tree containing many of the permutations at the same time.

The code for Exercise __2.5__ is contained in `A2_5.hs`.
The first definition for count is likewise called `count` and the second definition ik called `count'`.
The provided tests in the assignment can be executed by calling `test` and `test'` respectively.

The solution to Exercise __9.1__ is contained in `A9_1.txt`.
