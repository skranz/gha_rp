# repboxRun

Repbox package that contains functions to run all or specific analyses of a supplements.

## Stuff that does not work

Repbox will not be able to replicate all things even if data is not missing. But we aim to replicate a large percentage of reproduction packages / articles. Here are examples of constructions that do not work.

### A do file is run multiple times

aejmic_11_3_10: does call a particular do file multiple times instead defining a program that will then be called multiple times. This cannot be properly handled by repbox so far.

### Regressions use chained time series operators

aejmac_12_3_10: Has the regressions

eststo: reg s2.lCF EMILIA `controlli' l2.s2.lCF if OO==1 & INDEXLIQ==1, r

We cannot deal with the chained operator `l2.s2` in `l2.s2.lCF`.

### Regression using range time series prefixes

We do not yet deal with time series prefixes like
L(0/4).x1

### Regressions like reg view_npausezone 1.disab#0.vid_late 1.disab#1.vid_late 1.vid_late

The example is from aejapp_15_4_14 step 16:

```
. reg view_npausezone 1.disab#0.vid_late 1.disab#1.vid_late 1.vid_late

Linear regression                               Number of obs     =        946
                                                F(3, 942)         =       4.77
                                                Prob > F          =     0.0026
                                                R-squared         =     0.0139
                                                Root MSE          =     .45322

------------------------------------------------------------------------------
             |               Robust
view_npaus~e | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-------------+----------------------------------------------------------------
       disab#|
    vid_late |
        1 0  |   .0600738   .0457372     1.31   0.189    -.0296847    .1498323
        1 1  |   .0522676   .0369846     1.41   0.158     -.020314    .1248493
             |
    vid_late |
          1  |  -.0891529   .0381847    -2.33   0.020    -.1640898   -.0142159
             |
       _cons |   .2213115   .0301943     7.33   0.000     .1620556    .2805673
------------------------------------------------------------------------------
```

### Inline for loops

aejpol_7_2_11:

xi: for var Dgender no_tickets no_ticketorder avevalue_tickets value_tickets postcode_munich Dyear_firstentry: reg X i.match1_control if match_treat==1|match_treat==`i', robust;


The values `X` in the regression takes the values specified in the for statement before the :.
It is unlikely, that we will successfully be able to parse regression results from such constructions, since we cannot inject code in an inline for loop.


### We cannot manually download data

Some reproduction packages have not included all data but instructions in the README, how the data can be obtained. For repbox this is equivalent to missing data, while a human replicator might well be able to reproduce the study.

An example is `aejapp_1_1_5`. They write in the README:

> The primary dataset used for this analysis is the American National Election Studies Cumulative Data File freely available for download at 
> http://www.electionstudies.org/studypages/download/datacenter_all.htm.
