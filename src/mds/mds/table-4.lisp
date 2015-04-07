Return-path: <asl@santafe.edu>
Envelope-to: dsmith@cs.unm.edu
Delivery-date: Wed, 11 Mar 1998 18:43:07 -0700
Date: Wed, 11 Mar 1998 18:38:55 -0700
From: asl@santafe.edu
To: dsmith@cs.unm.edu

Derek,
Follows are the COMPUTED COORDINATES for the six files you sent (the
brackets now hold the computed coordinates), with each set of 
COMPUTED COORDINATES followed by PREDICTIONS (the PREDICTION format is
"specified distance" followed by "computed distance"). PREDICTIONS therefore
holds the computed distance based on the COMPUTED COORDINATES.

Note that PREDICTIONS only computes distances between things  for which
a distance was specified in the first place, so antigen-antigen and sera-sera
distances need to be computed by you based on the given coordinates.
However, from the PREDICTIONS file you can easily do a scatter plot of 
"specified distance" versus "computed distance" to see how well the fit
is in the specified dimension (I suppose we should add a hook to the code
to compute all distances between all points at the end of the day). 

I see that in some files (e.g. FILE 2) you distinguish sera (sr) from
antigen (ag) and in some files (e.g. FILE1) you do not. I was busy today,
so I didn't check to see if what you were specifying makes sense, and I
haven't done more than glance at the results. If there was an oversight
it's easy to run again where all the files have the sera and the antigens 
as distinguished from each other.
Alan


FILE 1 COMPUTED COORDINATES
n_dim=3;
rank=0;
energy=0.339132;
C ENG72	{0.442097,0.408449,0.856343};
C HK68	{1.794104,2.963170,4.054592};
C PC73	{-1.754959,-0.973456,-0.583872};
C VIC75	{-1.463585,-2.728493,-3.020068};
FILE 1 PREDICTIONS
ENG72 PC73 3 2.96832
PC73 VIC75 3 3.01664
ENG72 HK68 4 4.31084
ENG72 VIC75 5 5.33841
HK68 PC73 7 7.04331
HK68 VIC75 10 9.64668

FILE 2 COMPUTED COORDINATES
n_dim=3;
rank=0;
energy=4.95667;
C ENG72_ag	{0.232209,-0.020878,-0.945220};
C ENG72_sr	{0.680538,-0.165976,-1.020810};
C HK68_ag	{1.664335,-0.381698,-1.587862};
C HK68_sr	{1.706647,-0.443935,-1.403929};
C PC73_ag	{-2.085334,0.556330,0.127897};
C PC73_sr	{-1.697643,0.590514,-0.568418};
C VIC75_ag	{-0.020505,-0.944617,3.069961};
C VIC75_sr	{-0.450469,-0.765302,2.982945};
FILE 2 PREDICTIONS
ENG72_ag ENG72_sr 0 0.477249
HK68_ag HK68_sr 0 0.198734
PC73_ag PC73_sr 0 0.797701
VIC75_ag VIC75_sr 0 0.473914
ENG72_sr HK68_ag 0.5 1.15583
ENG72_ag HK68_sr 1 1.60105
ENG72_ag PC73_sr 1 2.05915
ENG72_sr PC73_ag 3 3.0808
PC73_ag VIC75_sr 3 3.54553
ENG72_ag VIC75_sr 4 4.05595
ENG72_sr VIC75_ag 4 4.22281
HK68_ag PC73_sr 4 3.64518
HK68_sr VIC75_ag 4.5 4.82177
HK68_sr PC73_ag 5 4.21024
PC73_sr VIC75_ag 5 4.29036
HK68_ag VIC75_sr 5.5 5.05092

FILE 3 COMPUTED COORDINATES
n_dim=3;
rank=0;
energy=1.11275;
C ENG72	{-0.037809,0.773199,1.009388};
C HK68	{0.929938,1.495377,1.843951};
C PC73	{-2.185753,0.100657,-0.175032};
C VIC75	{0.311281,-2.699563,-1.371311};
FILE 3 PREDICTIONS
ENG72 HK68 0.8 1.46785
ENG72 PC73 2 2.54339
ENG72 VIC75 4 4.22489
PC73 VIC75 4 3.93796
HK68 PC73 4.5 3.96599
HK68 VIC75 5.5 5.32148

FILE 4 COMPUTED COORDINATES
n_dim=2;
rank=0;
energy=0.339132;
C HK68	{4.891449,0.827547};
C ENG72	{0.584185,0.656452};
C PC73	{-1.932584,-0.917192};
C VIC75	{-4.734824,0.198930};
FILE 4 PREDICTIONS
ENG72 PC73 3 2.96825
PC73 VIC75 3 3.01634
ENG72 HK68 4 4.31066
ENG72 VIC75 5 5.33865
HK68 PC73 7 7.04355
HK68 VIC75 10 9.64678

(g-plot '((4.891449 0.827547)
	(0.584185 0.656452)
        (-1.932584 -0.917192)
	  (-4.734824 0.198930))
	  :style 'scatter)

FILE 5 COMPUTED COORDINATES
n_dim=2;
rank=0;
energy=4.95667;
C ENG72_ag	{0.904462,-0.541242};
C ENG72_sr	{1.279078,-0.245407};
C HK68_ag	{2.381775,0.100341};
C HK68_sr	{2.283235,0.273013};
C PC73_ag	{-1.509288,-1.554879};
C PC73_sr	{-0.743202,-1.776386};
C VIC75_ag	{-2.094026,2.295981};
C VIC75_sr	{-2.340874,1.891565};
FILE 5 PREDICTIONS
ENG72_ag ENG72_sr 0 0.477342
HK68_ag HK68_sr 0 0.198811
PC73_ag PC73_sr 0 0.797467
VIC75_ag VIC75_sr 0 0.4738
ENG72_sr HK68_ag 0.5 1.15563
ENG72_ag HK68_sr 1 1.60126
ENG72_ag PC73_sr 1 2.05922
ENG72_sr PC73_ag 3 3.08054
PC73_ag VIC75_sr 3 3.54535
ENG72_ag VIC75_sr 4 4.05595
ENG72_sr VIC75_ag 4 4.22333
HK68_ag PC73_sr 4 3.64521
HK68_sr VIC75_ag 4.5 4.82212
HK68_sr PC73_ag 5 4.21004
PC73_sr VIC75_ag 5 4.29056
HK68_ag VIC75_sr 5.5 5.05093

FILE 6 COMPUTED COORDINATES
n_dim=2;
rank=0;
energy=1.11275;
C ENG72	{0.816258,0.177211};
C HK68	{2.072336,0.936695};
C PC73	{-0.850993,-1.743393};
C VIC75	{-3.229375,1.395223};
FILE 6 PREDICTIONS
ENG72 HK68 0.8 1.46784
ENG72 PC73 2 2.54331
ENG72 VIC75 4 4.22501
PC73 VIC75 4 3.93797
HK68 PC73 4.5 3.96595
HK68 VIC75 5.5 5.3215

