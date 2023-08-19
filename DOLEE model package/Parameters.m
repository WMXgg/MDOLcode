(* ::Package:: *)

PARA[]:=Module[{},
(*Deterministic parameters in DOL system*)
a1=0.001;
a2=0.05;
y=0.5;
muN1=0.005;
muN3=2*muN1; (*muN3=muN4*)
co=1/3;(*competitive edge of the autonomy*)
pN=1000;
ki=0.00001;
(*Deterministic Parameters in Competition*)
muP1=0.005;
muP2list=muP1*Table[10^i,{i,-0.3,0.3,0.025}];
muP3=muP1; (*muP3=muP4*)
pP=2000;
(*Random Parameters: HGT rate*)
e=0.000000000005;
(*Operating parameters*)
di=1000; (*Dilution factor*)
Tx=1;(*Round[10^RandomReal[{0,0.5}],0.1];*)(*duration/time to reach stationary phase*)
para={a1,a2,y,muN1,muN3,co,pN,ki,muP1,muP2,muP3,pP,e,di,Tx};
];


(*Initial condition*)
x0={1,1,0,0};
i0=0; 
iniratio={0.5,0.5,0,0};
m1=0;
m2=0;


(*Simulation set-up*)
dt=1;
Replicates=50;
Cmax=25;(*Longest simulation cycles*)
exp=0.000001;(*Lowest HGT rate*)
Tmax=20000;(*longest simulation period*)
stand1=0.99;(*Determine the duration*)
stand2=0.99;(*Determine the cycle number*)
record=1; (*The frequency of data recording*)
pr=8; (*Precision of the calculation*)
tc=1;(*Time track in tc Simulations*)
