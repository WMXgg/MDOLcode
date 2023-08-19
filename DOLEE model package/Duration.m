(* ::Package:: *)

(*Determine the duration*)
DURATION:=Module[{},
(*Duration in DOL cycle*)
data={Join[{i0},{Min[x0[[1]],pN/(2*di)],Min[x0[[2]],pN/(2*di)],0,0}]};
data0=data[[-1]];
i=data0[[1]];
x1=data0[[2]];
x2=data0[[3]];
totalbio=Total[{x1,x2}];
For[Tn=0,totalbio<=stand1*pN,Tn++;
i=Chop@SetPrecision[Max[i+dt*(a1*x1-a2*i*x2),0],5];
x1=x1+dt*((muN1*x1*(1-(x1+x2)/pN))-i*ki*x1);
x1=SetPrecision[Max[x1,0],pr];
x2=x2+dt*((a2*i*y*x2*(1-(x1+x2)/pN))-i*ki*x2);
x2=SetPrecision[Max[x2,0],pr];
totalbio=Total[{x1,x2}];
If[Tn*dt>Tmax,Break[]];
];
Tn=Round[Tx*Tn*dt,dt];

(*Duration in Competition cycle*)
data={Join[{i0},{Min[x0[[1]],pP/(2*di)],Min[x0[[2]],pP/(2*di)],0,0}]};
data0=data[[-1]];
i=data0[[1]];
x1=data0[[2]];
x2=data0[[3]];
totalbio=Total[{x1,x2}];
For[Tp=0,totalbio<=stand1*pP,Tp++;
x1=x1+dt*(muP1*x1*(1-(x1+x2)/pP));
x1=SetPrecision[x1,pr];
x2=x2+dt*(muP2*x2*(1-(x1+x2)/pP));
x2=SetPrecision[x2,pr];
totalbio=Total[{x1,x2}];
If[Tp*dt>Tmax,Break[]];
];
Tp=Round[Tx*Tp*dt,dt];
]


COND1[]:=Module[{},
test1=If[Tn>Tmax||Tp>Tmax,False,True];
];


COND2[]:=Module[{},
pro1=e*0.25*pN*pN;
pro2=e*0.25*pP*pP;
test2=If[pro1<exp||pro2<exp,False,True];
];
