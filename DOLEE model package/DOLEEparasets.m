(* ::Package:: *)

Fluc[]:=Module[{},
datafluc={};
mutfluc1={};
mutfluc2={};
Do[
(*Simulation starts*)
(*ClearAll[data,abun];
data={Join[{i0},x0]};
abun={iniratio};*)
totalbio=Total[{x1,x2,x3,x4}];
data0=Join[{i0},x0];
Aabun=0;
Cabun=0.5;
Dabun=0.5;
jud=0;
For[Cn=0,jud<1,Cn++;
(*Simulation in DOL cycle*)
(*data0=data[[-1]];*)
i=data0[[1]];
x1=data0[[2]];
x2=data0[[3]];
x3=data0[[4]];
x4=data0[[5]];

Do[
m3=m1;
m4=m2;
m1=If[e*x1*x2<=0,0,RandomVariate[PoissonDistribution[e*x1*x2*dt]]];
m2=If[e*x1*x2<=0,0,RandomVariate[PoissonDistribution[e*x1*x2*dt]]];
If[m3==0&&m1!=0, mutfluc1=Join[mutfluc1,{{k,2*Cn-1,j*dt}}]];
If[m4==0&&m2!=0, mutfluc2=Join[mutfluc2,{{k,2*Cn-1,j*dt}}]];
i=Chop@SetPrecision[Max[i+dt*(a1*(x1+x3+x4)-a2*i*(x2+x3+x4)),0],5];
x1=x1+dt*((muN1*x1*(1-(x1+x2+co*(x3+x4))/pN))-i*ki*x1)-m1;
x1=SetPrecision[Max[x1,0],pr];
x2=x2+dt*((a2*i*y*x2*(1-(x1+x2+co*(x3+x4))/pN))-i*ki*x2)-m2;
x2=SetPrecision[Max[x2,0],pr];
x3=x3+dt*(muN3*x3*(1-(x1+x2+co*(x3+x4))/pN)-i*ki*x3)+m1;
x3=SetPrecision[Max[x3,0],pr];
x4=x4+dt*(muN3*x4*(1-(x1+x2+co*(x3+x4))/pN)-i*ki*x4)+m2;
x4=SetPrecision[Max[x4,0],pr];
(*data=Join[data,{{i,x1,x2,x3,x4}}];
abun=Join[abun,{SetPrecision[{x1/totalbio,x2/totalbio,x3/totalbio,x4/totalbio},pr]}];*)
,{j,1,Tn/dt}];

(*Dillution*)
(*dataD=Join[{0},data[[-1,2;;5]]/di];
data=Join[data,{dataD}];*)

(*Simulation in competition cycle*)
(*data0=data[[-1]];*)
data0={0,x1,x2,x3,x4}/di;
i=data0[[1]];
x1=data0[[2]];
x2=data0[[3]];
x3=data0[[4]];
x4=data0[[5]];
Do[
m3=m1;
m4=m2;
m1=If[e*x1*x2<=0,0,RandomVariate[PoissonDistribution[e*x1*x2*dt]]];
m2=If[e*x1*x2<=0,0,RandomVariate[PoissonDistribution[e*x1*x2*dt]]];
If[m3==0&&m1!=0, mutfluc1=Join[mutfluc1,{{k,2*Cn,j*dt}}]];
If[m4==0&&m2!=0, mutfluc2=Join[mutfluc2,{{k,2*Cn,j*dt}}]];
x1=x1+dt*(muP1*x1*(1-(x1+x2+x3+x4)/pP))-m1;
x1=SetPrecision[Max[x1,0],pr];
x2=x2+dt*(muP2*x2*(1-(x1+x2+x3+x4)/pP))-m2;
x2=SetPrecision[Max[x2,0],pr];
x3=x3+dt*(muP3*x3*(1-(x1+x2+x3+x4)/pP))+m1;
x3=SetPrecision[Max[x3,0],pr];
x4=x4+dt*(muP3*x4*(1-(x1+x2+x3+x4)/pP))+m2;
x4=SetPrecision[Max[x4,0],pr];
(*data=Join[data,{{i,x1,x2,x3,x4}}];
abun=Join[abun,{SetPrecision[{x1/totalbio,x2/totalbio,x3/totalbio,x4/totalbio},pr]}];*)
,{j,1,Tp/dt}];
(*Dillution*)
(*dataD=Join[{0},data[[-1,2;;5]]/di];
data=Join[data,{dataD}];*)
data0={0,x1,x2,x3,x4}/di;

totalbio=Total[{x1,x2,x3,x4}];
Aabun=(x3+x4)/totalbio;
Cabun=x2/totalbio;
Dabun=x1/totalbio;
If[Aabun>stand2,jud=1,
If[Cabun<1-stand2&&Aabun==0,jud=2,
If[Dabun<1-stand2&&Aabun==0,jud=3,
If[Cn>=Cmax&&Aabun==0,jud=4,
If[Cn>=Cmax&&Aabun!=0&&Cabun<1-stand2,jud=5,
If[Cn>=Cmax&&Aabun!=0&&Dabun<1-stand2,jud=6,
If[Cn>=Cmax&&Aabun!=0,jud=7,
jud=0]]]]]]];
];
datafluc=Join[datafluc,{Join[SetPrecision[{x1/totalbio,x2/totalbio,x3/totalbio,x4/totalbio},pr],{2*Cn},{jud}]}];
,{k,1,Replicates}];
(*plot=ListLinePlot[Transpose@abun,PlotRange->Automatic,PlotStyle->Directive[ColorData[95],Thickness[0.01]],PlotLegends->{"Degrader","Consumer","Autonomy (degrader)","Autonomy (consumer)"},Frame->True,Axes->False,FrameTicks->{{Automatic,None},{Automatic,None}},AspectRatio->0.65,FrameStyle->Directive[Thickness[0.005],27],LabelStyle->Directive[Black,20],ImageSize->600];
Print[datafluc];*)
];


DOL[]:=Module[{},
datadol={};
mutdol1={};
mutdol2={};
Do[
(*Simulation starts*)
(*ClearAll[data,abun];
data={Join[{i0},x0]};
abun={iniratio};*)
data0=Join[{i0},x0];
totalbio=Total[{x1,x2,x3,x4}];
Aabun=0;
Cabun=0.5;
Dabun=0.5;
jud=0;
For[Cn=0,jud<1,Cn++;
(*Simulation in DOL cycle*)
(*data0=data[[-1]];*)
i=data0[[1]];
x1=data0[[2]];
x2=data0[[3]];
x3=data0[[4]];
x4=data0[[5]];
Do[
m3=m1;
m4=m2;
m1=If[e*x1*x2<=0,0,RandomVariate[PoissonDistribution[e*x1*x2*dt]]];
m2=If[e*x1*x2<=0,0,RandomVariate[PoissonDistribution[e*x1*x2*dt]]];
If[m3==0&&m1!=0, mutdol1=Join[mutdol1,{{k,Cn,j*dt}}]];
If[m4==0&&m2!=0, mutdol2=Join[mutdol2,{{k,Cn,j*dt}}]];
i=Chop@Max[i+dt*(a1*(x1+x3+x4)-a2*i*(x2+x3+x4)),0];
x1=x1+dt*((muN1*x1*(1-(x1+x2+co*(x3+x4))/pN))-i*ki*x1)-m1;
x1=SetPrecision[Max[x1,0],pr];
x2=x2+dt*((a2*i*y*x2*(1-(x1+x2+co*(x3+x4))/pN))-i*ki*x2)-m2;
x2=SetPrecision[Max[x2,0],pr];
x3=x3+dt*(muN3*x3*(1-(x1+x2+co*(x3+x4))/pN)-i*ki*x3)+m1;
x3=SetPrecision[Max[x3,0],pr];
x4=x4+dt*(muN3*x4*(1-(x1+x2+co*(x3+x4))/pN)-i*ki*x4)+m2;
x4=SetPrecision[Max[x4,0],pr];
totalbio=Total[{x1,x2,x3,x4}];
(*data=Join[data,{{i,x1,x2,x3,x4}}];
abun=Join[abun,{SetPrecision[{x1/totalbio,x2/totalbio,x3/totalbio,x4/totalbio},pr]}];*)
,{j,1,Tn/dt}];

(*Dillution*)
(*dataD=Join[{0},data[[-1,2;;5]]/di];
data=Join[data,{dataD}];*)
data0={0,x1,x2,x3,x4}/di;

Aabun=(x3+x4)/totalbio;
Cabun=x2/totalbio;
Dabun=x1/totalbio;
If[Aabun>stand2,jud=1,
If[Cabun<1-stand2&&Aabun==0,jud=2,
If[Dabun<1-stand2&&Aabun==0,jud=3,
If[Cn>=2*Cmax&&Aabun==0,jud=4,
If[Cn>=2*Cmax&&Aabun!=0,jud=5,
jud=0]]]]];
];
datadol=Join[datadol,{Join[SetPrecision[{x1/totalbio,x2/totalbio,x3/totalbio,x4/totalbio},pr],{Cn},{jud}]}];
,{k,1,Replicates}];
(*plot1=ListLinePlot[Transpose@abun,PlotRange->Automatic,PlotStyle->Directive[ColorData[95],Thickness[0.01]],PlotLegends->{"Degrader","Consumer","Autonomy (degrader)","Autonomy (consumer)"},Frame->True,Axes->False,FrameTicks->{{Automatic,None},{Automatic,None}},AspectRatio->0.65,FrameStyle->Directive[Thickness[0.005],27],LabelStyle->Directive[Black,20],ImageSize->600];
biomass=Transpose@Take[data,All,{2,5}];
plot2=ListLinePlot[biomass,PlotRange->All,PlotStyle->Directive[ColorData[95],Thickness[0.01]],PlotLegends->{"Degrader","Consumer","Autonomy (degrader)","Autonomy (consumer)"},Frame->True,Axes->False,FrameTicks->{{Automatic,None},{Automatic,None}},AspectRatio->0.65,FrameStyle->Directive[Thickness[0.005],27],LabelStyle->Directive[Black,20],ImageSize->600];
Print[datadol];*)
];


MainLoop[]:=Module[{},
t0=AbsoluteTime[];
Paraset={};
outfluc={};
outdol={};
Mutfluc1={};
Mutfluc2={};
Mutdol1={};
Mutdol2={};
PARA[];
Do[
muP2=muP2list[[s]];
DURATION[];
Paraset=Join[Paraset,{Join[para,{Tn,Tp}]}];
Fluc[];
outfluc=Join[outfluc,{Join[{muP2},datafluc]}];
Mutfluc1=Join[Mutfluc1,{Join[{muP2},mutfluc1]}];
Mutfluc2=Join[Mutfluc2,{Join[{muP2},mutfluc2]}];
DOL[];
outdol=Join[outdol,{Join[{muP2},datadol]}];
Mutdol1=Join[Mutdol1,{Join[{muP2},mutdol1]}];
Mutdol2=Join[Mutdol2,{Join[{muP2},mutdol2]}];
If[Divisible[s,record]==True,
Export[FileNameJoin[{NotebookDirectory[],"data","Parasets.csv"}],Paraset];
Export[FileNameJoin[{NotebookDirectory[],"data","outcomefluc.csv"}],outfluc];
Export[FileNameJoin[{NotebookDirectory[],"data","Mutfluc1.csv"}],Mutfluc1];
Export[FileNameJoin[{NotebookDirectory[],"data","Mutfluc2.csv"}],Mutfluc2];
Export[FileNameJoin[{NotebookDirectory[],"data","outcomedol.csv"}],outdol];
Export[FileNameJoin[{NotebookDirectory[],"data","Mutdol1.csv"}],Mutdol1];
Export[FileNameJoin[{NotebookDirectory[],"data","Mutdol2.csv"}],Mutdol2];
If[Divisible[s,tc]==True,
Print["Simulation No."<>ToString@s<>" finished; Total time used: "<>ToString[AbsoluteTime[]-t0]<>" s"];
];
];
,{s,1,Length@muP2list}];
];
