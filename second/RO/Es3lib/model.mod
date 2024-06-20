set VAR;
set VIN;

var x {VAR} binary; 

param c {VAR}; 

param a {VIN,VAR};
param b {VIN};

maximize z: sum{j in VAR}c[j]*x[j];

s.t. M{i in VIN}: sum{j in VAR}a[i,j]*x[j]<=b[i];
