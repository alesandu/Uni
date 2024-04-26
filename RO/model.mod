var x1 >= 0;
var x2 >= 0;
var x3 >= 0;

minimize z: 4*x1 + 2*x2 - 3*x3;

s.t. M1: 2*x1 + 3*x2 + x3 <= 6;
s.t. M2: -4*x1 + 3*x2 - x3 <= 12;
