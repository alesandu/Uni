var x1 >= 0;
var x2 >= 0;

maximize z: 7*x1 + 10*x2;

s.t. M1: x1 + x2 <= 750;
s.t. M2: x1 + 2*x2 <= 1000;
s.t. M3: x2 <= 400;
