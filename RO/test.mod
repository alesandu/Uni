var x1 >= 0;
var x2 >= 0;
var x3 >= 0;

maximize z: 2*x1 + x2 + x3;

s.t. M1: x1 + 3*x2 + x3 <= 6;
s.t. M2: 2*x1 - x2 + x3 <= 5;
s.t. M3: x1 + x2 +4*x3 <= 10;
