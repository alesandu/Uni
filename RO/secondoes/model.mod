set VAR; #insieme variabili
set VIN; #insime vincoli

var x {VAR}>=0; # x1,x2,x3 ... var per ogni var
# anche per sl e sp bisogna aggiungere le due var distinte
param c {VAR}; # 4,2,-3 ... c per ogni var

param a {VIN,VAR}; #variabili colonne, righe vincoli
param b {VIN};

maximize z: sum{j in VAR}c[j]*x[j];

s.t. M{i in VIN}: sum{j in VAR}a[i,j]*x[j]<=b[i];
# se volessi avere >= creo un altro set per i VIN positivi
# la stessa cosa vale per le variabili se ho variabili >= 0 e >= 0 o var libere, 
# devo creare 3 insiemi diversi
#
# in caso volessi avere le varibili di slack o surplus devo mettere come Vincoli non più VIN
# bensi VINSL (slack) VINSP (surplus) VIN(vincoli non trasformati), avendo quindi anche 3 s.t.
