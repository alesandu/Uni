%% Problema 2.4 - Metodo di Jacobi per risolvere Ax = b

clear; clc;

% Dati del sistema
A = [5, 1, 2; 
    -1, 7, 1;
     0, 1, -3];

b = [13; 16; -7];

%% (a) Soluzione esatta del sistema con operatore \
x_exact = A \ b;
fprintf('(a) Soluzione esatta:\n');
disp(x_exact);

%% (b) Metodo di Jacobi - prime 10 iterazioni
x0 = [0; 0; 0];  % Vettore di partenza
n = length(b);
S = zeros(n, 12);  % 10 iterazioni + x(0) + soluzione esatta

S(:,1) = x0;
x_k = x0;

function [x, K] = jacobi(A, b, eps, x0, Nmax)
    n = length(b);
    x_old = x0;
    D = diag(diag(A));
    P = inv(D)*(D - A);
    q = inv(D)*b;

    for k = 1:Nmax
        x = P * x_old + q;
        r = norm(x - x_old, inf);  % errore relativo tra iterazioni
        if r < eps
            K = k;
            return;
        end
        x_old = x;
    end
    K = Nmax;
end

for k = 1:10
     [vettor, del1] = jacobi(A,b,0,x0,k);
     S(:,k+1) = vettor;
end
S(:,12) = x_exact;

fprintf('(b) Matrice S contenente x^(0), ..., x^(10), x esatta:\n');
disp(S);

%% (c) Iterazioni per vari epsilon
for i = 1:10
    epsilons(i) = 10^(-i);
end
results = zeros(length(epsilons), 4);  % [K, errore, , soluzione approssimata]

for i = 1:length(epsilons)
    eps = epsilons(i);
    Nmax = 1000;  % un limite massimo per sicurezza
    [x_eps, K] = jacobi(A, b, eps, x0, Nmax);

    err = norm(x_exact - x_eps, inf);
    results(i,:) = [K, eps, err, NaN];

    fprintf('\n epsilon = %.1e, K = %d, soluzione approssimata:\n', eps, K);
    disp(x_eps);
    fprintf('errore norma infinito = %.2e\n', err);
end