% Funzione da integrare
f = @(x) x .* exp(x);

% Estremi dell'intervallo
a = 0;
b = 1;

function I = formula_trapezi(f, a, b, n)
    x = linspace(a, b, n + 1);
    y = f(x);
    h = (b - a) / n;
    I = h * (sum(y) - 0.5 * (y(1) + y(end)));
end

% Valore esatto dell'integrale
I_esatto = integral(f,0,2);
fprintf("Valore esatto: %.15f\n", I_esatto);

% Vettore dei vari n
n_values = [20, 40, 80, 160, 320, 640, 1280, 2560, 5120];

% Calcolo e stampa degli errori
fprintf("  n\t\tI_n\t\tErrore\n");
for n = n_values
    I_n = formula_trapezi(f, 0, 2, n);
    errore = abs(I_n - I_esatto);
    fprintf("%5d\t%.15f\t%.10f\n", n, I_n, errore);
end