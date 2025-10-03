% Funzione da integrare
f = @(x) exp(x);

% Calcolo dell'integrale
a = 0;
b = 1;
I_exact = integral(f,a,b); 
fprintf('Valore esatto dell integrale: %.15f\n\n', I_exact);

%% Parte (a) e (b): Determinazione di n(eps) per vari valori di eps
for i = 1:10
    epsilon_values(i) = 10^(-i);
end

n_values = zeros(size(epsilon_values));
I_n_values = zeros(size(epsilon_values));
errors = zeros(size(epsilon_values));

%% Funzione per la formula dei trapezi
function I = formula_trapezi(f, a, b, n)
    x = linspace(a, b, n + 1); % Punti equidistanti
    y = f(x); % Valutazione della funzione
    h = (b - a) / n; % Passo di discretizzazione
    I = h * (sum(y) - 0.5 * (y(1) + y(end))); % Formula dei trapezi
end

for i = 1:length(epsilon_values)
    epsilon = epsilon_values(i);
    n = ceil(sqrt(exp(1)/(12*epsilon)));
    I_n = formula_trapezi(f, a, b, n);
    n_values(i) = n;
    I_n_values(i) = I_n;
    errors(i) = abs(I_exact - I_n);
end

% tabella
fprintf('   Epsilon       n    I_n               Error\n');
fprintf('---------------------------------------------------\n');
for i = 1:length(epsilon_values)
    fprintf('%10.1e   %4d   %.15f   %.5e\n', epsilon_values(i), n_values(i), I_n_values(i), errors(i));
end
fprintf('\n');

%% Parte (c): Approssimazioni per n = 2, 4, 8, 16
n_test = [2, 4, 8, 16];
I_test = arrayfun(@(n) formula_trapezi(f, a, b, n), n_test);
errors_test = abs(I_test - I_exact);

% Stampa tabella con I2, I4, I8, I16
fprintf('Val          I                 Error: |I_n - I|\n');
fprintf('--------------------------------------------------\n');
for i = 1:length(n_test)
    fprintf('I%d        %.15f      %.5e\n', n_test(i), I_test(i), errors_test(i));
end
fprintf('\n');

h_values = 1 ./ n_test; % Passi di discretizzazione h = 1/n

function p_t = newton_interpolation(x, y, t)
        n = length(x);
        m = length(t);
        
    % Calcolo della tabella delle differenze divise
        div_diff = zeros(n, n);
        div_diff(:,1) = y(:); % Prima colonna è y
    
        for j = 2:n
            for i = 1:n-j+1
                div_diff(i,j) = (div_diff(i+1,j-1) - div_diff(i,j-1)) / (x(i+j-1) - x(i));
            end
        end
        
        coeff = div_diff(1, :);
    
        p_t = zeros(size(t));
    
        for k = 1:m
            tk = t(k);
            p_t(k) = coeff(1);
            term = 1;
            for j = 2:n
                term = term * (tk - x(j-1));
                p_t(k) = p_t(k) + coeff(j) * term;
            end
        end
    end

function p0 = estrapolazione(f, a, b, n_vect)

    m = length(n_vect);

    for i = 1:m
       I(i) = formula_trapezi(f,a,b,n_vect(i));
       H(i) = ((b - a) / n_vect(i))^2;
    end 

    p0 = newton_interpolation(H,I,0);
    
end

p0 = estrapolazione(f,a,b,n_test);

fprintf('p(0): %.15f\n', p0);
fprintf('Errore interpolazione |p(0) - I|: %.15e\n', abs(p0 - I_exact));