f = @(x) x.^2 .* exp(-x);

%% (a) Calcolo dell'integrale
a = 0;
b = 1;
I_exact = integral(f, a, b);
fprintf('(a) Valore esatto dell integrale: %.15f\n\n', I_exact);

%% (b) Calcolo di I5, I10, I20, I40
n_vals = [5, 10, 20, 40];
I_n = zeros(size(n_vals));

function I = formula_trapezi(f, a, b, n)
        x = linspace(a, b, n + 1);
        y = f(x);
        h = (b - a) / n;
        I = h * (sum(y) - 0.5 * (y(1) + y(end)));
    end

for i = 1:length(n_vals)
    I_n(i) = formula_trapezi(f, a, b, n_vals(i));
end

%% (c) Interpolazione su h^2 e calcolo di p(0)
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

p0 = estrapolazione(f,a,b,n_vals);

fprintf('(c) p(0) = %.15f\n', p0);

%% (d) Tabella dei risultati
errors = abs(I_n - I_exact);
error_p0 = abs(p0 - I_exact);

fprintf('\n(d) Tabella riassuntiva:\n');
fprintf(' n      I_n                  Errore |I_n - I|\n');
fprintf('----------------------------------------------\n');
for i = 1:length(n_vals)
    fprintf('%2d     %.15f        %.5e\n', n_vals(i), I_n(i), errors(i));
end
fprintf('p(0)   %.15f        %.5e\n', p0, error_p0);

%% (e) Determinazione di n per cui |I_n - I| <= |p(0) - I|
epsilon = error_p0;
n = 1121000;
I_n_check = formula_trapezi(f, 0, 1, n);
while abs(I_n_check - I_exact) > epsilon
    n = n+1;
    I_n_check = formula_trapezi(f, 0, 1, n);
end

fprintf('\n(e) n per cui errore <= |p(0) - I| = %d\n', n);
fprintf('I_n = %.15f, Errore = %.5e\n', I_n_check, abs(I_n_check - I_exact));