% Problema 2.1

% (a) Calcolo del polinomio di interpolazione e del vettore delle differenze

x_nodes = [0, 1/64, 4/64, 9/64, 16/64, 25/64, 36/64, 49/64, 1];
y_nodes = sqrt(x_nodes);

zeta = (0:20) / 20;
sqrt_zeta = sqrt(zeta);

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


p = newton_interpolation(x_nodes, y_nodes, zeta);

diff_vector = p - sqrt_zeta;


% stampo il vettore
disp('Vettore differenze:');
for i = 1:length(diff_vector)
    fprintf('%d: %1.5f\n', i, diff_vector(i));
end

% (b) Grafico della funzione e del polinomio di interpolazione

% Creazione di punti per il grafico
x_plot = linspace(0, 1, 100);
p = polyfit(x_nodes, y_nodes, length(x_nodes) - 1);
% Valutazione del polinomio nei punti x_plot
p_plot = polyval(p, x_plot);

% Valutazione della funzione sqrt(x) nei punti x_plot
sqrt_plot = sqrt(x_plot);

% Creazione del grafico
figure;
plot(x_plot, sqrt_plot, 'b-', 'DisplayName', 'sqrt(x)');
hold on;
plot(x_plot, p_plot, 'r-', 'DisplayName', 'p(x)');
xlabel('x');
ylabel('y');
title('Grafico di sqrt(x) e p(x)');
legend;