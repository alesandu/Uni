x = [0,1,2,3];
y = [0,3,1,1];
t = [2.3];

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

% TEST
p_t = newton_interpolation(x, y, t);

for i = 1:length(t)
    fprintf('p(%.2f) = %.6f\n', t(i), p_t(i));
end