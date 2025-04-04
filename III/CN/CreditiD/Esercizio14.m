function [x, K, res_norm] = jacobi(A, b, eps, x0, Nmax)
    % Metodo di Jacobi per risolvere Ax = b
    n = length(b);
    x_old = x0;
    D = diag(diag(A));
    P = inv(D)*(D - A);
    q = inv(D)*b;
    
    for k = 1:Nmax
        x = P * x_old + q;
        r = b - A * x;
        res_norm = norm(r, 2);
        
        if res_norm <= eps * norm(b, 2)
            K = k;
            return
        end
        x_old = x;
    end
    
    % Se non converge entro Nmax iterazioni
    K = Nmax;
    res_norm = norm(b-A*x,2)/norm(b,2)
end