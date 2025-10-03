mangia(ragno,rosmarino).
mangia(cavallo,ragno).
mangia(io,cavallo).
mangia(io,rosmarino).

dfs(X, Y, [X,Y], LV):-
    mangia(X,Y),
    LV is 0.87.

dfs(X,Y,[X|P_Z_Y],LV):-
    mangia(X,Z),
    dfs(Z,Y,P_Z_Y,LVZ),!,
    LV is LVZ*0.87.