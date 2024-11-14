I = eye(19680);

for year = 1990:2028
    
    disp(year)
    
    A = readtable( [ num2str(year) '_A.csv' ] );
    A = A{:,:};
    
     sum(A, 'all')
    
    L = inv( I - A );
    
    sum(sum(L))
    
    writematrix(L, [ num2str(year) '_L.csv' ] );
    
end