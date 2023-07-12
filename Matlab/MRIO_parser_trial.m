I = eye(19680);

%for year = 1993:1994
    
%    disp(year)
    
%    A = csvread( [ num2str(year) '_A.csv' ] );
    
    
%     sum(A, 'all')
    
    L = inv( I - A_new );
    
%    sum(sum(L))
    
%    writematrix(L, [ num2str(year) '_L.csv' ] );
    
%end
