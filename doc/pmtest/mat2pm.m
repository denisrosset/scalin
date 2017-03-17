% MAT2PM Finds principal minors of an n x n real or complex matrix.
%   PM = MAT2PM(A)
%   where "A" is an n x n matrix in which zero can arise as a pivot at any
%   point.  MAT2PM returns a 2^n - 1 vector of all the principal minors
%   of the matrix "A".
%
%   PM = MAT2PM(A, THRESH)
%   Explicitly sets the pseudo-pivot threshold to THRESH.  Pseudo-pivoting 
%   will occur when a pivot smaller in magnitude than THRESH arises.  Set
%   THRESH = 0 to never pseudo-pivot except for a pivot of exactly zero.
%
%   The structure of PM, where |A[v]| is the principal minor of "A" indexed
%   by the vector v:
%   PM: |A[1]|, |A[2]|, |A[1 2]|, |A[3]|, |A[1 3]|, |A[2 3]|, |A[1 2 3]|,...
%
% Author: From website of Pr. Michael Tsatsomeros,
% seems to originate from PhD thesis of Kent E. Griffin
% Taken from: http://www.math.wsu.edu/faculty/tsat/files/pm/mat2pm.m on Mar. 13, 2017
function [pm] = mat2pm(a, thresh)
% Only works on up to 48x48 matrices due to restrictions
% on bitcmp and indices.

n = length(a);
scale = sum(sum(abs(a)))/(n*n); % average magnitude of matrix
if scale == 0
    scale = 1;              % prevent divide by 0 if matrix is zero
end
ppivot = scale;             % value to use as a pivot if near 0 pivot arises
if nargin == 1
    thresh = (1.0e-5)*scale;    % when to pseudo-pivot
end

zeropivs = [];
pm = zeros(1, 2^n - 1);     % where the principal minors are stored
ipm = 1;                    % index for storing principal minors
q = zeros(n,n,1);           % q is the input queue of unprocessed matrices
q(:,:,1) = a;               % initial queue just has 1 matrix to process
pivmin = inf;               % keep track of smallest pivot actually used

%
% Main 'level' loop
%
for level = 0:n-1
    [n1, n1, nq] = size(q);
    % The output queue has twice the number of matrices, each one smaller
    % in row and col dimension
    qq = zeros(n1-1, n1-1, nq*2);
    ipm1 = 1;               % for indexing previous pm elements
    for i = 1:nq
        a = q(:,:,i);
        pm(ipm) = a(1,1);
        if n1 > 1
            abspiv = abs(pm(ipm));
            if abspiv <= thresh
                zeropivs = union(zeropivs, ipm);
                % Pivot nearly zero, use "pseudo-pivot"
                pm(ipm) = pm(ipm) + ppivot;
                abspiv = abs(pm(ipm));
            end
            if abspiv < pivmin
                pivmin = abspiv;
            end
            b = a(2:n1,2:n1);
            d = a(2:n1,1)/pm(ipm);
            c = b - d*a(1,2:n1);

            % Order the output queue to make the elements of pm come out in
            % the correct order.
            qq(:,:,i) = b;
            qq(:,:,i+nq) = c;
        end
        if i > 1
            % if i > 1, to convert from a general pivot to a principal
            % minor, we need to multiply by every element of the pm matrix
            % we have already generated, in the order that we generated it.
            pm(ipm) = pm(ipm)*pm(ipm1);
            ipm1 = ipm1 + 1;
        end
        ipm = ipm + 1;
    end
    q = qq;
end

%
% Zero Pivot Loop
%
% Now correct principal minors for all places we used ppivot as a pivot
% in place of a (near) 0.
for i = length(zeropivs):-1:1
    mask = uint64(zeropivs(i));
    delta = msb(mask);
    delta2 = 2*delta;
    ipm1 = bitand(uint64(mask), bitcmp(delta,'uint64'));
    if ipm1 == 0
        pm(mask) = pm(mask) - ppivot;
    else
        pm(mask) = (pm(mask)/pm(ipm1) - ppivot)*pm(ipm1);
    end
    for j = mask+delta2:delta2:2^n - 1
        pm(j) = pm(j) - ppivot*pm(j - delta);
    end
end
% Warn user in case larger pivots are desired
fprintf(2, 'MAT2PM: pseudo-pivoted %d times, smallest pivot used: %e\n', ...
    length(zeropivs), pivmin);

% Returns the numerical value of the most significant bit of x.
% For example, msb(7) = 4, msb(6) = 4, msb(13) = 8.
function [m] = msb(x)
persistent MSBTABLE     % MSBTABLE persists between calls to mat2pm
if isempty(MSBTABLE)
    % If table is empty, initialize it
    MSBTABLE = zeros(255,1, 'uint64');
    for i=1:255
        MSBTABLE(i) = msbslow(i);
    end
end

m = 0;
% process 8 bits at a time for speed
if x ~= 0
    while x ~= 0
        x1 = x;
        x = bitshift(x, -8);    % 8 bit left shift
        m = m + 8;
    end
    m = bitshift(MSBTABLE(x1), m-8); % right shift
end

% Returns the numerical value of the most significant bit of x.
% For example, msb(7) = 4, msb(6) = 4, msb(13) = 8.  Slow version
% used to build a table.
function [m] = msbslow(x)
m = 0;
if x ~= 0
    m = 1;
    while  x ~= 0
        x = bitshift(x, -1);
        m = 2*m;
    end
    m = m/2;
end
