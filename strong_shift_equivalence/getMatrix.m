%% get all non-negative integral matrix with rank r and entry value at most k
function ret = getMatrix(m,n,k,r)
%ret1=zeros(m,n);
ret=[];
%range=(int64(k+1))^(m*n)-1
%e=int64(0);
%while e<=range
range=((k+1))^(m*n)-1
parfor e=0:range
	ret1=zeros(m,n);
	modu=(k+1);
	for i=1:m
		for j=1:n
			ret1(i,j)=mod(e,modu);
			modu=modu*(k+1);
		end
	end
	%e=e+1;
	if r>=0 && rank(ret1) ~= r
		continue;
	end
	ret=[ret;ret1];
end

end

%% unpack for the kth slice of the matrix mxn under the packing of getMatrix
function ret = unpack(A,m,n,k)
ret=A((k-1)*m+1:k*m,:);
return;
end
