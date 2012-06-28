%%enumerate via bitwise operator?
function [ret1 ret2]= testSSE(A,B,k)
global parentMap;
parentMap=java.util.HashMap();
%rank2_2x3=getMatrix(2,3,k,2);
%save rank23 rank2_2x3;
%rank2_2x4=getMatrix(2,4,k,2);
%save rank24 rank2_2x4;
%rank2_3x4=getMatrix(3,4,k,2);
%save rank34 rank2_3x4;
%rank2_2x5=getMatrix(2,5,k,2);
%save rank25 rank2_2x5;
%rank2_3x5=getMatrix(3,5,k,2);
%save rank35 rank2_3x5;
%rank2_4x5=getMatrix(4,5,k,2);
%save rank45 rank2_4x5;
%%inv4x4=getMatrix(4,4,k,4);
%%inv5x5=getMatrix(5,5,k,5);
%%all3x3=getMatrix(3,3,k,-1);
%%save all33 all3x3;
all4x4=getMatrix(4,4,k,-1);
save all44 all4x4;


rank2_2x3=(load('rank23'));
rank2_2x3=rank2_2x3.rank2_2x3;
all3x3=(load('all33'));
all3x3=all3x3.all3x3;
%rank2_2x4=load('rank24').rank2_2x4;
%rank2_3x4=load('rank34').rank2_3x4;
s23=size(rank2_2x3,1)/2;
%s24=size(rank2_2x4,1)/2;
%s34=size(rank2_3x4,1)/3;
s33=size(all3x3,1)/3;
%s25=size(rank2_2x5,1)/2;
%s35=size(rank2_3x5,1)/3
%s45=size(rank2_4x5,1)/4;
%%TODO:make everything matrix multiplication,this would be much much faster
fprintf('first\n');
for i=1:s23
	for j=i:s23
		R=unpack(rank2_2x3,2,3,i);
		S=unpack(rank2_2x3,2,3,j);
		AA=R'*S;
		BB=S*R';
		link(mat2str(AA),mat2str(BB));
	end
end

fprintf('second\n');
for i=1:s33
	for j=i:s33
		R=unpack(all3x3,3,3,i);
		S=unpack(all3x3,3,3,j);
		AA=R'*S;
		BB=S*R';
		link(mat2str(AA),mat2str(BB));
	end
end
%for i=1:s24
%	for j=i:s24
%		R=unpack(rank2_2x4,2,4,i);
%		S=unpack(rank2_2x4,2,4,j);
%		AA=R'*S;
%		BB=S*R';
%		link(mat2str(AA),mat2str(BB));
%	end
%end
%
%for i=1:s25
%	for j=i:s25
%		R=unpack(rank2_2x5,2,5,i);
%		S=unpack(rank2_2x5,2,5,j);
%		AA=R'*S;
%		BB=S*R';
%		link(mat2str(AA),mat2str(BB));
%	end
%end
%

%for i=1:s34
%	for j=i:s34
%		R=unpack(rank2_3x4,3,4,i);
%		S=unpack(rank2_3x4,3,4,j);
%		AA=R'*S;
%		BB=S*R';
%		link(mat2str(AA),mat2str(BB));
%	end
%end
%
%for i=1:s35
%	for j=i:s35
%		R=unpack(rank2_3x5,3,5,i);
%		S=unpack(rank2_3x5,3,5,j);
%		AA=R'*S;
%		BB=S*R';
%		link(mat2str(AA),mat2str(BB));
%	end
%end
%
%for i=1:s45
%	for j=i:s45
%		R=unpack(rank2_4x5,4,5,i);
%		S=unpack(rank2_4x5,4,5,j);
%		AA=R'*S;
%		BB=S*R';
%		link(mat2str(AA),mat2str(BB));
%	end
%end

if findroot(mat2str(A))==findroot(mat2str(B))
	fprintf('found SSE.\n');
	display(findroot(mat2str(A)));
else
	fprintf('not found.\n');
	display(s23);
	display(s24);
	display(s25);
	display(s34);
	display(s35);
	display(s45);

end
end



%% given mat2str of a matrix, find its root
function root = findroot(x)
global parentMap;
if ~ parentMap.containsKey(x)
	root = x;
	return;
else
	%root= findroot(parentMap.get(x));
	root=mat2str(parentMap.get(x));
	while parentMap.containsKey(root) 
		root1=mat2str(parentMap.get(root));
		if prod(double(root1 == root ))
			break;
		end
		root=root1;
	end

	parentMap.put(x,mat2str(root));
	return;
end
end

%% given 2 mat2str of 2 matrices, link them.
function link(x,y)
global parentMap;
px=mat2str(findroot(x));
py=mat2str(findroot(y));
if ~prod(double(size(px)==size(py))) || (~prod(double(px == py)))
	parentMap.put(py,px);
end
%% TODO: link by rank???implemente via another Map?
end

%% unpack for the kth slice of the matrix mxn under the packing of getMatrix
function ret = unpack(A,m,n,k)
ret=A((k-1)*m+1:k*m,:);
return;
end
