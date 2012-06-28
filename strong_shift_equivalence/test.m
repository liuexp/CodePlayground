function [ret1 ret2]= test(A,B,k,opt,d)
if d==0
	ret1 = [];
	ret2 = [];
	return;
end
markset1 = java.util.HashSet();
markset2 = java.util.HashSet();
markset1all = java.util.HashSet();
markset2all = java.util.HashSet();
curmarkset1 = java.util.HashSet();
curmarkset2 = java.util.HashSet();


curmarkset1.add(mat2str(A));
curmarkset2.add(mat2str(B));
%tmp=mat2str([NaN NaN NaN;NaN NaN NaN;NaN NaN NaN]);
for i=1:d
	markset1=curmarkset1;
	markset2=curmarkset2;
	la=markset1all.size;
	lb=markset2all.size;
	markset1all.addAll(markset1);
	markset2all.addAll(markset2);
	if la==markset1all.size && lb == markset2all.size
		fprintf('not found\n');
		display(i);
		display(la);
		display(lb);
	end
for a11=0:k
for a12=0:k
for a13=0:k
for a21=0:k
for a22=0:k
for a23=0:k
for a31=0:k
for a32=0:k
for a33=0:k
	R=[a11,a12,a13;a21,a22,a23;a31,a32,a33];
%%	if opt==0
%%		if det(R)==0
%%			continue;
%%		end
%%		S=inv(R)*A;
%%		C=S*R;
%%		if C == B
%%			ret=[S;R];
%%		else
%%			ret=test(C,k,opt,d-1);
%%
%%	else if opt==1
%%		if rank(R)<2
%%			continue;
%%		end
%%
%%	end

	if opt==0
		if abs(det(R))<1
			continue;
		end
		if ~prod(prod(double(floor(inv(R)) == inv(R))))
			continue;
		end

		curmarkset1 = java.util.HashSet();
		curmarkset2 = java.util.HashSet();


		m1a=markset1.toArray();

		for j=1:m1a.length
			Ap=((eval(m1a(j))));
			AA=inv(R)*Ap*R;
			As=mat2str(AA);

			if ~ prod(prod(double(~(isnan(AA)|isinf(AA)))))
				continue;
				%display(i);
				%display(AA);
				%display(Ap);
				%display(R);
			end

			if markset2all.contains(As) 
				fprintf('found!\n');
				display(i);
				display(AA);
				display(Ap);
				display(m1a(j));
				display(R);
				ret1=markset1;
				ret2=markset2;
				return;
			end
			curmarkset1.add(As);
		end
		%fprintf('mark');
		m2a=markset2.toArray();
		for j=1:m2a.length
			Bp=((eval(m2a(j))));
			BB=R*Bp*inv(R);
			Bs=mat2str(BB);

			if ~ prod(prod(double(~(isnan(BB)|isinf(BB)))))
				continue;
				%display(i);
				%display(BB);
				%display(Bp);
				%display(R);
			end

			if markset1all.contains(Bs) 
				fprintf('found!\n');
				display(i);
				display(Bp);
				display(BB);
				display(R);
				ret1=markset1;
				ret2=markset2;
				return;
			end
			curmarkset2.add(Bs);
		end
	else if opt==1
		if rank(R)<2
			continue;
		end

	end

end
end
end
end
end
end
end
end
end
end
end

