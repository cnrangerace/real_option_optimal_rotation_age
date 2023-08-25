
r=0.1;S=-76.11;E=100;T=1;sigma=0.3;

S_END=2;    % increase if more values of S are needed in the graph
M=2*r/(sigma^2);
N=2*r/(sigma^2);
K=1-exp(-r*T);
q_a=(-(N-1)-sqrt((N-1)^2+4*M/K))/2;
S_SS=find(S,E,r,T,sigma,q_a);
d1=DD(S_SS,E,r,T,sigma);
% Nd1=normcdf(-d1,0,1)

if S<0
    SE=0:0.05:(S_SS-0.05);
	SE2=[SE(end):0.05:S_SS*S_END];
    LSE=length(SE2);
    for il=1:LSE
        SV=SE2(il);
        A1=-(S_SS/q_a)*(1-exp(-0*T)*normcdf((-d1),0,1));
        [c,p] = blsprice(SV,E,r,T,sigma);
        P(il)=p+A1*(SV/S_SS)^q_a;
    end
    PE=E-SE;
    SE=SE;
    PE=PE;
    plot(SE,PE,'g')
    hold on
    SE2=SE2;
    P=P;
    plot(SE2,P,'r')
    title('American Put Option Approximation')
    xlabel('Stock Price')
    ylabel('Option Value')
    P=[PE P];
    S=[SE SE2];
%     S_SS
else
    if S>S_SS
        A1=-(S_SS/q_a)*(1-exp(-0*T)*normcdf((-d1),0,1));
        [c,p] = blsprice(S,E,r,T,sigma);
        P=p+A1*(S/S_SS)^q_a;
    else
        P=E-S;
    end
end

hold on;
clear;

r=0.1;S=-76.11;E=100;T=1;sigma=0.3;

S_END=2;    % increase if more values of S are needed in the graph
M=2*r/(sigma^2);
N=2*r/(sigma^2);
K=1-exp(-r*T);
q_a=(-(N-1)-sqrt((N-1)^2+4*M/K))/2;
S_SS=find_SS(S,E,r,T,sigma,q_a);
d1=DD(S_SS,E,r,T,sigma);
% Nd1=normcdf(-d1,0,1)

if S<0
    SE=0:0.05:(S_SS-0.05);
	SE2=[SE(end):0.05:S_SS*S_END];
    LSE=length(SE2);
    for il=1:LSE
        SV=SE2(il);
        A1=-(S_SS/q_a)*(1-exp(-0*T)*normcdf((-d1),0,1));
        [c,p] = blsprice(SV,E,r,T,sigma);
        P(il)=p+A1*(SV/S_SS)^q_a;
    end
    PE=E-SE;
    SE=SE;
    PE=PE;
    plot(SE,PE,'g')
    hold on
    SE2=SE2;
    P=P;
    plot(SE2,P,'c')
    title('American Put Option Approximation')
    xlabel('Stock Price')
    ylabel('Option Value')
    P=[PE P];
    S=[SE SE2];
%     S_SS
else
    if S>S_SS
        A1=-(S_SS/q_a)*(1-exp(-0*T)*normcdf((-d1),0,1));
        [c,p] = blsprice(S,E,r,T,sigma);
        P=p+A1*(S/S_SS)^q_a;
    else
        P=E-S;
    end
end
hold on;
clear

r=0.1;S=-76.11;E=100;T=1;sigma=0.3;

S_END=2;    % increase if more values of S are needed in the graph
M=2*r/(sigma^2);
N=2*r/(sigma^2);
K=1-exp(-r*T);
q_a=(-(N-1)-sqrt((N-1)^2+4*M/K))/2;
S_SS=find_SS(S,E,r,T,sigma,q_a);
d1=DD(S_SS,E,r,T,sigma);
% Nd1=normcdf(-d1,0,1)

if S<0
    SE=0:0.05:(S_SS-0.05);
	SE2=[SE(end):0.05:S_SS*S_END];
    LSE=length(SE2);
    for il=1:LSE
        SV=SE2(il);
        A1=-(S_SS/q_a)*(1-exp(-0*T)*normcdf((-d1),0,1));
        [c,p] = blsprice(SV,E,r,T,sigma);
        P(il)=p+A1*(SV/S_SS)^q_a;
    end
    PE=E-SE;
    SE=SE;
    PE=PE;
    plot(SE,PE,'g')
    hold on
    SE2=SE2;
    P=P;
    plot(SE2,P,'r')
    title('American Put Option Approximation')
    xlabel('Stock Price')
    ylabel('Option Value')
    P=[PE P];
    S=[SE SE2];
%     S_SS
else
    if S>S_SS
        A1=-(S_SS/q_a)*(1-exp(-0*T)*normcdf((-d1),0,1));
        [c,p] = blsprice(S,E,r,T,sigma);
        P=p+A1*(S/S_SS)^q_a;
    else
        P=E-S;
    end
end


hold on;
clear

r=0.1;S=-76.11;E=100;T=1;sigma=0.3;

S_END=2;    % increase if more values of S are needed in the graph
M=2*r/(sigma^2);
N=2*r/(sigma^2);
K=1-exp(-r*T);
q_a=(-(N-1)-sqrt((N-1)^2+4*M/K))/2;
S_SS=find_SS(S,E,r,T,sigma,q_a);
d1=DD(S_SS,E,r,T,sigma);
% Nd1=normcdf(-d1,0,1)

if S<0
    SE=0:0.05:(S_SS-0.05);
	SE2=[SE(end):0.05:S_SS*S_END];
    LSE=length(SE2);
    for il=1:LSE
        SV=SE2(il);
        A1=-(S_SS/q_a)*(1-exp(-0*T)*normcdf((-d1),0,1));
        [c,p] = blsprice(SV,E,r,T,sigma);
        P(il)=p+A1*(SV/S_SS)^q_a;
    end
    PE=E-SE;
    SE=SE;
    PE=PE;
    plot(SE,PE,'g')
    hold on
    SE2=SE2;
    P=P;
    plot(SE2,P,'r')
    title('American Put Option Approximation')
    xlabel('Stock Price')
    ylabel('Option Value')
    P=[PE P];
    S=[SE SE2];
%     S_SS
else
    if S>S_SS
        A1=-(S_SS/q_a)*(1-exp(-0*T)*normcdf((-d1),0,1));
        [c,p] = blsprice(S,E,r,T,sigma);
        P=p+A1*(S/S_SS)^q_a;
    else
        P=E-S;
    end
end

