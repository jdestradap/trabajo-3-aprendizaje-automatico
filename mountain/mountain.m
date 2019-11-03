clear all
load iris.dat

%% Bajas dimensiones

%A=rand(1000,2);
B = iris(:,1:4);

[X_embebbed, k_opt] = pca_aprendizaje(B,2);

X_embebbed_out = normalize(X_embebbed,'range');

%% Parámetros
% A = X_embebbed
A=X_embebbed_out;
alfa=5.71;
[m,n]=size(A);
puntos_malla=100;
maxIter=30;
gamma=0.1;

%% Crear malla
Grill = zeros(puntos_malla, puntos_malla);

%% Crear la primera montaña
for k=1:length(Grill)
    for l=1:length(Grill)
        %Grill i=Grill(k,i)
        suma=0;
        puntoi=Grill(k,l);
        for j=1:n
             valor_absoluto_resta=abs(A(j,:)-([k,l]/puntos_malla));
             dist=sum(valor_absoluto_resta.^2)^0.5;
            suma=suma+exp(-alfa*dist^2);
        end
        M{1}(k,l)=suma;
    end
end

[fila,columna] = find(M{1}==max(max(M{1})));
mMax(1)=max(max(M{1}));
mesh(M{1});

 for i=2:maxIter     
    for k=1:length(Grill)
        for l=1:length(Grill)
             %gbest=Grill(find(M{k-1}==mMax(k-1)),:)%la mejor de las Grills
             valor_absoluto_resta=abs([fila,columna]-[k,l]);
             dist=sum(valor_absoluto_resta.^2)^0.5;
             M{i}(k,l)=M{i-1}(k,l)-mMax(i-1)*exp(-gamma*dist^2);
        end
    end
    %mMax(k)=max(M{k});
    [fila,columna] = find(M{i}==max(max(M{i})));
    mMax(i)=max(max(M{i}));
 end
 
figure
mesh(M{i})
