% iris dat
load iris.dat

setosaIndex = iris(:,5)==1;
versicolorIndex = iris(:,5)==2;
virginicaIndex = iris(:,5)==3;

setosa = iris(setosaIndex,:);
versicolor = iris(versicolorIndex,:);
virginica = iris(virginicaIndex,:);

Characteristics = {'sepal length','sepal width','petal length','petal width'};
pairs = [1 2; 1 3; 1 4; 2 3; 2 4; 3 4];

for i = 1:6
    x = pairs(i,1); 
    y = pairs(i,2);   
    subplot(2,3,i)
    plot([setosa(:,x) versicolor(:,x) virginica(:,x)],...
         [setosa(:,y) versicolor(:,y) virginica(:,y)], '.')
    xlabel(Characteristics{x})
    ylabel(Characteristics{y})
end


% Nc-Number of clusters 
% M-Fuzzy partition matrix exponent, which indicates the degree of fuzzy
% overlap between clusters
% maxIter - Maximum number of iterations. 
% minImprove - Minimum improvement.

Nc = 3;
M = 2.0;
maxIter = 100;
minImprove = 1e-6;

clusteringOptions = [M maxIter minImprove true];
[centers,U] = fcm(iris,Nc,clusteringOptions);

for i = 1:6
    subplot(2,3,i);
    for j = 1:Nc
        x = pairs(i,1);
        y = pairs(i,2);
        text(centers(j,x),centers(j,y),int2str(j),'FontWeight','bold');
    end
end
