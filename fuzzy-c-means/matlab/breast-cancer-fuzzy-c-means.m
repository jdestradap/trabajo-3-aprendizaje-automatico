% 1. Sample code number: id number
% 2. Clump Thickness: 1 - 10
% 3. Uniformity of Cell Size: 1 - 10
% 4. Uniformity of Cell Shape: 1 - 10
% 5. Marginal Adhesion: 1 - 10
% 6. Single Epithelial Cell Size: 1 - 10
% 7. Bare Nuclei: 1 - 10
% 8. Bland Chromatin: 1 - 10
% 9. Normal Nucleoli: 1 - 10
% 10. Mitoses: 1 - 10
% 11. Class: (2 for benign, 4 for malignant)
     
load data/breast-cancer-wisconsin.data.csv

benignIndex = breast_cancer_wisconsin_data(:,11)==2;

malignIndex = breast_cancer_wisconsin_data(:,11)==4;

benign = breast_cancer_wisconsin_data(benignIndex,:);

malign = breast_cancer_wisconsin_data(malignIndex,:);

Characteristics = {'Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape',...
    'Marginal Adhesion', 'Single Epithelial Cell Size','Bare Nuclei', 'Bland Chromatin',... 
    'Normal Nucleoli','Mitoses', 'Class'};

pairs = combnk(2:10,2);

x = pairs(1,1); 
y = pairs(1,2);   
subplot(1,2,1)


plot([benign(:,x)],...
  [benign(:,y)], '.')
  
% hold on

xlabel(Characteristics{x})
ylabel(Characteristics{y})
% 
% x = pairs(2,1); 
% y = pairs(2,2);   
% subplot(1,2,2)
% 
% plot([benign(:,x)],...
%      [benign(:,y)], '.')
% xlabel(Characteristics{x})
% ylabel(Characteristics{y})


% Bajas dimensiones

% [X_reduced_2, best_k_2] = pca_aprendizaje(breast_cancer_wisconsin_data(:,2:10),2);

% Nc-Number of clusters 
% M-Fuzzy partition matrix exponent, which indicates the degree of fuzzy
% overlap between clusters
% maxIter - Maximum number of iterations. 
% minImprove - Minimum improvement.

Nc = 2;
M = 2.0;
maxIter = 100;
minImprove = 1e-6;

clusteringOptions = [M maxIter minImprove true];
[centers,U] = fcm(X_reduced_2,Nc,clusteringOptions);


