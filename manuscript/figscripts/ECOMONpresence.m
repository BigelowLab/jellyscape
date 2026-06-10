function dataout = ECOMONpresence(speciescol, pgon, years, mons)
% dataout = ECOMONpresence(speciescol, pgon, years, mons)

load ~/Work/Data/ECOMON/EcoMon_Plankton_Data_v3_10.mat

for i=1:length(years)
    for j=1:length(mons)
        I = find(inpolygon(ECOMON(:,3),ECOMON(:,2),pgon(:,1),pgon(:,2)));
        J=find(ECOMON(I,4)==years(i) & ECOMON(I,5)==mons(j));
        K=find(ECOMON(I(J),speciescol)>0);
        dataout(i,j)=length(K)/length(J);
    end
end

