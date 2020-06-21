

************************************************************************************
************************************************************************************
* This do file reproduces all the table of the paper and of the appendix
************************************************************************************
************************************************************************************
set scrollbufsize 300000
set more off, perm
* Link dta file: 
https://www.dropbox.com/s/kzimec52st3jse9/JHR_Cahuc_Carcillo_Minea.dta?dl=0
global path="your_path"
cd "${path}"




************************************************************************************
************************************************************************************
* Definition of profiles and other useful variables
************************************************************************************
************************************************************************************
use "$path/JHR_Cahuc_Carcillo_Minea.dta", clear

gen mois=month(DateEnvoi)

drop reponseb
gen reponseb=1 if reponse==1|  reponse==.5 
replace reponseb=0 if reponse==0| reponse==.
label var reponseb "=1 if interview or information"


capture drop new_profile
gen new_profile=profile
replace new_profile="Q" if profile=="M_Q"
replace new_profile="Q" if profile=="NM_Q"
replace new_profile="Q_NS" if profile=="M_Q_NS"
replace new_profile="Q_NS" if profile=="NM_Q_NS"
replace new_profile="SQ" if profile=="M_SQ"
replace new_profile="SQ" if profile=="NM_SQ"
replace new_profile="SQ_NS" if profile=="M_SQ_NS"
replace new_profile="SQ_NS" if profile=="NM_SQ_NS"
replace new_profile="CH" if profile=="CH_0CDD"|profile=="CH_1CDD"|profile=="CH_2CDD"|profile=="CH_3CDD"

br profile new_profile

ta new_profile

bysort marchand: ta new_profile

capture drop Q
gen Q=(M_Q==1 | NM_Q==1)
capture drop Q_NS
gen Q_NS=(M_Q_NS | NM_Q_NS)
capture drop SQ
gen SQ=(M_SQ | NM_SQ)
capture drop SQ_NS
gen SQ_NS=(M_SQ_NS | NM_SQ_NS)
gen CH=(CH_0CDD==1| CH_0CDD==1| CH_1CDD==1 |CH_3CDD==1)

capture drop certified
gen certified=(Q | Q_NS)
capture drop non_certified
gen non_certified=(SQ | SQ_NS)

drop M_Q M_Q_NS M_SQ M_SQ_NS NM_Q NM_Q_NS NM_SQ NM_SQ_NS

label var Q "Subsidized, Certified Skills"
label var Q_NS "Non-subsidized, Certified Skills"
label var SQ "Subsidized, No Certified Skills"
label var SQ_NS "Non-subsidized, No Certified Skills"
label var CH "Unemployed"

 

* Response types
capture drop reponsea
gen reponsea=reponse
replace reponsea=0 if reponse!=1
label var reponsea "=1 if interview 0 otherwwise"


capture drop reponseb
gen reponseb=1 if reponse==1|  reponse==.5 
replace reponseb=0 if reponse==0| reponse==.
sum reponseb


encode departement, gen(_departement)
gen mois_dep=(mois*departement)



capture drop poste_
egen poste_=group(Poste)

 
* Quintiles of commuting zone unemployment rates	  

capture drop uze
capture drop pct_u
capture drop percent_u
xtile uze=uze_av_Q1_Q2_16,nq(5)
pctile pct_u = uze_av_Q1_Q2_16, nq(5) genp(percent_u)

forvalues i=1/5 {
disp "uze=`i'"
 sum uze_av_Q1_Q2_16 if uze==`i'
 }


*

* Quintiles of commuting zone callback rates

capture drop a
capture drop callback_ze_
capture drop callback_ze

bys ZE2010: egen callback_ze_=sum(reponseb) 
bys ZE2010: egen callback_ze=max(callback_ze_) 
gen a=1
capture drop reponseb_nb
bys ZE2010: egen reponseb_nb=sum(a) 
capture drop mean_callback_ze
capture drop mean_callback_ze_
gen mean_callback_ze_=callback_ze/reponseb_nb if CH==1
bys ZE2010: egen mean_callback_ze=max(mean_callback_ze_) 
sum mean_callback_ze 


capture drop callbackze
xtile callbackze=mean_callback_ze,nq(5)
capture drop pct_c
capture drop percent_c
pctile pct_c = mean_callback_ze, nq(5) genp(percent_c)


* Timing of applications employed and unemployed profiles
capture drop cho1
sort NumeroTest DateEnvoi
bysort NumeroTest : gen cho1=(new_profile=="CH" & _n==1)

capture drop cho1_
bysort NumeroTest : egen cho1_=max(cho1)
label var cho1_ "Application of unemployed sent first"


capture drop smallfirm
gen smallfirm=(TailleEntreprise_nb==5|TailleEntreprise_nb==10|TailleEntreprise_nb==14| ///
TailleEntreprise_nb==4|TailleEntreprise_nb==8|TailleEntreprise_nb==13| ///
TailleEntreprise_nb==3|TailleEntreprise_nb==7)




************************************************************************************
************************************************************************************
* Table 2  Number of applications per proflile
************************************************************************************
************************************************************************************
tab new_profile Poste

		 
************************************************************************************
************************************************************************************
* Power test
************************************************************************************
************************************************************************************
power twoproportions .1, diff(.05)  alpha(0.05)  n1(2600) compute(n2)
power twoproportions .1, diff(.05)  alpha(0.01)  n1(1300) compute(n2)


		   
************************************************************************************
************************************************************************************
* Table 3  Callback Rate Descriptive Statistics
************************************************************************************
************************************************************************************


* For the responses including call for a job interview and request for additional information
*********************************************************************************************
mean reponseb,  vce(cluster NumeroTest)

mean reponseb if new_profile=="CH",  vce(cluster NumeroTest)
mean reponseb if new_profile=="Q",  vce(cluster NumeroTest)
mean reponseb if new_profile=="SQ",  vce(cluster NumeroTest)
mean reponseb if new_profile=="Q_NS",  vce(cluster NumeroTest)
mean reponseb if new_profile=="SQ_NS",  vce(cluster NumeroTest)

mean reponseb if  Poste=="AC",  vce(cluster NumeroTest)
mean reponseb if Poste=="JA",  vce(cluster NumeroTest)
mean reponseb if new_profile=="CH" & Poste=="JA",  vce(cluster NumeroTest)
mean reponseb if new_profile=="Q" & Poste=="JA",  vce(cluster NumeroTest)
mean reponseb if new_profile=="SQ" & Poste=="JA",  vce(cluster NumeroTest)
mean reponseb if new_profile=="Q_NS" & Poste=="JA",  vce(cluster NumeroTest)
mean reponseb if new_profile=="SQ_NS" & Poste=="JA",  vce(cluster NumeroTest)

mean reponseb if new_profile=="CH" & Poste=="AC",  vce(cluster NumeroTest)
mean reponseb if new_profile=="Q" & Poste=="AC",  vce(cluster NumeroTest)
mean reponseb if new_profile=="SQ" & Poste=="AC",  vce(cluster NumeroTest)
mean reponseb if new_profile=="Q_NS" & Poste=="AC",  vce(cluster NumeroTest)
mean reponseb if new_profile=="SQ_NS" & Poste=="AC",  vce(cluster NumeroTest)


* For the responses including call for a job interview only
*********************************************************************************************
mean reponsea,  vce(cluster NumeroTest)

mean reponsea if new_profile=="CH",  vce(cluster NumeroTest)
mean reponsea if new_profile=="Q",  vce(cluster NumeroTest)
mean reponsea if new_profile=="SQ",  vce(cluster NumeroTest)
mean reponsea if new_profile=="Q_NS",  vce(cluster NumeroTest)
mean reponsea if new_profile=="SQ_NS",  vce(cluster NumeroTest)
mean reponsea if  Poste=="AC",  vce(cluster NumeroTest)
mean reponsea if Poste=="JA",  vce(cluster NumeroTest)

mean reponsea if new_profile=="CH" & Poste=="JA",  vce(cluster NumeroTest)
mean reponsea if new_profile=="Q" & Poste=="JA",  vce(cluster NumeroTest)
mean reponsea if new_profile=="SQ" & Poste=="JA",  vce(cluster NumeroTest)
mean reponsea if new_profile=="Q_NS" & Poste=="JA",  vce(cluster NumeroTest)
mean reponsea if new_profile=="SQ_NS" & Poste=="JA",  vce(cluster NumeroTest)



mean reponsea if new_profile=="CH" & Poste=="AC",  vce(cluster NumeroTest)
mean reponsea if new_profile=="Q" & Poste=="AC",  vce(cluster NumeroTest)
mean reponsea if new_profile=="SQ" & Poste=="AC",  vce(cluster NumeroTest)
mean reponsea if new_profile=="Q_NS" & Poste=="AC",  vce(cluster NumeroTest)
mean reponsea if new_profile=="SQ_NS" & Poste=="AC",  vce(cluster NumeroTest)
 
 
***********************************************************************************
************************************************************************************
* Table 4: The Effect of Individual Pathway on Probability of Callback
************************************************************************************
************************************************************************************



* For the responses including call for a job interview and request for additional information
*********************************************************************************************

qui reg reponseb Q Q_NS SQ SQ_NS  ,  robust cluster(NumeroTest) 
estimate store All
qui reg reponseb Q Q_NS SQ SQ_NS i.mois ,  robust cluster(NumeroTest) 
estimate store All_mois
qui areg reponseb Q Q_NS SQ SQ_NS   i.mois   , absorb(departement) robust cluster(NumeroTest)
estimate store All_mois_dep
qui areg reponseb Q Q_NS SQ SQ_NS  i.mois   if Poste=="JA", absorb(departement)  robust cluster(NumeroTest)
estimate store Gard_mois_dep
qui areg reponseb Q Q_NS SQ SQ_NS i.mois  if Poste=="AC", absorb(departement)  robust cluster(NumeroTest)
estimate store Recept_mois_dep

                      

estout All All_mois All_mois_dep Gard_mois_dep Recept_mois_dep, starlevels(* 0.10 ** 0.05  *** 0.01)  keep(Q Q_NS SQ SQ_NS _cons)   ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)

 * Test avec le taux de chômage ne plus : cela ne change pas les résultats


qui reg reponseb Q Q_NS SQ SQ_NS  uze_av_Q1_Q2_16,  robust cluster(NumeroTest) 
estimate store All
qui reg reponseb Q Q_NS SQ SQ_NS i.mois uze_av_Q1_Q2_16,  robust cluster(NumeroTest) 
estimate store All_mois
qui areg reponseb Q Q_NS SQ SQ_NS   i.mois   uze_av_Q1_Q2_16, absorb(departement) robust cluster(NumeroTest)
estimate store All_mois_dep
qui areg reponseb Q Q_NS SQ SQ_NS  i.mois  uze_av_Q1_Q2_16 if Poste=="JA", absorb(departement)  robust cluster(NumeroTest)
estimate store Gard_mois_dep
qui areg reponseb Q Q_NS SQ SQ_NS i.mois uze_av_Q1_Q2_16 if Poste=="AC", absorb(departement)  robust cluster(NumeroTest)
estimate store Recept_mois_dep                 

estout AC1 AC2 AC3 Gardener Receptionist, starlevels(* 0.10 ** 0.05  *** 0.01)   indicate("mois=*.mois" "departement=*._departement") ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)

/*

----------------------------------------------------------------------------------------------------
                              AC1             AC2             AC3        Gardener    Receptionist   
                             b/se            b/se            b/se            b/se            b/se   
----------------------------------------------------------------------------------------------------
Subsidized,                 0.046***        0.044***        0.040***        0.044**         0.036** 
Certified Skills          (0.012)         (0.012)         (0.012)         (0.020)         (0.014)   
Non-subsidized,             0.026**         0.026**         0.029**         0.024           0.029*  
Certified Skills          (0.012)         (0.012)         (0.013)         (0.022)         (0.017)   
Subsidized, No              0.000           0.001           0.003           0.006          -0.001   
Certified Skills          (0.010)         (0.010)         (0.010)         (0.018)         (0.011)   
Non-subsidized, No          0.009           0.011           0.013           0.028          -0.003   
Certified Skills          (0.013)         (0.013)         (0.013)         (0.024)         (0.012)   
uze_av_Q1_Q2_16            -0.007***       -0.006          -0.006          -0.004          -0.004   
                          (0.002)         (0.004)         (0.005)         (0.008)         (0.007)   
_cons                       0.134***        0.174***        0.124***        0.140*          0.072   
                          (0.027)         (0.062)         (0.045)         (0.077)         (0.063)   
mois                          Yes             Yes             Yes             Yes             Yes   
departement                    No             Yes             Yes             Yes             Yes   
----------------------------------------------------------------------------------------------------
r2                          0.009           0.043           0.151           0.227           0.213   
N                        5144.000        5144.000        5144.000        2556.000        2588.000   
----------------------------------------------------------------------------------------------------


*/



*** Autre option de correction : on fait trois colonnes (i) mois (ii) mois + departement et (iii) mois X departement puis dans le reste du papier on choisit mois + departement


qui reg reponseb Q Q_NS SQ SQ_NS  i.mois ,  robust cluster(NumeroTest) 
estimate store All_mois
qui areg reponseb Q Q_NS SQ SQ_NS   i.mois   , absorb(departement) robust cluster(NumeroTest)
estimate store All_mois_dep
qui areg reponseb Q Q_NS SQ SQ_NS   i.mois   , absorb(mois_dep) robust cluster(NumeroTest)
estimate store All_moisXdep
qui areg reponseb Q Q_NS SQ SQ_NS  i.mois   if Poste=="JA", absorb(departement)  robust cluster(NumeroTest)
estimate store Gard_mois_dep
qui areg reponseb Q Q_NS SQ SQ_NS i.mois  if Poste=="AC", absorb(departement)  robust cluster(NumeroTest)
estimate store Recept_mois_dep

                      

estout All_mois All_mois_dep All_moisXdep Gard_mois_dep Recept_mois_dep, starlevels(* 0.10 ** 0.05  *** 0.01)  keep(Q Q_NS SQ SQ_NS _cons)   ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)

/*
----------------------------------------------------------------------------------------------------
                         All_mois    All_mois_dep    All_moisXdep    Gard_mois_~p    Recept_moi~p   
                             b/se            b/se            b/se            b/se            b/se   
----------------------------------------------------------------------------------------------------
Subsidized,                 0.045***        0.043***        0.040***        0.046**         0.034** 
Certified Skills          (0.012)         (0.012)         (0.012)         (0.018)         (0.013)   
Non-subsidized,             0.024**         0.024**         0.026**         0.021           0.028*  
Certified Skills          (0.012)         (0.012)         (0.013)         (0.019)         (0.015)   
Subsidized, No             -0.001          -0.001           0.001          -0.001           0.004   
Certified Skills          (0.009)         (0.009)         (0.010)         (0.016)         (0.010)   
Non-subsidized, No          0.006           0.008           0.007           0.020          -0.006   
Certified Skills          (0.012)         (0.012)         (0.013)         (0.021)         (0.012)   
_cons                       0.060***        0.060***        0.071***        0.105***        0.020*  
                          (0.012)         (0.012)         (0.005)         (0.021)         (0.011)   
----------------------------------------------------------------------------------------------------
r2                          0.007           0.040           0.142           0.071           0.040   
N                        5388.000        5388.000        5388.000        2720.000        2668.000   
----------------------------------------------------------------------------------------------------

*/

***********************************************************************************
************************************************************************************
* Table 5: The Effect of Skill Certification on Probability of Callback
************************************************************************************
************************************************************************************


*** Table 5 du papier soumis avec seulement des dummies mois et departement 
qui areg reponseb certified non_certified i.mois  ,absorb(departement)  robust cluster(NumeroTest)
estimate store AC3
qui areg reponseb certified non_certified i.mois    if Poste=="JA",absorb(departement) robust cluster(NumeroTest)
estimate store Gardener
qui areg reponseb certified non_certified i.mois   if Poste=="AC",absorb(departement)  robust cluster(NumeroTest)
estimate store Receptionist

estout AC3 Gardener Receptionist, starlevels(* 0.10 ** 0.05  *** 0.01)   keep (certified non_certified _cons) ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)

 
* With controls


qui reg reponseb Q Q_NS SQ SQ_NS i.mois i.Romain , robust cluster(NumeroTest) 
estimate store AC1
qui reg reponseb Q Q_NS SQ SQ_NS  i.mois i._departement  i.Romain,robust cluster(NumeroTest)
estimate store AC2
qui areg reponseb Q Q_NS SQ SQ_NS i.mois  i._departement  i.Romain,absorb(mois_dep)  robust cluster(NumeroTest)
estimate store AC3
qui areg reponseb Q Q_NS SQ SQ_NS i.mois  i._departement i.Romain if Poste=="JA",absorb(mois_dep)  robust cluster(NumeroTest)
estimate store Gardener
qui areg reponseb Q Q_NS SQ SQ_NS i.mois  i._departement i.Romain if Poste=="AC",absorb(mois_dep)  robust cluster(NumeroTest)
estimate store Receptionist

estout AC1 AC2 AC3 Gardener Receptionist, starlevels(* 0.10 ** 0.05  *** 0.01)   indicate("mois=*.mois" "departement=*._departement") ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)
 
             
************************************************************************************
************************************************************************************
* Table 6: The Effect of Skill Certification on Probability of Callback by Quintile of Unemployment Rate of the Commuting Zone where the Job was Posted
************************************************************************************
************************************************************************************

preserve
*drop if CH==1
quietly areg reponseb _form  i.mois , absorb(departement)  robust cluster(NumeroTest)
esti store c0,title(All)
quietly areg reponseb _form  i.mois  if uze==1 , absorb(departement)  robust cluster(NumeroTest)
esti store c1,title(Quintile 1)
quietly areg reponseb _form  i.mois   if uze==2 , absorb(departement)  robust cluster(NumeroTest)
esti store c2,title(Quintile 2)
quietly areg reponseb _form  i.mois   if uze==3 , absorb(departement)  robust cluster(NumeroTest)
esti store c3,title(Quintile 3)
quietly areg reponseb _form  i.mois   if uze==4 , absorb(departement)  robust cluster(NumeroTest)
esti store c4,title(Quintile 4)
quietly areg reponseb _form  i.mois  if uze==5 , absorb(departement)  robust cluster(NumeroTest)
esti store c5,title(Quintile 5)
estout c0 c1 c2 c3 c4 c5, keep(_form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01)  label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)
restore


************************************************************************************************************************************************************************************
 ************************************************************************************************************************************************************************************

******APPENDIX

************************************************************************************************************************************************************************************
 ************************************************************************************************************************************************************************************

		 
************************************************************************************
************************************************************************************
* Table A1 Randomization tests
************************************************************************************
************************************************************************************
* names and surnames are randomized between the control and the treatment groups by construction as we sent one application of the control group with each application of the treatment group, 
* each application having a different name and the name of the first application sent is randomized. 

* variables décrivant le traitement (caractéristique du CV dont on test l'impact)
capture drop control_unempl
gen control_unempl=(new_profile=="CH")
capture drop control_certif
gen control_certif=(new_profile=="Q"|new_profile=="Q_NS") if new_profile!="CH"
capture drop control_subsid
gen control_subsid=(new_profile=="Q"|new_profile=="SQ") if new_profile!="CH"
capture drop control_firstname
gen control_firstname=(prenom=="Romain")


* variables décrivant le traitement Comme dans le papier (caractéristique du CV dont on test l'impact)
capture drop control_Q
gen control_Q=(new_profile=="Q") if (new_profile=="Q"|new_profile=="CH")
capture drop control_NQ
gen control_SQ=(new_profile=="SQ") if (new_profile=="SQ"|new_profile=="CH")
capture drop control_Q_NS
gen control_Q_NS=(new_profile=="Q_NS") if (new_profile=="Q_NS"|new_profile=="CH")
capture drop control_SQ_NS
gen control_SQ_NS=(new_profile=="SQ_NS") if (new_profile=="SQ_NS"|new_profile=="CH")

* variable décrivant les offres d'emploi (caréctristique des offres auquelles on a candidaté)
         
capture drop caract_cdi
gen caract_cdi=(Contrat=="CDI")
capture drop caract_poste
gen caract_poste=(Poste=="JA")
capture drop caract_forprofit
gen caract_forprofit=(Secteur=="PRM"|Secteur=="PUM")
capture drop caract_public
gen caract_public=(Secteur=="PUM"|Secteur=="PUNM")
capture drop caract_private
gen caract_private=(Secteur=="PRM"|Secteur=="PRNM")
capture drop caract_smallfirm
gen caract_smallfirm=(smallfirm==1)
capture drop caract_bigfirm
gen caract_bigfirm=(smallfirm==0)
capture drop caract_SexRecruteur
gen caract_SexRecruteur=(SexRecruteur=="M")
capture drop caract_Tertiary
gen caract_Tertiary=(Secteur2=="D"|Secteur2=="E"|Secteur2=="F"|Secteur2=="G"|Secteur2=="H")
capture drop caract_Secondary
gen caract_Secondary=(Secteur2=="B"|Secteur2=="C")
capture drop caract_Primary
gen caract_Primary=(Secteur2=="A")

* tests: on veut vérifier que les cactéristiques des emplois sont identiques selon les différents traitements

* avec le premier type de traitement
* CDI: all good
ttest caract_cdi,by(control_unempl)
ttest caract_cdi,by(control_certif)
ttest caract_cdi,by(control_subsid)
ttest caract_cdi,by(control_firstname)
* Poste: all good
ttest caract_poste,by(control_unempl)
ttest caract_poste,by(control_certif)
ttest caract_poste,by(control_subsid)
ttest caract_poste,by(control_firstname)
* For Profit: all good
ttest caract_forprofit,by(control_unempl)
ttest caract_forprofit,by(control_certif)
ttest caract_forprofit,by(control_subsid)
ttest caract_forprofit,by(control_firstname)
* Public: all good
ttest caract_public,by(control_unempl)
ttest caract_public,by(control_certif)
ttest caract_public,by(control_subsid)
ttest caract_public,by(control_firstname)
* Private: all good
ttest caract_private,by(control_unempl)
ttest caract_private,by(control_certif)c
ttest caract_private,by(control_subsid)
ttest caract_private,by(control_firstname)
* Small firms: all good
ttest caract_smallfirm,by(control_unempl)
ttest caract_smallfirm,by(control_certif)
ttest caract_smallfirm,by(control_subsid)
ttest caract_smallfirm,by(control_firstname)
* Big firms: all good
ttest caract_bigfirm,by(control_unempl)
ttest caract_bigfirm,by(control_certif)
ttest caract_bigfirm,by(control_subsid)
ttest caract_bigfirm,by(control_firstname)
* Sex recuiter: all good
ttest caract_SexRecruteur,by(control_unempl)
ttest caract_SexRecruteur,by(control_certif)
ttest caract_SexRecruteur,by(control_subsid)
ttest caract_SexRecruteur,by(control_firstname)
* Sector Primary: all good
ttest caract_Primary,by(control_unempl)
ttest caract_Primary,by(control_certif)
ttest caract_Primary,by(control_subsid)
ttest caract_Primary,by(control_firstname)
* Sector Secondary: all good
ttest caract_Secondary,by(control_unempl)
ttest caract_Secondary,by(control_certif)
ttest caract_Secondary,by(control_subsid)
ttest caract_Secondary,by(control_firstname)
* Sector Tertiary: all good
ttest caract_Tertiary,by(control_unempl)
ttest caract_Tertiary,by(control_certif)
ttest caract_Tertiary,by(control_subsid)
ttest caract_Tertiary,by(control_firstname)

* RANDOMIZATION TEST in paper 
* avec le type de traitement présenté comme dans le papier
* CDI: all good
ttest caract_cdi,by(control_Q)
ttest caract_cdi,by(control_SQ)
ttest caract_cdi,by(control_Q_NS)
ttest caract_cdi,by(control_SQ_NS)
* Poste: all good
ttest caract_poste,by(control_Q)
ttest caract_poste,by(control_SQ)
ttest caract_poste,by(control_Q_NS)
ttest caract_poste,by(control_SQ_NS)
* For Profit: all good
ttest caract_forprofit,by(control_Q)
ttest caract_forprofit,by(control_SQ)
ttest caract_forprofit,by(control_Q_NS)
ttest caract_forprofit,by(control_SQ_NS)
* Public: all good
ttest caract_public,by(control_Q)
ttest caract_public,by(control_SQ)
ttest caract_public,by(control_Q_NS)
ttest caract_public,by(control_SQ_NS)
* Private: Differences à 5% pour SQ et SQ_NS mais pas énormes
ttest caract_private,by(control_Q)
ttest caract_private,by(control_SQ)
ttest caract_private,by(control_Q_NS)
ttest caract_private,by(control_SQ_NS)
* Small firms: all good
ttest caract_smallfirm,by(control_Q)
ttest caract_smallfirm,by(control_SQ)
ttest caract_smallfirm,by(control_Q_NS)
ttest caract_smallfirm,by(control_SQ_NS)
* Big firms: all good
ttest caract_bigfirm,by(control_Q)
ttest caract_bigfirm,by(control_SQ)
ttest caract_bigfirm,by(control_Q_NS)
ttest caract_bigfirm,by(control_SQ_NS)
* Sex recuiter: all good
ttest caract_SexRecruteur,by(control_Q)
ttest caract_SexRecruteur,by(control_SQ)
ttest caract_SexRecruteur,by(control_Q_NS)
ttest caract_SexRecruteur,by(control_SQ_NS)
* Sector Primary: all good
ttest caract_Primary,by(control_Q)
ttest caract_Primary,by(control_SQ)
ttest caract_Primary,by(control_Q_NS)
ttest caract_Primary,by(control_SQ_NS)
* Sector Secondary: all good
ttest caract_Secondary,by(control_Q)
ttest caract_Secondary,by(control_SQ)
ttest caract_Secondary,by(control_Q_NS)
ttest caract_Secondary,by(control_SQ_NS)
* Sector Tertiary: all good
ttest caract_Tertiary,by(control_Q)
ttest caract_Tertiary,by(control_SQ)
ttest caract_Tertiary,by(control_Q_NS)
ttest caract_Tertiary,by(control_SQ_NS)




 
************************************************************************************
************************************************************************************
* Table A.2: Effect of temp jobs for unemployed profiles
************************************************************************************
************************************************************************************
 
gen tri = (CH_0CDD==1 | CH_1CDD==1 | CH_2CDD==1 | CH_3CDD==1)
qui reg reponseb  CH_1CDD CH_2CDD CH_3CDD i.mois if tri==1 , robust cluster(NumeroTest) 
estimate store AC1
qui reg reponseb CH_1CDD CH_2CDD CH_3CDD  i.mois i._departement  if tri==1, robust cluster(NumeroTest)
estimate store AC2
qui areg reponseb CH_1CDD CH_2CDD CH_3CDD i.mois  i._departement if tri==1 ,absorb(mois_dep)  robust cluster(NumeroTest)
estimate store AC3

estout AC1 AC2 AC3 , starlevels(* 0.10 ** 0.05  *** 0.01)   indicate("mois=*.mois" "departement=*._departement") ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)


qui reg reponseb  CH_1CDD CH_2CDD CH_3CDD  if tri==1 , robust 
estimate store AC1
qui reg reponseb CH_1CDD CH_2CDD CH_3CDD  i.mois  if tri==1,  robust 
estimate store AC2
qui areg reponseb CH_1CDD CH_2CDD CH_3CDD i.mois   if tri==1 ,  absorb(departement)  robust 
estimate store AC3

estout AC1 AC2 AC3 , starlevels(* 0.10 ** 0.05  *** 0.01)   ///
keep(CH_1CDD CH_2CDD CH_3CDD _cons) label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)


 
************************************************************************************
************************************************************************************
* Table A.3:The Effect of Having Been Employed in Public Jobs vs. Private Jobs on Probability of Callback
************************************************************************************
************************************************************************************
 
gen employe_marchand=(profile=="M_SQ_NS"|profile=="M_Q_NS"|profile=="M_SQ"|profile=="M_Q")
capture drop employe_nonmarchand
gen employe_nonmarchand=(profile=="NM_SQ_NS"|profile=="NM_Q_NS"|profile=="NM_SQ"|profile=="NM_Q")

**Panel A (tous les secteurs)
* Column 1
qui areg reponseb employe_marchand employe_nonmarchand i.mois,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois ,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois if Poste=="JA",absorb(departement) robust cluster(NumeroTest)   
esti store c3
* Column 4
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois if Poste=="AC" ,absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(employe_marchand employe_nonmarchand _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel B (secteur marchand)
* Column 1
qui areg reponseb employe_marchand employe_nonmarchand i.mois if marchand,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois if marchand ,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois if Poste=="JA" & marchand,absorb(departement) robust cluster(NumeroTest)   
esti store c3
* Column 4
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois if Poste=="AC" & marchand ,absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(employe_marchand employe_nonmarchand _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel C (secteur non-marchand)
* Column 1
qui areg reponseb employe_marchand employe_nonmarchand i.mois if marchand==0,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois if marchand==0 ,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois if Poste=="JA" & marchand==0,absorb(departement) robust cluster(NumeroTest)   
esti store c3
* Column 4
qui areg reponseb employe_marchand employe_nonmarchand _form i.mois if Poste=="AC" & marchand==0 ,absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(employe_marchand employe_nonmarchand _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)


 
 
************************************************************************************
************************************************************************************
* Table A4: The Effect of Having Been Employed in Subsidized Jobs vs. Non-Subsidized Jobs on Probability of Callback
************************************************************************************
************************************************************************************
 
gen employe_sanssubv=(profile=="M_SQ_NS"|profile=="NM_SQ_NS"|profile=="M_Q_NS"|profile=="NM_Q_NS")
capture drop employe_subv
gen employe_subv=(profile=="M_SQ"|profile=="NM_SQ"|profile=="M_Q"|profile=="NM_Q")
capture drop employe_sansformsanssubv

**Panel A (tous les secteurs)
* Column 1
qui areg reponseb employe_sanssubv employe_subv i.mois,absorb(departement) robust cluster(NumeroTest)
esti store c1
* Column 2
qui areg reponseb employe_sanssubv employe_subv _form i.mois ,absorb(departement) robust cluster(NumeroTest)
esti store c2
* Column 3
qui areg reponseb employe_sanssubv employe_subv _form i.mois if Poste=="JA",absorb(departement) robust cluster(NumeroTest)
esti store c3
* Column 4
qui areg reponseb employe_sanssubv employe_subv _form i.mois if Poste=="AC",absorb(departement) robust cluster(NumeroTest)
esti store c4
estout c1 c2 c3 c4, keep(employe_sanssubv employe_subv _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel B (secteur marchand)
* Column 1
qui areg reponseb employe_sanssubv employe_subv i.mois if marchand ,absorb(departement) robust cluster(NumeroTest)
esti store c1
* Column 2
qui areg reponseb employe_sanssubv employe_subv _form i.mois if marchand,absorb(departement) robust cluster(NumeroTest)
esti store c2
* Column 3
qui areg reponseb employe_sanssubv employe_subv _form i.mois if Poste=="JA" & marchand,absorb(departement) robust cluster(NumeroTest)
esti store c3
* Column 4
qui areg reponseb employe_sanssubv employe_subv _form i.mois if Poste=="AC" & marchand ,absorb(departement) robust cluster(NumeroTest)
esti store c4
estout c1 c2 c3 c4, keep(employe_sanssubv employe_subv _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel C (secteur non-marchand)
* Column 1
qui areg reponseb employe_sanssubv employe_subv i.mois if marchand==0 ,absorb(departement) robust cluster(NumeroTest)
esti store c1
* Column 2
qui areg reponseb employe_sanssubv employe_subv _form i.mois if marchand==0,absorb(departement) robust cluster(NumeroTest)
esti store c2
* Column 3
qui areg reponseb employe_sanssubv employe_subv _form i.mois if Poste=="JA" & marchand==0,absorb(departement) robust cluster(NumeroTest)
esti store c3
* Column 4
qui areg reponseb employe_sanssubv employe_subv _form i.mois if Poste=="AC" & marchand==0 ,absorb(departement) robust cluster(NumeroTest)
esti store c4
estout c1 c2 c3 c4, keep(employe_sanssubv employe_subv _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)



 
 ************************************************************************************
************************************************************************************
*Table A.6: The Effect of Individual Pathway on Probability of Callback (=INTERVIEW ONLY)
************************************************************************************
************************************************************************************
 
qui reg reponsea Q Q_NS SQ SQ_NS i.mois  , robust cluster(NumeroTest) 
estimate store AC1
qui reg reponsea Q Q_NS SQ SQ_NS  i.mois i._departement  ,robust cluster(NumeroTest)
estimate store AC2
qui areg reponsea Q Q_NS SQ SQ_NS i.mois  i._departement  ,absorb(mois_dep)  robust cluster(NumeroTest)
estimate store AC3
qui areg reponsea Q Q_NS SQ SQ_NS i.mois  i._departement  if Poste=="JA",absorb(mois_dep)  robust cluster(NumeroTest)
estimate store Gardener
qui areg reponsea Q Q_NS SQ SQ_NS i.mois  i._departement  if Poste=="AC",absorb(mois_dep)  robust cluster(NumeroTest)
estimate store Receptionist
estout AC1 AC2 AC3 Gardener Receptionist, starlevels(* 0.10 ** 0.05  *** 0.01)   indicate("mois=*.mois" "departement=*._departement") ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)


***WITH DEPARTMENT AND MONTH FE SEPARATELY	

qui reg reponsea Q Q_NS SQ SQ_NS  ,  robust cluster(NumeroTest) 
estimate store All
qui reg reponsea Q Q_NS SQ SQ_NS i.mois ,  robust cluster(NumeroTest) 
estimate store All_mois
qui areg reponsea Q Q_NS SQ SQ_NS   i.mois   , absorb(departement) robust cluster(NumeroTest)
estimate store All_mois_dep
qui areg reponsea Q Q_NS SQ SQ_NS  i.mois   if Poste=="JA", absorb(departement)  robust cluster(NumeroTest)
estimate store Gard_mois_dep
qui areg reponsea Q Q_NS SQ SQ_NS i.mois  if Poste=="AC", absorb(departement)  robust cluster(NumeroTest)
estimate store Recept_mois_dep
estout All All_mois All_mois_dep Gard_mois_dep Recept_mois_dep, starlevels(* 0.10 ** 0.05  *** 0.01)  keep(Q Q_NS SQ SQ_NS _cons)   ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)


 
************************************************************************************
************************************************************************************
* Table A.7: The Effect of Skill Certification on Probability of Callback
************************************************************************************
************************************************************************************
  
qui areg reponsea  non_certified certified i.mois  ,absorb(departement)  robust cluster(NumeroTest)
estimate store AC3
qui areg reponsea  non_certified certified i.mois    if Poste=="JA",absorb(departement) robust cluster(NumeroTest)
estimate store Gardener
qui areg reponsea  non_certified certified i.mois   if Poste=="AC",absorb(departement)  robust cluster(NumeroTest)
estimate store Receptionist

estout AC3 Gardener Receptionist, starlevels(* 0.10 ** 0.05  *** 0.01)   keep (certified non_certified _cons) ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)


************************************************************************************
************************************************************************************
* Table A.8:The Effect of Having Been Employed in Subsidized Jobs vs. Public Jobs on Proba-bility of Callback
************************************************************************************
************************************************************************************
 
**Panel A (tous les secteurs)
* Column 1
qui areg reponsea employe_sanssubv employe_subv i.mois,absorb(departement) robust cluster(NumeroTest)
esti store c1
* Column 2
qui areg reponsea employe_sanssubv employe_subv _form i.mois ,absorb(departement) robust cluster(NumeroTest)
esti store c2
* Column 3
qui areg reponsea employe_sanssubv employe_subv _form i.mois if Poste=="JA",absorb(departement) robust cluster(NumeroTest)
esti store c3
* Column 4
qui areg reponsea employe_sanssubv employe_subv _form i.mois if Poste=="AC",absorb(departement) robust cluster(NumeroTest)
esti store c4
estout c1 c2 c3 c4, keep(employe_sanssubv employe_subv _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel B (secteur marchand)
* Column 1
qui areg reponsea employe_sanssubv employe_subv i.mois if marchand ,absorb(departement) robust cluster(NumeroTest)
esti store c1
* Column 2
qui areg reponsea employe_sanssubv employe_subv _form i.mois if marchand,absorb(departement) robust cluster(NumeroTest)
esti store c2
* Column 3
qui areg reponsea employe_sanssubv employe_subv _form i.mois if Poste=="JA" & marchand,absorb(departement) robust cluster(NumeroTest)
esti store c3
* Column 4
qui areg reponsea employe_sanssubv employe_subv _form i.mois if Poste=="AC" & marchand ,absorb(departement) robust cluster(NumeroTest)
esti store c4
estout c1 c2 c3 c4, keep(employe_sanssubv employe_subv _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel C (secteur non-marchand)
* Column 1
qui areg reponsea employe_sanssubv employe_subv i.mois if marchand==0 ,absorb(departement) robust cluster(NumeroTest)
esti store c1
* Column 2
qui areg reponsea employe_sanssubv employe_subv _form i.mois if marchand==0,absorb(departement) robust cluster(NumeroTest)
esti store c2
* Column 3
qui areg reponsea employe_sanssubv employe_subv _form i.mois if Poste=="JA" & marchand==0,absorb(departement) robust cluster(NumeroTest)
esti store c3
* Column 4
qui areg reponsea employe_sanssubv employe_subv _form i.mois if Poste=="AC" & marchand==0 ,absorb(departement) robust cluster(NumeroTest)
esti store c4
estout c1 c2 c3 c4, keep(employe_sanssubv employe_subv _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

 ************************************************************************************
************************************************************************************
* Table A.9:The Effect of Having Been Employed in Public Jobs vs. Private Jobs on Probability of Callback
************************************************************************************
************************************************************************************
 
**Panel A (All sectors)
* Column 1
qui areg reponsea employe_marchand employe_nonmarchand i.mois,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois ,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois if Poste=="JA",absorb(departement) robust cluster(NumeroTest)   
esti store c3
* Column 4
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois if Poste=="AC" ,absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(employe_marchand employe_nonmarchand _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel B (Private sector)
* Column 1
qui areg reponsea employe_marchand employe_nonmarchand i.mois if marchand,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois if marchand ,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois if Poste=="JA" & marchand,absorb(departement) robust cluster(NumeroTest)   
esti store c3
* Column 4
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois if Poste=="AC" & marchand ,absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(employe_marchand employe_nonmarchand _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel C (Public sector)
* Column 1
qui areg reponsea employe_marchand employe_nonmarchand i.mois if marchand==0,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois if marchand==0 ,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois if Poste=="JA" & marchand==0,absorb(departement) robust cluster(NumeroTest)   
esti store c3
* Column 4
qui areg reponsea employe_marchand employe_nonmarchand _form i.mois if Poste=="AC" & marchand==0 ,absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(employe_marchand employe_nonmarchand _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)


 
 
************************************************************************************
************************************************************************************
* Table A10: The Effect of Subsidized and Public Job Experience on Probability of Callback
************************************************************************************
************************************************************************************
 
gen emp_nm_sub=employe_nonmarchand*employe_subv
gen emp_m_sub=employe_marchand*employe_subv
gen emp_m_nsub=employe_marchand*employe_sanssubv
gen emp_nm_nsub=employe_nonmarchand*employe_sanssubv

**Panel A (All sectors)
* Column 1
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub i.mois,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois if Poste=="JA",absorb(departement) robust cluster(NumeroTest) 
esti store c3
* Column 4
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois if Poste=="AC",absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form _cons)  varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel B (Private sector)
* Column 1
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub i.mois if marchand ,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois if marchand ,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois if Poste=="JA" & marchand ,absorb(departement) robust cluster(NumeroTest) 
esti store c3
* Column 4
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois if Poste=="AC" & marchand,absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)

**Panel C (Public sector)
* Column 1
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub i.mois if marchand==0 ,absorb(departement) robust cluster(NumeroTest) 
esti store c1
* Column 2
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois if marchand==0,absorb(departement) robust cluster(NumeroTest) 
esti store c2
* Column 3
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois if Poste=="JA" & marchand==0,absorb(departement) robust cluster(NumeroTest) 
esti store c3
* Column 4
qui areg reponsea emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form i.mois if Poste=="AC" & marchand==0,absorb(departement) robust cluster(NumeroTest) 
esti store c4

estout c1 c2 c3 c4, keep(emp_m_nsub emp_nm_nsub emp_nm_sub emp_m_sub  _form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)


 
************************************************************************************
************************************************************************************
* Table A.11: The Effect of Certified Skills on Probability of Callback by Quintile of Unemployment Rate of the Commuting Zone where the Job was Posted
************************************************************************************
************************************************************************************
 
qui reg reponsea    _form##c.uze_av_Q1_Q2_16 i.mois   , robust cluster(NumeroTest)
estimate store AC3

estout  AC3 , starlevels(* 0.10 ** 0.05  *** 0.01)   indicate("mois=*.mois" ) ///
/*keep(Q Q_NS SQ  _cons)*/ label wrap cells(b(star fmt(3)) se(par fmt(3)) )   stats(r2 N)

** soumis JHR
preserve
*drop if CH==1
quietly areg reponsea _form  i.mois , absorb(departement)  robust cluster(NumeroTest)
esti store c0,title(All)
quietly areg reponsea _form  i.mois  if uze==1 , absorb(departement)  robust cluster(NumeroTest)
esti store c1,title(Quintile 1)
quietly areg reponsea _form  i.mois   if uze==2 , absorb(departement)  robust cluster(NumeroTest)
esti store c2,title(Quintile 2)
quietly areg reponsea _form  i.mois   if uze==3 , absorb(departement)  robust cluster(NumeroTest)
esti store c3,title(Quintile 3)
quietly areg reponsea _form  i.mois   if uze==4 , absorb(departement)  robust cluster(NumeroTest)
esti store c4,title(Quintile 4)
quietly areg reponsea _form  i.mois  if uze==5 , absorb(departement)  robust cluster(NumeroTest)
esti store c5,title(Quintile 5)
estout c0 c1 c2 c3 c4 c5, keep(_form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01)  label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)
restore




************************************************************************************
************************************************************************************
* Table A.12: The Effect of Certified Skills on Probability of Callback by Quintile of Callback Rate of the Commuting Zone where the Job was Posted
************************************************************************************
************************************************************************************
 
preserve

capture drop a
capture drop callback_ze_
capture drop callback_ze

bys ZE2010: egen callback_ze_=sum(reponsea) 
bys ZE2010: egen callback_ze=max(callback_ze_) 
gen a=1
capture drop reponseb_nb
bys ZE2010: egen reponseb_nb=sum(a) 
capture drop mean_callback_ze
capture drop mean_callback_ze_
gen mean_callback_ze_=callback_ze/reponseb_nb if CH==1
bys ZE2010: egen mean_callback_ze=max(mean_callback_ze_) 
sum mean_callback_ze 


capture drop callbackze
xtile callbackze=mean_callback_ze,nq(5)
capture drop pct_c
capture drop percent_c
pctile pct_c = mean_callback_ze, nq(5) genp(percent_c)

*drop if CH==1
*drop if Poste!="AC" 
quietly areg reponsea _form  i.mois if callbackze==1 | callbackze==2| callbackze==3| callbackze==4 | callbackze==5,   absorb(departement)  robust   cluster(NumeroTest)
esti store c0,title(All)
quietly areg reponsea _form  i.mois   if callbackze==1 , absorb(departement)  robust  cluster(NumeroTest)
esti store c1,title(Quintile 1)
quietly areg reponsea _form  i.mois   if callbackze==2 ,   absorb(departement)  robust cluster(NumeroTest)
esti store c2,title(Quintile 2) 
quietly areg reponsea _form  i.mois    if callbackze==3 ,   absorb(departement)  robust cluster(NumeroTest)
esti store c3,title(Quintile 3)
quietly areg reponsea _form  i.mois   if callbackze==4 ,   absorb(departement)  robust cluster(NumeroTest)
esti store c4,title(Quintile 4)
quietly areg reponsea _form  i.mois   if callbackze==5 ,   absorb(departement)  robust cluster(NumeroTest)
esti store c5,title(Quintile 5)
estout c0 c1 c2 c3 c4 c5, keep(_form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)
restore

************************************************************************************
************************************************************************************
* Table A.13: The EFFect of Individual Pathway on Probability of Callback (Marginal EFFects at the Mean); PROBIT estiamates
************************************************************************************
************************************************************************************
 
probit  reponseb Q SQ Q_NS SQ SQ_NS,robust cluster(NumeroTest)
margins,dydx(Q SQ Q_NS SQ SQ_NS) atmeans  post
est store All

*Column 2 (gardener)
probit reponseb Q SQ Q_NS SQ SQ_NS if Poste=="JA",robust cluster(NumeroTest)
margins,dydx( Q SQ Q_NS SQ SQ_NS) atmeans post
est store Gardener

*Column 3 (receptionist)
probit  reponseb Q SQ Q_NS SQ SQ_NS if Poste=="AC",robust cluster(NumeroTest)
margins,dydx( Q SQ Q_NS SQ SQ_NS) atmeans post
est store Receptionist


estout All  Gardener Receptionist  , varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)




************************************************************************************
************************************************************************************
*Table A.14  Effect of Certified Skills on Probablilty of Callback by Local Mean Callback rate 
************************************************************************************
************************************************************************************

table callbackze,co(mean mean_callback_ze)


preserve
*drop if CH==1
*drop if Poste!="AC" 
quietly areg reponseb _form  i.mois i._departement,   absorb(mois_dep)  robust 
esti store c0,title(All)
quietly areg reponseb _form  i.mois i._departement  if callbackze==1 , absorb(mois_dep)  robust 
esti store c1,title(Tercile 1)
quietly areg reponseb _form  i.mois i._departement  if callbackze==2 ,   absorb(mois_dep)  robust 
esti store c2,title(Tercile 2)
quietly areg reponseb _form  i.mois i._departement   if callbackze==3 ,   absorb(mois_dep)  robust 
esti store c3,title(Tercile 3)
quietly areg reponseb _form  i.mois i._departement  if callbackze==4 ,   absorb(mois_dep)  robust 
esti store c4,title(Tercile 4)
quietly areg reponseb _form  i.mois i._departement  if callbackze==5 ,   absorb(mois_dep)  robust 
esti store c5,title(Tercile 5)
estout c0 c1 c2 c3 c4 c5, keep(_form _cons) varwidth(10) starlevels(* 0.10 ** 0.05  *** 0.01) label wrap cells(b(star fmt(3)) se(par fmt(3)))   stats(r2 N)
restore



************************************************************************************************************************************************************************************
 ************************************************************************************************************************************************************************************
 * Computation of the share of youth on temporary jobs from the labor force survey 2013-2016 available on http://quetelet.progedo.fr/rechercher-des-donnees/
 ************************************************************************************************************************************************************************************
 ************************************************************************************************************************************************************************************
  use "$path/qc2013_2016.dta", clear
 
/* 
STAT2: tatut (Salarié, Non salarié) mis en cohérence avec la profession
= "2" correspond satut salarié
ageprm: age en annee au dernier jour de la semaine de référence
poids: pondération annuelle

statutr type contrat regroupé mis en cohérence avec la profession :
Vide 1 2 3 4 5 9
  
1: Sans objet (personnes non actives occupées) Non salariés (indépendants, employeurs) 
2: Intérimaires
3: Apprentis
4: CDD
5: CDI
9; Non renseigné


ddipl
Diplôme le plus élevé obtenu (2 chiffres, 6 postes)

vde non renseigné
1 Diplôme supérieur à baccalauréat + 2 ans
3 Baccalauréat + 2 ans
4 Baccalauréat ou brevet professionnel ou autre diplôme de ce niveau 
5 CAP, BEP ou autre diplôme de ce niveau
6 Brevet des collèges
7 Aucun diplôme ou certificat d'études primaires

*/

capture drop temp_
gen temp_=(statutr=="4" | statutr=="2")
destring ageprm, gen(age_)
gen salarie=(STAT2=="2")
capture drop poids_
gen poids_=int(EXTRI16)
capture drop dropout
gen dropout=(ddipl=="6" | ddipl=="7")

gen formation=(!missing(form))

sum temp_ if salarie==1 & age_>20 & age_<25 & statutr!="3"  & statutr!="9" & dropout==1 & sexe=="1"  & annee=="2016" [fweight=poids_]

sum formation  if salarie==1 & age_>20 & age_<25 & statutr!="3"  & statutr!="9" & dropout==1 & sexe=="1"  & annee=="2016" [fweight=poids_] 
  
 
 
 
 
 
