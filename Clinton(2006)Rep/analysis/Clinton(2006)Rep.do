
*___________________________________________________________________________________________
*	josh clinton
*	princeton university
*	code for "Representation in Congress: Constituents and Roll Calls"
*	forthcoming in Journal of Politics
*	May 2005
*___________________________________________________________________________________________


use "/Users/dsimp/GitHub/Clinton(2006)Rep/raw/dataverse_files/HOUSE.106.AKN.dta", clear

* use "C:\***PATH"""\HOUSE.106.AKN.dta", clear

recode pty 100=0 200=1 328=0

generate n = numsp + numi + numop	/*	Generate weights (% in district for each)	*/

gen pctsp = numsp/n
gen pctnsp = numnsp/n
gen pcti = numi/n
gen pctop = numop/n

sum pctsp pctnsp pcti pctop n

gen pctspid = pctsp*splc  			/*	Same party percent * same party id	*/
gen pctnspid = pctnsp*nsplc         /*	Non Same party percent * Non same party id	*/
gen pctiid = pcti*ilc				/*	Independent percent * idenependent party id	*/
gen pctopid = pctop*oplc			/*	Opposite party percent * opposite party id	*/


*	All Roll Call Votes
*	Table 1
*___________________________________________________________________________________________


* Note --- look up error in variance regression and look up weighted regression

regress x106mean lc pty [aweight=n] /*	Reg Rep idelogy on district average ideology and party = Table 1 column 1 	*/
regress x106mean pctspid pctnspid pty [aweight=n] /* Reg Rep ideology on percent independent and party = Table 1 column 2	*/
test pctspid=pctnspid /* Ftest */
eivreg x106mean pctspid pctnspid pty [aweight=n], r(pctspid .95 pctnspid .85) /* EIVReg Rep ideology on percent independent and party = Table 1 column 2	*/
test pctspid=pctnspid /* Ftest */

*	regress x106mean lc pty 	/* 	No weights 		*/
*	regress x106mean pctspid pctnspid pty
*	test pctspid=pctnspid
*	eivreg x106mean pctspid pctnspid pty, r(pctspid .95 pctnspid .85) 
*	test pctspid=pctnspid

cor pctspid pctnspid pty			/*  Corr:  */
cor pctspid pctnspid if pty==1		/* 	Corr - within Republican party --> ideology same party and non-same party less correlated 		*/
cor pctspid pctnspid if pty==0		/* 	Corr - wihtin Democratic party --> ideolgoy between same party and non-same party is higher than for GOP		*/


* Simpson Review
*___________________________________________________________________________________________
regress x106mean pctspid pctsp splc pctnspid pctnsp nsplc pty [aweight=n]  /* Reg Rep ideology on percent independent and party = Table 1 column 2	*/
eivreg x106mean pctspid pctsp splc pctnspid nsplc pty [aweight=n], r(splc .95 nsplc .85)  /* Can to EIV Reg with high R-squared	*/






*	Table 2
*___________________________________________________________________________________________

cor pctspid lc if pty==1 	/*  Corr: within GOP between same party ideolgoy and average ideogloy */
cor pctnspid lc if pty==1	/*  Corr: within GOP between non-same party ideolgoy and average ideolgy */

cor pctspid lc if pty==0 	/*  Corr: wihtin DEM same party ideology and average ideology */
cor pctnspid lc if pty==0	/*  Corr: wihtin DEM non same party ideology and average ideology the correlations are nearly identical */

sum pctspid pctnspid
sum pctspid pctnspid splc nsplc pctsp pctnsp x106mean if pty==1
sum pctspid pctnspid splc nsplc pctsp pctnsp x106mean if pty==0


regress x106mean lc pty [aweight=n] 					/* Baseline if MVT */
regress x106mean  pctspid pctnspid pty [aweight=n] 
test pctspid=pctnspid

regress x106mean lc [aweight=n] if pty==1				
eivreg x106mean lc [aweight=n] if pty==1, r(lc .86) /* Table 2 Column 1/Model 4 EIV for GOP*/

regress x106mean lc [aweight=n] if pty==0
eivreg x106mean lc [aweight=n] if pty==0, r(lc .86) /* Table 2 Column 4/Model 7 EIV for GOP*/

regress x106mean  pctspid pctnspid [aweight=n]  if pty==1 /* Table 2 Column 2/Model 5 for GOP*/
eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .32 pctnspid .63) /* Table 2 Column 3/Model 5 EIV for GOP*/
test pctspid=pctnspid

*	regress x106mean  pctspid pctnspid if pty==1	/* No weights */
*	eivreg x106mean pctspid pctnspid if pty==1, r(pctspid .32 pctnspid .63)
*	test pctspid = pctnspid

regress x106mean  pctspid pctnspid [aweight=n]  if pty==0 /* Table 2 Column 5 /Model 8  for DEM*/
/* Table 2 Column 6/Model 8 EIV for DEM*/
test pctspid=pctnspid

* Simpson Hypothesis
* Results show that 
regress x106mean pctspid pctiid pctopid
test pctspid==pctiid 
test pctspid==pctopid
test pctiid==pctopid
regress x106mean pctspid pctiid pctopid if pty==1
regress x106mean pctspid pctiid pctopid if pty==0 


sem (x106mean <- pctspid pctnspid if pty==0 )
sem (x106mean <- pctspid pctnspid if pty==1 )
sem (x106mean <- pctspid pctiid pctopid if pty==0 )
sem (x106mean <- pctspid pctiid pctopid if pty==1 )



corr  pctspid pctiid pctopid 
corr  splc ilc oplc /* interesting */

* Ct1_1 --> Same as R
regress x106mean lc pty [aweight=n]
* Ct1_2 --> same as R
regress x106mean pctspid pctnspid pty [aweight=n]
* Ct1_2b --> same as R
regress x106mean pctspid pctnspid splc nsplc pctsp pctnsp pty [aweight=n]
* Ct1_2c --> same as R
regress x106mean pctspid pctiid pctopid splc ilc oplc pctsp pcti pctop pty [aweight=n]

* Ct2_5 --> Same as R
regress x106mean pctspid pctnspid [aweight = n] if pty==1
* Ct2_8 --> Same as R
regress x106mean pctspid pctnspid [aweight = n] if pty==0

* Ct2_5b --> Same as R when you drop pctnsp as the reference
regress x106mean pctspid pctnspid splc nsplc pctsp pctnsp [aweight = n] if pty==1
* Ct2_5c --> Same as R when you drop pctop as the reference
regress x106mean pctspid pctiid pctopid splc ilc oplc pctsp pcti pctop [aweight = n] if pty==1

* Ct2_8b --> Same as R when you drop pctnsp as the reference
regress x106mean pctspid pctnspid splc nsplc pctsp pctnsp [aweight = n] if pty==0
* Ct2_8c --> Same as R when you drop pctop as the reference
regress x106mean pctspid pctiid pctopid splc ilc oplc pctsp pcti pctop [aweight = n] if pty==0




regress x106mean pctspid pctiid pctopid splc ilc oplc pctsp pcti pctop [aweight=n]






regress x106mean pctspid pctiid pctopid splc ilc oplc pctsp pcti pctop [aweight=n]
regress x106mean pctspid pctiid pctopid splc ilc oplc pctsp pcti pctop if pty ==1
regress x106mean pctspid pctiid pctopid splc ilc oplc pctsp pcti pctop if pty ==0


* Ct2_8c = lm(x106mean ~ C_pctspid + C_pctiid + C_pctopid + data$splc + data$ilc + data$oplc + data$C_pctsp + data$C_pcti + data$C_pctop, data = data, weights = C_N, R=0)
* Ct2_8c = lm(x106mean ~ C_pctspid C_pctiid C_pctopid splc ilc oplc C_pctsp + C_pcti C_pctop, data = data, weights = C_N, R=0)


*	regress x106mean  pctspid pctnspid  if pty==0	/* No weights */
*	eivreg x106mean pctspid pctnspid if pty==0, r(pctspid .7 pctnspid .64)
*	test pctspid=pctnspid

/*	Given change in signs from using EIV, examine hypothetical reliabilities	*/

*	eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .9 pctnspid .9)
*	eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .8 pctnspid .8)
*	eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .7 pctnspid .7)
*	eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .6 pctnspid .7)
*	eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .5 pctnspid .7)
*	eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .4 pctnspid .7)
*	eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .3 pctnspid .7)


*	Key Roll Call Votes
*	Table 3
*___________________________________________________________________________________________


regress x106kmean  pctspid pctnspid pty [aweight=n] 
test pctspid=pctnspid

eivreg x106kmean lc [aweight=n] if pty==1, r(lc .86) 
regress x106kmean  pctspid pctnspid [aweight=n] if pty==1 
test pctspid=pctnspid
eivreg x106kmean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .32 pctnspid .63)
test pctspid=pctnspid

/*	No Weights
*	eivreg x106kmean lc if pty==1, r(lc .86) 
*	regress x106kmean  pctspid pctnspid if pty==1 
*	test pctspid=pctnspid
*	eivreg x106kmean pctspid pctnspid if pty==1, r(pctspid .32 pctnspid .63)
*	test pctspid=pctnspid
*/

eivreg x106kmean lc [aweight=n] if pty==0, r(lc .86) 
regress x106kmean  pctspid pctnspid [aweight=n] if pty==0
test pctspid=pctnspid
eivreg x106kmean pctspid pctnspid [aweight=n] if pty==0, r(pctspid .7 pctnspid .64)
test pctspid=pctnspid

/*	No Weights
*	eivreg x106kmean lc if pty==0, r(lc .86) 
*	regress x106kmean  pctspid pctnspid if pty==0 
*	test pctspid=pctnspid
*	eivreg x106kmean pctspid pctnspid if pty==0, r(pctspid .32 pctnspid .63)
*	test pctspid=pctnspid
*/


*	Try alternative specifications
*___________________________________________________________________________________________


*regress x106mean oplc ilc splc pty [aweight=n]			/*	sub-constituency means	*/
*regress x106mean oplc ilc splc [aweight=n] if pty==1
*regress x106mean oplc ilc splc [aweight=n] if pty==0

*regress x106mean pctspid pctiid pctopid pty [aweight=n] 	/*	wgtd. sub-constituency means */
*regress x106mean pctspid pctiid pctopid  [aweight=n] if pty==1
*regress x106mean pctspid pctiid pctopid  [aweight=n] if pty==0

