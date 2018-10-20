
*___________________________________________________________________________________________
*	josh clinton
*	princeton university
*	code for "Representation in Congress: Constituents and Roll Calls"
*	forthcoming in Journal of Politics
*	May 2005
*___________________________________________________________________________________________


use "C:\***PATH"""\HOUSE.106.AKN.dta", clear

recode pty 100=0 200=1 328=0

generate n = numsp + numi + numop	/*	Generate weights (% in district for each)	*/

gen pctsp = numsp/n
gen pctnsp = numnsp/n
gen pcti = numi/n
gen pctop = numop/n

sum pctsp pctnsp pcti pctop n

gen pctspid = pctsp*splc
gen pctnspid = pctnsp*nsplc
gen pctiid = pcti*ilc
gen pctopid = pctop*oplc


*	All Roll Call Votes
*	Table 1
*___________________________________________________________________________________________


regress x106mean lc pty [aweight=n] 
regress x106mean pctspid pctnspid pty [aweight=n]
test pctspid=pctnspid
eivreg x106mean pctspid pctnspid pty [aweight=n], r(pctspid .95 pctnspid .85) 
test pctspid=pctnspid

*	regress x106mean lc pty 	/* 	No weights 		*/
*	regress x106mean pctspid pctnspid pty
*	test pctspid=pctnspid
*	eivreg x106mean pctspid pctnspid pty, r(pctspid .95 pctnspid .85) 
*	test pctspid=pctnspid

cor pctspid pctnspid pty
cor pctspid pctnspid if pty==1
cor pctspid pctnspid if pty==0


*	Table 2
*___________________________________________________________________________________________

cor pctspid lc if pty==1
cor pctnspid lc if pty==1

cor pctspid lc if pty==0
cor pctnspid lc if pty==0

sum pctspid pctnspid
sum pctspid pctnspid splc nsplc pctsp pctnsp x106mean if pty==1
sum pctspid pctnspid splc nsplc pctsp pctnsp x106mean if pty==0


regress x106mean lc pty [aweight=n] 					/* Baseline if MVT */
regress x106mean  pctspid pctnspid pty [aweight=n] 
test pctspid=pctnspid

regress x106mean lc [aweight=n] if pty==1
eivreg x106mean lc [aweight=n] if pty==1, r(lc .86) 

regress x106mean lc [aweight=n] if pty==0
eivreg x106mean lc [aweight=n] if pty==0, r(lc .86) 

regress x106mean  pctspid pctnspid [aweight=n]  if pty==1
eivreg x106mean pctspid pctnspid [aweight=n] if pty==1, r(pctspid .32 pctnspid .63)
test pctspid=pctnspid

*	regress x106mean  pctspid pctnspid if pty==1	/* No weights */
*	eivreg x106mean pctspid pctnspid if pty==1, r(pctspid .32 pctnspid .63)
*	test pctspid = pctnspid

regress x106mean  pctspid pctnspid [aweight=n]  if pty==0
eivreg x106mean pctspid pctnspid [aweight=n] if pty==0, r(pctspid .7 pctnspid .64)
test pctspid=pctnspid

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

