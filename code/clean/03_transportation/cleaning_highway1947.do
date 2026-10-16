*** Code to get the 1947 miles of lane per county ***
*** From the Baum-Snow dataset ***
*** Started December 19th ---

clear all		
set more off	
pause on

set scheme plotplain
	
* Run from code/ after opening replication.Rproj.
global data "../data/raw/transportation"
global build "../data/intermediate/.build/transportation/highway"
capture mkdir "../data/intermediate/.build"
capture mkdir "../data/intermediate/.build/transportation"
capture mkdir "$build"
set maxvar 32767

***** 1. Ready Data Set of center city highway segments ******

/*Get cc dataset ready to be merged in.  This
means stack it in a dataset that is
intst statefips cntyfips*/
insheet using "$data/highway/baum_snow/data/cc-roads.txt", clear

rename state_fips statefips
rename cnty_fips cntyfips
egen state2 = max(statefips), by(state puma1)
egen cnty2 = max(cntyfips), by(state puma1)
replace statefips = state2 if statefips==.
replace cntyfips = cnty2 if cntyfips==. 
list if cntyfips==.

**This is clearly new york city - so put in any county in NY MSA
replace cntyfips = 47 if cntyfips==.

**THE CODE BELOW IS FROM ROADS-CONSTRUCTx.do
rename route_num hwyname
rename route_2 altname1
rename route_3 altname2

*generate interstate numbers
replace altname1 = . if altname1 == 0
replace altname2 = . if altname2 == 0
gen str3 intst = substr(hwyname,2,3)
rename altname1 intst2
rename altname2 intst3

*generate real versions of interstate numbers
gen rintst=real(intst)
gen rintst2=intst2
gen rintst3=intst3
*There are only Es and Ws for the primary name
replace rintst=real(substr(intst,1,2)) if rintst==.
replace intst=ltrim(rtrim(intst))

**order multiple-name roads to put the 3-digit versions last
gen rintstx = 0
gen str3 intstx = "0"
replace rintstx = rintst if (rintst2~=. & rintst2<100) & rintst>100
replace rintst = rintst2 if rintstx>0
replace rintst2 = rintstx if rintstx>0
replace intstx = intst if rintstx>0
replace intst = string(intst2) if rintstx>0
replace intst2 = real(intstx) if rintstx>0

replace rintstx = 0
replace intstx = "0"
replace rintstx = rintst if (rintst3~=. & rintst3<100) & rintst>100
replace rintst = rintst3 if rintstx>0
replace rintst3 = rintstx if rintstx>0
replace intstx = intst if rintstx>0
replace intst = string(intst3) if rintstx>0
replace intst3 = real(intstx) if rintstx>0

drop intstx rintstx

*create duplicate segments where they exist based on intst2
gen roadnum = 1
save temp,replace
keep if intst2~=.
replace roadnum = 2
replace intst=string(intst2)
replace rintst=rintst2
replace intst3 = .
replace rintst3 = .
append using temp.dta
save temp.dta,replace
keep if intst3~=.
replace roadnum = 3
replace intst=string(intst3)
replace rintst=rintst3
append using temp.dta
drop intst2 intst3 rintst2 rintst3

keep statefips cntyfips intst roadnum
sort statefips cntyfips intst roadnum
by statefips cntyfips intst roadnum: keep if _n==1
sort statefips cntyfips intst roadnum

/**This dataset is of statefips cntyfips intst roadnum
combos that run through center cities**/
save temp2.dta,replace


******* 2. Merge the Form 500 Data and GIS Data Together ********

use "$data/highway/baum_snow/data/form500.dta", clear

/**These are roads for which only a portion not at the
beginning is observed in the Form 500 data.  These line
set the mileposts appropriately to compensate so the 
merge works correctly.**/
replace mpost = mpost+15 if statefips==45 & intst=="77"
replace mpost = mpost+71 if statefips==55 & intst=="43"
replace mpost = mpost+91 if statefips==17 & intst=="72"

sort statefips intst mpost
merge statefips intst mpost using "$data/highway/baum_snow/data/gisdat.dta"

*flag problem roads:
*set problem = 1 for problems <=6 mi, and problem = 2 for > 6 mi off
gen problem = 0

/*Drop roads whose lengths from the form 500 data don't match up to
what is in the route log and finder list.
Any road with 3 mi or less discrepancy is ignored here, unless that
constitutes the whole road.
Any discrepancy over 6 mi has been thoroughly investigated and corrected.
*/

** form 500 mileage > gis data mileage

*6 mi too long in form 500
replace problem = 1 if statefips==6 & intst=="5"
*5 mi too long in form 500
replace problem = 1 if statefips==17 & intst=="39"
*4 mi too long in form 500
replace problem = 1 if statefips==24 & intst=="595"
*5 mi too long in form 500
replace problem = 1 if statefips==24 & intst=="895"
*5 mi too l ong in form 500 (but big dig part included)
replace problem = 1 if statefips==25 & intst=="90"
*No GIS record of this highway
replace problem = 1 if statefips==32 & intst=="580"
*too short in gis data
replace problem = 1 if statefips==37 & intst=="40"
*4 mi too long in form 500
replace problem = 1 if statefips==41 & intst=="84"
*4 mi too long in form 500
replace problem = 1 if statefips==42 & intst=="380"
*5 mi too long in form 500
replace problem = 1 if statefips==42 & intst=="81"
*No evidence of road existing in gis
replace problem = 1 if statefips==45 & intst=="326"
*4 mi too long in form 500
replace problem = 1 if statefips==54 & intst=="77"
*4 mi too long in form 500
replace problem = 1 if statefips==54 & intst=="79"


** form 500 mileage < gisdata mileage

*9 84 too long in gisdata
replace problem = 1 if statefips==9 & intst=="84"
*12 75 too long in gisdata
replace problem = 1 if statefips==12 & intst=="75"
*12 95 too long in gisdata
replace problem = 1 if statefips==12 & intst=="95"
*13 24 too long in gisdata
replace problem = 1 if statefips==13 & intst=="24"
*13 75 too long in form 500
replace problem = 1 if statefips==13 & intst=="75"
*17 280 OK b/c it overlaps with 17 74
*17 294 too short in gisdat
replace problem = 1 if statefips==17 & intst=="294"
*18 90 OK b/c overlap at end w/ 80
replace d_open = d_open[_n-1] if d_open==. & statefips==18 & intst=="465"
*21 71 OK b/c overlap at end w/ 75
*30 15 is 11 mi short in form 500 but give 1 b/c in MT
replace problem = 1 if statefips==30 & intst=="15"
*34 276 is not observed in route log or form 500 data
replace problem = 1 if statefips==34 & intst=="276" 
*form 500 data inexplicably too short
replace problem = 1 if statefips==39 & intst=="270"
*47 24
*48 10
*51 495
*This road doesn't exist - just bad gis spatial merge
drop if statefips==53 & intst=="84"
*34 95X is just used as a placeholder and should be dropped
drop if statefips==34 & intst=="95X"

/** Assign correct opening dates based on internet reports: **/
replace d_open=66 if statefips==19 & intst=="480"
replace d_open=77 if statefips==19 & intst=="129"
replace d_open=90 if (statefips==20|statefips==29) & intst=="670"
replace d_open=83 if statefips==24 & intst=="395"
replace d_open=67 if statefips==30 & intst=="315"
** doesn't exist, gis error
drop if statefips==33 & intst=="495"
replace d_open=56 if statefips==34 & intst=="276"
** doesn't exist, gis error
drop if statefips==34 & intst=="478"
replace d_open=37 if statefips==34 & intst=="495"
replace d_open=61 if statefips==36 & intst=="695"
replace d_open=81 if statefips==39 & intst=="471"
replace d_open=67 if statefips==48 & intst=="110"

/*Deal with 103(c)4(a) & NHS roads (parts not listed in form500 data):
http://www4.law.cornell.edu/uscode/23/103.html
All of these opening dates are taken from internet reports.
*/

**This variable indicates part of NHS -- assigned to 95
gen NHSp = 0

*Observe the oldest section of 6 215 for which have form500 data:
*the mileposts are wrong wrt the route log but correct otherwise 
*don't observe 94 mi in form 500
replace d_open=73 if statefips==17 & intst=="72" & mpost>=62 & mpost<=91
replace d_open=91 if statefips==17 & intst=="72" & mpost>=1 & mpost<=61
replace d_open=99 if statefips==17 & intst=="155"
replace d_open=77 if statefips==17 & intst=="172"
replace d_open=99 if statefips==18 & intst=="469"
*The listed portion of 32 515 is OK, though
*most miles not observed in form 500 or GIS data
replace d_open = 55 if statefips==36 & intst=="495" & mpost<16
replace d_open = 65 if statefips==36 & intst=="495" & mpost>15 & mpost<63
replace d_open = 72 if statefips==36 & intst=="495" & mpost>62
*form 500 misses b/c NHS
replace NHSp = 1 if statefips==37 & intst=="73"
**37 485 missing from both gis and form 500, but not in CC
**37 540 missing from both gis and form 500, but not in CC
*form 500 misses b/c NHS
replace NHSp = 1 if statefips==42 & intst=="99"
replace d_open=92 if statefips==42 & intst=="476"
*unobserved sec103 miles in form 500 data
replace d_open =99 if statefips==45 & intst=="77" & _merge==2
**47 140 missing from both datasets but not in CC
replace d_open=85 if statefips==47 & intst=="181"
**51 264 has part missing from GIS and form 500 -- OK b/c from 99
replace d_open=92 if statefips==51 & intst=="664" & _merge==2
replace d_open=77 if statefips==54 & intst=="68"
replace d_open=92 if statefips==55 & intst=="39" & mpost>90
replace mpost = mpost-21 if statefips==17 & intst=="39"
drop if mpost<1 & statefips==17 & intst=="39"
replace d_open=92 if statefips==17 & intst=="39" & mpost<60
replace d_open=69 if statefips==55 & intst=="43" & mpost>=50 & mpost<=61
replace d_open=72 if statefips==55 & intst=="43" & mpost>=40 & mpost<=49
replace d_open=73 if statefips==55 & intst=="43" & mpost>=27 & mpost<=39
replace d_open=76 if statefips==55 & intst=="43" & mpost<=38

** If only in GIS data, then not federally funded
replace fedfund = 0 if NHSp==1

/** Even if observe some of these in the form500 data, don't know
which portion of the road we observe, so recode the opening date of
the WHOLE road. **/
replace d_open = 99 if NHSp==1 

***ADD ACCURATE OPENING DATES TO MULTIPLY-NAMED ROADS
** AND SEGMENTS WITH MISSING DATA
sort statefips segnum intst mpost
by statefips segnum intst: gen count = _n
sort statefips segnum count d_open

by statefips segnum count: replace d_open=d_open[_n-1] if _n>1 & d_open==. 
by statefips segnum count: replace d_first=d_first[_n-1] if _n>1 & d_first==.
sort statefips segnum count fedfund
by statefips segnum count: replace fedfund=fedfund[_n-1] if _n>1 & fedfund==.
drop count
sort statefips intst mpost
by statefips intst: replace d_open=d_open[_n-1] if d_open==.
by statefips intst: replace d_first=d_first[_n-1] if d_first==.
replace fedfund = 0 if fedfund==.


tab _merge

/*merge = 1 from form500 data only
  merge = 2 from gisdat data only*/

**list problem roads that don't match between the two datasets
list statefips intst mpost _merge if _merge~=3 & (real(intst)<100|real(intst)==.) & problem==0
/*drop unmerged observations from form500 only (b/c some roads are missed
in form 500)*/
drop if _merge==1
drop if problem == 2
drop _merge

*create an indicator for local roads
gen rdtype = .
replace rdtype = 0 if real(intst)<100
replace rdtype = 1 if real(intst)>100 & real(intst)~=.
*make E and W roads nonlocal
replace rdtype = 0 if real(intst)==.


**** 3. Deal with center city highways *****

**Merge on dataset with cc road info
sort statefips cntyfips intst roadnum
merge statefips cntyfips intst roadnum using temp2.dta
tab _merge
/**
1 is roads not in center cities
2 is roads in gisdat but not form 500 data
3 is roads in center cities
**/
drop if _merge==2

gen cc = 0
replace cc = 1 if _merge==3
drop _merge


***** 4. Create Different Miles Measures *******

/*Create variables that have the miles of federally funded highway 
within each msa.  Variables created are as follows:
lenaX - all long-distance interstates in data based on d_open
lenbX - all short-distance interstates in data based on d_open
lencX - all fedfunded interstates (w/ no short distance) based on d_open
lenpX - all interstates in 1947 plan
firaX - all long-distance interstates in data based on d_first
firbX - all short-distance interstates in data based on d_first
for each census year 50, 60, 70, 80, 90
and then later for each year individually.
*/

*year-by-year include all fed-funded interstates
local i = 40
while `i'<=100 {
	**lena are 2-digit interstates
	gen lena`i' = 0
	replace lena`i' = 1 if d_open<`i' & rdtype==0

	**lenb are 3-digit interstates
	gen lenb`i' = 0
	replace lenb`i' = 1 if d_open<`i' & rdtype==1

	/**lenc are highways with federal funding commitments
	in 1956 and can include 2-digit and 3-digit highways**/
	gen lenc`i' = 0
	replace lenc`i' = 1 if d_open<`i' & fedfund==1

	/**The lenp variable is supposed to reflect highways in
	the 1947 plan.  It takes all of the highways in the main
	form 500 file and deletes segments not in the plan**/
	gen lenp`i' = 0
	replace lenp`i' = 1 if d_open<`i' & newrd==0 & rdtype==0
	#delimit ;
	replace lenp`i' = 0 if intst=="12" 
	| (intst=="15" & statefips==6 & mpost<102)
	| (intst=="17" & statefips==4 & mpost<7)
	| (intst=="24" & statefips~=47) | (intst=="24" & statefips==47 & mpost<47)
	| intst=="27"
	| (intst=="29" & statefips==46 & mpost>84) | (intst=="29" & statefips==38 & mpost<65)
	| (intst=="30" & statefips==48 & mpost<46)
	| intst=="37"
	| (intst=="35W" & statefips==27)
	| intst=="39"
	| intst=="43"
	| (intst=="44" & statefips==48) | (intst=="44" & statefips==40 & mpost<131)
	| intst=="49"
	| intst=="68"
	| (intst=="69" & statefips==26) | (intst=="69" & statefips==18 & mpost>155)
	| (intst=="70" & statefips==49) | (intst=="70" & statefips==8 & mpost<270)
	| intst=="72"
	| intst=="73"
	| (intst=="74" & statefips==19) | (intst=="74" & statefips==17 & mpost<16)
	| (intst=="75" & statefips==12 & mpost<261)
	| (intst=="77" & statefips~=39) | (intst=="77" & statefips==39 & mpost<106)
	| intst=="79"
	| intst=="82"
	| intst=="88"
	| intst=="97";
	#delimit cr


	gen fira`i' = 0
	replace fira`i' = 1 if d_first<`i' & rdtype==0
	gen firb`i' = 0
	replace firb`i' = 1 if d_first<`i' & rdtype==1
	gen fir`i' = fira`i'+firb`i'
	local i = `i'+1
	**this is for roads not completed yet in 93
	if `i'==94 {
		local i = 100
	}
}

save temprd.dta,replace

**** 5. Final Data *****
// Only this last part is modified from Baum-Snow

**create dataset of miles of hwy per cnty 50-90
use temprd
drop if roadnum>1

**recode counties to be consistent
do "$data/highway/baum_snow/xwalks/county-change-code.do"

sort statefips cntyfips intst
collapse (sum) fir* len*, by(statefips cntyfips intst)
*This creates a dataset of the miles of road in each county in each year
#delimit ;
keep fira* firb* lena* lenb* lenc* lenp*
statefips cntyfips intst;
#delimit cr
rename lena100 lenatot
rename lenb100 lenbtot
rename lenc100 lenctot
rename fira100 firatot
rename firb100 firbtot
rename lenp100 lenptot
reshape long lena lenb lenc lenp fira firb, i(statefips cntyfips intst) j(year)

sort statefips cntyfips year

#delimit ;
collapse (sum) lena lenb lenc lenp fira firb 
lenatot lenbtot lenctot lenptot firatot firbtot, 
by(statefips cntyfips year);
#delimit cr

**Label variables
label variable fira "miles of all 2-digit hwy w/construction started"
label variable firb "miles of all 3-digit hwy w/construction started"
label variable lena "miles of all 2-digit hwy open"
label variable lenb "miles of all 3-digit hwy open"
label variable lenc "miles of all fedfund 2-digit hwy open"
label variable lenp "miles of all hwys in plan open"

sort statefips cntyfips year
save "$build/baum_snow_county.dta", replace

erase temp.dta
erase temp2.dta
erase temprd.dta



