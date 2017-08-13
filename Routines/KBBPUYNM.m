KBBPUYNM	;JIVEYSOFT.COM/JLI - Unit tests for KBBPYNM ;03/07/16  14:56;
	;;0.0;KBBP;
	; includes tests for:
	;   KBBPYREF
	;   KBBPYNM
	;   KBBPYREL
	;   KBBPYQRY
	D EN^%ut($T(+0))
	Q
	;
STARTUP	;
	; ZEXCEPT: UTSTGLOB - created in this code, killed in SHUTDOWN
	K ^XTMP("KBBPY")
	S UTSTGLOB=$NA(^TMP("KBBPYROU-OUTPUT",$J)) K @UTSTGLOB
	Q
	;
SHUTDOWN	;
	; ZEXCEPT: UTSTGLOB - created in STARTUP, killed in this code
	D QUE^KBBPYREF
	K @UTSTGLOB
	K UTSTGLOB
	Q
	; For routine KBBPYREF
BUILD1	;@TEST - run build process
	L +^XTMP("KBBPY"):1 I '$T W !,"Skipped BUILD1 - could not get lock on global" Q
	D QUE1^KBBPYREF
	D CHKTF($D(^XTMP("KBBPY","FNM","NEW PERSON",200)),"NEW PERSON entry in ^XTMP(""KBBPY"",""FNM"") not found")
	D CHKEQ("200^",$G(^XTMP("KBBPY","FNM","NEW PERSON",200)),"Value for NEW PERSON entry incorrect")
	D CHKTF($D(^XTMP("KBBPY","FNM","KEYS",200.051)),"KEYS entry for NEW PERSON file, not found")
	D CHKEQ("200.051;200^51",$G(^XTMP("KBBPY","FNM","KEYS",200.051)),"Incorrect value for KEYS entry for NEW PERSON file")
	D CHKTF($D(^XTMP("KBBPY","XR1","PATIENT",44,44.001,2)),"Value for PATIENT field in 44.001 not present")
	D CHKEQ("1;0^S^44.001;44^",$G(^XTMP("KBBPY","XR1","PATIENT",44,44.001,2)),"Incorrect value for PATIENT field in 44.001")
	D CHKTF($D(^XTMP("KBBPY","XR1","PATIENT",44,44.003,.01)),"Value for PATIENT field in 44.003 not present")
	D CHKEQ("0;1^S;1^44.003;44.001;44^",$G(^XTMP("KBBPY","XR1","PATIENT",44,44.003,.01)),"Incorrect value for PATIENT field in 44.003")
	L -^XTMP("KBBPY")
	Q
	;
CHECK1	;@TEST - run build process via global not present KBBPYREF
	N ZTQUEUED
	K ^XTMP("KBBPY","FNM"),^XTMP("KBBPY","XR1")
	S ZTQUEUED=1 ; turns off printing of . as it builds the globals
	D CHECK^KBBPYREF
	K ZTQUEUED
	D CHKBUILD ; check actual tests
	Q
	;
CHECK2	;@TEST - run build via code that would ask about rebuilding KBBPYREF
	N ZTQUEUED
	S ZTQUEUED=1
	D CHECK^KBBPYREF
	D CHKBUILD
	Q
	;
QUE	;@TEST - run build directly by forcing it KBBPYREF
	N ZTQUEUED
	S ZTQUEUED=1 ; turns off printing of . as it builds the globals
	D QUE^KBBPYREF ; sets variables and calls BUILD to generate globals
	D CHKBUILD
	Q
	;
CHKBUILD	; tests on each version that builds the x-reference globals
	D CHKTF($D(^XTMP("KBBPY","FNM","NEW PERSON",200)),"NEW PERSON entry in ^XTMP(""KBBPY"",""FNM"") not found")
	D CHKEQ("200^",$G(^XTMP("KBBPY","FNM","NEW PERSON",200)),"Value for NEW PERSON entry incorrect")
	D CHKTF($D(^XTMP("KBBPY","FNM","KEYS",200.051)),"KEYS entry for NEW PERSON file, not found")
	D CHKEQ("200.051;200^51",$G(^XTMP("KBBPY","FNM","KEYS",200.051)),"Incorrect value for KEYS entry for NEW PERSON file")
	D CHKTF($D(^XTMP("KBBPY","XR1","PATIENT",44,44.001,2)),"Value for PATIENT field in 44.001 not present")
	D CHKEQ("1;0^S^44.001;44^",$G(^XTMP("KBBPY","XR1","PATIENT",44,44.001,2)),"Incorrect value for PATIENT field in 44.001")
	D CHKTF($D(^XTMP("KBBPY","XR1","PATIENT",44,44.003,.01)),"Value for PATIENT field in 44.003 not present")
	D CHKEQ("0;1^S;1^44.003;44.001;44^",$G(^XTMP("KBBPY","XR1","PATIENT",44,44.003,.01)),"Incorrect value for PATIENT field in 44.003")
	Q
	;
	; for routine KBBPYNM
KBBPFIL	;@TEST - build file structure KBBPYNM
	N KBBPFIL,ZTQUEUED
	K ^XTMP("KBBPY","FNM")
	K ^XTMP("KBBPY","XR1")
	;S ZTQUEUED=1 D QUE^KBBPYREF K ZTQUEUED
	S KBBPFIL=200
	D KBBPFIL^KBBPYNM(KBBPFIL) ;(200)
	D CHKTF($D(^XTMP("KBBPY","FNM","NEW PERSON",200)),"NEW PERSON entry in ^XTMP(""KBBPY"",""FNM"") not found")
	D CHKEQ("200^",$G(^XTMP("KBBPY","FNM","NEW PERSON",200)),"Value for NEW PERSON entry incorrect")
	Q
	;
KBBPFIL1	;@TEST - check single level subfile processing KBBPYNM
	N FIL,KBBP1,KBBPNN,KBBPNOD,KBBPT,UPFIL,XFIL,XUPFLD,XUPNOD
	N KBBP1,KBBPNN,KBBPNOD,KBBPT
	S XFIL=0
	; try and get a subfile
	S FIL=200 F  S FIL=$O(^DD(FIL)) Q:(FIL'<201)  I $G(^DD(FIL,0,"UP"))=200 S XFIL=FIL,UPFIL=200 Q
	I XFIL'>0 W !,"Did not find a subfile for testing in file 200." Q
	S XUPFLD=$O(^DD(UPFIL,"SB",XFIL,0)),XUPNOD=$P($P(^DD(UPFIL,XUPFLD,0),U,4),";")
	S KBBPNN=0,KBBPNOD=""
	S KBBP1=200,KBBPT=XFIL,KBBPNOD=KBBPNOD,KBBPNN=KBBPNN
	D KBBPFIL1^KBBPYNM(.KBBPT,.KBBPNOD,.KBBPNN,KBBP1) ;(UPFIL,.XFIL,.KBBPNN,.KBBPNOD)
	D CHKEQ(200,KBBPT,"Returned up file value not correct.")
	D CHKEQ(1,KBBPNN,"Bad value returned for KBBPNN.")
	D CHKEQ(XUPNOD,KBBPNOD,"Incorrect value for node returned.")
	Q
	;
KBBPFIL2	;@TEST - check two level subfile processing in KBBPFIL1 KBBPYNM
	N FIL,KBBP1,KBBPNN,KBBPNOD,KBBPT,UPFIL,XFIL,XUPFLD,XUPNOD
	N KBBP1,KBBPNN,KBBPNOD,KBBPT
	S XFIL=0
	; try and get a second level subfile
	S FIL=200 F  S FIL=$O(^DD(FIL)) Q:(FIL'<201)  I $G(^DD(FIL,0,"UP"))>200 S XFIL=FIL,UPFIL=^DD(FIL,0,"UP") I $G(^DD(UPFIL,0,"UP"))=200 Q
	I XFIL'>0 W !,"Did not find a second level subfile for testing in file 200." Q
	S XUPFLD=$O(^DD(UPFIL,"SB",XFIL,0)),XUPNOD=$P($P(^DD(UPFIL,XUPFLD,0),U,4),";")
	S KBBPNN=0,KBBPNOD=""
	S KBBP1=UPFIL,KBBPT=XFIL,KBBPNOD=KBBPNOD,KBBPNN=KBBPNN
	D KBBPFIL1^KBBPYNM(.KBBPT,.KBBPNOD,.KBBPNN,KBBP1)
	D CHKEQ(UPFIL,KBBPT,"1-Returned up file value not correct.")
	D CHKEQ(1,KBBPNN,"1-Bad value returned for KBBPNN.")
	D CHKEQ(XUPNOD,KBBPNOD,"1-Incorrect value for node returned.")
	;
	S XUPFLD=$O(^DD(200,"SB",KBBPT,0)),XUPNOD=$P($P(^DD(200,XUPFLD,0),U,4),";")_";"_XUPNOD
	S KBBP1=200
	D KBBPFIL1^KBBPYNM(.KBBPT,.KBBPNOD,.KBBPNN,KBBP1)
	D CHKEQ(200,KBBPT,"2-Returned up file value not correct.")
	D CHKEQ(2,KBBPNN,"2-Bad value returned for KBBPNN.")
	D CHKEQ(XUPNOD,KBBPNOD,"2-Incorrect value for node returned.")
	Q
	;
KBBPFLD	;@TEST - in KBBPYNM
	N KBBP1,KBBP2,KBBPNOD,KBBPTOP,KBBPXREF,NAME,KBBPFLD,KBBPFIL,KBBPNN
	N KBBP1,KBBP2,KBBPFIL,KBBPFLD,KBBPNM,KBBPNN,KBBPNOD,KBBPTOP,KBBPDIC
	S KBBPFLD=.01,KBBPFIL=200,KBBPNN=0
	S NAME=$O(^DD(KBBPFIL,0,"NM","")),KBBP1=$P(^DD(KBBPFIL,KBBPFLD,0),U,4),KBBP2="",KBBPTOP=KBBPFIL,KBBPNOD="0"
	S KBBPDIC="^VA(200,"
	K ^XTMP("KBBPY","XR1")
	S KBBPNM=NAME,KBBPFLD=KBBPFLD,KBBPFIL=KBBPFIL,KBBPNN=0,KBBP1=KBBP1,KBBP2=KBBP2,KBBPTOP=KBBPTOP,KBBPNOD=KBBPNOD
	D KBBPFLD^KBBPYNM(.KBBPNM,KBBPFIL,KBBPDIC,KBBPFLD,KBBPNN,KBBP1,KBBP2,KBBPTOP,KBBPNOD) ;(KBBPFIL,"^VA(200,",KBBPFLD,KBBPNN,.NAME,KBBP1,KBBP2,KBBPTOP,KBBPNOD)
	D CHKTF($D(^XTMP("KBBPY","XR1",NAME,KBBPTOP,KBBPFIL,KBBPFLD)),"XR1 Global for NAME field on NEW PERSON file not built correctly.")
	D CHKEQ(KBBP1_U_KBBPNOD_U_KBBP2_U_"""B""",$G(^XTMP("KBBPY","XR1",KBBPNM,KBBPTOP,KBBPFIL,KBBPFLD)),"Expected value not found")
	Q
	;
XREF	;@TEST - Get standard x-ref if any KBBPYNM
	N KBBPXREF
	N KBBPDIC,KBBPFIL,KBBPFLD,KBBPNN,KBBPXREF
	S KBBPFIL=200,KBBPFLD=.01,KBBPXREF="",KBBPNN=0,KBBPDIC="^VA(200,"
	D XREF^KBBPYNM(.KBBPXREF,KBBPFIL,KBBPDIC,KBBPFLD,KBBPNN) ;(200,"^VA(200,",.01,.KBBPXREF,0)
	D CHKEQ("""B""",KBBPXREF,"Bad x-ref for NAME field of NEW PERSON file.")
	;
	S KBBPFIL=200.194,KBBPFLD=.02,KBBPNN=1
	D XREF^KBBPYNM(.KBBPXREF,KBBPFIL,KBBPDIC,KBBPFLD,KBBPNN) ;(200.194,"^VA(200,",.02,.KBBPXREF,1)
	D CHKEQ("""AXQA""",KBBPXREF,"Bad x-ref on PACKAGE ID for ALERT DATE/TIME subfile of NEW PERSON file.")
	Q
	;
XREF1	;@TEST - get xref name, and rest of set code KBBPYNM
	N FILEDIC,SETCODE,XREF
	N KBBPDIC,KBBPXK,KBBPXREF
	S FILEDIC="^DPT(",SETCODE="S ^DPT(""B"",$E(X,1,30),DA)="""""
	S KBBPDIC=FILEDIC,KBBPXK=SETCODE,KBBPXREF=""
	D XREF1^KBBPYNM(.KBBPXK,.KBBPXREF,KBBPDIC) ;(FILEDIC,.SETCODE,.XREF)
	D CHKEQ("""B""",KBBPXREF,"Not the expected x-ref")
	D CHKEQ("$E(X,1,30),DA)=""""",KBBPXK,"Not the following code for x-ref set")
	Q
	;
CHKX	;@TEST - check xref structure KBBPYNM
	N KBBPNN,KBBPXK,KBBPXREF
	N KBBPNN,KBBPXK,KBBPXREF
	S KBBPNN=0,KBBPXREF="""B""",KBBPXK="$E(X,1,30),DA)="""""
	S KBBPNN=0,KBBPXREF=KBBPXREF,KBBPXK=KBBPXK
	D CHKX^KBBPYNM(.KBBPXREF,KBBPXK,KBBPNN) ;(.KBBPXK,.KBBPXREF,KBBPDIC) ;(KBBPXK,KBBPNN,.KBBPXREF)
	D CHKEQ("""B""",KBBPXREF,"Incorrect value returned for ""B"" X-REF")
	;
	S KBBPNN=1,KBBPXREF="""AXQA""",KBBPXK="$E(X,1,30),DA(1),DA)="""""
	S KBBPNN=KBBPNN,KBBPXREF=KBBPXREF,KBBPXK=KBBPXK
	D CHKX^KBBPYNM(.KBBPXREF,KBBPXK,KBBPNN) ;(KBBPXK,KBBPNN,.KBBPXREF)
	D CHKEQ("""AXQA""",KBBPXREF,"Incorrect value returned for ""AXQA"" X-REF")
	;
	S KBBPNN=1,KBBPXREF="DA(1)",KBBPXK="""USC3"",""B"",$E(X,1,30),DA)="""""
	S KBBPNN=KBBPNN,KBBPXREF=KBBPXREF,KBBPXK=KBBPXK
	D CHKX^KBBPYNM(.KBBPXREF,KBBPXK,KBBPNN) ;(KBBPXK,KBBPNN,.KBBPXREF)
	D CHKEQ("",KBBPXREF,"Value returned for x-ref when it shouldn't have")
	Q
	;
XRPNT	;@TEST - build file relationships data for file 200
	N GLOB
	S GLOB=$NA(^TMP("KBBPY",$J)) K @GLOB
	D XRPNT^KBBPYNM(200)
	D CHKEQ(200,$G(@GLOB@("FIL")),"Bad file number after XRPNT")
	D CHKEQ(1,$G(@GLOB@("PN",1,200,3.7)),"Unexpected number of entries for 3.7")
	D CHKEQ(".01;200R3.7",$G(@GLOB@("PN",1,200,3.7,1)),"Incorrect file data for 3.7")
	K @GLOB
	Q
	;
	; following tests for routine KBBPYREL
	;
LISTOUT	;@TEST - output data from global if printing
	N UTSTGLOB,X,KBBPFILX,KBBPXV,LINNUM,SETNUM,LINESOUT
	S UTSTGLOB=$NA(^TMP("KBBPUYNM-LINEOUT",$J)) K @UTSTGLOB
	S KBBPFILX="This Is My Header Text",KBBPXV="",SETNUM=1
	K ^TMP("KBBPYREL",$J)
	F LINNUM=1:1:24+10 S ^TMP("KBBPYREL",$J,SETNUM,LINNUM)="My Line "_LINNUM
	F LINNUM=1:1:24+10 D LISTOUT^KBBPYREL(KBBPFILX,KBBPXV,SETNUM,LINNUM)
	D CHKTF(40=$G(@UTSTGLOB@(0)),"Incorrect number of lines shown in zero node")
	D CHKEQ("My Line 1",$G(@UTSTGLOB@(1)),"Incorrect text in node 1 of global data")
	D CHKEQ("My Line 34",$G(@UTSTGLOB@(40)),"Incorrect text in node 40 of global data")
	D CHKEQ("",$G(@UTSTGLOB@(41)),"Unexpected text found in global node 41")
	K @UTSTGLOB,^TMP("KBBPYREL",$J)
	Q
	;
LISTDATA	;@TEST - output data to text (or for tests global nodes)
	N UTSTGLOB,X,KBBPFIL,KBBPFILX,KBBPXV,SETNUM,LINES,LINNUM,IOSL,LINESOUT
	S UTSTGLOB=$NA(^TMP("KBBPUYNM-LINEOUT",$J)) K @UTSTGLOB
	S KBBPFIL="AAABBB",KBBPFILX="This Is My Header Text",KBBPXV="",SETNUM=1
	S IOSL=300
	K ^TMP("KBBPYREL",$J)
	S LINES=24+10
	F LINNUM=1:1:LINES S ^TMP("KBBPYREL",$J,SETNUM,LINNUM)="My Line "_LINNUM
	D LISTDATA^KBBPYREL(KBBPFIL,KBBPFILX,SETNUM,LINES)
	D CHKEQ(43,$G(@UTSTGLOB@(0)),"Incorrect number of lines listed")
	D CHKEQ("@IOF",$G(@UTSTGLOB@(1)),"Not the expected form feed in node 1")
	D CHKEQ("                             This Is My Header Text",$G(@UTSTGLOB@(4)),"Not the expected node four (header line)")
	D CHKEQ("     <There are 34 subfile tables for the 'AAABBB' file>",$G(@UTSTGLOB@(8)),"Expected line with number of entries as node 8")
	D CHKEQ("My Line 34",$G(@UTSTGLOB@(43)),"Not expected text for node 42")
	D CHKEQ(0,$D(@UTSTGLOB@(44)),"Did not expect any data in node 43")
	K @UTSTGLOB,^TMP("KBBPYREL",$J)
	Q
	;
SELCTREL	;@TEST - selection of item number
	N VALUE
	S VALUE=$$SELCT^KBBPYREL(15,5)
	D CHKEQ(1,VALUE,"SELCT didn't return the expected value")
	Q
	;
CHEK	;@TEST - check if should stop or continue (if user interaction)
	D CHKEQ(1,$$CHEK^KBBPYREL(),"Expected '1' to be returned")
	Q
	;
HEDR	;@TEST - check output for header text
	N KBBPFILX,UTSTGLOB,LINESOUT
	S UTSTGLOB=$NA(^TMP("KBBPUYNM-LINEOUT",$J)) K @UTSTGLOB
	S KBBPFILX="This Is My Header Text"
	D HEDR^KBBPYREL(KBBPFILX)
	D CHKEQ(5,$G(@UTSTGLOB@(0)),"Incorrect number of lines listed")
	D CHKEQ("@IOF",$G(@UTSTGLOB@(1)),"Not the expected form feed in node 1")
	D CHKEQ("                             This Is My Header Text",$G(@UTSTGLOB@(4)),"Not the expected node four (header line)")
	D CHKEQ("",$G(@UTSTGLOB@(5)),"Expected null line as node 5")
	D CHKEQ(0,$D(@UTSTGLOB@(6)),"Did not expect any node 6")
	K @UTSTGLOB
	Q
	;
GETLIST	; @TEST - return number of names beginning with specified characters
	N COUNT,LENGTH,LONGNAME,NAME,START,STRTLEN
	S NAME="PATIENT",START=NAME,STRTLEN=$L(START)
	S COUNT=0 I $D(^DIC("B",NAME)) S COUNT=1
	F  S NAME=$O(^DIC("B",NAME)) Q:$E(NAME,1,STRTLEN)'=START  S COUNT=COUNT+1
	D CHKEQ(COUNT,$$GETLIST^KBBPYREL(START),"Count returned for PATIENT not expected count")
	;
	S NAME="PAT",START=NAME,STRTLEN=$L(START)
	S COUNT=0 I $D(^DIC("B",NAME)) S COUNT=1
	S LONGNAME=0,LENGTH=0
	F  S NAME=$O(^DIC("B",NAME)) Q:$E(NAME,1,STRTLEN)'=START  S COUNT=COUNT+1 I $L(NAME)>LENGTH S LONGNAME=NAME,LENGTH=$L(NAME)
	D CHKEQ(COUNT,$$GETLIST^KBBPYREL(START),"Count returned for PAT not expected count")
	D CHKEQ(1,$D(^TMP("KBBPYREL",$J,"SHO",COUNT)),"Expected global for full count to be present")
	D CHKEQ(1,$$GETLIST^KBBPYREL(LONGNAME),"Count for matches to longest name should be only 1")
	D CHKEQ(LONGNAME,$P($G(^TMP("KBBPYREL",$J,"SHO",1)),U,2),"First node in global should match longest name")
	D CHKEQ(0,$D(^TMP("KBBPYREL",$J,"SHO",2)),"Should not have data in second node of global for single match")
	;
	S NAME="ZZZXXYY"
	F  Q:$D(^DIC("B",NAME))=0  S NAME=NAME_"x"
	D CHKEQ(0,$$GETLIST^KBBPYREL(NAME),"Should have found no matches for "_NAME)
	Q
	;
NUMTHERE	;@TEST - output of header for related files with number found
	N UTSTGLOB,KBBPCNT,KBBPFIL,NONETEXT
	S UTSTGLOB=$NA(^TMP("KBBPUYNM-NUMTHERE",$J)) K @UTSTGLOB
	S KBBPCNT=14,NONETEXT="SUBFILE ENTRIES",KBBPFIL="TESTING SAMPLE"
	D NUMTHERE^KBBPYREL(KBBPFIL,NONETEXT,KBBPCNT)
	D CHKEQ("     <There are "_KBBPCNT_" "_NONETEXT_" for the '"_KBBPFIL_"' file>",@UTSTGLOB@(1),"not the expected text for the output")
	K @UTSTGLOB
	Q
	;
ENREL	;@TEST
	N UTSTGLOB,LINESOUT,UTESTXV
	S UTSTGLOB=$NA(^TMP("KBBPUYNM-ENREL",$J)) K @UTSTGLOB
	K ^XTMP("KBBPY")
	S UTESTXV=U
	D EN^KBBPYREL
	; the following are based on text at expected positions no matter what related files might be added or removed
	D CHKEQ(54,$G(@UTSTGLOB@(0)),"Not the expected number of lines")
	D CHKEQ($C(7)_"First I have to rebuild the field cross-reference file",$G(@UTSTGLOB@(2)),"Expected line 2 not found")
	D CHKEQ("         1.  NEW PERSON  (200)",$G(@UTSTGLOB@(7)),"Line listing file for selection not as expected")
	D CHKEQ("In the M world, these tables are subfiles to the NEW PERSON file.",$G(@UTSTGLOB@(13)),"Not the expected heading at line 13 ")
	D CHKEQ("In the M world, the following tables are files and subfiles that contain links",$G(@UTSTGLOB@(37)),"Not the expected heading at line 32")
	D CHKEQ("               Enter '^' to STOP or <ret> to continue...",$G(@UTSTGLOB@(+$G(@UTSTGLOB@(0)))),"Not the expected prompt at the last line")
	K @UTSTGLOB
	Q
	;
LIST	;
	;
	Q
	;
LOOP	;
	;
	Q
	;
SHOW1	;
	;
	Q
	;
CHKEQ(EXPECTED,ACTUAL,COMMENT)	;
	D CHKEQ^%ut(EXPECTED,ACTUAL,$G(COMMENT))
	Q
	;
CHKTF(TEST,COMMENT)	;
	D CHKTF^%ut(TEST,$G(COMMENT))
	Q
	;
XTROU	;
	;;KBBPUYSH
	;;KBBPUYRO
	;;KBBPUYSE
	;;KBBPUYDO
	;;KBBPUYPR
	;;KBBPUYQR
