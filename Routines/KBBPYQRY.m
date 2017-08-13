KBBPYQRY	;ISC-SF.SEA/JLI - WRAPPER FOR USER INTERFACE ;03/10/16  16:22
	;;0.0;KBBPTOOLS;;
	;A6AXFQRY ;ISC-SF.SEA/JLI - WRAPPER FOR USER INTERFACE ;4/21/95  09:35
	;;
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" G EN
	N X R !,"Press Enter NOW to run the application or wait 5 seconds to run the Unit Tests: ",X:5 I $T G EN
	W !,"Running Unit Tests",!
	D EN^%ut("KBBPUYNM")
	W !!,"To run the KBBPYQRY routine itself use the command DO EN^KBBPYQRY"
	Q
	;
EN	;
	N I,ISDEBUG,J,K,KBBPEXIT,KBBPFIL,KBBPFILN,KBBPI,KBBPQRY,KBBPSET,KBBPROU,KBBPTST,TOTSETS
	N PATH,SETNUM,UTESTING,X,X1
	K ^TMP("KBBPYS",$J),PATH,^TMP("KBBPY",$J,"C")
	S KBBPQRY=1,KBBPEXIT=0
	S UTESTING=0 I $T(EN^%ut)'="" S UTESTING=$$ISUTEST^%ut()
	; moved checking of available file name/number to before entering any data
	; KBBPZ999 is reserved for unit tests
	I UTESTING S KBBPROU="KBBPZ999"
	I 'UTESTING F I=1:1 Q:I>998  S J="00"_I,J=$E(J,$L(J)-2,$L(J)) S X="KBBPZ"_J X ^%ZOSF("TEST") I '$T S KBBPROU=X Q
	I 'UTESTING I I>998 D WRITEIT("Names for KBBPZnnn are all used up.  Clean out the routines and try again.") Q
	;
	I UTESTING S KBBPFIL=200,KBBPI=0
	I 'UTESTING D GETFILE^KBBPYSET(.KBBPFIL,.KBBPFILN,.KBBPI)
	I $G(KBBPFIL)'>0 Q
	S KBBPFILN=$P(^DIC(KBBPFIL,0),U)
	I UTESTING S KBBPSET=2 D
	. N X
	. S X=$P(^VA(200,DUZ,0),U)
	. S ^TMP("KBBPYS",$J,1,1,200)="NAME^200^.01^0;1^^200^""B""^^^X="""_X_""""
	. S X=$P(^VA(200,DUZ,1),U,2)
	. S ^TMP("KBBPYS",$J,1,2,200)="SEX^200^4^1;2^^200^^^^X="""_X_""""
	. S ^TMP("KBBPYS",$J,1,3,200)="KEY^200.051^.01^0;1^51^200.051;200^""AB""^^^X=""XUMGR"""
	. S ^TMP("KBBPYS",$J,"TOT",1,200)="NAME^200^.01^0;1^^200^""B""^^^"
	. S ^TMP("KBBPYS",$J,"TOT",2,200)="SEX^200^4^1;2^^200^^^^"
	. S ^TMP("KBBPYS",$J,"TOT",3,200)="KEY^200.051^.01^0;1^51^200.051;200^""AB""^^^X[""XU"""
	. Q
	I 'UTESTING S KBBPSET=$$GETSETS(.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY) I KBBPEXIT Q
	K KBBPTST S KBBPTST=0 I KBBPSET<2 S KBBPTST(1)="",KBBPTST=0 I KBBPSET=1 S KBBPTST(1)=1,KBBPTST=1
	;
	I KBBPSET>1 D ANDOR(.KBBPTST,KBBPSET)
	;
	S PATH=""
	I 'UTESTING D OUTDATA(.PATH,.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY) I KBBPEXIT Q
	D OUTANDOR(.TOTSETS,KBBPFIL)
	D WRITEIT(""),WRITEIT("")
	;
	D ROU(.KBBPTST,.PATH,.SETNUM,.TOTSETS,KBBPROU) ; was originally fall through
	K ^TMP("KBBPY"),^TMP("KBBPYS")
	Q
	;
GETSETS(KBBPI,KBBPEXIT,KBBPFIL,KBBPQRY)	;.EF - get information about sets of conditions for search
	N KBBPSET,KBBPI,KBBPSET,DONE,FLDS,X
	F KBBPSET=1:1 Q:(KBBPSET>1)&$$ISUTEST()  K ^TMP("KBBPY",$J,"X") D  Q:KBBPEXIT  I X'="@" Q:'$D(^TMP("KBBPYS",$J,KBBPSET))
	. D WRITEIT(""),WRITEIT("Set of fields and conditions for Test "_KBBPSET)
	. S X=$$GETFLDS(.KBBPSET,.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY)
	. I (KBBPSET=0)&(X="") S KBBPEXIT=1
	. Q
	Q $S(KBBPSET>0:KBBPSET-1,1:0)
	;
GETFLDS(KBBPSET,KBBPI,KBBPEXIT,KBBPFIL,KBBPQRY)	; .EF - returns last input
	N DONE,FLDS,X
	S KBBPI=0,DONE=0
	F FLDS=1:1 D  Q:KBBPEXIT!$$ISUTEST()  Q:X=""
	. ; get fields for current set, X="@" indicates delete fields for current set
	. S X=$$GETFLDS^KBBPYSET(.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY)
	. I KBBPEXIT Q  ; exit from application
	. I '$D(^TMP("KBBPY",$J,"X",FLDS)) Q  ; finished entry for current set
	. I $G(^TMP("KBBPY",$J,"X",FLDS))'="@" D
	. . M ^TMP("KBBPYS",$J,KBBPSET)=^TMP("KBBPY",$J,"X")
	. . Q
	. I $G(^TMP("KBBPY",$J,"X",FLDS))="@" D
	. . ; entry of only @ causes deletion of the CURRENT set of fields and can
	. . ; be used if an error in entry is found, instead of requiring that
	. . ; application be restarted.
	. . K ^TMP("KBBPYS",$J,KBBPSET)
	. . D WRITEIT(""),WRITEIT("Set "_KBBPSET_" DELETED!"),WRITEIT("")
	. . S KBBPSET=KBBPSET-1
	. Q
	Q X
	;
ANDOR(KBBPTST,KBBPSET,OUTPUT)	;
	; KBBPTST - passed by reference -
	; KBBPSET - input
	; OUTPUT  - OPTIONAL input      - if present, true indicates set up for output values
	N KBBPI,CNT,DIR,I,X,Y,TEXT,ISUTEST
	S ISUTEST=$$ISUTEST()
	S OUTPUT=+$G(OUTPUT)
	S CNT=0 F I=1:1:KBBPSET S KBBPI(I)=""
	S DIR(0)="L^1:"_KBBPSET
	S DIR("A")="Enter numbers for TESTS to be 'AND'ed together (enter ?? for help): "
	F I=1:1 S TEXT=$T(@("DIRQ+"_I)),TEXT=$P(TEXT,";",3,99) Q:TEXT=""  S DIR("?",I)=TEXT
	D WRITEIT("The response(s) must be a comma-separated series of digits ranging from 1 to "_KBBPSET_".")
	D WRITEIT("  Used values from one response may be used in a subsequent response as necessary")
	F  D  Q:'$D(KBBPI)
	. S DIR("B")="" F I=1:1:KBBPSET I $D(KBBPI(I)) S DIR("B")=DIR("B")_$S(DIR("B")'="":",",1:"")_I
	. D  ;
	. . I ISUTEST S:CNT=0 Y=$S(KBBPSET=2:1,1:"1,2") S:CNT=1 Y=$S(KBBPSET=2:2,1:"1,3")
	. . D:'ISUTEST ^DIR
	. . F I=1:1:KBBPSET I (","_Y_",")[(","_I_",") K KBBPI(I)
	. . Q
	. F I=0:0 S I=$O(KBBPTST(I)) Q:I'>0  I KBBPTST(I)=Y K Y
	. I $D(Y) S CNT=CNT+1,KBBPTST(CNT)=Y,KBBPTST=CNT
	. Q
	Q
	;
OUTDATA(PATH,KBBPI,KBBPEXIT,KBBPFIL,KBBPQRY)	;
	;D OUTDATA(.PATH,.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY)
	N I,J,K,X,X1,OUTPUT,UTESTING
	S UTESTING=$$ISUTEST()
	D WRITEIT(""),WRITEIT("Data to **Report** for Matches (Conditions are optional)")
	S KBBPI=0,X="",OUTPUT=1
	F  S X=$$GETFLDS^KBBPYSET(.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY,OUTPUT) Q:$D(^TMP("KBBPY",$J,"X"))!KBBPEXIT
	I KBBPEXIT Q
	M ^TMP("KBBPYS",$J,"TOT")=^TMP("KBBPY",$J,"X")
	K ^TMP("KBBPY",$J,"X")
	D WRITEIT(""),WRITEIT("")
	I X["^" Q
	K X1 F I=0:0 S I=$O(^TMP("KBBPY",$J,"C",I)) Q:I'>0  F J=1:1 I '$D(X1((255-$L(^(I))),J)) S X1(255-$L(^(I)),J)=^(I) Q  ; naked globals
	F I=0:0 S I=$O(X1(I)) Q:I'>0  D
	. F J=0:0 S J=$O(X1(I,J)) Q:J'>0  D
	. . S X1=X1(I,J)
	. . S X=0 F K=0:0 S K=$O(PATH(K)) Q:K'>0  I $E(PATH(K),1,$L(X1))=X1 S X=1 Q
	. . I 'X D
	. . . F K=1:1 I '$D(PATH(K)) Q
	. . . S PATH(K)=X1
	. . K X1(I,J)
	. . Q
	. Q
	Q
	;
OUTANDOR(TOTSETS,KBBPFIL)	;
	; TOTSETS - passed by reference -
	; KBBPFIL - input
	N KBBPI,CNT,DIR,I,X,Y,TEXT,ISUTEST,COND,FILETYP,FILNAME,FLDNAME,J,TOTSETSX,XCNT,XPATH,XTN,KBBPSET
	S ISUTEST=$$ISUTEST()
	K XTN
	F I=1:1 S J=$O(^TMP("KBBPYS",$J,"TOT",I,"")) Q:J'>0  S XTN(I)=$P(^(J),U,6)
	S CNT=0,J="" F  S J=$O(^TMP("KBBPY",$J,"C",J)) Q:J'>0  S CNT=CNT+1,XPATH(CNT)=^(J)
	D FILETYPS^KBBPYROU(.FILETYP,.XPATH,.XTN,KBBPFIL)
	K TOTSETS S TOTSETS=0
	F I=1:1 Q:'$D(^TMP("KBBPYS",$J,"TOT",I))  S X=$O(^(I,"")) Q:X'>0  I FILETYP(I)="SINGLE" D
	. S FLDNAME=$P(^(X),U),FILNAME=$P(^DIC(X,0),U)
	. D WRITEIT("   "_FILNAME_" "_FLDNAME_" is not a multiple and will be included on each line of output")
	. S TOTSETS=TOTSETS+1,TOTSETS(TOTSETS)=I
	. Q
	;
	D WRITEIT(""),WRITEIT("Select which of the following should be 'AND'ed together, entry of only one")
	D WRITEIT("number will 'OR' that value with the others.  Entry of 1,2 would have 1 and 2")
	D WRITEIT("ANDed together, while 1,3 would also have 1 and 3 ANDed together."),WRITEIT("")
	;
	S XCNT=0
	K KBBPI
	F I=1:1 Q:'$D(^TMP("KBBPYS",$J,"TOT",I))  S X=$O(^(I,"")) Q:X'>0  I FILETYP(I)'="SINGLE" D
	. S XCNT=XCNT+1,XCNT(XCNT)=I
	. S FLDNAME=$P(^TMP("KBBPYS",$J,"TOT",I,X),U),COND=$P(^(X),U,10),FILNAME=$P(^DIC(X,0),U)
	. D WRITEIT(XCNT_"  "_FILNAME_" "_FLDNAME_"    *** "_COND)
	. S KBBPI(XCNT)="",KBBPSET=XCNT
	. Q
	S DIR(0)="L^1:"_KBBPSET
	S DIR("A")="Enter numbers for TESTS to be 'AND'ed together (enter ?? for help): "
	F I=1:1 S TEXT=$T(@("DIRQ+"_I)),TEXT=$P(TEXT,";",3,99) Q:TEXT=""  S DIR("?",I)=TEXT
	D WRITEIT("Response(s) must be a comma-separated series of digits ranging from 1 to "_KBBPSET_".")
	D WRITEIT("Used values from one response may be used in a subsequent response as necessary")
	S CNT=0
	F  D  Q:'$D(KBBPI)
	. S DIR("B")=""
	. F I=1:1:KBBPSET I $D(KBBPI(I)) S DIR("B")=DIR("B")_$S(DIR("B")'="":",",1:"")_I
	. D  ;
	. . I ISUTEST S:CNT=0 Y=$S(KBBPSET=2:1,1:"1,2") S:CNT=1 Y=$S(KBBPSET=2:2,1:"1,3")
	. . D:'ISUTEST ^DIR
	. . F I=1:1:KBBPSET I (","_Y_",")[(","_I_",") K KBBPI(I)
	. . Q
	. F I=0:0 S I=$O(TOTSETS(I)) Q:I'>0  I TOTSETS(I)=Y K Y
	. I $D(Y) S CNT=CNT+1,TOTSETSX(CNT)=Y,TOTSETSX=CNT
	. Q
	F I=1:1:TOTSETSX D
	. S TOTSETS=TOTSETS+1,TOTSETS(TOTSETS)=""
	. F J=1:1:XCNT S Y=$P(TOTSETSX(I),",",J) Q:Y=""  S TOTSETS(TOTSETS)=TOTSETS(TOTSETS)_XCNT(Y)_","
	. Q
	Q
	;
ROU(KBBPTST,PATH,SETNUM,TOTSETS,KBBPROU)	;
	N DIF,I,J,LINNUM,X,XCNP
	S XCNP=0
	K ^TMP($J,"KBBPYR") S X="KBBPYROU",XCNP=0,DIF="^TMP($J,""KBBPYR""," X ^%ZOSF("LOAD")
	; change first line to mark routine
	S ^TMP($J,"KBBPYR",1,0)=KBBPROU_" "_$P(^TMP($J,"KBBPYR",1,0)," ",2,200)
	; update the routine date/time
	S X=$$NOW^XLFDT()
	S X=$E(X,4,5)_"/"_$E(X,6,7)_"/"_$E(X,2,3)_"  "_$E(X,9,10)_":"_$E(X,11,12)
	S $P(^TMP($J,"KBBPYR",1,0),";",3)=X
	; find the line with tag FILE
	S LINNUM=XCNP
	F LINNUM=1:1 I $P(^TMP($J,"KBBPYR",LINNUM,0)," ")="FILE" Q
	; remove all lines from FILE to the end of the routine they will be replaced by the specifications for the new search
	F I=LINNUM:1 Q:'$D(^TMP($J,"KBBPYR",I,0))  K ^(0) ; naked global
	; update FILE tag
	S LINNUM=LINNUM-1 D ADD(.LINNUM,"FILE","KBBPFIL")
	; get and set type of output NORMAL or PACKED (^ or , separated)
	D OUTTYPE(.LINNUM)
	S PATH="" D ADD(.LINNUM,"PATH","PATH")
	D ADD(.LINNUM,"SETS","KBBPTST")
	F SETNUM=1:1 Q:'$D(^TMP("KBBPYS",$J,SETNUM))  D ADDSET(.LINNUM,SETNUM)
	I $G(TOTSETS)>0 D ADD(.LINNUM,"TOTSETS","TOTSETS")
	S SETNUM="TOT" D ADDSET(.LINNUM,SETNUM)
	D SAVE(KBBPROU) ; was originally fall through
	Q
	;
SAVE(KBBPROU)	;
	N DIE,XCN,X
	S DIE="^TMP($J,""KBBPYR"",",XCN=0,X=KBBPROU X ^%ZOSF("SAVE") K DIE
	D WRITEIT(""),WRITEIT("")
	D WRITEIT("The routine to perform the specified analysis has been saved as")
	D WRITEIT(""),WRITEIT($$INDENT^KBBPYSET(20)_KBBPROU)
	D WRITEIT(""),WRITEIT("You may use this routine name in the future to repeat the analysis")
	D WRITEIT(""),WRITEIT("")
	D LAUNCH(KBBPROU)
	Q
	;
LAUNCH(KBBPROU)	;
	; launch routine
	N %ZIS,POP,ZTIO,ION,ZTRTN,ZTDESC,KBBPQRY
	S KBBPROU="EN1^"_KBBPROU
	I '$$ISUTEST() S %ZIS="QN" D ^%ZIS Q:POP  I IO'=IO(0) S ZTIO=ION S ZTRTN=KBBPROU,ZTDESC="KBBPY QUERY" D ^%ZTLOAD D HOME^%ZIS Q
	I '$$ISUTEST() D @KBBPROU
	Q
	;
OUTTYPE(LINNUM)	;
	N DIR,Y,PACK
	S Y=1
	S DIR(0)="Y",DIR("A")="Output Packed"
	S DIR("A",1)="The output may be PACKED with only a separator between data fields,",DIR("A",2)="or the output may be in a more readable format"
	I '$$ISUTEST() D ^DIR K DIR
	S PACK=$S(Y:"PACK",1:"NORMAL")
	I PACK="PACK" S PACK="^-PACK" D WRITEIT("PACK as ^-separated or ,-separated for Excel (^ or ,)?  ^// ")  R:'$$ISUTEST() Y:DTIME I Y="," S PACK=",-PACK"
	D ADD(.LINNUM,"PRINT","PACK")
	Q
	;
ADD(LINNUM,NAME,ARRAY)	;
	; LINNUM  - passed by reference - line number of addition to routine
	; NAME    - input               -
	; ARRAY   - input
	N I,LNUM1
	S LNUM1=LINNUM ; DEBUG
	S LINNUM=LINNUM+1,^TMP($J,"KBBPYR",LINNUM,0)=NAME_" ;;"_@ARRAY
	F I=0:0 S I=$O(@ARRAY@(I)) Q:I'>0  S LINNUM=LINNUM+1,^TMP($J,"KBBPYR",LINNUM,0)=" ;;"_@ARRAY@(I)
	S LINNUM=LINNUM+1,^TMP($J,"KBBPYR",LINNUM,0)=" ;;"
	Q
	;
ADDSET(LINNUM,SETNUM)	; add set of data to list
	; LINNUM - passed by reference - LINNUM subscript
	; SETNUM - input - set number
	N J,K,LNUM1,X
	S LNUM1=LINNUM
	S LINNUM=LINNUM+1,^TMP($J,"KBBPYR",LINNUM,0)=SETNUM_" ;" ; mark start of set number
	F J=0:0 S J=$O(^TMP("KBBPYS",$J,SETNUM,J)) Q:J'>0  D  ; and place data for each entry in set
	. S K=$O(^TMP("KBBPYS",$J,SETNUM,J,0)) ; get file number for search or output
	. S X=^TMP("KBBPYS",$J,SETNUM,J,K) ; get data for item
	. S LINNUM=LINNUM+1,^TMP($J,"KBBPYR",LINNUM,0)=" ;;"_$P(X,U,3)_"*"_$P(X,U,2)_";"_$P(X,U,10,100) ; create entry in routine
	. Q
	S LINNUM=LINNUM+1,^TMP($J,"KBBPYR",LINNUM,0)=" ;;" ; mark end of set data
	Q
	;
WRITEIT(TEXT,NOLINFED)	;
	D WRITEOUT^KBBPYROU(TEXT,$G(NOLINFED))
	Q
	;
ISUTEST()	; .EF
	I $T(ISUTEST^%ut)="" Q 0
	Q $$ISUTEST^%ut()
	;
DIRQ	;  The following is used in ANDOR for output as response to ?? entry
	;;"The separate tests indicated to be performed may be used to select entries"
	;;"which meet all of the criteria (all are 'and'ed together) or which meet a"
	;;"combination of the tests 'or' another combination of tests.  The"
	;;"test numbers entered together indicate that they should be 'anded'"
	;;"together that result is 'or'ed together with other 'and'ed sets."
	;;"The most direct is all numbers entered together so all conditions"
	;;"must be met for the entry to be selected."
	;;" "
	;;"For 3 sets of conditions, (1, 2, and 3), entry of 1,2,3 would indicate that"
	;;"all three conditons must be met for an entry to be selected.  However, entry"
	;;"as separate responses of 1, then 2, then 3 would result in an entry meeting"
	;;"any one of the conditions being selected (they are ORed together)."
	;;" "
	;;"Entry of 1,2 for one response followed by 1,3 for a second response would"
	;;"indicate that conditions for set one AND two must be met OR conditions"
	;;"one AND three must be met for an entry to be selected for output."
	;;" "
	;;
EOR	;
