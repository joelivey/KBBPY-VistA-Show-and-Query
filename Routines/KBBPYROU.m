KBBPYROU	;ISC-SF.SEA/JLI - TEMPLATE ROUTINE FOR QUERY INTERFACE ;05/01/17  17:04
	;;0.0;KBBPTOOLS;;
	;A6AXFROU ;ISC-SF.SEA/JLI - TEMPLATE ROUTINE FOR QUERY INTERFACE ;9/22/99  12:50
	;
	; Note - after saving from KBBPYQRY, routine name would be different from KBBPYROU
	;
	I $T(EN^%ut)="" W !,"Can't run Unit Tests",! G EN
	N X
	R !,"Press Enter NOW to run the application or wait 5 seconds to run the Unit Tests: ",X:5 I $T G EN
	W !,"Running Unit Tests",!
	D EN^%ut("KBBPUYRO")
	N ROUNAME S ROUNAME=$P($T(+1)," ")
	W !!!,"*** To run the routine ^"_ROUNAME_", use DO EN^"_ROUNAME_". ***"
	Q
	;
EN	; Entry from queued job or from user running top of routine
	; ZEXCEPT: ION - Kernel IO variable
	; ZEXCEPT: ZTQUEUED - if defined set by TaskManager to indicate currently in a task
	;    ZTQUEUED set if run from KBBPYQRY and user is not printing to terminal
	; ZEXCEPT: ZTRTN - if being queued, this indicates the name of the routine to be run
	; ZEXCEPT: KBBPYMIN,KBBPYLIM,KBBPYMAX - if defined, they are set in tags TESTRANG, TESTMAX or TESTLIM and are KILLed before exiting EN1
	N %ZIS,ZTDESC,ZTIO,ZTSAVE
	S KBBPYMIN=2801,KBBPYMAX=2803 ; DEBUG
	I '$D(ZTQUEUED),'$$ISUTEST() S %ZIS="QN" D ^%ZIS Q:POP  I IO'=IO(0) S ZTIO=ION,ZTRTN="EN1^"_$P($T(+1)," "),ZTDESC="KBBPY QUERY - "_ZTRTN D  D ^%ZTLOAD,HOME^%ZIS Q
	. S:$D(KBBPYMIN) ZTSAVE("KBBPYMIN")=KBBPYMIN S:$D(KBBPYLIM) ZTSAVE("KBBPYLIM")=KBBPYLIM S:$D(KBBPYMAX) ZTSAVE("KBBPYMAX")=KBBPYMAX
	S ROOT=$NA(^TMP($J))
	D EN1
	Q
	;
EN1	; entry from KBBPYQRY when initially printing to terminal or called from EN above
	; ZEXCEPT: KBBPYMIN,KBBPYLIM,KBBPYMAX - if defined, they are set in tags TESTRANG, TESTMAX or TESTLIM and are KILLed before exiting EN1
	I '$D(KBBPYMIN) S KBBPYMIN=0
	I '$D(KBBPYLIM) S KBBPYLIM=0
	I '$D(KBBPYMAX) S KBBPYMAX=0
	I $$ISUTEST() S KBBPYMIN=DUZ,KBBPYMAX=DUZ
	;
	I '$D(^XTMP("KBBPY")) D QUE^KBBPYREF
	D DOWORK
	K KBBPYMIN,KBBPYLIM,KBBPYMAX ; just in case
	Q
	;
DOWORK	;
	; ZEXCEPT - IOF
	N COND,DABASE,DATA,I,J,K,KBBPYLCN,KBBPPACK,KBBPSETS,KEY,L,L1,LL,NODE
	N PATH,PRMRYFIL,RESULT,TOTSETS,VAR,X,XN,XN1,XN2,XNMAX,XWID,XWIDT
	K ^TMP($J)
	; get data specifications from tags at bottom of file
	S PRMRYFIL=$P($T(FILE),";",3) ; primary file number
	S NODE=$G(^DIC(PRMRYFIL,0,"GL"))
	S NODE=$E(NODE,1,$L(NODE)-1) ; remove comma at end
	S:NODE["(" NODE=NODE_")" ; if opening paren, then close base for file
	;
	; get info path(s) from under PATH tag
	D GETPATHS(.PATH)
	; type of output from PRINT tag - added cvs packed file as 2nd choice
	S KBBPPACK=$$GETPACK()
	; get specifications for fields and conditions for sets under SETS tags
	D GETSETS(.KBBPSETS)
	;  get specifications for output data fields and possible conditions under TOTSETS tag
	D GETTOT(.TOTSETS)
	;
	D SETBASE(.VAR,.COND,.DATA)
	;
	F I=1:1 S:'$D(DATA(I)) I="TOT" D GETCODE^KBBPYPRO(DATA(I),PRMRYFIL,VAR(I),"PATH") Q:I="TOT"
	;
	D SETORDR(.DATA,.XN1,.XN2,PRMRYFIL)
	D SETWID(.DATA,.XWID,.XWIDT)
	D SETLINK(.XN1,.XN2,PRMRYFIL)
	S XNMAX=@DATA("TOT")@("XT") ; 160110
	; output selection criteria
	I '$D(TOTSETS) S TOTSETS=1 S TOTSETS(1)="" S I=0 F  S I=$O(@DATA("TOT")@("XT",I)) Q:I'>0  S TOTSETS(1)=TOTSETS(1)_I_","
	;
	D SELCTOUT(.KBBPSETS,.DATA)
	; list values displayed
	D LISTVALS(.DATA,.COND)
	;
	; and finally find and output the actual data
	D DATAOUT(.DATA,.COND,.PATH,.KBBPSETS,.XWID,.XWIDT,KBBPPACK,.TOTSETS,NODE,XNMAX,PRMRYFIL)
	K ^TMP($J)
	Q
	;
GETPATHS(PATH)	; get info path(s) from under PATH tag
	; PATH - passed by reference - returns with data for paths in array PATH
	N I,K,X
	K PATH
	F I=1:1 S K="S X=$P($T(PATH+"_I_"),"";"",3,99)" X K Q:X=""  S PATH(I)=X
	Q
	;
GETPACK()	;.EF - type of output from PRINT tag - added cvs packed file as 2nd choice
	N KBBPPACK
	S KBBPPACK=$E($P($T(PRINT),";;",2)),KBBPPACK=$S(KBBPPACK="^":1,KBBPPACK="PACK":1,KBBPPACK=",":2,1:0)
	Q KBBPPACK
	;
GETSETS(KBBPSETS)	;
	; KBBPSETS - passed by reference - returns with data on sets of variables and condtions
	N I,K,X
	K KBBPSETS S KBBPSETS=+$P($T(SETS),";",3)
	F I=1:1 S K="S X=$P($T(SETS+"_I_"),"";"",3,99)" X K Q:X=""  S:X["DUZ" X=$P(X,"DUZ")_"DABASE"_$P(X,"DUZ",2,99) S KBBPSETS(I)=X
	Q
	;
GETTOT(TOTSETS)	; get specifications for output data fields and possible conditions under TOTSETS tag
	; TOTSETS - passed by reference - returns with data on fields and condtions for output
	N I,K,X
	K TOTSETS S TOTSETS=+$P($T(TOTSETS),";",3)
	F I=1:1:TOTSETS S K="S X=$P($T(TOTSETS+"_I_"),"";"",3)" X K S:X["DUZ" X=$P(X,"DUZ")_"DABASE"_$P(X,"DUZ",2,99) S TOTSETS(I)=$TR(X,",",";")
	I TOTSETS'>0 K TOTSETS
	Q
	;
SETBASE(VAR,COND,DATA)	;
	; VAR  - passed by reference
	; COND - passed by reference
	; DATA - passed by reference
	N I,J,K,L,TOTCODE,X
	K VAR,COND,DATA
	; set base for upto 10 sets of search variables and conditions and for output ("TOT")
	F I=1:1:10,"TOT" S J="VAR"_I K @J S J=$NA(^TMP($J,"DATA"_I)) K @J S J="COND"_I K @J,VAR(I),COND(I),DATA(I)
	; work from set 1 up, if no set at the number use "TOT" to finish
	F I=1:1 S K="S:$T("_I_")="""" I=""TOT""" X K S DATA(I)=$NA(^TMP($J,"DATA"_I)),VAR(I)="VAR"_I,COND(I)="COND"_I K @DATA(I),@VAR(I),@COND(I) D  Q:I="TOT"
	. F J=1:1 S K="S X=$T("_I_"+"_J_")" X K Q:$P(X,";",3)=""  S @VAR(I)@(J)=$TR($P(X,";",3),"*",";") I $P(X,";",4)'="" S @COND(I)@(J)="I "_$P(X,";",4)
	. Q
	Q
	;
SETORDR(DATA,XN1,XN2,PRMRYFIL)	;
	N I,I1,J,J1,K,L,M,XN,XN1,XN2
	F I=0:0 S I=$O(@DATA("TOT")@("XTN",I)) Q:I'>0  S J=^(I) D  ; naked global
	. S K=$P(J,";",$L(J,";")) I K="" S K=$P(J,";",$L(J,";")-1)
	. S L="",M=J F  S L=$P(M,";")_";"_L,M=$P(M,";",2,90) Q:M=""
	. S XN(K,L,I)=I
	. Q
	K XN1,XN2 F I=0:0 S I=$O(XN(I)) Q:I'>0  D
	. S J="" F  S J=$O(XN(I,J)) Q:J=""  D
	. . F K=0:0 S K=$O(XN(I,J,K)) Q:K'>0  S J1=J D
	. . . I $P(J1,";",2)="" S:I=PRMRYFIL XN1(I,1,K)="" S:I'=PRMRYFIL XN1(PRMRYFIL,"D",I)="",XN2(I,1,K)="" Q
	. . . F  Q:J1=""  D
	. . . . S I1=$P(J1,";"),L=$P(J1,";",2)
	. . . . I L="" S XN2(I1,1,K)="",J1="" Q
	. . . . S XN1(I1,"D",L)=""
	. . . . S J1=$P(J1,";",2,99)
	. . . . Q
	. . . Q
	. . Q
	. Q
	Q
	;
SETWID(DATA,XWID,XWIDT)	;
	N B100,I,FIL,FLD,TYP,X,X1,XL
	S $P(B100," ",100)=""
	F I=0:0 S I=$O(@DATA("TOT")@("XM",I)) Q:I'>0  D
	. S FLD=@DATA("TOT")@("XM",I) I FLD=.001 S XWID(I)=10 Q
	. S FIL=$P(@DATA("TOT")@("XTN",I),";")
	. S X=^DD(FIL,FLD,0),TYP=$P(X,U,2)
	. D SETTYP(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	. Q
	Q
	;
SETTYP(XWID,XWIDT,FIL,FLD,TYP,I,X)	;
	; XWID,XWIDT,FIL,FLD - passed by reference
	; ZEXCEPT: IOM - Kernel value for margin for printing
	N XL,J,X1
	I TYP["F" S X=+$P(X,"$L(X)>",2) S XWID(I)=$S(X>0:X,1:30),XWIDT(I)="F" Q  ; free text
	I TYP["D" D  S XWIDT(I)="D" Q  ; date
	. N Y S Y=""
	. S XWID(I)=12
	. I X["%DT=" S Y=$P(X,"%DT=",2),Y=$P(Y,"""",2)
	. I Y["T" S XWID(I)=18 I Y["S" S XWID(I)=21
	. Q
	I TYP["S" D  Q  ; set
	. S X=$P(X,U,3),XL=0
	. F J=2:1 S X1=$P(X,":",J),X1=$P(X1,";") Q:X1=""  I $L(X1)>XL S XL=$L(X1)
	. S XWID(I)=XL,XWIDT(I)="S;"_X
	. Q
	I TYP["N" D  S XWIDT(I)="N" Q  ; number
	. I TYP["J" S XWID(I)=+$P(TYP,"J",2)_","_+$P(TYP,",",2) Q
	. S XWID(I)=10
	. Q
	I TYP["C" D  S XWIDT(I)="C" Q  ; computed
	. I TYP["B" S XWID(I)=1 Q  ; BINARY 1 or 0
	. I TYP["J" S XWID(I)=+$P(TYP,"J",2) Q
	. S XWID(I)=10
	I TYP["P" D  D SETTYP(.XWID,.XWIDT,FIL,FLD,TYP,I,X) Q  ; pointer
	. S TYP=+$P(TYP,"P",2),X=$G(^DD(TYP,.01,0)),FIL=TYP,FLD=.01,TYP=$P(X,U,2)
	I TYP["V" S XWIDT(I)="V",XWID(I)=30 Q  ; variable pointer
	I TYP["K" S XWIDT(I)="K",XWID(I)=245 Q  ; mumps
	I TYP["W" S XWIDT(I)="W",XWID(I)=IOM Q  ; word processing
	D WRITEOUT("Unknown data type file="_FIL_"  field="_FLD) S XWID(I)=30,XWIDT(I)="U"
	Q
	;
SETLINK(XN1,XN2,PRMRYFIL)	;
	; XN1 - passed by reference
	; XN2 - passed by reference
	; PRMRYFIL - input - primary file number
	N I,J,I1,NCNT,XC,XLINK
	S I1=PRMRYFIL,XC=0,NCNT=1,XLINK(1)=""
LINK1	;
	F I=0:0 S I=$O(XN1(I1,"D",I)) Q:I'>0  F J=0:0 S J=$O(XN2(I,1,J)) Q:J'>0  S XLINK(NCNT)=XLINK(NCNT)_J_";" I $D(XN1(+I,"D")) N I1 S I1=I N I D LINK1 S NCNT=NCNT+1,XLINK(NCNT)=""
	I NCNT>1 K XLINK(NCNT)
	Q
	;
SELCTOUT(KBBPSETS,DATA)	;
	; KBBPSETS - passed by reference -
	; DATA     - passed by reference -
	N I,J,K,L,L1
	I KBBPSETS>0 D WRITEOUT("@IOF") D WRITEOUT("") D WRITEOUT("Selection based on ") D
	. F I=1:1:KBBPSETS D
	. . S L1=0 F J=1:1 S K=$P(KBBPSETS(I),",",J) Q:K=""  D
	. . . F L=0:0 S L=$O(@DATA(K)@("XTNM",L)) Q:L'>0  D
	. . . . S L1=L1+1
	. . . . D WRITEOUT($S(L1=1:"     ",1:"  and")_"  "_@DATA(K)@("XTNM",L)_" (file #"_@DATA(K)@("XTN",L)_")   *** "_@COND(K)@(L))
	. . . . Q
	. . . Q
	. . I I<KBBPSETS D WRITEOUT(""),WRITEOUT("OR"),WRITEOUT("")
	. . Q
	. Q
	Q
	;
LISTVALS(DATA,COND)	;
	; DATA - passed by reference
	N I,L1,TEXT
	D WRITEOUT(""),WRITEOUT("DATA VALUES DISPLAYED ARE: ")
	S L1=0 F I=0:0 S I=$O(@DATA("TOT")@("XTNM",I)) Q:I'>0  D
	. S TEXT="    "_@DATA("TOT")@("XTNM",I)
	. I $G(@COND("TOT")@(I))'="" S TEXT=TEXT_"   *** condition: "_@COND("TOT")@(I)
	. D WRITEOUT(TEXT)
	. Q
	D WRITEOUT("")
	Q
	;
DATAOUT(DATA,COND,PATH,KBBPSETS,XWID,XWIDT,KBBPPACK,TOTSETS,NODE,XNMAX,PRMRYFIL)	;
	; ZEXCEPT: KBBPYMIN - if present newed and defined in TESTRANG for testing
	N DABASE,FILETYP,I,J,K,KBBPYLCN,KEY,L,LL,MAX,RESULT,VAR,X,XN,XTN,XVAL
	S NFLDS=$O(DATA(" "),-1)
	F DABASE=$S(KBBPYMIN>0:KBBPYMIN-1,1:0):0 S DABASE=$O(@NODE@(DABASE)) Q:DABASE'>0  Q:(KBBPYMAX>0)&(DABASE>KBBPYMAX)  Q:(KBBPYLIM>0)&(DABASE'<KBBPYLIM)  D
	. S KEY=1 K RESULT,XTN
	. F I=1:1 Q:'KEY  Q:'$D(DATA(I))  D
	. . K @DATA(I)@("XVAL") D GET1^KBBPYDO1(DATA(I))
	. . F J=0:0 S J=$O(@DATA(I)@("XTN",J)) Q:J'>0  S XTN(J)=@DATA(I)@("XTN",J)
	. . D FILETYPS(.FILETYP,.PATH,.XTN,PRMRYFIL)
	. . S XVAL=$NA(@DATA(I)@("XVAL"))
	. . I $D(@XVAL) D CHKIT^KBBPYDO1(DATA(I),COND(I)) D
	. . . F J=0:0 S J=$O(@COND(I)@(J)) Q:J'>0  D
	. . . . I '$D(@XVAL@(J)) K @DATA(I)@("XVAL")
	. . . . Q
	. . . Q
	. . S RESULT(I)=$S('$D(@DATA(I)@("XVAL")):0,1:1)
	. . Q
	. S X=0 I KBBPSETS>0 S X=1 F I=1:1:NFLDS D  ;S KEY=KEY+X
	. . F J=1:1 Q:'$D(KBBPSETS(I))  Q:$P(KBBPSETS(I),",",J)=""  I 'RESULT($P(KBBPSETS(I),",",J)) S X=0 Q
	. . Q
	. S KEY=X
	. I KEY D
	. . K @DATA("TOT")@("XVAL") D GET1^KBBPYDO1(DATA("TOT"))
	. . K XTN
	. . F J=0:0 S J=$O(@DATA("TOT")@("XTN",J)) Q:J'>0  S XTN(J)=@DATA("TOT")@("XTN",J)
	. . D FILETYPS(.FILETYP,.PATH,.XTN,PRMRYFIL)
	. . D CHKIT^KBBPYDO1(DATA("TOT"),COND("TOT"))
	. . S XVAL=$NA(@DATA("TOT")@("XVAL")),MAX=@DATA("TOT")@("XT")
	. . S ZZMAX=$$NUMSETS($NA(@DATA("TOT")@("XVAL")),MAX)
	. . F N=1:1:ZZMAX D
	. . . S FOUND=0
	. . . F J=1:1:TOTSETS D  Q:FOUND=1
	. . . . S NY=0
	. . . . F Y=1:1 S Y1=$P(TOTSETS(J),";",Y) Q:Y1=""   I $D(@DATA("TOT")@("XVAL",Y1,N))  D
	. . . . . S NY=NY+1
	. . . . I NY=(Y-1) S FOUND=1
	. . . . Q
	. . . I 'FOUND F J=1:1:MAX K @DATA("TOT")@("XVAL",J,N)
	. . . Q
	. . I TOTSETS>0 S KEY=0 F I=1:1:NFLDS D  S KEY=KEY+X
	. . . S X=1 F J=1:1 Q:'$D(KBBPSETS(I))  Q:$P(KBBPSETS(I),",",J)=""  I 'RESULT($P(KBBPSETS(I),",",J)) S X=0 Q  ; WRITES DEBUG
	. . . Q
	. . I $D(@DATA("TOT")@("XVAL")) D
	. . . D PRNT(.DATA,.XWID,.XWIDT,.FILETYP,XNMAX,KBBPPACK)
	. . . Q
	. . Q
	. Q
	F I=1:1:10,"TOT" S J="VAR"_I K @J S J=$NA(^TMP($J,"DATA"_I)) K @J S J="COND"_I K @J,VAR(I),COND(I),DATA(I)
	Q
	;
FILETYPS(FILETYP,PATH,XTN,PRMRYFIL)	; determine and whether fields are single or multiple relative to primary file
	; FILETYP  - passed by reference - contains indicators of single or multiple for files on return
	; PATH     - passed by reference - contains array of PATHS connecting files
	; XTN      - passed by reference - array containing file info for field
	; PRMRYFIL - input               - primary file number for searches
	N FILE,I,J,K,THISPATH,VAL
	K FILETYP
	F I=1:1 S FILE(I)=+$G(XTN(I)) Q:FILE(I)'>0  D
	. I FILE(I)=PRMRYFIL S FILETYP(I)="SINGLE" Q
	. I '$D(^DIC(FILE(I),0)) S FILETYP(I)="MULTIPLE" Q
	. S FILETYP(I)=""
	. F J=1:1 S THISPATH=$G(PATH(J)) Q:THISPATH=""  D  I FILETYP(I)'="" Q
	. . N FROM,TO,TYPE
	. . S TYPE=$S(THISPATH_U["T"_FILE(I)_U:"T",THISPATH_U["R"_FILE(I)_U:"R",1:"") I TYPE="" Q
	. . F K=1:1:$L(THISPATH,U) D  I FILETYP(I)'="" Q
	. . . S VAL=$P($P(THISPATH,U,K),";",2)
	. . . S FROM(K)=+VAL,TO(K)=$S(VAL["T":$P(VAL,"T",2),1:$P(VAL,"R",2))
	. . . I '$D(^DIC(FROM(K),0))!'$D(^DIC(TO(K),0)) S FILETYP(I)="MULTIPLE" Q
	. . . I TO(K)=FILE(I) S FILETYP(I)="SINGLE"
	. . . Q
	. . Q
	. I FILETYP(I)="" S FILETYP(I)="SINGLE"
	. Q
	Q
	;
NUMSETS(DATA,MAX)	;.EF - returns number of data sets present
	N ZZMAX,ZZ,ZZK
	S ZZMAX=0
	F ZZ=1:1:MAX S ZZK=$O(@DATA@(ZZ,""),-1) I ZZK>ZZMAX S ZZMAX=ZZK
	Q ZZMAX
	;
PRNT(DATA,XWID,XWIDT,FILETYP,XNMAX,KBBPPACK)	;
	N BASEFILE,I,J,FILE,FILES,FILESV,K,KEY,MAXCOLS,MAXNUM,NFILES,NI,NUM,ROOTD,ROOTX,V,VAL,VALS,XLXN
	S ROOTX=$NA(@DATA("TOT")@("XTN")),ROOTD=$NA(@DATA("TOT")@("XVAL"))
	S BASEFILE=@ROOTX@(1)
	S BASEFILE=@ROOTX@(1)
	S NFILES=0,FILE=BASEFILE
	S NUMSETS=$$NUMSETS(ROOTD,XNMAX)
	F I=1:1 Q:'$D(@ROOTX@(I))  D
	. S FILES(I)=+@ROOTX@(I),MAXCOLS=I
	. I FILES(I)'=BASEFILE D
	. . S KEY=0
	. . F J=1:1 Q:'$D(FILESV(J))  I FILESV(J)=FILES(I) S KEY=1 Q
	. . I 'KEY S FILESV(J)=FILES(I),NFILES=NFILES+1
	. . Q
	. Q
	S MAXNUM=1
	S XT=@DATA("TOT")@("XT")
	N XLN
	S XLN="XLN",XLN(0)=0
	F I=1:1:NUMSETS D
	. K VALS
	. S ZCNT=0
	. S SEEN=0 D  I 'SEEN K:I>1 VALS(I) Q
	. . S MAXARG=1
	. . F VAR=1:1:XT D
	. . . I FILETYP(VAR)="SINGLE" S VALS(VAR)=$G(@ROOTD@(VAR,1)) Q
	. . . I '$D(@ROOTD@(VAR,I)) S VALS(VAR)="" Q
	. . . I $D(@ROOTD@(VAR,I))=1 S VALS(VAR)=@ROOTD@(VAR,I),SEEN=1 Q
	. . . F Z=0:0 S Z=$O(@ROOTD@(VAR,I,Z)) Q:Z'>0  S ZCNT=ZCNT+1,VALS(VAR,ZCNT)=@ROOTD@(VAR,I,Z),SEEN=1,MAXARG=ZCNT
	. . . Q
	. . I 'SEEN Q
	. . F Z=1:1:MAXARG D
	. . . S XLXN=XLN(0)+1,XLN(0)=XLXN,XLN(XLXN)=""
	. . . F K=1:1:XT D
	. . . . I $D(VALS(K))=1 D  S XLN(XLXN)=XLN(XLXN)_VALS(K)_U Q
	. . . . . I $P(XWIDT(K),";")="S" F NI=2:1 S VAL=$P(XWIDT(K),";",NI) Q:VAL=""  I $P(VAL,":")=VALS(K) S VALS(K)=$P(VAL,":",2) Q
	. . . . . I XWIDT(K)="D" S VALS(K)=$$FMTE^XLFDT($G(VALS(K)))
	. . . . . Q
	. . . . ;
	. . . . D  S XLN(XLXN)=XLN(XLXN)_$G(VALS(K,Z))_U
	. . . . . I $P(XWIDT(K),";")="S" F NI=2:1 S VAL=$P(XWIDT(K),";",NI) Q:VAL=""  I $P(VAL,":")=VALS(K,Z) S VALS(K,Z)=$P(VAL,":",2) Q
	. . . . . I XWIDT(K)="D" S VALS(K,Z)=$$FMTE^XLFDT($G(VALS(K,Z)))
	. . . Q
	. . Q
	. Q
	D OUTPUT(.XLN,.XWID,.XWIDT,KBBPPACK,XNMAX)
	Q
	;
	;
OUTPUT(XLN,XWID,XWIDT,KBBPPACK,XNMAX)	;
	N I,LINE,NOLINFED,XVALUE,XLV
	S NOLINFED=""
	F I=0:0 S I=$O(@XLN@(I)) Q:I=""  D
	. I $D(@XLN@(I))'=1 S XLV=XLN N XLN S XLN=$NA(@XLV@(I)) D OUTPUT(.XLN,.XWID,.XWIDT,KBBPPACK,XNMAX) Q
	. S XVALUE=@XLN@(I) S LINE=$$SETOUT(.XWID,.XWIDT,XVALUE,KBBPPACK,XNMAX) D WRITEOUT(LINE)
	Q
	;
SETOUT(XWID,XWIDT,XVALUE,KBBPPACK,XNMAX)	; convert input ^-separated data into output form and write out
	N B100,J,NOLINFED,RESULT,ULEN,V,VAL,X,XNL1
	; ^-separated text
	I KBBPPACK=1 Q XVALUE
	; csv for excel
	I KBBPPACK>1 D  Q XNL1
	. S XNL1="" S ULEN=$L(XVALUE,U) F J=1:1:ULEN S VAL=$P(XVALUE,U,J) S XNL1=XNL1_$S(VAL=+VAL:VAL,VAL="":"",1:""""_VAL_"""")_","
	S RESULT="",$P(B100," ",100)=" "
	F J=1:1:XNMAX S V=$P(XVALUE,U,J) D  S RESULT=RESULT_V
	. S V=$E(V_B100,1,$S($L(V)>XWID(J):$L(V),1:XWID(J)))_" "
	. Q
	Q RESULT
	;
WRITEOUT(TEXT,NOLINFED)	; if is in a unit test save to global, otherwise write to device
	; ZEXCEPT: IOF - Kernel value for form feed
	; ZEXCEPT: UTSTGLOB - if present, defined and killed in unit tests
	N LINE
	I $$ISUTEST() D  Q
	. I '$D(UTSTGLOB) S UTSTGLOB=$NA(^TMP("KBBPYROU-OUTPUT",$J))
	. S LINE=+$G(@UTSTGLOB@(0))
	. I (+$G(NOLINFED)=0)!(LINE=0) S LINE=$G(@UTSTGLOB@(0))+1,^(0)=LINE ; ;naked global
	. S @UTSTGLOB@(LINE)=$G(@UTSTGLOB@(LINE))_TEXT
	. Q
	I TEXT="@IOF" W @IOF Q
	I +$G(NOLINFED)=0 W !
	W TEXT
	Q
	;
TESTRANG	; SETS UP A TEST RUN WITH DABASE FROM KBBPYMIN TO KBBPYMAX
	N KBBPYMIN,KBBPYMAX,KBBPYLIM,DIR,Y K DIR,Y S DIR("A")="STARTING INTERNAL ENTRY NUMBER",DIR(0)="N^1" D ^DIR S KBBPYMIN=+Y Q:KBBPYMIN'>0  K DIR S DIR("A")="MAXIMUM INTERNAL ENTRY NUMBER",DIR(0)="N^1" D ^DIR S KBBPYMAX=+Y Q:KBBPYMAX<KBBPYMIN  G EN
	;
TESTMAX	; SETS UP A TEST RUN WITH DABASE FROM 0 TO KBBPYMAX
	N KBBPYMIN,KBBPYMAX,KBBPYLIM,DIRUT,DIR,Y S KBBPYMIN=0,DIR("A")="MAXIMUM INTERNAL ENTRY NUMBER",DIR(0)="N^"_KBBPYMIN D ^DIR Q:$D(DIRUT)  S KBBPYMAX=+Y Q:KBBPYMAX<KBBPYMIN  K DIR,Y G EN
	;
TESTLIM	; SETS UP A TEST RUN TO PRODUCE A MAXIMUM OF KBBPYLIM ENTRIES SELECTED FOR OUTPUT
	N KBBPYMIN,KBBPYMAX,KBBPYLIM,DIR,Y S DIR("A")="MAXIMUM NUMBER OF ENTRIES TO SELECT",DIR(0)="N^1" D ^DIR K DIR S KBBPYLIM=+Y Q:KBBPYLIM<0  G EN
	;
ISUTEST()	; .EF
	I $T(ISUTEST^%ut)="" Q 0
	Q $$ISUTEST^%ut()
	;
FILE	;;200
	;;
PRINT	;;NORMAL
	;;
PATH	;;
	;;
SETS	;;1
	;;1,2,3,
	;;
1	;
	;;.01*200;X[$P(^VA(200,DUZ,0),U)
	;;
2	;
	;;4*200;X=$P(^VA(200,DUZ,1),U,2)
	;;
3	;
	;;.01*200.051;X="XUPROGMODE"
	;;
TOTSETS	;;1
	;;1,2,3
	;;
TOT	;
	;;.01*200;
	;;4*200;
	;;.01*200.051;X["XU"
	;;
