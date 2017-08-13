KBBPYSET	;ISC-SF/JLI - SELECT AND SET UP FIELDS RELATED TO PRIMARY FILE ;04/28/17  20:03;
	;;0.0;KBBPTOOLS;;
	;A6AXFSET ;ISC-SF/JLI - SELECT AND SET UP FIELDS RELATED TO PRIMARY FILE ;2/25/94  12:05 ;
	;;2.0; ;;May 15, 1992
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" Q
	D EN^%ut("KBBPUYNM")
	Q
	;
GETFILE(KBBPFIL,KBBPFILN,KBBPI)	; called from EN^KBBPYQRY
	; KBBPFIL  - passed by reference - contains file number on return (or zero)
	; KBBPFILN - passed by reference - contains file name on return
	; KBBPI    - passed by reference -
	N KBBPPT,KBBPPTA,KBBPPTB,DIC,DIR,X1H,X2H,Y,ISUTEST,X
	S ISUTEST=$$ISUTEST()
	K KBBPPT,KBBPPTA,KBBPPTB,^TMP("KBBPY",$J,"B"),^("C"),^("F"),^("Z"),^("X") ; naked globals
	S KBBPI=0
	I ISUTEST S X=200
	S DIC=1,DIC(0)=$S(ISUTEST:"EQ",1:"AEQM"),DIC("A")="Select Primary File: ",DIC("S")="I +Y'<2"
	D ^DIC
	K DIC
	I Y'>0 Q 0
	S KBBPFIL=+Y,KBBPFILN=$P(Y,U,2)
	S DIR(0)="Y",DIR("A")=$$INDENT(5)_"Is this the correct file",DIR("B")="YES" D:'$$ISUTEST() ^DIR D WRITEIT("")
	I 'Y D GETFILE(.KBBPFIL,.KBBPFILN,.KBBPI) Q
	K KBBPPT
	D SETGLOBS(KBBPFIL)
	Q
	;
GETFLDS(KBBPI,KBBPEXIT,KBBPFIL,KBBPQRY,OUTPUT)	;.EF - also called from EN^KBBPYQRY
	; ZEXCEPT: UTSTX - if present, set in newed and set in unit test routines
	;GETSET(KBBPQRY) ; .EF - RETURNS null for no further input or ^ for exit
	; X - passed by reference -
	; KBBPI - passed by reference
	; KBBPEXIT - passed by reference
	; KBBPFIL - primary file number
	; KBBPQRY - indicates if called from KBBPYQRY
	N ISUTEST,KBBPA,KBBPB,DIC,X1,XA,F,Y,XF,UTSTCNT,KBBPCOND,YSEL
NXT	;
FLDLOOP	;
	S OUTPUT=+$G(OUTPUT)
	S ISUTEST=$$ISUTEST()
	I ISUTEST S X=$G(UTSTX) S:'$D(UTSTCNT) UTSTCNT=0 I KBBPFIL=0 S X="@",^TMP("KBBPY",$J,"X",1)="@" Q 1 ;
	I ISUTEST,OUTPUT K KBBPCOND S UTSTCNT=UTSTCNT+1,X=$G(UTSTX(UTSTCNT)) S:$D(UTSTCOND(UTSTCNT)) KBBPCOND=UTSTCOND(UTSTCNT)
	I 'ISUTEST R !!!,"Field name: ",X:DTIME I '$T S KBBPEXIT=1 Q ""
	I X[U S KBBPEXIT=1 Q U
	I X="" Q X
	I X["?" D HUHFIELD G FLDLOOP
	I X="$" D SHOFILS G FLDLOOP
	; data present to identify
	S X=$$UP^XLFSTR(X) ; convert all to upper case
	K ^TMP("KBBPY",$J,"F"),^("Z")
	S XF=""
	; if file included, get file number
	I X["@" D GETFILID(.X,.XF) I XF'>0 G NXT
	S FLDNAME=X,PFILENUM=XF
	D LOOK(KBBPFIL,FLDNAME,PFILENUM,0,0)
	D LOOK(KBBPFIL,FLDNAME,PFILENUM,1,0)
	D SELCT^KBBPYSE1(.YSEL,.KBBPI,KBBPFIL,KBBPQRY,OUTPUT)
	I ISUTEST,'OUTPUT Q 1
	I ISUTEST,UTSTCNT=4 Q 1
	G NXT
	Q
	;
GETFILID(X,XF)	; get and return file number, if included as part of filename input
	; X  - passed by reference - on input contains fieldname@fileid
	;        fileid may be full file number or file name or at least part of it
	;                          - on return contains fieldname (or at least part of it)
	; XF - passed by reference - on return contains file number for field specified
	N XA,DIC,Y
	S XA=X,X=$P(X,"@",2),DIC(0)="EQ",DIC=1
	; if file name not entered look it up or have user choose it from options
	I X'=+X W:'$$ISUTEST() !,"Selecting File to be searched  " D ^DIC ; get name of file
	I X'=+X S X=+Y
	S XF=+X,X=$P(XA,"@")
	Q
	;
HUHFIELD	;
HUH	; responses for user asking for information
	N ISUTEST,DIR,DIRUT
	D WRITEIT("")
	F I=1:1 S TEXT=$P($T(@("HUHTEXT+"_I)),";",3,99) Q:TEXT=""  D WRITEIT(TEXT)
	Q
	;
LOOK(KBBPFIL,FLDNAME,PFILENUM,KBBPA,KBBPB)	;
	N L,I,FLDNAME1
	S FLDNAME1=FLDNAME,L=$L(FLDNAME1) I FLDNAME'="",$D(^XTMP("KBBPY","XR1",FLDNAME)) D JLOOP(KBBPFIL,FLDNAME,XF,KBBPA,KBBPB)
	F I=0:0 S FLDNAME=$O(^XTMP("KBBPY","XR1",FLDNAME)) Q:FLDNAME=""!($E(FLDNAME,1,L)'=FLDNAME1)  D JLOOP(KBBPFIL,FLDNAME,XF,KBBPA,KBBPB)
	;Q FLDNAME1
	Q
	;
JLOOP(KBBPFIL,FLDNAME,PFILENUM,KBBPA,KBBPB)	;
	N CFILENUM ; current file number
	F CFILENUM=0:0 S CFILENUM=$O(^XTMP("KBBPY","XR1",FLDNAME,CFILENUM)) Q:CFILENUM'>0  D
	. I (PFILENUM=""!(CFILENUM=PFILENUM)),$S((CFILENUM=KBBPFIL&'KBBPA):1,(CFILENUM'=KBBPFIL&KBBPA):1,1:0) D JLOOP1(KBBPFIL,CFILENUM,FLDNAME,KBBPA,.KBBPB)
	Q
	;
JLOOP1(KBBPFIL,CFILENUM,FLDNAME,KBBPA,KBBPB)	;
	N VAL,M,CFLDNUM,CSUBFIL,N,FLDNAME2,XM,Z
	S Z=0
	S CONNECTED=(CFILENUM=KBBPFIL!$D(^TMP("KBBPY",$J,"PNT",CFILENUM)))
	I CONNECTED D
	. F CSUBFIL=0:0 S CSUBFIL=$O(^XTMP("KBBPY","XR1",FLDNAME,CFILENUM,CSUBFIL)) Q:CSUBFIL'>0  D
	. . S N=$O(^XTMP("KBBPY","XR1",FLDNAME,CFILENUM,CSUBFIL,0)),XM=^(N) ; naked global
	. . S:'KBBPA ^TMP("KBBPY",$J,"Z",FLDNAME,CSUBFIL,N)=XM I KBBPA S FLDNAME2=FLDNAME I $P($P(XM,U),";",2)'=0 S ^TMP("KBBPY",$J,"F",CFILENUM,FLDNAME2,CSUBFIL,N)=XM,Z=1
	. . Q
	. Q
	S:Z KBBPB=1
	Q
	;
SUBFIL	;
	S Y=+$P(^DD(M,N,0),U,2),Y1=$P(^DD(Y,.01,0),U) S ^TMP("KBBPY",$J,"F",J,Y1,Y,.01)=^XTMP("KBBPY","XR1",Y1,J,Y,.01),X2=X_" subfile"
	Q
	;
SHOFILS(KBBPFIL)	;
	; ZEXCEPT: UTSTFILE,UTSTFIRST,UTSTLAST,UTSTSUBS - variables if present newed and set in unit test routines
	N DIR,I,ISUTEST,KBBPFRST,KBBPLAST,KBBPSUBS,X,Y,STATUS
	S ISUTEST=$$ISUTEST
	I ISUTEST S X=$S($D(UTSTFIRST):UTSTFIRST,1:200),KBBPFIL=$S($D(UTSTFILE):UTSTFILE,1:200) D SETGLOBS(KBBPFIL)
	I 'ISUTEST R !!,"Show Related Files Starting with Number: FIRST// ",X:DTIME Q:'$T!(X[U)
	S:X="" X=0 I +X'=X D WRITEIT($C(7)_"Enter a NUMBER to start the file range") G SHOFILS
	S:X>0 X=X-.0000001 S KBBPFRST=X
LAST	; fall through
	I 'ISUTEST D WRITEIT(""),WRITEIT($$INDENT(5)_"And Ending with: LAST// ") R X:DTIME Q:'$T!(X[U)
	I ISUTEST S X=$S($D(UTSTLAST):UTSTLAST,1:KBBPFRST+1)
	S:X="" X=0 I +X'=X D WRITEIT($C(7)_"Enter a NUMBER to end the file range") G LAST
	S KBBPLAST=X
	S DIR(0)="Y",DIR("A")="List ONLY FILES (not sub-files) ?",DIR("B")="YES"
	I 'ISUTEST D ^DIR
	K DIR
	S KBBPSUBS=$S(ISUTEST:$S($D(UTSTSUBS):UTSTSUBS,1:0),1:'Y)
	S I=$S($D(^TMP("KBBPY",$J,"FIL")):^("FIL"),1:KBBPFRST)
	D WRITEIT("@IOF")
	I I>KBBPFRST,$S(KBBPLAST=0:1,1:I'>KBBPLAST) D WRITEIT(""),WRITEIT("Primary File:") Q:$$BLDSTK(KBBPSUBS,I)
	S I=KBBPFRST
	F  S I=$O(^TMP("KBBPY",$J,"PNT",I)) Q:$S(I'>0:1,KBBPLAST=0:0,1:I>KBBPLAST)  I $D(^DIC(I,0)) S STATUS=$$BLDSTK(KBBPSUBS,I) Q:STATUS
	Q
	;
BLDSTK(KBBPSUBS,I)	; .EF
	N J,K,LEV
	K ^TMP("KBBPY",$J,"SHOF")
	S ^TMP("KBBPY",$J,"SHOF",I)=1
	I KBBPSUBS F J=0:0 S J=$O(^TMP("KBBPY",$J,"SHOF",J)) Q:J'>0  D
	. S LEV=^TMP("KBBPY",$J,"SHOF",J)
	. F K=0:0 S K=$O(^DD(J,"SB",K)) Q:K'>0  D
	. . I $D(^TMP("KBBPY",$J,"VW",I))>1 N X1 D  Q:'X1
	. . . S X1=1 I '$D(^TMP("KBBPY",$J,"VW",K)) S X1=0
	. . I $S('$D(^DD(K,.01,0)):1,$P(^(0),U,2)="W":0,1:1) S ^TMP("KBBPY",$J,"SHOF",J,K)=LEV+1,^TMP("KBBPY",$J,"SHOF",K)=LEV+1
	S LEV=0,J=$O(^TMP("KBBPY",$J,"SHOF",0)) D:KBBPSUBS WRITEIT("") Q:$$SHOWEM(LEV,J) 1
	Q 0
	;
SHOWEM(LEV,J)	;.EF
	; ZEXCEPT: IOSL - Kernel IO variable
	; ZEXCEPT: LINESOUT - unit test variable newed in unit tests
	N LEVBLNK,K,Y,STATUS
	S LEVBLNK=$$INDENT(3*LEV)
	D WRITEIT(LEVBLNK_"#"_J_"  "_$S($D(^DIC(J,0)):$P(^(0),U),1:$O(^DD(J,0,"NM","")))_" "_$S('LEV:"FILE",1:"SUB-FILE")_" .01 field is "_$S('$D(^DD(J,.01,0)):" *** UNDEFINED ***",1:"'"_$P(^DD(J,.01,0),U)_"'")) ; naked global
	I LINESOUT>(IOSL-6) D  I Y[U Q 1
	. S Y=U
	. D WRITEIT("") D WRITEIT($$INDENT(10)_"RETURN to continue OR  Enter '^' to stop listing: ")
	. I '$$ISUTEST R Y:DTIME I '$T!(Y[U) S Y=U Q
	. D WRITEIT("@IOF")
	S STATUS=0
	F K=0:0 S K=$O(^TMP("KBBPY",$J,"SHOF",J,K)) Q:K'>0  S LEV=LEV+1,J(LEV)=J,J=K S STATUS=$$SHOWEM(LEV,J) Q:STATUS  S K=J,J=J(LEV),LEV=LEV-1 K J(LEV+1)
	I STATUS Q 1
	K ^TMP("KBBPY",$J,"SHOF",J)
	Q 0
	;
SETGLOBS(KBBPFIL)	;
	N X1H,X2H
	I '$D(^XTMP("KBBPY","XR1"))!'$D(^XTMP("KBBPY","FNM")) D
	. D WRITEIT(""),WRITEIT($C(7)_"First I have to rebuild the field cross-reference file")
	. D WRITEIT($$INDENT(10)_"This may take a little bit..."),WRITEIT(""),WRITEIT("")
	. D QUE^KBBPYREF
	. Q
	I $G(KBBPFIL)'="",$S('$D(^TMP("KBBPY",$J,"FIL")):1,^("FIL")'=KBBPFIL:1,'$D(^TMP("KBBPY",$J,"PNT")):1,1:0) D  ; naked global
	. ; build a table of relationships between files related to the primary file and show time in seconds
	. S X1H=$H
	. D XRPNT^KBBPYNM(KBBPFIL)
	. S X2H=$H
	. S X2H=$P(X2H,",",2)-$P(X1H,",",2)
	. D WRITEIT(X2H_" second"_$S(X2H=1:"",1:"s"))
	Q
	;
INDENT(NUMBER)	;.EF
	N VALUE
	S VALUE=""
	S $P(VALUE," ",NUMBER)=" "
	Q VALUE
	;
ISUTEST()	;
	I $T(^%ut)="" Q 0
	Q $$ISUTEST^%ut()
	;
WRITEIT(TEXT,NOLINFED)	;
	; ZEXCEPT: LINESOUT - newed and/or killed  in unit test routines
	S NOLINFED=+$G(NOLINFED)
	I 'NOLINFED S LINESOUT=$G(LINESOUT)+1
	I TEXT="@IOF" S LINESOUT=0
	D WRITEOUT^KBBPYROU(TEXT,$G(NOLINFED))
	Q
	;
HUHTEXT	;
	;;Enter all or a part of a FIELD NAME.
	;;This will generate a list of all fields beginning with the specified
	;;characters that can be reached from the primary file.  If desired The
	;;characters may be followed by the '@' character and a FILE NAME (or
	;;FILE NUMBER) to limit the field selection to the indicated file.
	;;For example, if the PATIENT file (#2) is first selected as the primary
	;;file, entry of  DATE@PRES  will first ask the user to select from the
	;;related files which begin with PRES, and once one of these is selected,
	;;it will then list those fields beginning with DATE in the selected file
	;;so that one can be selected.
	;;.
	;;Enter $ to get a listing of related files which are available.
	;;You will be asked to select a file number range to be listed, these
	;;can be the first to the last.  You may then select to include a listing
	;;of sub-files as well.  If subfiles are included, they will be listed
	;;under the main file indented for each level of sub-file.
	;
EOR	;
