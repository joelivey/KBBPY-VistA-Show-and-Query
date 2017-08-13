KBBPYSE1	;ISC-SF/JLI - SELECT AND SET UP FIELDS RELATED TO PRIMARY FILE ;04/28/17  18:56;
	;;2.0;KBBP;;May 15, 1992
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" Q
	D EN^%ut("KBBPUYNM")
	Q
	;
SELCT(YSEL,KBBPI,KBBPFIL,KBBPQRY,OUTPUT)	;
	; ZEXCEPT: UTSTCOND - Newed and set by unit tests if present
	N C,I,IC,J,K,KBBPPTA,M,N,X,Z,ROOT
	N CFILENUM,COUNT,CURFILEN,CURNUM,DATA,FLDNAME,LSTFILEN
	S ROOT=$NA(^TMP("KBBPY",$J))
	K @ROOT@("A")
	S FLDNAME="",I=0
	F K=0:0 S FLDNAME=$O(@ROOT@("Z",FLDNAME)) Q:FLDNAME=""  D
	. F CFILENUM=0:0 S CFILENUM=$O(@ROOT@("Z",FLDNAME,CFILENUM)) Q:CFILENUM'>0  D
	. . F N=0:0 S N=$O(@ROOT@("Z",FLDNAME,CFILENUM,N)) Q:N'>0  D
	. . . S Z=@ROOT@("Z",FLDNAME,CFILENUM,N) K ^(N) ; naked global
	. . . I $P($P(Z,U),";",2)'=0 S I=I+1,@ROOT@("A",I,KBBPFIL)=FLDNAME_U_CFILENUM_U_N_U_Z
	. . . Q
	. . Q
	. Q
	F J=0:0 S J=$O(@ROOT@("F",J)) Q:J'>0  D
	. S FLDNAME=""
	. F K=0:0 S FLDNAME=$O(@ROOT@("F",J,FLDNAME)) Q:FLDNAME=""  D
	. . F CFILENUM=0:0 S CFILENUM=$O(@ROOT@("F",J,FLDNAME,CFILENUM)) Q:CFILENUM'>0  D SELCT1(.I,J,CFILENUM,FLDNAME)
	. . Q
	. Q
	S COUNT=0,LSTFILEN=KBBPFIL,YSEL=0
	F CURRNUM=1:1 Q:'$D(@ROOT@("A",CURRNUM))  D  Q:YSEL'=0
	. S CURFILEN=$O(@ROOT@("A",CURRNUM,0)) Q:CURFILEN'>0  S DATA=@ROOT@("A",CURRNUM,CURFILEN) D LIST(.COUNT,.LSTFILEN,.YSEL,CURRNUM,CURFILEN,DATA)
	. Q
	I YSEL=0 D ASKU(.YSEL,COUNT,I)
	I YSEL>0 D
	. S J=$O(@ROOT@("A",YSEL,0)) I J'=KBBPFIL,'$D(@ROOT@("C",J)) D CONECT Q:(YSEL[U)!(YSEL'>0)
	. S KBBPI=KBBPI+1,@ROOT@("X",KBBPI,J)=@ROOT@("A",YSEL,J) F  D  Q:X'=-1
	. . S X=""
	. . I $$ISUTEST() S X=$G(UTSTCOND(KBBPI))
	. . I $D(KBBPQRY) D
	. . . D WRITEIT("Code for condition on X: ")
	. . . I '$$ISUTEST() R X:DTIME I '$T S X=-1 Q
	. . . I X'="",X'["X" D WRITEIT(""),WRITEIT("A condition must be on the variable X"),WRITEIT("") S X=-1
	. . . I X["""",'($L(X,"""")#2) D WRITEIT(""),WRITEIT("The condition appears to contain unmatched quote marks!") S X=-1
	. . . I X="",'OUTPUT D WRITEIT("A condition should be specified on the search variables") S X=-1
	. . . I X'=-1,X'="" S $P(@ROOT@("X",KBBPI,J),U,10)=X
	. . . Q
	. . Q
	. Q
	K KBBPPTA,@ROOT@("A")
	Q
	;
CONECT	;
	; ZEXCEPT: IOSL - Kernel value for number of lines on screen or page
	; ZEXCEPT: YSEL,ROOT,J -
	N C,I1,K,LINES,M,NLINES,X,XM,XN,YSEL1
	S K=0 F M=0:0 S M=$O(@ROOT@("PNT",J,M)) Q:M'>0  S K=M
	I K=1 S @ROOT@("C",J)=@ROOT@("PNT",J,1),C=1 D XPAND Q
	D WRITEIT("Select Method of Connection Between Files ("_K_" choices) from:")
	S YSEL1=YSEL,YSEL=0,XN=0,XM=0,NLINES=0
	F M=1:1:K Q:(YSEL[U)!YSEL  D
	. S XM=XM+1 D WRITEIT("") S X=@ROOT@("PNT",J,M) D PLIST D  Q:(YSEL[U)!YSEL
	. I (NLINES+XM)>(IOSL-2) S M=M-1 D ASKC Q:(YSEL[U)!YSEL  S M=M+1,XM=0
	. F I1=1:1:NLINES D WRITEIT(LINES(I1)) S XM=XM+1
	. S NLINES=0
	. Q
	I YSEL[U Q
	I 'YSEL D ASKC
	Q
	;
ASKC	;
	; ZEXCEPT M,K,YSEL,YSEL1
	N XN,C
	S XN=0
	D WRITEIT("          Select Method of Connection (1 to "_M_$S(M'=K:" out of "_K,1:"")_"): ")
	I $$ISUTEST() S C=1
	I '$$ISUTEST R C:DTIME I '$T!(C[U) S YSEL=U Q
	I +C'=C!(C>K)!(C<0) S C=0 I M=K D WRITEIT("  ??",1) G ASKC
	I C>0 S YSEL=YSEL1,@ROOT@("C",J)=@ROOT@("PNT",J,C) D XPAND
	S:C=0 YSEL=0
	Q
	;
XPAND	;
	; ZEXCEPT: C,J
	N X,M,X1,X0
	Q:C'>0
	S X=@ROOT@("PNT",J,C)
	F M=1:1 S X1=$P(X,U,M) Q:X1=""  S X0=+$P(X1,"T",2),X0=$S(X0>0:X0,1:+$P(X1,"R",2)) I X0>0,'$D(@ROOT@("C",X0)) S ^(X0)=$P(X,U,1,M)
	Q
	;
PLIST	;
	; ZEXCEPT: YSEL
	N M1,X1,X0,X2,X3
	F M1=1:1 Q:YSEL  S X1=$P(X,U,M1) Q:X1=""  S X0=+$P(X1,";",2),X2=$P(X1,";"),X3=$P(X1,"T",2),X3=$S(X3'="":+X3,(X1["P"):+$P(X1,"P",2),1:+$P(X1,"R",2)) D PLIST1(X1,X0,X2,X3)
	Q
	;
PLIST1(X1,X0,X2,X3)	;
	; ZEXCEPT: IOM - Kernel variable for print margin
	; ZEXCEPT: LINES
	N JI,M12,MSPAC,REVERSE,X4,X5
	S REVERSE=$P(X1,"R",2)>0
	S MSPAC=$$INDENT^KBBPYSET(IOM+1)
	S X4=X0
	F M12=0:0 Q:'$D(^DD(X4,0,"UP"))  S X4=^DD(X4,0,"UP"),X5=$O(^DD(X4,0,"NM",""))
	S XP=$S(M1>1:$E(MSPAC,1,$S(M1=2:7,1:M1-2*3+7)),1:$J(M,2)_"  ")_$C(96+M1)_". "_$P(^DD(X0,X2,0),U)_" field of the " S:X0'=X4 XP=XP_X5_" sub-file (#"_X0_") of the "
	S XP=XP_$P(^DIC(X4,0),U)_" file "_$S(REVERSE:"is POINTED TO by ",1:"POINTS to ")_"the "_$P(^DIC(X3,0),U)_" file ("_X3_")"
PLX	; falls through and loops from below
	I $L(XP)<IOM S NLINES=NLINES+1,LINES(NLINES)=XP Q
	F JI=IOM:-1 I $E(XP,JI)=" " S NLINES=NLINES+1,LINES(NLINES)=$E(XP,1,JI-1),XP=$E(MSPAC,1,M1+1*5)_$E(XP,JI+1,$L(XP)) Q
	G PLX
	Q
	;
LIST(LINCOUNT,LSTFILEN,YSEL,CURRNUM,CURFILEN,DATA)	;
	; COUNT - passed by reference - count of lines
	; LSTFILEN - passed by reference - last file number - updated when file numbers change and line space is added
	; YSEL     - passed by reference - contains the selected item number or zero on return
	; CURRNUM   -
	; ZEXCEPT: IOM,IOSL - Kernel variables for margin and page length
	N FLDNAME,FILNUM,FILNUMS,I,SUBFILN,X1,X2,XCNT,XLIN,Y,N
	I LSTFILEN'=CURFILEN D WRITEIT("") S LINCOUNT=LINCOUNT+1,LSTFILEN=CURFILEN
	S FLDNAME=$P(DATA,U),SUBFILN=$P(DATA,U,2),N=$P(DATA,U,3),FILNUMS=$P(DATA,U,6),Y=$P(DATA,U,4,99)
	I '$D(^DIC(CURFILEN,0))#2 Q  ; only use fields from real files
	S XCNT=0,X1=$J(CURRNUM,3)_"  "_FLDNAME_"   in  "
	F FILNUM=1:1:$L(FILNUMS,";")-1,0 D
	. I FILNUM=0 S X2="'"_$P(^DIC(CURFILEN,0),U)_"'  file"
	. I FILNUM>0 S X2="'"_$O(^DD($P(FILNUMS,";",FILNUM),0,"NM",""))_"'  sub-file of  "
	. I $L(X1_X2)>IOM S XCNT=XCNT+1,XLIN(XCNT)=X1,X1="          "
	. S X1=X1_X2
	. I FILNUM=0 S XCNT=XCNT+1,XLIN(XCNT)=X1
	. Q
	I (LINCOUNT+XCNT)'<(IOSL-1) D ASKU(.YSEL,.LINCOUNT,CURRNUM-1) Q:YSEL'=0
	F I=1:1:XCNT D WRITEIT(XLIN(I)) S LINCOUNT=LINCOUNT+1
	Q
	;
ASKU(YSEL,LINCOUNT,CURRNUM)	; Ask user to select an entry from list
	; YSEL     - passed by reference - contains selection or -1 if no valid selection
	; LINCOUNT - passed by reference - current number of lines on current screen - set to zero for new page
	; CURRNUM  - input - entry number for last entry shown to user
	N TOTALNUM,TEXT
	S LINCOUNT=0
	I CURRNUM=1 S YSEL=1 Q
	I CURRNUM'>0 D WRITEIT("     No matching entries found") S YSEL=-1 Q
	S TOTALNUM=$O(@ROOT@("A",""),-1)
	D WRITEIT("")
	S TEXT=""
	I CURRNUM<TOTALNUM S TEXT=TEXT_"RETURN to continue OR  "
	D WRITEIT($$INDENT^KBBPYSET(10)_TEXT_"Enter '^' to exit OR")
	D WRITEIT($$INDENT^KBBPYSET(10)_"Select (1 to "_CURRNUM_" [out of "_TOTALNUM_" entries]) : ")
	I '$$ISUTEST R YSEL:DTIME I '$T!(YSEL[U) S YSEL=-1 Q
	I $$ISUTEST() S YSEL=1 D WRITEIT(1)
	S YSEL=+YSEL I YSEL<0!(YSEL'<CURRNUM) D WRITEIT($C(7)_"  ??") D ASKU(.YSEL,.LINCOUNT,CURRNUM)
	Q
	;
SELCT1(I,J,M,X)	;
	; I - passed by reference - is a counter
	N N,Z
	F N=0:0 S N=$O(@ROOT@("F",J,X,M,N)) Q:N'>0  S Z=@ROOT@("F",J,X,M,N) K @ROOT@("F",J,X,M,N) S I=I+1,@ROOT@("A",I,J)=X_U_M_U_N_U_Z
	Q
	;
WRITEIT(TEXT,NOLINFED)	;
	; ZEXCEPT: LINESOUT - newed and/or killed  in unit test routines
	S NOLINFED=+$G(NOLINFED)
	I 'NOLINFED S LINESOUT=$G(LINESOUT)+1
	I TEXT="@IOF" S LINESOUT=0
	D WRITEOUT^KBBPYROU(TEXT,$G(NOLINFED))
	Q
	;
ISUTEST()	;
	I $T(ISUTEST^%ut)="" Q 0
	Q $$ISUTEST^%ut()
