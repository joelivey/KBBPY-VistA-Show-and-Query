KBBPYSHO	;ISC-SF/JLI - SHOW KEY STRUCTURE AND VARIABLES FOR FILE ;03/10/16  16:14;
	;;0.0;KBBPTOOLS;;May 15, 1992
	;
	;A6AXFSHO ;ISC-SF/JLI - SHOW KEY STRUCTURE AND VARIABLES FOR FILE ;2/25/94  12:06 ;
	;;0.0; ;;May 15, 1992
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" G EN
	N X
	R !,"Press Enter NOW to run the application or wait 5 seconds to run the Unit Tests: ",X:5 I $T G EN
	W !,"Running Unit Tests",!
	D EN^%ut("KBBPUYNM")
	W !!,"To run the KBBPYQRY routine itself use the command DO EN^KBBPYQRY"
	Q
	;
EN	;
	; ZEXCEPT: UTESTVAL,UTESTFIL,UTESTNUM - newed in unit test set only for unit test
	N KBBP1,KBBPFIL,KBBPFN,KBBPI,KBBPY,ISUTEST
	S ISUTEST=$$ISUTEST^%ut()
	I '$D(^XTMP("KBBPY","FNM"))!'$D(^XTMP("KBBPY","XR1")) D
	. D WRITEIT(""),WRITEIT($C(7)_"First I have to rebuild the field cross-reference file")
	. D WRITEIT("          THIS MAY TAKE A WHILE")
	. D WRITEIT(""),WRITEIT("")
	. D QUE^KBBPYREF
	. Q
	;
	S KBBPFIL=$S($D(UTESTFIL):UTESTFIL,1:200)
	I 'ISUTEST R !,"Select File: ",KBBPFIL:DTIME I '$T!(KBBPFIL="")!(KBBPFIL[U) K ^TMP("KBBPY",$J) Q
	I KBBPFIL'=+KBBPFIL S KBBP1=$E(KBBPFIL,1,$L(KBBPFIL)-1)_$C($A($E(KBBPFIL,$L(KBBPFIL)))-1)_"z"
	K ^TMP("KBBPY",$J,"SHO")
	I KBBPFIL'=+KBBPFIL F KBBPI=0:0 S KBBP1=$O(^XTMP("KBBPY","FNM",KBBP1)) Q:$E(KBBP1,1,$L(KBBPFIL))'=KBBPFIL  F KBBPY=0:0 S KBBPY=$O(^XTMP("KBBPY","FNM",KBBP1,KBBPY)) Q:KBBPY'>0  S KBBPI=KBBPI+1,^TMP("KBBPY",$J,"SHO",KBBPI)=KBBPY_U_KBBP1
	I KBBPFIL=+KBBPFIL S KBBPY=KBBPFIL-.0000001 F KBBPI=0:0 S KBBPY=$O(^DD(KBBPY)) Q:$E(KBBPY,1,$L(KBBPFIL))'=KBBPFIL  I $D(^DD(KBBPY,0))#2 D SET1(KBBPY,.KBBPI)
	I KBBPI=0 D WRITEIT($C(7)_"  ??",1) G EN
	S KBBPFN=0
	I KBBPI=1 S KBBPFN=1
	I KBBPI>1 D
	. D WRITEIT("     "_"Select From:")
	. F KBBPY=1:1:KBBPI D  Q:KBBPFN>0!(KBBPFN=U)
	. . D WRITEIT("       "_$J(KBBPY,3)_".  "_$P(^TMP("KBBPY",$J,"SHO",KBBPY),U,2)_"  ("_+^(KBBPY)_")")
	. . I '(KBBPY#10)!(KBBPY=KBBPI) S KBBPFN=$$SELCT(KBBPY,KBBPI)
	. . I ISUTEST S KBBPFN=1
	. . Q
	. Q
	I KBBPFN'>0 G EN
	I ISUTEST S UTESTVAL=KBBPFN,UTESTNUM=$G(^TMP("KBBPY",$J,"SHO",KBBPFN))
	D SHOW^KBBPYSH1(KBBPFN)
	D WRITEIT(""),WRITEIT("")
	I ISUTEST Q
	G EN
	;
	;
SET1(KBBPY,KBBPI)	;
	N KBBP1
	S KBBP1=$S($D(^DD(KBBPY,0,"NM")):$O(^("NM","")),1:$P(^DD(KBBPY,0),U))
	I $D(^XTMP("KBBPY","FNM",KBBP1,KBBPY)) S KBBPI=KBBPI+1,^TMP("KBBPY",$J,"SHO",KBBPI)=KBBPY_U_KBBP1
	Q
	;
SELCT(KBBPY,KBBPI)	;.EF
	N KBBPFN,BLNKS,TEXT
	I KBBPI=1 S KBBPFN=1
	I KBBPI>1 D
	. D WRITEIT(""),WRITEIT("")
	. S $P(BLNKS," ",15)=" "
	. I KBBPY<KBBPI S TEXT=BLNKS_"Enter '^' to STOP or" D WRITEIT(TEXT)
	. S TEXT=BLNKS_"Select 1 to "_KBBPY_": " D WRITEIT(TEXT)
	. I $$ISUTEST() S KBBPFN=1
	. I '$$ISUTEST() R KBBPFN:DTIME S:'$T!(KBBPFN[U) KBBPFN=U Q
	. S:KBBPFN>KBBPY!(KBBPFN<0) KBBPFN=0
	. I (KBBPFN\1)'=KBBPFN D WRITEIT($C(7)_"  ??",1) D SELCT(KBBPY,KBBPI)
	. Q
	Q KBBPFN
	;
WRITEIT(TEXT,NOLINFED)	;
	D WRITEOUT^KBBPYROU(TEXT,$G(NOLINFED))
	Q
	;
ISUTEST()	;
	I $T(EN^%ut)="" Q 0
	Q $$ISUTEST^%ut()
	;
EOR	; End Of Routine