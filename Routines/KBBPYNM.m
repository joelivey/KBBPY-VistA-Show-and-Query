KBBPYNM	;SF-ISC.JLI -- SET UP SQL CROSS-REFS ON VARIABLES FOR DATA DICTIONARY ;03/14/16  20:13;
	;;0.0;KBBPTOOLS;;
	;A6AXFNM ;SF-ISC.JLI -- SET UP SQL CROSS-REFS ON VARIABLES FOR DATA DICTIONARY ;3/1/94  13:46 ;
	;;2.0; ;;May 15, 1992
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" Q
	D EN^%ut("KBBPUYNM")
	Q
	;
BUILD	;
	N %DT,KBBPI,KBBPFIL,X,Y
	I $G(^XTMP("KBBPY","FNM"))'="" Q
	S ^XTMP("KBBPY","FNM")=$H
	S KBBPFIL=1 F KBBPI=0:0 S ^XTMP("KBBPY","FNM")=$H S KBBPFIL=$O(^DD(KBBPFIL)) Q:KBBPFIL'>0  D KBBPFIL(KBBPFIL)
	S X="N",%DT="T" D ^%DT S ^XTMP("KBBPY","FNM")=Y K %DT,X,Y
	;K KBBP1,KBBP2,KBBP3,KBBPANM,KBBPDIC,KBBPFIL,KBBPFLD,KBBPI,KBBPJ,KBBPK,KBBPM,KBBPNM,KBBPNN,KBBPNOD,KBBPT,KBBPTOP,KBBPXK,KBBPXREF,KBBPXVAL
	Q
	;
KBBPFIL(KBBPFIL)	; get filename and x-refs for one file (may be a subfile) entry
	I '($D(^DD(KBBPFIL,0))#2) QUIT
	I (KBBPFIL=2.98)!(KBBPFIL=2.001) ;W !,"KBBPFIL=",KBBPFIL,"  KBBPFIL IS HERE"
	N KBBP1,KBBP2,KBBPDIC,KBBPFLD,KBBPJ,KBBPNM,KBBPNN,KBBPNOD,KBBPT,KBBPTOP
	S KBBP2="",(KBBPTOP,KBBP1,KBBPT)=KBBPFIL,KBBPNOD="",KBBPNN=0
	; if a subfile, determine order of stack
	F KBBPJ=0:0 S KBBP2=$S(KBBP2'="":KBBP2_";",1:"")_KBBP1 Q:'$D(^DD(KBBP1,0,"UP"))  S (KBBPTOP,KBBP1)=^("UP") I $O(^DD(KBBP1,"SB",KBBPT,0))>0 D KBBPFIL1(.KBBPT,.KBBPNOD,.KBBPNN,KBBP1) ; naked global reference
	S KBBPDIC=$S($D(^DIC(KBBPTOP,0,"GL")):^("GL"),1:"")
	S KBBPNM=$S($D(^DD(KBBPFIL,0,"NM")):$O(^("NM","")),1:$P(^DD(KBBPFIL,0),U)),^XTMP("KBBPY","FNM",KBBPNM,KBBPFIL)=KBBP2_U_KBBPNOD
	; get information for each field
	F KBBPFLD=0:0 S KBBPFLD=$O(^DD(KBBPFIL,KBBPFLD)) Q:KBBPFLD'>0  I $D(^(KBBPFLD,0))#2 S KBBPNM=$P(^(0),U),KBBP1=$P(^(0),U,4) D KBBPFLD(.KBBPNM,KBBPFIL,KBBPDIC,KBBPFLD,KBBPNN,KBBP1,KBBP2,KBBPTOP,KBBPNOD) ; naked global reference
	Q
	;
KBBPFIL1(KBBPT,KBBPNOD,KBBPNN,KBBP1)	;
	; KBBPT - passed by reference
	; KBBPNOD - passed by reference
	; KBBPNN  - passed by reference
	; KBBP1
	N KBBP3
	I (KBBP1=2.98)!(KBBP1=2.001) ;W !,"KBBP1=",KBBP1,"  KBBPFIL1 IS HERE"
	S KBBP3=$O(^DD(KBBP1,"SB",KBBPT,0))
	S KBBPT=KBBP1,KBBPNOD=$P($P(^DD(KBBP1,KBBP3,0),U,4),";")_$S(KBBPNOD'="":";"_KBBPNOD,1:""),KBBPNN=KBBPNN+1
	Q
	;
KBBPFLD(KBBPNM,KBBPFIL,KBBPDIC,KBBPFLD,KBBPNN,KBBP1,KBBP2,KBBPTOP,KBBPNOD)	;
	N KBBPANM,KBBPK,KBBPXREF
	I (KBBPFIL=2.98)!(KBBPFIL=2.001) ;W !,"KBBPFIL=",KBBPFIL,"  KBBPFLD IS HERE"
	I KBBPNM?.E1L.E F KBBPK=1:1:$L(KBBPNM) I $E(KBBPNM,KBBPK)?1L S KBBPNM=$E(KBBPNM,1,KBBPK-1)_$C($A($E(KBBPNM,KBBPK))-32)_$E(KBBPNM,KBBPK+1,$L(KBBPNM))
	F KBBPK=0:0 Q:$E(KBBPNM)'=" "  S KBBPNM=$E(KBBPNM,2,99)
	S KBBPANM=$S($E(KBBPNM)="*":$E(KBBPNM,2,99),1:"") F KBBPK=0:0 Q:$E(KBBPANM)'=" "&($E(KBBPANM)'="*")  S KBBPANM=$E(KBBPANM,2,99)
	I KBBPNM="" S KBBPNM="<null name>"
	D XREF(.KBBPXREF,KBBPFIL,KBBPDIC,KBBPFLD,KBBPNN)
	S ^XTMP("KBBPY","XR1",KBBPNM,KBBPTOP,KBBPFIL,KBBPFLD)=KBBP1_U_KBBPNOD_U_KBBP2_U_KBBPXREF
	Q:KBBPANM=""
	S ^XTMP("KBBPY","XR1",KBBPANM,KBBPTOP,KBBPFIL,KBBPFLD)=KBBP1_U_KBBPNOD_U_KBBP2_U_KBBPXREF
	Q
	;
XREF(KBBPXREF,KBBPFIL,KBBPDIC,KBBPFLD,KBBPNN)	;
	N KBBPK,KBBPXK
	I (KBBPFIL=2.98)!(KBBPFIL=2.001) ;W !,"KBBPFIL=",KBBPFIL,"  XREF IS HERE"
	S KBBPXREF=""
	F KBBPK=0:0 S KBBPK=$O(^DD(KBBPFIL,KBBPFLD,1,KBBPK)) Q:KBBPK'>0  I $D(^(KBBPK,1)) S KBBPXK=^(1) I $E(KBBPXK,1,2)="S " D XREF1(.KBBPXK,.KBBPXREF,KBBPDIC),CHKX(.KBBPXREF,KBBPXK,KBBPNN) I KBBPXREF'="" Q  ; naked global reference
	Q
	;
XREF1(KBBPXK,KBBPXREF,KBBPDIC)	;
	S KBBPXK=$P(KBBPXK,KBBPDIC,2) I KBBPXK'="" S KBBPXREF=$P(KBBPXK,","),KBBPXK=$P(KBBPXK,",",2,99)
	Q
	;
CHKX(KBBPXREF,KBBPXK,KBBPNN)	;
	N KBBPXVAL,KBBPM
	S KBBPXVAL=$P(KBBPXK,",DA"),KBBPXK="DA"_$P(KBBPXK,",DA",2,99) I $E(KBBPXVAL,1,7)'="$E(X,1,"&(KBBPXVAL'="X") S KBBPXREF="" Q
	I KBBPNN>0 F KBBPM=KBBPNN:-1:1 S KBBPXVAL=$P(KBBPXK,","),KBBPXK=$P(KBBPXK,",",2,99) Q:KBBPXVAL'=("DA("_KBBPM_")")
	I $P(KBBPXK,"=")'="DA)" S KBBPXREF=""
	Q
	;
XRPNT(KBBPFIL)	; called from KBBPYSET all tags below (XRP0 through CHKSEC) are called internally
	;  KBBPYSET only uses the first argument
	N II,J,J1,J3,K,M,N,X,X0,X1,XA,XAB,Z1,SILENT
	S SILENT=$S($D(ZTQUEUED):1,$T(ISUTEST^%ut)'="":$$ISUTEST^%ut(),1:0)
	K ^TMP("KBBPY",$J,"P")
	S ^TMP("KBBPY",$J,"P",KBBPFIL)=""
	F J=0:0 S J=$O(^DD(KBBPFIL,"SB",J)) Q:J'>0  S ^TMP("KBBPY",$J,"P",J)="" I (J=2.98)!(J=2.001) ;W !,"J=",J,"  XRPNT-A"
	K ^TMP("KBBPY",$J,"PN"),^("PNT") ; naked global reference
	S X="",X0=0,XA=KBBPFIL,J1=1
	F Z1=0:0 S Z1=$O(^TMP("KBBPY",$J,"P",Z1)) Q:Z1'>0  D
	. S X1=Z1 D XRPNT0
	K ^TMP("KBBPY",$J,"P")
	S XAB=KBBPFIL
	F II=0:0 S II=$O(^DD(XAB,0,"PT",II)) Q:II'>0  D
	. I (XAB=2.98)!(XAB=2.001) ;W !,"XAB=",XAB,"  XRPNT IS HERE"
	. I $D(^DIC(II,0)),$D(^DD(XAB,0,"PT",II,.01)),$D(^DD(II,.01,0)) D
	. . ;I ^DD(II,.01,0)'["DINUM" S (J,^(II))=$G(^TMP("KBBPY",$J,"PNT",II))+1,^(II,J)=".01;"_II_"P"_XAB Q  ; naked global reference
	. . I ^DD(II,.01,0)'["DINUM" S (J,^(II))=$G(^TMP("KBBPY",$J,"PNT",II))+1,^(II,J)=".01;"_II_"T"_XAB Q  ; naked global reference
	. . D XRP0
	F J=0:0 S J=$O(^TMP("KBBPY",$J,"PN",J)) Q:J'>0  D
	. W:'SILENT "|"
	. S J1=J+1
	. F K=0:0 S K=$O(^TMP("KBBPY",$J,"PN",J,KBBPFIL,K)) Q:K'>0  D
	. . W:'SILENT "."
	. . F J3=0:0 S J3=$O(^TMP("KBBPY",$J,"PN",J,KBBPFIL,K,J3)) Q:J3'>0  D
	. . . S X=^(J3),(XA,X1)=K ; naked global reference
	. . . D XRPNT0
	F J=0:0 S J=$O(^TMP("KBBPY",$J,"PN",J)) Q:J'>0  D
	. F K=0:0 S K=$O(^TMP("KBBPY",$J,"PN",J,KBBPFIL,K)) Q:K'>0  D
	. . F M=0:0 S M=$O(^TMP("KBBPY",$J,"PN",J,KBBPFIL,K,M)) Q:M'>0  D
	. . . S X=^TMP("KBBPY",$J,"PN",J,KBBPFIL,K,M)
	. . . S (N,^(K))=$S($D(^TMP("KBBPY",$J,"PNT",K)):^(K),1:0)+1,^(K,N)=X ; naked global references
	K ^TMP("KBBPY",$J,"P") S ^TMP("KBBPY",$J,"FIL")=KBBPFIL
	Q
	;
XRP0	;
	S (Z2,^(II))=$S($D(^TMP("KBBPY",$J,"PN",1,XAB,II)):^(II),1:0)+1,(X,^(II,Z2))=".01;"_XAB_"R"_II D XRP1 ; naked global references
	Q
	;
XRP1	;
	K ^TMP("KBBPY",$J,"P") F J=0:0 S J=$O(^DD(II,"SB",J)) Q:J'>0  S ^TMP("KBBPY",$J,"P",J)=""
	I (J=2.98)!(J=2.001) ;W !,"J=",J,"  XRP1 IS HERE"
	S X0=0,XA=II,J1=2 F Z1=0:0 S Z1=$O(^TMP("KBBPY",$J,"P",Z1)) Q:Z1'>0  S X1=Z1 W:'SILENT "." D XRPNT0
	Q
	;
XRPNT0	;
	F I=0:0 S I=$O(^DD(X1,I)) Q:I'>0  D
	. I (X1=2.98)!(X1=2.001) ;W !,"X1=",X1,"  XRPNT0 IS HERE"
	. I $D(^DD(X1,I,0)) D
	. . S X2=$P(^DD(X1,I,0),U,2)
	. . I X2["P" D
	. . . S X2=+$P(X2,"P",2)
	. . . I X_U[("T"_X2_U) Q
	. . . I X2>0,X2'=XA,X2'=KBBPFIL D
	. . . . D XRPNT1
	. . . . I CHK D
	. . . . . S (J2,^(X2))=$S($D(^TMP("KBBPY",$J,"PN",J1,KBBPFIL,X2)):^(X2),1:0)+1,^(X2,J2)=X_$S(X'="":U,1:"")_M0 ; naked global references
	Q
	;
XRPNT1	;
	I (KBBPFIL=2.98)!(KBBPFIL=2.001) ;W !,"KBBPFIL=",KBBPFIL,"  XRPNT1 IS HERE"
	S CHK=1
	S M0=I_";"_X1_"T"_X2
	S M1=U_M0_U
	F M=0:0 S M=$O(^TMP("KBBPY",$J,"PN",M)) Q:M'>0  D
	. I $D(^TMP("KBBPY",$J,"PN",M,KBBPFIL,X2)) D
	. . S:^TMP("KBBPY",$J,"PN",M,KBBPFIL,X2)>10 CHK=0
	. . S:J1=1&(X1'=XA) CHK=0
	. . Q:'CHK
	. . F Z=0:0 S Z=$O(^TMP("KBBPY",$J,"PN",M,KBBPFIL,X2,Z)) Q:Z'>0  D
	. . . I U_^TMP("KBBPY",$J,"PN",M,KBBPFIL,X2,Z)[M1 S CHK=0 Q
	Q
	;
EOR	;
