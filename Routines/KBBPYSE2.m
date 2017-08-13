KBBPYSE2	;ISC-SF/JLI - SELECT AND SET UP FIELDS RELATED TO PRIMARY FILE ;03/10/16  17:11;
	;;0.0;KBBP;;May 15, 1992
	;
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" Q
	D EN^%ut("KBBPUYNM")
	Q
	;
EXIT	; called from KBBPYPRO
	I $D(KBBPQRY) Q
	S:'$D(ROOT) ROOT="^TMP($J)" K @ROOT
	I '$D(KBBPXQ) D WRITEIT^KBBPYSET(""),WRITEIT^KBBPYSET("     DABASE is used to indicate the internal entry point for the desired"),WRITEIT^KBBPYSET("entry in the "_KBBPFILN_" file.")
	D PNTRS
	I '$D(KBBPXQ),XN>0 D WRITEIT^KBBPYSET(""),WRITEIT^KBBPYSET("The following commands establish the connections between files:"),WRITEIT^KBBPYSET("") F I=1:1:XN D WRITEIT^KBBPYSET("     "_XN(I))
	K XT,XTN,^TMP($J,"XT")
	F I=1:1 Q:'$D(^TMP("KBBPY",$J,"X",I))  D
	. S J=$O(^TMP("KBBPY",$J,"X",I,0)),(X,X9)=^(J) S XT=I,XTN(I)=J
	. I '$D(KBBPXQ) D WRITEIT^KBBPYSET(""),WRITEIT^KBBPYSET($P(X,U)_" field (#"_$P(X,U,3)_") ")
	. D LISTF
	. S M=$P(X,U,7)
	. I '$D(KBBPXQ) S $P(BLANK," ",50)=" " D:M'="" WRITEIT^KBBPYSET(BLANK_"X-REF: "_M)
	. D  D X2
	. . S XNM(I)="",X("KBBP")=$P(X,U,6) F I("KBBP")=$L(X("KBBP"),";")-1:-1:1 I I("KBBP")'=1!($P(X,U,3)'=.01) S XNM(I)=XNM(I)_$O(^DD($P(X("KBBP"),";",I("KBBP")),0,"NM",""))_" "
	. . K X("KBBP"),I("KBBP")
	. . S M=$P(X,U,4),N=$P(X,U,5),XM(I)=$P(X,U,3),XNM(I)=$P(^DIC(+XTN(I),0),U)_" "_XNM(I)_$P(X,U)
	. . Q
	M @ROOT@("XTDA")=XN,@ROOT@("XTNM")=XNM,@ROOT@("XTS")=XS,@ROOT@("XTN")=XJLI,@ROOT@("XM")=XM K XN,XM,XNM,XS,XJLI
	I ROOT'="^TMP($J)" M @ROOT@("XT")=^TMP($J,"XT") K ^TMP($J,"XT")
	Q
LISTF	;
	N BLANK
	S Y=$P(X,U,6)
	F M=1:1 Q:Y=""  S Y(0)=$P(Y,";"),Y=$P(Y,";",2,99) I '$D(KBBPXQ) D
	. S $P(BLANK," ",5*M+20)=""
	. D WRITEIT^KBBPYSET(BLANK_" of "_$S(Y'="":$O(^DD(Y(0),0,"NM","")),1:$P(^DIC(Y(0),0),U))_" ")
	. D:Y'="" WRITEIT^KBBPYSET("sub-",1)
	. D WRITEIT^KBBPYSET("file",1) D:Y'="" WRITEIT^KBBPYSET("")
	. Q
	Q
	;
X2	;
	S XJLI(XT)=$P(X,U,6)
	I XM(I)=.001,$L(XJLI(XT),";")=1 S XT(XT)="I $D(DA(""P"_J_""")) S @ROOT@(""XVAL"","_XT_",NVAL)=DA(""P"_J_""")" S ^TMP($J,"XT")=XT,^("XT",XT)=XT(XT) Q
	S KM=$S(N="":0,1:$L(N,";")),XT(XT)="I $D(DA(""P"_J_""")) S DA"_$S(KM>0:"("_KM_")",1:"")_"=DA(""P"_J_""")"
	I $P(M,";",2)=" " S XT(XT)=XT(XT)_",D0=DA X $P(^DD("_$P(X,U,2)_","_$P(X,U,3)_",0),U,5,99) S @ROOT@(""XVAL"","_XT_",NVAL)=X" D:'$D(KBBPXQ) WRITEIT^KBBPYSET(""),WRITEIT^KBBPYSET("     "_XT(XT)) S ^TMP($J,"XT")=XT,^("XT",XT)=XT(XT) K XT(XT) Q
	S $P(X,U,2)=Y(0),M1=$S($E($P(M,";",2),1)="E":1,1:0)
	S XTXT="=$S($D("_^DIC($P(X,U,2),0,"GL")
	F K=KM:-1 S XTXT=XTXT_"DA"_$S(K>0:"("_K_")",1:"")_"," Q:N=""  S N(0)=$P(N,";"),N=$P(N,";",2,99) S XTXT=XTXT_$S(+N(0)'=N(0):"""",1:"")_$P(N(0),";")_$S(+N(0)'=N(0):"""",1:"")_","
	S N(0)=$P(M,";"),M=$P(M,";",2)
	S XTXT=XTXT_$S(+N(0)'=N(0):"""",1:"")_N(0)_$S(+N(0)'=N(0):"""",1:"")_")):"_$S(M1:"$E(",1:"$P(")_"^("_$S(N(0)'=+N(0):"""",1:"")_N(0)_$S(+N(0)'=N(0):"""",1:"")_"),"_$S(M1:$E(M,2,$L(M)),1:"""^"","_(+M))_"),1:"""")"
	S X8="",X5=$P(X9,U,2),X6=$P(X9,U,3) I $D(^DD(X5,X6,0)),$P(^DD(X5,X6,0),U,2)["P" S DIC=U_$P(^(0),U,3),PNT=+$P($P(^(0),U,2),"P",2) D SETPNT(DIC,PNT)
	S:KM=0 XT(XT)=XT(XT)_",XTVDATA"_XTXT_X8_" S:XTVDATA'="""" @ROOT@(""XVAL"","_XT_",NVAL)=XTVDATA"
	S:KM>0 XT(XT)=XT(XT)_" X @ROOT@(""XT"","_XT_","_KM_")"
	S:KM>0 XT(XT,0)=" S:XTVDATA'="""" @ROOT@(""XVAL"","_XT_",NVAL" F K=KM:-1 Q:K'>0  S XT(XT,0)=XT(XT,0)_",NVAL"_K
	S XLX=$P(^TMP("KBBPY",$J,"X",I,J),U,3)
	I KM>0 D  F K=KM:-1 Q:K'>0  D LOOPA S XT(XT,K)=XTXT
	. I XLX'=.001 S XT(XT,0)="S XTVDATA"_XTXT_X8_XT(XT,0)_")=XTVDATA"
	. E  S XT(XT,0)="S XTVDATA=DA "_XT(XT,0)_")=XTVDATA"
	I $D(XT(XT))>9 F II=-1:0 S II=$O(XT(XT,II)) Q:II=""  D:'$D(KBBPXQ) WRITEIT^KBBPYSET("         "_XT(XT,II))
	S ^TMP($J,"XT")=XT,^("XT",XT)=XT(XT) F II=-1:0 S II=$O(XT(XT,II)) Q:II=""  S ^TMP($J,"XT",XT,II)=XT(XT,II)
	K XT(XT)
	Q
LOOPA	;
	S M=$P(^TMP("KBBPY",$J,"X",I,J),U,4),N=$P(^(J),U,5)
	S DAX="DA"_$S(K>1:"("_(K-1)_")",1:""),XTXT="S "_DAX_"=0 F NVAL"_K_"=1:1 S "_DAX_"=$O("_^DIC(J,0,"GL")
	F L=KM:-1 Q:L<K  S XTXT=XTXT_"DA"_$S(L>0:"("_L_")",1:"")_"," Q:N=""  S N(0)=$P(N,";"),N=$P(N,";",2,99) S XTXT=XTXT_$S(+N(0)'=N(0):"""",1:"")_$P(N(0),";")_$S(+N(0)'=N(0):"""",1:"")_","
	S XTXT=XTXT_DAX_")) Q:"_DAX_"'>0  X @ROOT@(""XT"","_XT_","_(K-1)_")" ;S N(0)=$P(M,";"),M=$P(M,";",2)
	Q
	;
PNTRS	;
	K XN,XL,XS S XN=1,XL(KBBPFIL)=XN,XN(XN)="S DA(""P"_KBBPFIL_""")=DABASE",XS(XN)="",XP(KBBPFIL)=""
	F I=0:0 S I=$O(^TMP("KBBPY",$J,"C",I)) Q:I'>0  D
	. S X=^(I)
	. F M=1:1 S X1=$P(X,U,M) Q:X1=""  D
	. . S X2=+$P(X1,"T",2),X2=$S(X2>0:X2,1:+$P(X1,"R",2))
	. . I X2>0,'$D(XL(X2)) S XN=XN+1,XL(X2)=XN D SETIT
	. . Q
	. Q
	Q
	;
SETIT	;
	I $P(X1,"R",2)>0 S X2=+$P(X1,"R",2),XN(XN)="S DA(""P"_X2_""")=DABASE",XS(XN)="",XP(X2)="" Q
	S XFLD=+X1,X0=+$P(X1,";",2) I '$D(^DIC(X0,0)) D SETSUB Q
	S DAVAL=$S(X0=KBBPFIL:"DABASE",1:"DA(""P"_X0_""")"),XNOD=$P(^DD(X0,XFLD,0),U,4),XPC=$P(XNOD,";",2),XNOD=$P(XNOD,";"),XNOD=$S(+XNOD=XNOD:XNOD,1:""""_XNOD_"""")
	S XN(XN)="S DA(""P"_X2_""")=$S($D("_^DIC(X0,0,"GL")_DAVAL_","_XNOD_")):+$P(^("_XNOD_"),U,"_XPC_"),1:0)" S XS(XN)=$S(XP(X0)>0:XP(X0),1:""),XP(X2)=XS(XN)
	Q
	;
SETSUB	;
	S X5=^DD(X0,0,"UP"),X4=$O(^DD(X5,"SB",X0,0)) S XNOD=$P($P(^DD(X5,X4,0),U,4),";")
	I +XNOD'=XNOD S XNOD=""""_XNOD_""""
	S XBASE=^DIC(X5,0,"GL")_"DABASE,"_XNOD_",DALOOP"_XN
	S XN(XN,1)="S DALOOP"_XN_"=$O("_XBASE_"))"
	S XS(XN)=XN,XP(X2)=XN
	S XNOD=$P(^DD(X0,XFLD,0),U,4),XPC=$P(XNOD,";",2),XNOD=$P(XNOD,";"),XNOD=$S(+XNOD=XNOD:XNOD,1:""""_XNOD_"""")
	S XN(XN)="S DA(""P"_X2_""")=$S($D("_XBASE_","_XNOD_")):+$P(^("_XNOD_"),U,"_XPC_"),1:0)"
	Q
	;
SETPNT(DIC,PNT)	;
	S X8=X8_",XTVDATA=$S($D("_DIC_"+XTVDATA,0)):$P(^(0),U),1:"""")"
	I $P(^DD(PNT,.01,0),U,2)["P" S PNT=+$P($P(^(0),U,2),"P",2),DIC=U_$P(^(0),U,3) G SETPNT
	Q
	;
