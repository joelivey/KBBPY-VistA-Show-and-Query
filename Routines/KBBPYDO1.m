KBBPYDO1	;ISC-SF.SEA/JLI - DO THE THINGS ;05/01/17  13:47;
	;;2.0;KBBP;;May 15, 1992
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" Q
	D EN^%ut("KBBPUYNM")
	Q
	;
	;
GET1(ROOT)	;
	N XT,NROOT
	S:'$D(ROOT) ROOT="^TMP($J)"
GETONE	;
	N DA,I,XN,XVAL,NVAL,J,K
	I '$D(ROOT) S ROOT="^TMP($J)"
	S XT=$G(@ROOT@("XT"))
	S XN=$G(@ROOT@("XTDA"))
	F I=0:0 S I=$O(@ROOT@("XTS",I)) Q:I'>0  D
	. I @ROOT@("XTS",I)="" X @ROOT@("XTDA",I)
	. Q
	K XVAL,@ROOT@("XVAL")
	S NVAL=1
	F J=1:1:XT X @ROOT@("XT",J)
	K DA F I=1:1:XN D
	. I $D(@ROOT@("XTDA",I))>9 S @("DALOOP"_I)=0 D
	. . F NVAL=1:1 X @ROOT@("XTDA",I,1) Q:@("DALOOP"_I)'>0  D
	. . . F J=1:1:XN X:@ROOT@("XTS",J)=I @ROOT@("XTDA",J) I J=XN D
	. . . . F K=1:1:XT X @ROOT@("XT",K)
	. . . . Q
	. . . Q
	. . Q
	. Q
	Q
	;
CHKIT(ROOT,XCONDR)	;
	; ROOT    - input - global root for related data
	; XCONDR  - input - indicator for array or global with conditions
	; FILETYP - passed by reference - indicator of multiple or single fields
	N NVAL,XY,I,X,I1,NVAL1,XCOND,XT,XX
	Q:'$D(ROOT)  Q:'$D(XCONDR)  Q:XCONDR=""  Q:'$D(@XCONDR)
	;W !,"CHKIT-1 ROOT=",ROOT,"   XCONDR=",XCONDR
	S XT=$G(@ROOT@("XT"))
	M XCOND=@XCONDR
	F NVAL=1:1:XT I $D(XCOND(NVAL)) D
	. ;W !,"CHKIT-2 NVAL=",NVAL,"  XCOND(NVAL)=",XCOND(NVAL)
	. S XY=$NA(@ROOT@("XVAL",NVAL))
	. I '$D(@XY) S XX="@ROOT@(""XVAL"",NVAL)" K @XX Q
	. S I=""
	. F  S I=$O(@XY@(I)) Q:I=""  D:$D(@XY@(I))>9  I $D(@XY@(I)),$D(@XY@(I))<9 S X=@XY@(I) X XCOND(NVAL) I '$T K @XY@(I) S XX=$E(ROOT,1,$L(ROOT)-1)_",""XVAL"",NVAL,I)" K @XX
	. . S I1="" F  S I1=$O(@XY@(I,I1)) Q:I1=""  I $D(@XY@(I,I1))<9 S X=@XY@(I,I1) X XCOND(NVAL) I '$T K @XY@(I,I1) S XX=$E(ROOT,1,$L(ROOT)-1)_",""XVAL"",NVAL,I,I1)" K @XX
	. . Q
	. Q
	Q
	;
EOR	;
