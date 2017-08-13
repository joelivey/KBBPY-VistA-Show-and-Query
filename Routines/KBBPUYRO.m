KBBPUYRO	;JIVEYSOFT.COM/JLI - Unit tests for KBBPYNM ;05/01/17  17:09;
	;;0.0;KBBP;
	; Includes tests for:
	;   KBBPYROU
	;
	D EN^%ut($T(+0))
	Q
	;
STARTUP	;
	D STARTUP^KBBPUYNM
	Q
	;
SHUTDOWN	;
	D SHUTDOWN^KBBPUYNM
	Q
	;
	; Unit tests for KBBPYROU - base version for created query routines
	;
EN	; @TEST - run the whole process
	N UTSTGLOB,KEY,NAME,SEX,SPACES
	S UTSTGLOB=$NA(^TMP("KBBPYROU-OUTPUT",$J)) K @UTSTGLOB
	D EN^KBBPYROU
	D CHKEQ("@IOF",$G(@UTSTGLOB@(1)),"Missing page feed line as first line of output")
	D CHKEQ("  and  NEW PERSON KEY (file #200.051;200)   *** I X=""XUPROGMODE""",$G(@UTSTGLOB@(6)),"Incorrect 6th line of output")
	D CHKEQ("    NEW PERSON KEY   *** condition: I X[""XU""",$G(@UTSTGLOB@(11)),"Incorrect 11th line of output")
	S $P(SPACES," ",40)=" ",NAME=$E($P(^VA(200,DUZ,0),U)_SPACES,1,36)
	S SEX=$P(^VA(200,DUZ,1),U,2),SEX=$E($S(SEX="M":"MALE",SEX="F":"FEMALE",1:"")_SPACES,1,7)
	S KEY="XU" ; just look at first two characters of key output
	D CHKEQ(NAME_SEX_KEY,$E($G(@UTSTGLOB@(13)),1,45),"Not expected first line of data")
	; since only the KEY should be different, and we are looking at only the first two characters of it
	D CHKEQ(NAME_SEX_KEY,$E($G(@UTSTGLOB@(+$G(@UTSTGLOB@(0)))),1,45),"Not expected last line of data")
	K @UTSTGLOB
	Q
	;
DOWORK	;@TEST - DOWORK
	N UTSTGLOB,B100,CNT,KBBPUDAT,VALUE
	S UTSTGLOB=$NA(^TMP("KBBPYROU-OUTPUT",$J)) K @UTSTGLOB
	S CNT=$$GETDATA(.KBBPUDAT)
	S $P(B100," ",100)=""
	S VALUE=$E(KBBPUDAT(1)_B100,1,35)_" "_$E(KBBPUDAT(2)_B100,1,6)_" "_$E(KBBPUDAT(3)_B100,1,27)_" "
	D EN^KBBPYROU
	D CHKEQ(12+CNT,$G(@UTSTGLOB@(0)),"Incorrect number of lines in zero node")
	D CHKEQ("@IOF",$G(@UTSTGLOB@(1)),"Incorrect text on first line")
	D CHKEQ("  and  NEW PERSON SEX (file #200)   *** I X=$P(^VA(200,DUZ,1),U,2)",$G(@UTSTGLOB@(5)),"Incorrect text for fifth line of output")
	D CHKEQ("    NEW PERSON KEY   *** condition: I X[""XU""",$G(@UTSTGLOB@(11)),"Incorrect text for eleventh line of output")
	D CHKEQ(VALUE,$G(@UTSTGLOB@(+$G(@UTSTGLOB@(0)))),"Incorrect text for last line")
	K @UTSTGLOB
	Q
	;
SETTYP	;@TEST
	N XWID,XWIDT,FIL,FLD,TYP,I,X,UTSTGLOB
	S I=1,FIL=2.011,FLD=2,X=^DD(FIL,FLD,0),TYP=$P(X,U,2)
	D SETTYP^KBBPYROU(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	D CHKEQ(12,XWID(1),"Wrong value for width of 2.011,2")
	D CHKEQ("C",XWIDT(1),"Incorrect type data for 2.011,2")
	;
	S I=1,FIL=2,FLD=2,X="",TYP="CB"
	D SETTYP^KBBPYROU(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	D CHKEQ(1,XWID(1),"Wrong value for width of binary computed value")
	D CHKEQ("C",XWIDT(1),"Incorrect type data for binary computed value")
	;
	S UTSTGLOB=$NA(^TMP("SETTYP-OUTPUT",$J)) K @UTSTGLOB
	S I=1,FIL="FILE_NUMBER",FLD="FLD_NUMBER",X="",TYP="z"
	D SETTYP^KBBPYROU(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	D CHKEQ(30,XWID(1),"Wrong value for width of unknown type")
	D CHKEQ("U",XWIDT(1),"Incorrect type data for unknown type")
	S X=+$G(@UTSTGLOB@(0))
	D CHKEQ(1,X,"Incorrect number of lines output to user")
	D CHKEQ("Unknown data type file=FILE_NUMBER  field=FLD_NUMBER",$G(@UTSTGLOB@(X)),"Incorrect text returned to user")
	K @UTSTGLOB
	;
	S I=1,FIL="FILE_NUMBER",FLD="FLD_NUMBER",X="S %DT=""ER"",%DT(0)=""-NOW"" D ^%DT",TYP="D"
	D SETTYP^KBBPYROU(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	D CHKEQ(12,XWID(1),"Wrong value for Date without time")
	D CHKEQ("D",XWIDT(1),"Incorrect type data for Date type without time")
	;
	S I=1,FIL="FILE_NUMBER",FLD="FLD_NUMBER",X="S %DT=""ETR"",%DT(0)=""-NOW"" D ^%DT W Y",TYP="D"
	D SETTYP^KBBPYROU(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	D CHKEQ(18,XWID(1),"Wrong value for Date with time, no seconds")
	D CHKEQ("D",XWIDT(1),"Incorrect type data for Date type with time")
	;
	S I=1,FIL="FILE_NUMBER",FLD="FLD_NUMBER",X="S %DT=""ESTR"",%DT(0)=""-NOW"" D ^%DT W Y",TYP="D"
	D SETTYP^KBBPYROU(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	D CHKEQ(21,XWID(1),"Wrong value for width of Date type with time and seconds")
	D CHKEQ("D",XWIDT(1),"Incorrect type data for Date type with time and seconds")
	;
	S I=1,FIL="FILE_NUMBER",FLD="FLD_NUMBER",X="",TYP="NJ4,0"
	D SETTYP^KBBPYROU(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	D CHKEQ("4,0",XWID(1),"Wrong value for width of Number type")
	D CHKEQ("N",XWIDT(1),"Incorrect type data for Number type")
	;
	S I=1,FIL="FILE_NUMBER",FLD="FLD_NUMBER",X="",TYP="NJ8,2"
	D SETTYP^KBBPYROU(.XWID,.XWIDT,FIL,FLD,TYP,I,X)
	D CHKEQ("8,2",XWID(1),"Wrong value for width of Number type with decimal values")
	D CHKEQ("N",XWIDT(1),"Incorrect type data for Number type with decimal values")
	;
	K ^TMP("*SETTYP-UYRO-OUTPUT",$J)
	M ^TMP("*SETTYP-UYRO-OUTPUT",$J)=^TMP("KBBPYROU-OUTPUT",$J)
	K @UTSTGLOB
	Q
	;
PRNT	; @TEST
	N DATA,UTSTGLOB,KBBPPACK,LINE1,LINE5,TOTSETS,XNMAX,XWID,XWIDT,PRMRYFIL,FILETYP,PATHS,XTN
	S UTSTGLOB=$NA(^TMP("KBBPYROU-OUTPUT",$J)) K @UTSTGLOB
	S PRMRYFIL=200
	S XTN(1)=200,XTN(2)=200,XTN(3)="200.051;200",XTN(4)="200.03;200"
	D FILETYPS^KBBPYROU(.FILETYP,.PATHS,.XTN,PRMRYFIL)
	S DATA("TOT")=$NA(^TMP($J,"DATATOT")) K @DATA("TOT")
	S @DATA("TOT")@("XT")=3
	S @DATA("TOT")@("XTN",1)="200"
	S @DATA("TOT")@("XTN",2)="200"
	S @DATA("TOT")@("XTN",3)="200.051;200"
	S @DATA("TOT")@("XVAL",1,1)="USER,SELECTED"
	S @DATA("TOT")@("XVAL",2,1)="M"
	S @DATA("TOT")@("XVAL",3,1,1)="XUPROG"
	S @DATA("TOT")@("XVAL",3,1,2)="XUMGR"
	S @DATA("TOT")@("XVAL",3,1,3)="XUPROGMODE"
	S @DATA("TOT")@("XVAL",3,1,5)="XUSCREENMAN"
	S @DATA("TOT")@("XVAL",3,1,19)="XUMF INSTITUTION"
	S XWID(1)=35,XWID(2)=6,XWID(3)=27
	S XWIDT(1)="F",XWIDT(2)="S;M:MALE;F:FEMALE;",XWIDT(3)="F"
	S XNMAX=3,KBBPPACK=0
	D PRNT^KBBPYROU(.DATA,.XWID,.XWIDT,.FILETYP,XNMAX,KBBPPACK)
	D CHKEQ(5,$G(@UTSTGLOB@(0)),"Incorrect number of lines returned")
	S LINE1="USER,SELECTED                       MALE   XUPROG                      "
	S LINE5="USER,SELECTED                       MALE   XUMF INSTITUTION            "
	D CHKEQ(LINE1,$G(@UTSTGLOB@(1)),"Text not correct for LINE1")
	D CHKEQ(LINE5,$G(@UTSTGLOB@(5)),"Text not correct for LINE5")
	K @UTSTGLOB
	Q
	;
OUTPUT	; @TEST
	N UTSTGLOB,XLN,XWID,XWIDT,KBBPPACK,XNMAX
	N B100,CNT,GVAL,I,KBBPUDAT,LASTLINE,NAM,PNT,VAL,VALUE
	S UTSTGLOB=$NA(^TMP("KBBPYROU-OUTPUT",$J)) K @UTSTGLOB
	S CNT=$$GETDATA(.KBBPUDAT)
	S VAL=KBBPUDAT(1)_U_KBBPUDAT(2)_U_KBBPUDAT(3)_U
	S XWID(1)=35,XWID(2)=6,XWID(3)=27
	S XWIDT(1)="F",XWIDT(2)="S",XWIDT(3)="F"
	S XLN="XLN",XLN(1)=VAL ;
	S KBBPPACK=0,XNMAX=3
	D OUTPUT^KBBPYROU(.XLN,.XWID,.XWIDT,KBBPPACK,XNMAX)
	S LASTLINE=+$G(@UTSTGLOB@(0))
	D CHKEQ(1,LASTLINE,"Incorrect number of lines for normal")
	S $P(B100," ",100)=" "
	S VALUE=$E(KBBPUDAT(1)_B100,1,35)_" "_$E(KBBPUDAT(2)_B100,1,6)_" "_$E(KBBPUDAT(3)_B100,1,27)_" "
	D CHKEQ(VALUE,$G(@UTSTGLOB@(LASTLINE)),"Incorrect text for normal")
	K @UTSTGLOB
	S KBBPPACK=1
	D OUTPUT^KBBPYROU(.XLN,.XWID,.XWIDT,KBBPPACK,XNMAX)
	S LASTLINE=+$G(@UTSTGLOB@(0))
	D CHKEQ(1,LASTLINE,"Incorrect number of lines for normal")
	D CHKEQ(VAL,$G(@UTSTGLOB@(LASTLINE)),"Incorrect text for normal")
	;
	K @UTSTGLOB
	S KBBPPACK=2
	D OUTPUT^KBBPYROU(.XLN,.XWID,.XWIDT,KBBPPACK,XNMAX)
	S LASTLINE=+$G(@UTSTGLOB@(0))
	D CHKEQ(1,LASTLINE,"Incorrect number of lines for normal")
	D CHKEQ(""""_KBBPUDAT(1)_""","""_KBBPUDAT(2)_""","""_KBBPUDAT(3)_""",,",$G(@UTSTGLOB@(LASTLINE)),"Incorrect text for normal")
	;
	K @UTSTGLOB
	Q
	;
SETOUT	;@TEST - setup format and write output
	N LASTLINE,XWID,XWIDT,XVALUE,KBBPPACK,XNMAX
	N B100,CNT,GVAL,I,KBBPUDAT,NAM,PNT,VAL,VALUE,XX
	S XWID(1)=35,XWID(2)=6,XWID(3)=27
	S XWIDT(1)="F",XWIDT(2)="S",XWIDT(3)="F"
	S CNT=$$GETDATA(.KBBPUDAT)
	S XVALUE=KBBPUDAT(1)_U_KBBPUDAT(2)_U_KBBPUDAT(3)_U
	;
	S KBBPPACK=0,XNMAX=3
	S XX=$$SETOUT^KBBPYROU(.XWID,.XWIDT,XVALUE,KBBPPACK,XNMAX) ; convert input ^-separated data into output form and write out
	S $P(B100," ",100)=" "
	S VALUE=$E(KBBPUDAT(1)_B100,1,35)_" "_$E(KBBPUDAT(2)_B100,1,6)_" "_$E(KBBPUDAT(3)_B100,1,27)_" "
	D CHKEQ(VALUE,XX,"Incorrect text for normal")
	;
	S KBBPPACK=1
	S XX=$$SETOUT^KBBPYROU(.XWID,.XWIDT,XVALUE,KBBPPACK,XNMAX) ; convert input ^-separated data into output form and write out
	D CHKEQ(XVALUE,XX,"Incorrect text for ^-separated")
	;
	S KBBPPACK=2
	S XX=$$SETOUT^KBBPYROU(.XWID,.XWIDT,XVALUE,KBBPPACK,XNMAX) ; convert input ^-separated data into output form and write out
	D CHKEQ(""""_KBBPUDAT(1)_""","""_KBBPUDAT(2)_""","""_KBBPUDAT(3)_""",,",XX,"Incorrect text for csv")
	K ^TMP("*SETOUT-UYRO-OUTPUT",$J)
	M ^TMP("*SETOUT-UYRO-OUTPUT",$J)=^TMP("KBBPYROU-OUTPUT",$J)
	Q
	;
GETDATA(KBBPUDAT)	; get data for name and sex fields, last key containing XU and return number of keys containing XU
	N CNT,GVAL,I,NAM,PNT,VAL
	S KBBPUDAT(1)=$P(^VA(200,DUZ,0),U),KBBPUDAT(2)=$P(^VA(200,DUZ,1),U,2)
	S GVAL=$P(^DD(200,4,0),U,3)
	F I=1:1 S VAL=$P(GVAL,";",I) Q:VAL=""  I $P(VAL,":")=KBBPUDAT(2) S KBBPUDAT(2)=$P(VAL,":",2) Q
	S VAL="",CNT=0 F I=0:0 S I=$O(^VA(200,DUZ,51,I)) Q:I'>0  S PNT=+^(I,0) I PNT>0 S NAM=$P($G(^DIC(19.1,PNT,0)),U) I NAM["XU" S VAL=NAM,CNT=CNT+1
	S KBBPUDAT(3)=VAL
	Q CNT
	;
FILETYPS	; @TEST - determine and whether fields are single or multiple relative to primary file
	N FILETYP,PATH,XTN,PRMRYFIL
	S PRMRYFIL=200
	S XTN(1)=200,XTN(2)=200,XTN(3)="200.051;200",XTN(4)="200.03;200"
	D FILETYPS^KBBPYROU(.FILETYP,.PATH,.XTN,PRMRYFIL)
	D CHKEQ("SINGLE",$G(FILETYP(1)),"Incorrect for main file in NEW PERSON file (#200)")
	D CHKEQ("MULTIPLE",$G(FILETYP(3)),"Incorrect value for KEY subfile for NEW PERSON file (#200)")
	D CHKEQ("MULTIPLE",$G(FILETYP(4)),"Incorrect value for SECONDARY OPTION subfile for NEW PERSON file (#200)")
	;
	S PRMRYFIL=2
	S PATH(1)=".01;2R55^.01;55.03T52"
	S XTN(1)=2,XTN(2)=52,XTN(3)=52,XTN(4)="52.1;52"
	D FILETYPS^KBBPYROU(.FILETYP,.PATH,.XTN,PRMRYFIL)
	D CHKEQ("SINGLE",$G(FILETYP(1)),"Incorrect for main file in PATIENT file (#2)")
	D CHKEQ("MULTIPLE",$G(FILETYP(3)),"Incorrect value for PRESCRIPTION file (#52)")
	D CHKEQ("MULTIPLE",$G(FILETYP(4)),"Incorrect value for SECONDARY OPTION subfile for PRESCRIPTION file (#52)")
	Q
	;
CHKEQ(EXPECTED,ACTUAL,COMMENT)	;
	D CHKEQ^%ut(EXPECTED,ACTUAL,COMMENT)
	Q
	;
CHKTF(TEST,COMMENT)	;
	D CHKTF^%ut(TEST,COMMENT)
	Q
	;
XTROU	;
	;;;KBBPUYSH
	;;;KBBPUYNM
