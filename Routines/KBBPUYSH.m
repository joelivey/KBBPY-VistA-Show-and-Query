KBBPUYSH	;JIVEYSOFT.COM/JLI - Unit tests for KBBPYNM ;02/26/16  15:21;
	;;0.0;KBBP;
	; Includes tests for:
	;   KBBPYSHO
	;   KBBPYSH1
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
	; For routine KBBPYSHO
	;
EN	; @TEST
	N UTESTVAL,UTESTNUM,UTESTFIL
	D EN^KBBPYSHO
	D CHKEQ("200^NEW PERSON",$G(^TMP("KBBPY",$J,"SHO",UTESTVAL)),"Incorrect value set up")
	Q
	;
SET1	;@TEST - set one value in list of possible matches.
	N KBBPI S KBBPI=4
	I '$D(^XTMP("KBBPY","FNM","ALERT DATE TIME")) D QUE^KBBPYREF
	K ^TMP("KBBPY",$J,"SHO")
	D SET1^KBBPYSHO(200.194,.KBBPI)
	D CHKEQ(5,KBBPI,"Wrong value number returned")
	D CHKEQ("200.194^ALERT DATE TIME",$G(^TMP("KBBPY",$J,"SHO",KBBPI)),"Incorrect value set up")
	K ^TMP("KBBPY",$J,"SHO")
	Q
	;
SELCTSHO	;@TEST - return value to be selected
	; when only one choice is available, the value 1 is returned without user interaction
	D CHKEQ(1,$$SELCT^KBBPYSHO(1,1),"Bad selection value returned")
	;
	S KBBPFIL=200
	S KBBPY=KBBPFIL-.0000001 F KBBPI=0:0 S KBBPY=$O(^DD(KBBPY)) Q:$E(KBBPY,1,$L(KBBPFIL))'=KBBPFIL  I $D(^DD(KBBPY,0))#2 D SET1^KBBPYSHO(KBBPY,.KBBPI)
	D CHKEQ("200^NEW PERSON",$G(^TMP("KBBPY",$J,"SHO",$$SELCT^KBBPYSHO(10,KBBPI))),"Incorrect value returned from SELCT^KBBPYSHO")
	Q
	;
ENSHO	;@TEST - get selected value
	N UTESTFIL,UTESTVAL,UTESTNUM S UTESTFIL=8994.03
	D EN^KBBPYSHO
	D CHKEQ("8994.03^RETURN PARAMETER DESCRIPTION",$G(^TMP("KBBPY",$J,"SHO",UTESTVAL)),"Expected value not returned")
	K ^TMP("KBBPY",$J,"SHO")
	Q
	;
	; For routine KBBPYSH1
	;
TYPESH1	;@TEST - returns description, if possible, for field type
	N X
	S X=^DD(200,.01,0) ; NAME
	D CHKEQ("Free Text, 3 to 35 chars",$$TYPE^KBBPYSH1(X),"Incorrect value for Free Text")
	;
	S X=^DD(200,30,0) ; DATE ENTERED
	D CHKEQ("Date",$$TYPE^KBBPYSH1(X),"Incorrect value for Date field")
	;
	D CHKEQ("Unhandled type KBBPTYPE=Z",$$TYPE^KBBPYSH1("^Z"),"Unexpected value for unknown type")
	Q
	;
XREFSH1	;@TEST - returns standard cross-ref for field, if any
	N FILE,FIELD,DICNODE,KBBPNN
	S FILE=200,DICNODE="^VA(200,",KBBPNN=0
	; NAME
	S FIELD=.01
	D CHKEQ("B",$$XREF^KBBPYSH1(FILE,FIELD,DICNODE),"Not expected value for field")
	; ACCESS CODE
	S FIELD=2
	D CHKEQ("A",$$XREF^KBBPYSH1(FILE,FIELD,DICNODE),"Not expected value for field")
	; SOCIAL WORKER ?
	S FIELD=654
	D CHKEQ("",$$XREF^KBBPYSH1(FILE,FIELD,DICNODE),"Not expected value for field")
	; NICK NAME
	S FIELD=13
	D CHKEQ("D",$$XREF^KBBPYSH1(FILE,FIELD,DICNODE),"Not expected value for field")
	Q
	;
CHKXSH1	;@TEST - Determine whether cross-reference fits standard pattern for look-up
	N KBBPXREF,KBBPXK,KBBPNN
	; NAME
	S KBBPXREF="B",KBBPXK="$E(X,1,30),DA)=""""",KBBPNN=0
	D CHKX^KBBPYSH1(.KBBPXREF,KBBPXK,KBBPNN)
	D CHKEQ("B",KBBPXREF,"Not expected value for B x-ref")
	; ACCESS CODE
	S KBBPXREF="A",KBBPXK="X,DA)=+$H",KBBPNN=0
	D CHKX^KBBPYSH1(.KBBPXREF,KBBPXK,KBBPNN)
	D CHKEQ("A",KBBPXREF,"Not expected value for A x-ref")
	; SOCIAL WORKER ?
	S KBBPXREF="ASWB",KBBPXK="DA,DA)=""""",KBBPNN=0
	D CHKX^KBBPYSH1(.KBBPXREF,KBBPXK,KBBPNN)
	D CHKEQ("",KBBPXREF,"Not expected value for ASWB x-ref")
	; SOCIAL WORKER'S NUMBER
	S KBBPXREF="ASWD",KBBPXK="$E(X,1,30),DA)=""""",KBBPNN=0
	D CHKX^KBBPYSH1(.KBBPXREF,KBBPXK,KBBPNN)
	D CHKEQ("ASWD",KBBPXREF,"Not expected value for  x-ref")
	; SSN
	S KBBPXREF="BS5",KBBPXK="$E(",KBBPNN=0
	D CHKX^KBBPYSH1(.KBBPXREF,KBBPXK,KBBPNN)
	D CHKEQ("",KBBPXREF,"Not expected value for  x-ref")
	; SSN
	S KBBPXREF="SSN",KBBPXK="$E(X,1,30),DA)=""""",KBBPNN=0
	D CHKX^KBBPYSH1(.KBBPXREF,KBBPXK,KBBPNN)
	D CHKEQ("SSN",KBBPXREF,"Not expected value for  x-ref")
	; NICK NAME
	S KBBPXREF="D",KBBPXK="$$UP^XLFSTR($E(X,1,30)),DA)=""""",KBBPNN=0
	D CHKX^KBBPYSH1(.KBBPXREF,KBBPXK,KBBPNN)
	D CHKEQ("D",KBBPXREF,"Not expected value for  x-ref")
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
	;;;KBBPUYNM
