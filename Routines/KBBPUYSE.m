KBBPUYSE	;JLI/JIVEYSOFT.COM-Unit tests related to KBBPYSE* routines ;03/07/16  13:45
	;;0.0;KBBP;;;
	;
	I $T(EN^%ut)'="" D EN^%ut($T(+0))
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
GETFILE	; @TEST - file name and number
	;GETFILE(KBBPFIL,KBBPFILN,KBBPI) ; called from EN^KBBPYQRY
	; KBBPFIL  - passed by reference - contains file number on return (or zero)
	; KBBPFILN - passed by reference - contains file name on return
	; KBBPI    - passed by reference -
	N KBBPFIL,KBBPFILN,KBBPI S KBBPI=0
	D GETFILE^KBBPYSET(.KBBPFIL,.KBBPFILN,.KBBPI)
	D CHKEQ(200,KBBPFIL,"Incorrect file number returned")
	D CHKEQ("NEW PERSON",KBBPFILN,"Incorrect file name returned")
	D CHKEQ(0,KBBPI,"Incorrect value for KBBPI returned")
	K ^TMP("KBBPYROU-OUTPUT",$J)
	Q
	;
GETFLDS1	; @TEST - get criteria for one search set
	N KBBPI,KBBPFIL,X,UTSTX,KBBPQRY,KBBPEXIT
	S KBBPFIL=200,KBBPI=0,KBBPQRY=1,KBBPEXIT=0
	D QUE^KBBPYREF
	I $S('$D(^TMP("KBBPY",$J,"FIL")):1,^("FIL")'=KBBPFIL:1,'$D(^TMP("KBBPY",$J,"PNT")):1,1:0) D XRPNT^KBBPYNM(KBBPFIL)
	;
	S UTSTX=U
	S X=$$GETFLDS^KBBPYSET(.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY)
	D CHKEQ(U,X,"Incorrect value returned for ^ input")
	K ^TMP("KBBPYROU-OUTPUT",$J)
	Q
	;
GETFLDS2	; @TEST - get criteria for one search set
	N KBBPI,KBBPFIL,X,UTSTX,KBBPQRY,KBBPEXIT
	S KBBPFIL=200,KBBPI=0,KBBPQRY=1,KBBPEXIT=0
	D QUE^KBBPYREF
	I $S('$D(^TMP("KBBPY",$J,"FIL")):1,^("FIL")'=KBBPFIL:1,'$D(^TMP("KBBPY",$J,"PNT")):1,1:0) D XRPNT^KBBPYNM(KBBPFIL)
	;
	S UTSTX="",X="",KBBPEXIT=0,KBBPFIL=200,KBBPI=0
	S X=$$GETFLDS^KBBPYSET(.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY)
	D CHKEQ("",X,"Unexpected value returned for null input")
	K ^TMP("KBBPYROU-OUTPUT",$J)
	Q
	;
GETFLDS3	; @TEST - get criteria for one search set
	N KBBPI,KBBPFIL,X,UTSTX,KBBPQRY,UTSTCOND,KBBPEXIT
	S KBBPFIL=200,KBBPI=0,KBBPQRY=1,KBBPEXIT=0
	D QUE^KBBPYREF
	I $S('$D(^TMP("KBBPY",$J,"FIL")):1,^("FIL")'=KBBPFIL:1,'$D(^TMP("KBBPY",$J,"PNT")):1,1:0) D XRPNT^KBBPYNM(KBBPFIL)
	S UTSTX="SEX@200",KBBPFIL=200,KBBPI=0,KBBPEXIT=0,UTSTCOND(1)="X=$P(^VA(200,DUZ,1),U,2)"
	K ^TMP("KBBPY",$J,"X")
	S X=$$GETFLDS^KBBPYSET(.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY)
	D CHKEQ("SEX^200^4^1;2^^200^^^^X=$P(^VA(200,DUZ,1),U,2)",$G(^TMP("KBBPY",$J,"X",1,200)),"Unexpected global value returned for SEX@NEW PERSON")
	K ^TMP("KBBPYROU-OUTPUT",$J)
	Q
	;
GETFLDS4	; @TEST - get criteria for one search set
	N KBBPI,KBBPFIL,X,UTSTX,KBBPQRY,KBBPEXIT,UTSTCOND
	S KBBPFIL=200,KBBPI=0,KBBPQRY=1,KBBPEXIT=0
	D QUE^KBBPYREF
	I $S('$D(^TMP("KBBPY",$J,"FIL")):1,^("FIL")'=KBBPFIL:1,'$D(^TMP("KBBPY",$J,"PNT")):1,1:0) D XRPNT^KBBPYNM(KBBPFIL)
	;
	S KBBPFIL=200,UTSTX(1)="surrogate@mailbox",KBBPI=0,KBBPEXIT=0
	K ^TMP("KBBPY",$J,"X")
	S X=$$GETFLDS^KBBPYSET(.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY,1)
	D CHKEQ("SURROGATE^3.703^.01^0;1^9^3.703;3.7^""AB""",$G(^TMP("KBBPY",$J,"X",1,3.7)),"Unexpected global value returned for SURROGAT@MAILBOX")
	K ^TMP("KBBPYROU-OUTPUT",$J)
	Q
	;
GETFLDS5	; @TEST - get criteria for one search set
	N KBBPI,KBBPFIL,X,UTSTX,KBBPQRY,UTSTCOND,KBBPEXIT
	S KBBPFIL=2,KBBPFIL=2,KBBPI=0
	I $S('$D(^TMP("KBBPY",$J,"FIL")):1,^("FIL")'=KBBPFIL:1,'$D(^TMP("KBBPY",$J,"PNT")):1,1:0) D XRPNT^KBBPYNM(KBBPFIL)
	D QUE^KBBPYREF
	S KBBPI=0,KBBPFIL=2 D XRPNT^KBBPYNM(KBBPFIL)
	S UTSTX(1)="DRUG ALLERGY INDICATION",KBBPEXIT=0,UTSTCOND(1)="X=$P(^VA(200,DUZ,1),U,2)",KBBPQRY=1
	K ^TMP("KBBPY",$J,"X")
	S X=$$GETFLDS^KBBPYSET(.KBBPI,.KBBPEXIT,KBBPFIL,KBBPQRY,1)
	D CHKEQ("DRUG ALLERGY INDICATION^52^34.1^3;6^^52^^^^X=$P(^VA(200,DUZ,1),U,2)",$G(^TMP("KBBPY",$J,"X",1,52)),"Unexpected global value returned for DRUG ALLERGY INDICATION data node")
	D CHKEQ(".01;2R55^.01;55.03T52",$G(^TMP("KBBPY",$J,"C",52)),"Unexpected global value returned for Prescription file path global")
	D CHKEQ(".01;2R55",$G(^TMP("KBBPY",$J,"C",55)),"Unexpected global value returned for Pharmacy Patient path global")
	;
	K ^TMP("KBBPY",$J)
	K ^TMP("KBBPYROU-OUTPUT",$J)
	Q
	;
HUHFIELD	; @TEST - check output for ? input to request for fields
	N N,UTSTGLOB,TEXT,X
	S UTSTGLOB=$NA(^TMP("HUHFIELD-OUTPUT",$J)) K @UTSTGLOB
	D HUHFIELD^KBBPYSET
	S N=$G(@UTSTGLOB@(0)),TEXT=@UTSTGLOB@(N)
	D CHKEQ(17,N,"Incorrect number of lines listed in output")
	D CHKEQ("Enter all or a part of a FIELD NAME.",$G(@UTSTGLOB@(2)),"Incorrect text for first line of output")
	D CHKEQ("under the main file indented for each level of sub-file.",TEXT,"Incorrect text returned for last text line response to ?")
	K @UTSTGLOB
	Q
	;
SHOFILS1	; @TEST
	N UTSTFIRST,UTSTSUBS,UTSTGLOB,UTSTFILE,KBBPFIL,UTSTLAST,I
	S UTSTGLOB=$NA(^TMP("SHOFILS1",$J)) K @UTSTGLOB
	S UTSTFILE=200,UTSTFIRST=52,UTSTLAST=54
	D SHOFILS^KBBPYSET
	; may have other files added, so check not based on fixed entry numbers
	D CHKTF($G(@UTSTGLOB@(0))>3,"Expected at least four files to be found, found only "_+$G(@UTSTGLOB@(0)))
	F I=0:0 S I=$O(@UTSTGLOB@(I)) Q:I'>0  I @UTSTGLOB@(I)["53.44" Q
	I I'>0 D FAIL^%ut("Failed to find data for file 53.44")
	I I>0 D CHKEQ("#53.44  PHYSICIANS' ORDERS FILE .01 field is 'USER'",@UTSTGLOB@(I),"Expected text not found for file 53.44")
	K @UTSTGLOB
	Q
	;
SHOFILS2	; @TEST
	N UTSTFIRST,UTSTSUBS,UTSTGLOB,UTSTFILE,UTSTLAST,I
	S UTSTFIRST=52,UTSTSUBS=1,UTSTFILE=200,UTSTLAST=54
	S UTSTGLOB=$NA(^TMP("SHOFILS2",$J)) K @UTSTGLOB
	D SHOFILS^KBBPYSET
	D CHKTF($G(@UTSTGLOB@(0))>22,"Expected at least 23 lines of output including sub-files, found only "_$G(@UTSTGLOB@(0)))
	F I=0:0 S I=$O(@UTSTGLOB@(I)) Q:I'>0  I @UTSTGLOB@(I)["#53.455" Q
	I I'>0 D FAIL^%ut("Failed to find data for sub-file 53.455")
	I I>0 D CHKEQ("   #53.455  SPECIAL INSTRUCTIONS SUB-FILE .01 field is 'SPECIAL INSTRUCTIONS'",@UTSTGLOB@(I),"Expected text not found for sub-file 53.455")
	K @UTSTGLOB
	Q
	;
GETFILID	;@TEST - get file id if part of field input as field@file
	N X,XF,FILENUM
	S X="FIELDNAME@200",FILENUM=""
	D GETFILID^KBBPYSET(.X,.FILENUM)
	D CHKEQ("FIELDNAME",X,"Expected field name not returned")
	D CHKEQ(200,FILENUM,"Expected file number not returned")
	;
	S X="FIELDNAME@NEW PERSON",FILENUM=""
	D GETFILID^KBBPYSET(.X,.FILENUM)
	D CHKEQ("FIELDNAME",X,"Expected field name not returned")
	D CHKEQ(200,FILENUM,"Expected file number not returned")
	M ^TMP("GETFILID",$J)=^TMP("KBBPYROU-OUTPUT",$J)
	K ^TMP("KBBPYROU-OUTPUT",$J)
	Q
	;
CHKEQ(EXPECTED,ACTUAL,COMMENT)	;
	D CHKEQ^%ut(EXPECTED,ACTUAL,COMMENT)
	Q
	;
CHKTF(VALUE,COMMENT)	;
	D CHKTF^%ut(VALUE,COMMENT)
	Q
	;
XTROU	;
	;;;KBBPUYNM;
