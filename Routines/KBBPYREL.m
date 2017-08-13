KBBPYREL	;ISC-SF/JLI - LIST ALL FILES RELATED TO SPECIFIED MAIN FILE ;03/07/16  14:38;
	;;0.0;KBBPTOOLS;;May 15, 1992
	;
	; This routine attempts to present the relationships between a file in MUMPS and all
	; of the files and fields that are related to it, either as subfiles of the file or
	; via pointer relationships to the file, as table relatioships in a relational world
	; It is basically informational only.
	;
	;A6AXFREL ;ISC-SF/JLI - LIST ALL FILES RELATED TO SPECIFIED MAIN FILE ;5/15/92  13:25 ;
	;;2.0; ;;May 15, 1992
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" G EN
	N X R !,"Press Enter NOW to run the application or wait 5 seconds to run the Unit Tests: ",X:5 I $T G EN
	W !,"Running Unit Tests",!
	D EN^%ut("KBBPUYNM")
	W !!,"To run the KBBPYREL routine itself use the command DO EN^KBBPYREL"
	Q
	;
EN	; ENTRY FOR STAND ALONE
	N KBBP1,KBBPASK,KBBPI,KBBPY,KBBPFIL,KBBPFN
	I '$D(^XTMP("KBBPY","FNM"))!'$D(^XTMP("KBBPY","XR1")) D
	. D WRITEIT(""),WRITEIT($C(7)_"First I have to rebuild the field cross-reference file")
	. D WRITEIT("          THIS MAY TAKE A WHILE"),WRITEIT(""),WRITEIT("")
	. D QUE^KBBPYREF
	. Q
	;
	S KBBPFIL="NEW" ;"ALERT" ; default for unit tests
	I '$$ISUTEST() R !,"Select File Name: ",KBBPFIL:DTIME Q:'$T!(KBBPFIL="")!(KBBPFIL[U)
	I KBBPFIL=+KBBPFIL S KBBPFIL=$G(^DIC(KBBPFIL,0),U) I KBBPFIL="" Q  ; if file number entered convert to name
	; get number of files beginning with same characters - found entries are stored in list
	;      in format  ^TMP("KBBPYREL",$J,"SHO",matchnumber)=fileNumber^fileName
	S KBBPI=$$GETLIST(KBBPFIL)
	S KBBPASK="ASK"
	I KBBPI=0 G EN
	I KBBPI=1 S KBBP1=$P(^TMP("KBBPYREL",$J,"SHO",1),U,2),KBBPFN=+^(1) D WRITEIT($E(KBBP1,$L(KBBPFIL)+1,$L(KBBP1))_"    ("_KBBPFN_")",1),WRITEIT(""),WRITEIT("") S KBBPFN=1 ; naked global reference
	I KBBPI'=1 S KBBPFN=1 D
	. D WRITEIT("     Select From:")
	. F KBBPY=1:1:KBBPI D  Q:KBBPFN>0!(KBBPFN=U)
	. . D WRITEIT("       "_$J(KBBPY,3)_".  "_$P(^TMP("KBBPYREL",$J,"SHO",KBBPY),U,2)_"  ("_+^(KBBPY)_")")
	. . I '(KBBPY#10)!(KBBPY=KBBPI) S KBBPFN=$$SELCT(KBBPI,KBBPY) ; naked global reference
	. . Q
	. Q
	I KBBPFN'>0 Q:$$ISUTEST()  G EN
	S KBBPFIL=$P(^TMP("KBBPYREL",$J,"SHO",KBBPFN),U,2),KBBPFN=+^(KBBPFN) ; naked global reference
	D SHOW1(KBBPFIL,KBBPFN)
	I KBBPASK G EN
	Q
	;
	; entry at SHOW1 also from KBBPYSH1
SHOW1(KBBPFIL,KBBPFN)	;
	; KBBPFIL - input - Primary File Name
	; KBBPFN  - input - Primary File Number
	N KBBPL,KBBPB,KBBPFILX,LINNUM
	S KBBPFILX="TABLES RELATED TO "_KBBPFIL_" FILE ("_KBBPFN_")"
	K ^TMP("KBBPYREL",$J,"SHO")
	; KBBPB array with values of file numbers
	S KBBPL=1,KBBPB(KBBPL)=KBBPFN,^TMP("KBBPYREL",$J,"SHO",KBBPFN)=""
	S LINNUM=0
	D LOOP(KBBPFIL,KBBPFILX,.KBBPB,.KBBPL,1,.LINNUM)
	Q
	;
LOOP(KBBPFIL,KBBPFILX,KBBPB,KBBPL,SETNUM,LINNUM)	;
	; KBBPFIL  - input               - primary file name
	; KBBPFILX - input               - Header line to be used with file name and number
	; KBBPB    - passed by reference -
	; KBBPL    - passed by reference -
	; SETNUM   - input               - set number 1 for subfiles of main file, 2 for other references
	; LINNUM   - passed by reference - accumulated number of lines - updated
	N KBBPI,KBBPXV,KBBPC,LINNUM1
	S KBBPXV="",KBBPB=0,KBBPC=""
	; KBBPB(KBBPL) is file number check for subfiles from it, if entry doesn't already exist under "SHO", add it
	F  S KBBPB=$O(^DD(KBBPB(KBBPL),"SB",KBBPB)) Q:KBBPB'>0  D
	. I '$D(^TMP("KBBPYREL",$J,"SHO",KBBPB)) S ^(KBBPB)="" D LIST(KBBPB,KBBPC,.KBBPXV,SETNUM,.LINNUM) Q:KBBPXV[U  D  ; naked global reference
	. . S KBBPB(KBBPL,KBBPB)=KBBPB,KBBPL=KBBPL+1,KBBPB(KBBPL)=KBBPB D LOOP(KBBPFIL,KBBPFILX,.KBBPB,.KBBPL,SETNUM,.LINNUM) S KBBPL=KBBPL-1,KBBPB=$O(KBBPB(KBBPL,0)) K KBBPB(KBBPL,KBBPB)
	. . Q
	. Q
	Q:KBBPL>1
	S SETNUM=2,LINNUM1=LINNUM,LINNUM=0
	; check for pointers to the current file [KBBPB(KBBPL)] if entry doesn't already exist under "SHO" add it
	S KBBPB=0
	F KBBPI=0:0 S KBBPB=$O(^DD(KBBPB(KBBPL),0,"PT",KBBPB)) Q:KBBPB'>0  I '$D(^TMP("KBBPYREL",$J,"SHO",KBBPB)) D  Q:KBBPXV[U
	. ; get field number of pointer in KBBPC and zero node for field in X
	. N X,KBBPC S KBBPC=$O(^DD(KBBPB(KBBPL),0,"PT",KBBPB,0)) S X=$G(^DD(KBBPB,KBBPC,0))
	. ; make sure there is a direct pointer indicated - this omits variable pointers
	. S X=$P(X,U,2) I X'[("P"_KBBPB(KBBPL)) S X=""
	. I X="" Q
	. ; add pointing file/subfile under "SHO"
	. S ^TMP("KBBPYREL",$J,"SHO",KBBPB)="" D LIST(KBBPB,KBBPC,.KBBPXV,SETNUM,.LINNUM)
	. Q
	K ^TMP("KBBPYREL",$J,"SHO")
	D LISTDATA(KBBPFIL,KBBPFILX,1,LINNUM1)
	;W!,"LOOP"
	S LINNUM1=$$CHEK(1)
	;W!,"LOOP BACK"
	D LISTDATA(KBBPFIL,KBBPFILX,2,LINNUM)
	Q
	;
NUMTHERE(KBBPFIL,NONETEXT,KBBPCNT)	;
	; KBBPOUT - input - global location for text storage
	; KBBPFIL  - input - name of the active file
	; NONETEXT - input - description of output values
	; KBBPCNT   - input - number of entries for output values
	;
	D WRITEIT("     <There are "_KBBPCNT_" "_NONETEXT_" for the '"_KBBPFIL_"' file>")
	D WRITEIT("")
	Q
	;
GETLIST(KBBPFIL)	; .EF
	; returns number of possible matches found for input name in KBBPFIL
	; possible matches stored under ^TMP("KBBPYREL",$J,"SHO",matchnumber)=FileNumber^FileName
	;
	; KBBPFIL - input - name to be searched for matches
	;
	N KBBP1,KBBPI,KBBPY
	S KBBP1=$E(KBBPFIL,1,$L(KBBPFIL)-1)_$C($A($E(KBBPFIL,$L(KBBPFIL)))-1)_"z"
	K ^TMP("KBBPYREL",$J,"SHO")
	F KBBPI=0:0 S KBBP1=$O(^DIC("B",KBBP1)) Q:$E(KBBP1,1,$L(KBBPFIL))'=KBBPFIL  D
	. F KBBPY=0:0 S KBBPY=$O(^DIC("B",KBBP1,KBBPY)) Q:KBBPY'>0  D
	. . S KBBPI=KBBPI+1,^TMP("KBBPYREL",$J,"SHO",KBBPI)=KBBPY_U_KBBP1
	. . Q
	. Q
	I KBBPI=0 D WRITEIT($C(7)_"  ??",1) QUIT 0
	QUIT KBBPI
	;
LIST(KBBPFILN,KBBPC,KBBPXV,SETNUM,LINNUM)	;
	; KBBPFILN - input               - file number
	; KBBPC    - input               - field number or null if subfile under the main file
	; KBBPXV   - passed by reference - contains input from prompts to stop or continue, updated during output
	; SETNUM   - input               - set number = 1 for subfiles of main file, 2 for other related files
	; LINNUM   - passed by reference - current line number of data accumulated for set updated for each entry
	;
	; ZEXCEPT: IOSL - Kernel variable for page length
	N KBBPLBL,KBBPLBN,XLINE
	; if file number has a .01 field with zero node
	I '($D(^DD(KBBPFILN,.01,0))#2) Q
	; and isn't a word processing field
	I $P(^DD(KBBPFILN,.01,0),U,2)["W" Q
	; generate descending list of up file/subfile numbers for the selected file/subfile number,
	; with the initial file at the end of the list
	S KBBPLBL="",KBBPLBN=KBBPFILN
	F  S KBBPLBL=$O(^DD(KBBPLBN,0,"NM",""))_$S(KBBPLBL'="":" - ",1:"")_KBBPLBL Q:'$D(^DD(KBBPLBN,0,"UP"))  S KBBPLBN=^("UP") ; naked global reference
	; add file number of pointing file/subfile at end of list
	S XLINE="     "_KBBPLBL_"    ("_KBBPFILN_")"
	; if the pointing field is .01 mark with *, if DINUMed add X marker
	I ($G(KBBPC)=".01") S XLINE=XLINE_"*"_$S(^DD(KBBPFILN,.01,0)["DINUM=":"X",1:"")
	S LINNUM=LINNUM+1
	S ^TMP("KBBPYREL",$J,SETNUM,LINNUM)=XLINE
	Q
	;
HEDR(KBBPFILX)	;
	; ZEXCEPT: IOF - Kernel page feed variable
	; KBBPFILX - input - file name for display
	N KBBPI,KBBPTEXT
	; output previous page, then generate a new header
	D WRITEIT("@IOF")
	F KBBPI=1:1:2 D WRITEIT("")
	S $P(KBBPTEXT," ",(80-$L(KBBPFILX)\2))=" "
	D WRITEIT(KBBPTEXT_KBBPFILX)
	D WRITEIT("")
	Q
	;
CHEK(TYPE)	;.EF
	; TYPE - input - set to 1 if user should not be asked to stop
	; ZEXCEPT: UTESTXV - if present is a value newed and set in unit tests
	N KBBPXV
	S KBBPXV=$S($D(UTESTXV):UTESTXV,1:1),TYPE=+$G(TYPE)
	D WRITEIT("               Enter "_$S(TYPE=0:"'^' to STOP or ",1:"")_"<ret> to continue...")
	I '$$ISUTEST() R KBBPXV:DTIME S:'$T!(KBBPXV[U) KBBPXV=U
	Q KBBPXV
	;
SELCT(KBBPI,KBBPY)	;.EF
	;  returns KBBPFN as the number selected by the user
	; KBBPI - input - maximum number for selection
	; KBBPY - input - current number of items shown for selection
	N KBBPFN
	S KBBPFN=1
	D WRITEIT("")
	D:KBBPY<KBBPI WRITEIT("               Enter '^' to STOP or")
	D WRITEIT("              Select 1 to "_KBBPY_" (out of "_KBBPI_"): ")
	I '$$ISUTEST() R KBBPFN:DTIME S:'$T!(KBBPFN[U) KBBPFN=U S:(KBBPFN>KBBPY)!(KBBPFN<0) KBBPFN=0
	Q KBBPFN
	;
LISTDATA(KBBPFIL,KBBPFILX,SETNUM,LINES)	;
	N KBBPXV,LINNUM
	D HEDR(KBBPFILX)
	S KBBPXV=""
	I SETNUM=1 D  Q:KBBPXV[U
	. I LINES>0 D
	. . D WRITEIT("In the M world, these tables are subfiles to the "_KBBPFIL_" file.")
	. . D WRITEIT("")
	. . Q
	. D NUMTHERE(KBBPFIL,"subfile tables",LINES)
	. F LINNUM=1:1:LINES D LISTOUT(KBBPFILX,.KBBPXV,SETNUM,LINNUM) Q:KBBPXV[U
	. Q
	I SETNUM=2 D
	. D WRITEIT("In the M world, the following tables are files and subfiles that contain links")
	. D WRITEIT("to the "_KBBPFIL_" file")
	. D WRITEIT("  (an * indicates that the link is the primary identifier (.01) for the file,")
	. D WRITEIT("   an *X indicates that the link is also DINUMed to the "_KBBPFIL_" file")
	. D WRITEIT("")
	. D NUMTHERE(KBBPFIL,"related tables",LINES)
	. F LINNUM=1:1:LINES D LISTOUT(KBBPFILX,.KBBPXV,SETNUM,LINNUM) Q:KBBPXV[U
	. Q
	Q
	;
LISTOUT(KBBPFILX,KBBPXV,SETNUM,LINNUM)	;
	; ZEXCEPT: IOSL - Kernel line width variable
	D WRITEIT(^TMP("KBBPYREL",$J,SETNUM,LINNUM))
	S KBBPXV=""
	I ('$$ISUTEST()&((IOSL-4)<$Y))!($$ISUTEST()&((IOSL-4)<LINESOUT)) S KBBPXV=$$CHEK() I KBBPXV'[U D HEDR(KBBPFILX)
	Q
	;
ISUTEST()	;
	I $T(ISUTEST^%ut)="" Q 0
	Q $$ISUTEST^%ut()
	;
WRITEIT(TEXT,NOLINFED)	;
	S NOLINFED=+$G(NOLINFED)
	I 'NOLINFED S LINESOUT=$G(LINESOUT)+1
	I TEXT="@IOF" S LINESOUT=0
	D WRITEOUT^KBBPYROU(TEXT,$G(NOLINFED))
	Q
EOR	; END OF ROUTINE
