KBBPYPRO	;ISC-SF/JLI-PROGRAMMER'S ENTRY FOR EVALUATION ;03/14/16  20:11;
	;;0.0;KBBP;;;
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" Q
	D EN^%ut("KBBPUYNM")
	Q
	;
ENTRY	;
	; THE VARIABLE   A6AXFILE   contains the primary file number (this is
	;  the base file in which a look-up for a particular entry will give
	;  an internal number which is then used in all of the subsequent
	;  navigation and evaluation.
	;
	;  ENTRY IS WITH THE DESIRED FIELDS SPECIFIED IN A6AXFLD ARRAY
	;    A6AXFLD(i)=field.number;file.number
	;  example
	;    A6AXFLD(1)=".01;200"     SELECTS NAME FIELD OF NEW PERSON FILE
	;    A6AXFLD(3)="1;200.03"
	;    A6AXFLD(4)=".01;10.01"
	;
	; The array A6AXFPTH holds the path to use to fields in a related file
	;  These are processed in the numerical order of the subscripts, and
	;  the first path to a specific file will be used for all references
	;  to that file.
	;
	;   A6AXFPTH(1)="201;200T19^3.6;19T3^100;3T16^5;16T10"
	;    this path indicates field 201 from file 200 (the first field must
	;    start with the base file (or a sub-file within that file) points
	;    TO file 19; field 3.6 in file 19 is used to point TO file 3;
	;    field 100 in file 3 points To file 16, and field 5 in file 16
	;    points TO file 10.
	;      This one entry indicates the paths to be used to navigate from
	;    File 200 (or a sub-file of file 200) to files 19, 3, 16, and 10.
	;  A6AXFPTH(2)="
	;
	;
	; After this routine is called, the code to establish the necessary
	; DA values for the files is contained in ^TMP($J,"XTDA",n
	;
	; The code to establish the data values is stored in ^TMP($J,"XT",n
	;
	;  The DA values need to be established for the desired entry, the
	;  variable DABASE is set to the desired internal entry number in the
	;  main routine.  Then the following code can be used to establish
	;  the correct DA values for each file.
	;    F I=0:0 S I=$O(^TMP($J,"XTDA",I)) Q:I'>0  X ^(I)
	;
	;  the correct values are stored as DA("Pnnn") where nnn represents
	;  the file number.
	;
	;  Once the DA values have been established for the desired entry,
	;  the actual values for the fields can be obtained.  The variable
	;  NVAL must be defined -  this value is used to separate data for
	;  different entries in the file, and could be sequential numbers or
	;  it could be the internal entry number (DABASE).
	;  The following code can be used to obtain the values
	;    F I=0:0 S I=$O(^TMP($J,"XT",I)) Q:I'>0  X ^(I)
	;
	;  The resulting values are stored in ^TMP($J,"XVAL",nn,mm,pp) where
	;   nn is the value NVAL, mm is the sequence number for the variable
	;  (i.e., the subscript in A6AXFLD), and pp and additional subscripts
	;  may appear for nested values in a multiple.
	;
	;  The names of the fields are contained in ^TMP($J,"XTNM",n
	;
	;
	; NOTE that the code indicated above will 'walk' through the series
	; of fields indicated in A6AXFLD.  It is of course possible to focus
	; on data from a single field to determine the entries which fall
	; into a desired range for that field prior to processing additional
	; fields.  The subscript of the fields (1,2,...) will be in the order
	; of the subscripts specified for A6AXFLD
	;
	;  F DABASE=0:0 S DABASE=$O(^VA(200,DABASE)) Q:DABASE'>0 D
	;  . F I=0:0 S I=$O(^TMP($J,"XTDA",I)) Q:I'>0  X ^(I)
	;  . S XVAL=DABASE X ^TMP($J,"XT",4) ; get data for the fourth field
	;  . S X=^TMP($J,"XVAL",NVAL,4) ; assuming it is a single value field
	;  . ;                          ; otherwise, would have to check each
	;  . ;                          ; value
	;  . I X<2930101!(X>2931232) K ^TMP($J,"XVAL",NVAL) Q  ;assuming date not
	;  . ;                                           ;wanted, kill, quit
	;  . F I=0:0 S I=$O(^TMP($J,"XT",I)) Q:I'>0  X ^(I) ; wanted, get all values
	;
	;
GETCODE(ROOT,KBBPFILE,KBBPFLDR,KBBPPATH)	;
	N I,X,N,J,Y,CNT,XNUM,YNAM,XMAJ,Z,KBBPXQ,XN,XNM,XS,KBBPFLD,KBBPFPTH
	S:'$D(ROOT) ROOT="^TMP($J)"
	K @ROOT
	Q:'$D(KBBPFILE)  Q:'$D(KBBPFLDR)  Q:KBBPFLDR=""  Q:'$D(@KBBPFLDR)
	S KBBPPATH=$S('$D(KBBPPATH):"",1:KBBPPATH)
	I $S(KBBPPATH="":1,'$D(@KBBPPATH):1,1:0) S KBBPFPTH=""
	E  M KBBPFPTH=@KBBPPATH
	M KBBPFLD=@KBBPFLDR
	K ^TMP("KBBPY",$J,"X"),^("C"),^("FIL")
	S ^TMP("KBBPY",$J,"FIL")=KBBPFILE S KBBPFIL=KBBPFILE,KBBPFILN=$P(^DIC(KBBPFIL,0),U)
	F I=0:0 S I=$O(KBBPFPTH(I)) Q:I'>0  S X=KBBPFPTH(I) D
	. N X1
	. S N=$L(X,U)
	. F J=N:-1:1 D
	. . S X1=$P(X,U,J),Y=$S(X1["P":+$P(X1,";",2),X1["R":+$P(X1,"R",2),1:+$P(X1,"T",2))
	. . I Y>0,'$D(^TMP("KBBPY",$J,"C",+Y)) S ^(+Y)=$P(X,U,1,J)
	. . Q
	. Q
	S CNT=0 F I=0:0 S I=$O(KBBPFLD(I)) Q:I'>0  S Y=$P(KBBPFLD(I),";"),X=$P(KBBPFLD(I),";",2) D
	. S XNUM="",YNAM=""
	. I X>0,X=+X,$D(^DD(X,.01,0)) S XNUM=X
	. I X'>0,X'="" S XNUM=$O(^DIC("B",X,0)) I XNUM'="" S:$O(^DIC("B",X,XNUM))'="" XNUM=""
	. I XNUM="" Q
	. I Y=.001 S YNAM="NUMBER",XMAJ=XNUM S CNT=CNT+1,^TMP("KBBPY",$J,"X",CNT,XMAJ)=YNAM_U_XNUM_U_Y_U_" "_U_U_XMAJ
	. I Y>0,Y=+Y,$D(^DD(XNUM,Y,0)) S YNAM=$P(^(0),U)
	. I Y'>0,Y'="",$D(^DD(XNUM,"B",Y)) S YNAM=Y,Y=$O(^(YNAM,0))
	. I YNAM="" Q
	. S XMAJ=XNUM F  Q:'$D(^DD(XMAJ,0,"UP"))  S XMAJ=^("UP")
	. S Z=$G(^XTMP("KBBPY","XR1",YNAM,XMAJ,XNUM,Y))
	. I Z'="" S CNT=CNT+1,^TMP("KBBPY",$J,"X",CNT,XMAJ)=YNAM_U_XNUM_U_Y_U_Z
	S KBBPXQ=1 D EXIT^KBBPYSE2
	M @ROOT@("XTDA")=XN
	M @ROOT@("XTNM")=XNM
	M @ROOT@("XTS")=XS
	I ROOT'="^TMP($J)" M @ROOT@("XT")=^TMP($J,"XT")
	Q
