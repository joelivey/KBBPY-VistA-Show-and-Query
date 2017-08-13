KBBPYREF	;ISC-SF.SEA/JLI - CHECK ON STATUS OF OVERALL XREF, START BUILD IF NECESSARY ;02/19/16  16:26
	;;0.0;KBBPTOOLS;;
	;A6AXFREF ;ISC-SF.SEA/JLI - CHECK ON STATUS OF OVERALL XREF, START BUILD IF NECESSARY ;3/1/94  13:50
	;;
	I $T(EN^%ut)="" W !,"Can't run Unit Tests" Q
	D EN^%ut("KBBPUYNM")
	Q
	;
CHECK	; Check current status of globals needed for analysis
	N DIR,X,Y
	L +^XTMP("KBBPY"):1
	I '$T W !,"Can't get the lock on ^XTMP(""KBBPY"") - Quitting." Q
	I '$D(^XTMP("KBBPY","FNM")) D  L -^XTMP("KBBPY") Q
	. D WRITEIT("The cross-reference file does NOT exist.  Will have to build it.")
	. D QUE1
	. Q
	S X=^XTMP("KBBPY","FNM")
	S X=$E(X,4,5)_"/"_$E(X,6,7)_"/"_$E(X,2,3)_"  "_$E(X_"00000",9,10)_":"_$E(X_"00000",11,12)
	D WRITEIT("^KBBPY last completed re-building "_X)
	D REQUE
	L -^XTMP("KBBPY")
	Q
	;
REQUE	;
	N Y,DIR
	S Y=1
	S DIR(0)="Y",DIR("A")="Do you REALLY want to re-build the x-ref",DIR("B")="NO"
	I '$$ISUTEST D ^DIR
	I Y D QUE1
	Q
	;
QUE	; Originally set to setup queued process, but it is very quick, so don't worry about using a queued task
	N XLOCK
	L +^XTMP("KBBPY"):1 I '$T W !,"Can't get the lock on ^XTMP(""KBBPY"" at the moment - quitting." Q
	S XLOCK=1
QUE1	; come in with lock on ^XTMP("KBBPY") - from unit test routine
	N ZTQUEUED
	K ^XTMP("KBBPY","FNM")
	K ^XTMP("KBBPY","XR1")
	S ^XTMP("KBBPY","FNM")="",^XTMP("KBBPY",0)=$$HTFM^XLFDT($$HADD^XLFDT($H,7),1)_U_$$HTFM^XLFDT($H,1)_U_"KBBPY METADATA DICTIONARY OF FileMan Files"
	S ZTQUEUED=1 ; turn off verbose text of periods
	D BUILD^KBBPYNM
	I $G(XLOCK) L -^XTMP("KBBPY")
	Q
	;
WRITEIT(TEXT,NOLINFED)	;
	S NOLINFED=+$G(NOLINFED)
	I 'NOLINFED S LINESOUT=$G(LINESOUT)+1
	I TEXT="@IOF" S LINESOUT=0
	D WRITEOUT^KBBPYROU(TEXT,$G(NOLINFED))
	Q
	;
	;
ISUTEST()	;
	I $T(^%ut)="" Q 0
	Q $$ISUTEST^%ut()
	;
EOR	; Marks end of routine
