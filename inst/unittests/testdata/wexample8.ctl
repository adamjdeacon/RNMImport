;Model Desc: Two compartment model with extra Bayesian information
;Project Name: wexamples
;Project ID: NO PROJECT DESCRIPTION

$PROB wexample8, obtain extra Bayesian information
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
$DATA wexample8.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4

$PRIOR NWPRI NTHETA=4, NETA=4, NTHP=4, NETP=4

$PK
"USE NMBAYES_INT, ONLY: ITER_REPORT,BAYES_EXTRA_REQUEST,BAYES_EXTRA
; Request extra information for Bayesian analysis.  An extra call will then be made
; for accepted samples
"BAYES_EXTRA_REQUEST=1
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
MU_4=THETA(4)
CL=DEXP(MU_1+ETA(1))
V1=DEXP(MU_2+ETA(2))
Q=DEXP(MU_3+ETA(3))
V2=DEXP(MU_4+ETA(4))
S1=V1
; When Bayes_extra=1, then this particular set of individual parameters were "accepted"
; So you may record them if you wish
"IF(BAYES_EXTRA==1 .AND. ITER_REPORT>=0 .AND. TIME==0.0) THEN
"WRITE(50,98) ITER_REPORT,ID,CL,V1,Q,V2
"98 FORMAT(I12,1X,F14.0,4(1X,1PG12.5))
"ENDIF

$ERROR
"USE NMBAYES_INT, ONLY: ITER_REPORT,BAYES_EXTRA_REQUEST,BAYES_EXTRA
"BAYES_EXTRA_REQUEST=1
Y = F + F*EPS(1)
"IF(BAYES_EXTRA==1 .AND. ITER_REPORT>=0 ) THEN
"WRITE(51,97) ITER_REPORT,ID,TIME,F
"97 FORMAT(I12,1X,F14.0,2(1X,1PG12.5))
"ENDIF

; Initial values of THETA
$THETA 
(0.001, 2.0) ;[LN(CL)]
(0.001, 2.0) ;[LN(V1)]
(0.001, 2.0) ;[LN(Q)]
(0.001, 2.0) ;[LN(V2)]
;INITIAL values of OMEGA
$OMEGA BLOCK(4)
0.15   ;[P]
0.01  ;[F]
0.15   ;[P]
0.01  ;[F]
0.01  ;[F]
0.15   ;[P]
0.01  ;[F]
0.01  ;[F]
0.01  ;[F]
0.15   ;[P]
;Initial value of SIGMA
$SIGMA 
(0.6 )   ;[P]

$THETA (2.0 FIX) (2.0 FIX) (2.0 FIX) (2.0 FIX)

$OMEGA BLOCK(4)
10000 FIX 
0.00 10000
0.00  0.00 10000
0.00  0.00 0.0 10000

; Prior information to the OMEGAS.
$OMEGA BLOCK(4)
0.2 FIX 
0.0  0.2 
0.0  0.0 0.2
0.0  0.0 0.0 0.2
$THETA (4 FIX)

$EST METHOD=BAYES INTERACTION FILE=wexample8.txt NBURN=10000 NITER=1000 PRINT=100 NOPRIOR=0
     CTYPE=3 CINTERVAL=100
