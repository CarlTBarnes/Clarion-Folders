!--------------------------
! ListSortColClass by Carl Barnes
!    INCLUDE('ListSortCol.INC'),ONCE      
!   ListSortCls1  CLASS(ListSortColClass).
!----------------- ListSortColClass -----------------
ListSortColClass    CLASS,TYPE,MODULE('ListSortColCls.CLW'),LINK('ListSortColCls.CLW',1),DLL(0)   !,_ABCLinkMode_),DLL(_ABCDllMode_)
!
!Properties
!----------
QRef    &QUEUE
ListFEQ LONG 
ColNow     SHORT
ColLast    SHORT
ColNowWho  STRING(128)
ColLastWho STRING(128)

!Methods
!-------
Init            PROCEDURE(QUEUE xSortQ, LONG xListFEQ)
Kill            PROCEDURE()
InitColors      PROCEDURE(LONG HdrTextClr=-1,LONG HdrBackClr=-1,LONG DataTextClr=-1,LONG DataBackClr=-1)
InitSortCol     PROCEDURE(SHORT xColNow,SHORT xColLast=0),VIRTUAL
SetSortCol1     PROCEDURE(*SHORT SelfColNow, *STRING SelfColWho, SHORT xColumn),VIRTUAL,PROTECTED
TakeHeaderPressed   PROCEDURE(USHORT xForceMouseDownCol=0),VIRTUAL


!Properties Internal
!-------------------


!Methods Internal
!----------------
Construct               PROCEDURE()
Destruct                PROCEDURE(),VIRTUAL

                    END
