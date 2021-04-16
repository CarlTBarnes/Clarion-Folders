                    MEMBER()
!--------------------------
! ListSortColClass by Carl Barnes
!--------------------------
    INCLUDE('EQUATES.CLW')
    INCLUDE('ListSortColCls.INC'),ONCE
    MAP
    END      
!----------------------------------------
ListSortColClass.Construct        PROCEDURE()
!----------------------------------------
    CODE
    RETURN
!---------------------------------------
ListSortColClass.Destruct PROCEDURE()
!---------------------------------------
    CODE
    RETURN
!-----------------------------------
ListSortColClass.Init     PROCEDURE(QUEUE xSortQ, LONG xListFEQ)
!-----------------------------------
    CODE
    SELF.QRef   &= xSortQ
    SELF.ListFEQ = xListFEQ
    xListFEQ{PROPLIST:HasSortColumn}=1
    RETURN

!-----------------------------------
ListSortColClass.Kill     PROCEDURE()
!-----------------------------------
    CODE
    RETURN


!-----------------------------------
ListSortColClass.InitColors      PROCEDURE(LONG HdrTextClr=-1,LONG HdrBackClr=-1,LONG DataTextClr=-1,LONG DataBackClr=-1)
!-----------------------------------
    CODE
    SELF.ListFEQ{PROPList:HdrSortTextColor} = HdrTextClr      !Color Header Text
    SELF.ListFEQ{PROPLIST:HdrSortBackColor} = HdrBackClr      !Color Header Background    
    SELF.ListFEQ{PROPList:SortTextColor}    = DataTextClr     !Color List Text       
    SELF.ListFEQ{PROPList:SortBackColor}    = DataBackClr     !Color List Background
    RETURN
!-----------------------------------
ListSortColClass.InitSortCol PROCEDURE(SHORT xColNow,SHORT xColLast=0)
!-----------------------------------
    CODE
    SELF.SetSortCol1(SELF.ColNow ,SELF.ColNowWho ,xColNow)
    SELF.SetSortCol1(SELF.ColLast,SELF.ColLastWho,xColLast)
    RETURN 
!-----------------------------------
ListSortColClass.SetSortCol1 PROCEDURE(*SHORT SelfColNow, *STRING SelfColWho, SHORT xColumn)
!-----------------------------------
    CODE
    SelfColNow = xColumn
    IF ~xColumn THEN
        SelfColWho=''
    ELSE
        xColumn = SELF.ListFEQ{PROPLIST:FieldNo,ABS(xColumn)}
        SelfColWho = CHOOSE(SelfColNow<0,'-','+') & WHO(SELF.QRef,ABS(xColumn))
    END
    RETURN
    
!-----------------------------------
ListSortColClass.TakeHeaderPressed     PROCEDURE(USHORT xForceMDCol=0)
!-----------------------------------
SortNow     &SHORT   !I hate my SELF. so much
SortLast    &SHORT
SortNowWho  &STRING
SortLastWho &STRING
QRecord     STRING(SIZE(SELF.QRef))
QueRef      &QUEUE
ListQue     LONG
QX          LONG,AUTO 
    CODE
    QueRef &= SELF.QRef      ; ListQue = SELF.ListFEQ
    SortNow  &= SELF.ColNow  ; SortNowWho  &= SELF.ColNowWho
    SortLast &= SELF.ColLast ; SortLastWho &= SELF.ColLastWho
    GET(QueRef, CHOICE(ListQue))    !To preserve selected row for later GET 
    QRecord = QueRef
    IF xForceMDCol THEN
       SortNow=xForceMDCol
       ListQue{PROPLIST:SortColumn}=xForceMDCol   !Color column 1 as sorted
    ELSE
       SortNow=ListQue{PROPList:MouseDownField}  !This is really Column and NOT Q Field
    END
    SortNow=ListQue{PROPLIST:FieldNo,SortNow}   !Now we have Queue Field to use with WHO()
    IF SortNow<>ABS(SortLast) THEN SortLastWho=SortNowWho.
    SortNowWho=CHOOSE(SortNow=SortLast,'-','+') & WHO(QueRef,SortNow)
!Debug    0{PROP:Text}='SORT (' & SortNow &' /'& SortLast &') ' & CLIP(SortNowWho) & SortLastWho
    SORT(QueRef,CLIP(SortNowWho) & |
                   CHOOSE(~SortLastWho,'',','& CLIP(SortLastWho)) )
    SortLast = CHOOSE(SortNow=ABS(SortLast),-1*SortLast,SortNow) 
    LOOP QX=1 TO RECORDS(QueRef)   !Reselect the same row
        GET(QueRef,QX)
        IF QueRef=QRecord THEN 
           ListQue{PROP:Selected}=QX
           BREAK
        END
    END
    DISPLAY
    RETURN
!-----------------------------------
!ListSortColClass.GetQueueAfterSort     PROCEDURE()!,Virtual
!!-----------------------------------
!    CODE
!    !Derive does GET(SELF.QRef, SELF.QRef:Name) !If no Unique field to GET() then Loop until find it 
!    RETURN
