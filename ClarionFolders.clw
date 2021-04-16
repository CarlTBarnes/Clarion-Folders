!Claron Folders example by Carl Barnes
!Example using new Property for From(Queue)  7A23h.
!This program displays 10+ LISTs that show Directory(). It does not have 10x code.
!It uses very little code by scanning the Window to FIND all LISTs and their From(Q) using 7A23h new Property
!--------------------------------------------------------------------------------------------
![ ] Todo Examples Folder
![ ] Make work with versions prior to 11.13505
!--------------------------------------------------------------------------------------------
!E.g. to add another TAB to show the "TextLib" folder just takes 2 steps. PROPs are used to Find and Fill it
!
 OMIT('** END STEPS **')
DirTextLibQ     QUEUE(DirQType).          !Step 1. To Data add a Queue 
    TAB('TextLib'),  USE(?Tab:TextLib)    !Step 2. To Window add a Tab with LIST From(Q) and MSG('\folder\')
       LIST,AT(1,42),USE(?List:DirTextLibQ),MSG('\Data\Options\TextLib\'),FROM(DirTextLibQ)
    END                   ^^^^^^^^^^^^^^^^       ^^^^^^^^^^^^^^^^^^^^^^        ^^^^^^^^^^^
 !end of OMIT('** END STEPS **')
!--------------------------------------------------------------------------------------------
  PROGRAM
  INCLUDE('KEYCODES.CLW') 
  INCLUDE('ListSortColCls.INC'),ONCE
  INCLUDE('CBWndPreview.INC'),ONCE    !Comment out or Download https://github.com/CarlTBarnes/WindowPreview
    COMPILE('**END**', _IFDef_CBWndPreview_)
WndPreviewCls CBWndPreviewClass 
    !end of COMPILE('**END**', _IFDef_CBWndPreview_)

  MAP
ListFromQProp   PROCEDURE(LONG ListFEQ),LONG    !New 11.13505 LIST FROM() &Queue as a NUMBER to &=()
ClarionFolders  PROCEDURE() 
LoadAllQ_TreeQ  PROCEDURE(STRING PathBS, STRING SubFolder, AllQ Out_AllQ, TreeQ Out_TreeQ)
DirQFromListMSG PROCEDURE(LONG ListFEQ, FILE:Queue FromDirQ, LONG PathEntryFEQ)
DirQNormalize   PROCEDURE(FILE:Queue InOutDirQ)  !Upper Name[1] letter, Short Name as EXT
GetClarionRoots PROCEDURE(QUEUE Cla_Queue,*STRING ClaQ_RootPath,*STRING ClaQ_ClarionName,*DECIMAL ClaQ_VersioNo)
GetConfigPathBS PROCEDURE(),STRING !Return C:\Users\..\AppData\Roaming\SoftVelocity\Clarion\
Upper1          PROCEDURE(*STRING InOutStr)
Upper1          PROCEDURE(STRING InStr),STRING
SortClassInit   PROCEDURE(ListSortColClass SortCls, LONG ListFEQ, QUEUE FromQ)
Hex8            PROCEDURE(LONG _Lng),STRING 
ManifestFixup   PROCEDURE(BYTE SheetNoTheme=1, BYTE ColorAsWindow=0)
        MODULE('RTL')
ClaFieldName    PROCEDURE(LONG pFEQ),CSTRING,RAW,NAME('Cla$FIELDNAME'),DLL(dll_mode)
LenFastClip     PROCEDURE(CONST *STRING Text2Measure),LONG,NAME('Cla$FASTCLIP'),DLL(dll_mode)
Sleep           PROCEDURE(LONG),PASCAL,DLL(1)
SHGetFolderPathA PROCEDURE(LONG Reserved=0, LONG nFolder, LONG hToken, LONG dwFlags, *CSTRING pszPath),LONG,PROC,PASCAL,RAW,DLL(1)
        END  
  END
Glo:ClarionRoot     STRING(128) 
Glo:ClarionFolder   STRING(64) 
AllQ    QUEUE(FILE:Queue),PRE(AllQ)
Folder     STRING(64)
PathBS     STRING(255)
        END            
TreeQ    QUEUE,PRE(TreeQ)
Name           STRING(FILE:MaxFileName) 
Level          LONG         !TreeQ:Level
ShortName      STRING(13)
Date           LONG
Time           LONG
Size           LONG
Attrib         BYTE
Folder         STRING(64)   !TreeQ:Folder
PathBS         STRING(255)  !TreeQ:PathBS
        END

Glo:ConfigAppDataBS PSTRING(256)  !The default Config C:\Users\..\AppData\Roaming\SoftVelocity\Clarion\
Glo:ConfigSub       STRING(16)    !E.g. 11.0 
Glo:ConfigPathBS    PSTRING(256)  !The Full Path AppData+Sub
CfgSubsQ    QUEUE,PRE(CfgSubQ)    !Folders under AppData\Sv\Clarion for Combo
Folder         PSTRING(32)
            END
CfgAllQ     QUEUE(AllQ),PRE(CfgAllQ)
            END
CfgTreeQ    QUEUE(TreeQ),PRE(CfgTreeQ)
            END
                        
  CODE
  SYSTEM{PROP:PropVScroll}=1 
  SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY 
  ClarionFolders()
!------------------------
ListFromQProp   PROCEDURE(LONG ListFEQ) ! wrap code to get 7A23h &Queue to protect from GPF
FromQ LONG
FromP LONG
PROP:FromPtr  EQUATE(7A0EH) !ABLLIST.INT &= IMappedListContents ? 0800xxxxH usually, 7C98h also an II ptr?
PROP:FromQRef EQUATE(7A23H) !Showed up in 13505 = &QUEUE unless no FROM(Q) then ?= Interface for WB see 0800xxxxh
  CODE
  IF ~ListFEQ{PROP:From} AND ListFEQ{PROP:VLBval}=0 THEN    !From('Text') or VLB &A23h would return a pointer that GPFs 
     FromQ=ListFEQ{PROP:FromQRef}                           !The New 11.13505 Property of LIST for FROM(Q)
     IF FromQ=08004FF0h THEN FromQ=0.                       !If No From(Q) RTL {7A23h}=08004FF0h always ????
     IF FromQ THEN 
        FromP=ListFEQ{PROP:FromPtr} 
        IF ABS(FromQ-FromP)<7FFFh THEN FromQ=0. !HACK - Interface pointer?  not From(Q), can't use this.
     END                                        !       This would only happen with No FROM() at all.
     IF FromQ>0 AND FromQ<=4096 THEN FromQ=0.   !Null Pointer memory region
  END
  RETURN FromQ
!------------------------
ClarionFolders PROCEDURE()
X       LONG,AUTO
QNdx    LONG,AUTO
ClarionQ QUEUE,PRE(ClaQ)
Root        STRING(128)
VersionNo   DECIMAL(5,1)
Clarion     STRING(16)      !e.g. Clarion11
         END
ListsQ  QUEUE,PRE(LstQ:) !Must use Dot syntax with &Refs usually so don't use LstQ:
ListFEQ         LONG                !ListsQ.ListFEQ    
FEQName         CSTRING(48)         !ListsQ.FEQName
FromQRefNum     LONG                !ListsQ.FromQRefNum
FromQHex        STRING(9)           !ListsQ.FromQHex
DirPathFEQ      LONG                !ListsQ.DirPathFEQ
DirPathDebug    STRING(255)         !ListsQ.DirPathDebug  LIST show &PSTRING so for Debug 
DirPathBS       &PSTRING            !ListsQ.DirPathBS
FromQRef        &QUEUE              !ListsQ.FromQRef   
FileQRef        &FILE:Queue         !ListsQ.FileQRef   
ListSortCls     &ListSortColClass   !ListsQ.ListSortCls 
        END

DirQType       QUEUE,TYPE!Must be same as (FILE:Queue)
Name           STRING(FILE:MaxFileName)
ShortName      STRING(13)
Date           LONG
Time           LONG
Size           LONG
Attrib         BYTE
           END ! DirBinQ:Name  DirBinQ:ShortName(8.3?)  DirBinQ:Date  DirBinQ:Time  DirBinQ:Size  DirBinQ:SizeU  DirBinQ:Attrib 

DirBinQ         QUEUE(DirQType).    !-- Queues for FROM() of LIST that show Folders
DirAccBinQ      QUEUE(DirQType).
DirLibQ         QUEUE(DirQType).
DirAccLibQ      QUEUE(DirQType).
DirLibSrcQ      QUEUE(DirQType).
DirAccLibSrcQ   QUEUE(DirQType).
DirTplQ         QUEUE(DirQType).
DirAccTplQ      QUEUE(DirQType).
DirImagesQ      QUEUE(DirQType).
DirAccImgQ      QUEUE(DirQType).
DirDocsQ        QUEUE(DirQType).
!! DirTextLibQ     QUEUE(DirQType).  ! Uncomment this line and below the Tab to see TextLib
            !!            TAB('TextLib'),USE(?Tab:TextLib)   !Uncomment this and above DirTextLibQ to see TextLib on Window
            !!                LIST,AT(1,42),USE(?List:DirTextLibQ),MSG('\Data\Options\TextLib\'),FROM(DirTextLibQ)
            !!            END        
FilesWindow WINDOW('Clarion Folders'),AT(,,423,250),GRAY,SYSTEM,MAX,ICON(ICON:Clarion),FONT('Segoe UI',10,,FONT:regular), |
            RESIZE
        PROMPT('&Clarion:'),AT(5,2),USE(?Root:Cmd)
        COMBO(@s128),AT(36,2,200,9),USE(Glo:ClarionRoot),SKIP,VSCROLL,DROP(9),FROM(ClarionQ),FORMAT('110L(2)|M~Root~@s12' & |
                '8@30L(2)|M~Version~@n5.1@60L(2)~Clarion~@s16@')
        SHEET,AT(1,12),FULL,USE(?SHEET1),JOIN
            TAB('BIN'),USE(?Tab:BIN)
                BOX,AT(0,27,,11),FULL,USE(?Box_Bin),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                ENTRY(@s255),AT(0,27,,10),FULL,USE(?Path_Bin),SKIP,TRN,LEFT(3),FONT('Consolas'),TIP('Double click to Explore'), |
                        READONLY,ALRT(MouseLeft2)
                LIST,AT(2,40),FULL,USE(?List:DirBinQ),VSCROLL,MSG('\BIN\'),VCR,FROM(DirBinQ),FORMAT('34R(2)|M~Date~C(0)@' & |
                        'd1@#3#33R(2)|M~Time~C(0)@T4@42R(2)|M~Size~C(0)@n13@36L(2)|M~Extension~L(1)@s13@#2#120L(2)|FM~Na' & |
                        'me~L(1)@s255@#1#')
            END
            TAB('Acc BIN'),USE(?Tab:AccBIN),TIP('Accessory BIN')
                LIST,AT(1,42),USE(?List:DirAccBinQ),MSG('\Accessory\BIN\'),FROM(DirAccBinQ)
            END
            TAB('LIB'),USE(?Tab:LIB)
                LIST,AT(1,42),USE(?List:DirLibQ),MSG('\LIB\'),FROM(DirLibQ)
            END
            TAB('Acc LIB'),USE(?Tab:AccLIB),TIP('Accessory LIB')
                LIST,AT(1,42),USE(?List:DirAccLibQ),MSG('\Accessory\LIB\'),FROM(DirAccLibQ)
            END
            TAB('LibSrc'),USE(?Tab:LibSrc)
                LIST,AT(1,42),USE(?List:DirLibSrcQ),MSG('\LIBSRC\WIN\'),FROM(DirLibSrcQ)
            END
            TAB('Acc LibSrc'),USE(?Tab:AccLibSrc),TIP('Accessory LibSrc')
                LIST,AT(1,42),USE(?List:DirAccLibSrcQ),MSG('\Accessory\LIBSRC\WIN\'),FROM(DirAccLibSrcQ)
            END
            TAB('Template'),USE(?Tab:Tpl)
                LIST,AT(1,42),USE(?List:DirTplQ),MSG('\TEMPLATE\WIN\'),FROM(DirTplQ)
            END
            TAB('Acc Tpl'),USE(?Tab:AccTpl),TIP('Accessory Template')
                LIST,AT(1,42),USE(?List:DirAccTplQ),MSG('\Accessory\TEMPLATE\WIN\'),FROM(DirAccTplQ)
            END
            TAB('Images'),USE(?Tab:Images)
                LIST,AT(1,42),USE(?List:DirImagesQ),MSG('\IMAGES\'),FROM(DirImagesQ)
            END
            TAB('Acc Img'),USE(?Tab:AccImg),TIP('Accessory Images')
                LIST,AT(1,42),USE(?List:DirAccImgQ),MSG('\Accessory\IMAGES\'),FROM(DirAccImgQ)
            END
            TAB('Docs'),USE(?Tab:Docs)
                LIST,AT(1,42),USE(?List:DirDocsQ),MSG('\DOCS\'),FROM(DirDocsQ)
            END
            !!            TAB('TextLib'),USE(?Tab:TextLib)   !Uncomment this and above DirTextLibQ to see TextLib on Window
            !!                LIST,AT(1,42),USE(?List:DirTextLibQ),MSG('\Data\Options\TextLib\'),FROM(DirTextLibQ)
            !!            END  
            TAB('ALL'),USE(?Tab:ALL),TIP('All files in one list')
                LIST,AT(2,30),FULL,USE(?List:AllQ),VSCROLL,VCR,FROM(AllQ),FORMAT('120L(2)|M~Name~L(1)@s255@38L(2)|M~Extensio' & |
                        'n~C(0)@s13@36R(2)|M~Date~C(0)@d1@34R(2)|M~Time~C(0)@T4@42R(2)|M~Size~C(0)@n13@50L(2)|M~Folder~L' & |
                        '(1)@s64@#7#100L(2)|M~Path~L(1)@s255@')
            END
            TAB('Tree'),USE(?Tab:Tree),TIP('All files with a Directory Tree')
                LIST,AT(2,30),FULL,USE(?List:TreeQ),VSCROLL,VCR,FROM(TreeQ),FORMAT('135L(2)|MT~Name~L(1)@s255@38L(2)|M~E' & |
                        'xtension~C(0)@s13@33R(2)|M~Date~C(0)@d1b@32R(2)|M~Time~C(0)@T4b@42R(2)|M~Size~C(0)@n13b@50L(2)|' & |
                        'M~Folder~L(1)@s64@#8#100L(2)|M~Path~L(1)@s255@')
            END
            TAB('Config'),USE(?Tab:ConfigAll),TIP('AppData SoftVelocity Config')
                COMBO(@s16),AT(5,28,70,9),USE(Glo:ConfigSub),SKIP,VSCROLL,DROP(9),FROM(CfgSubsQ)
                ENTRY(@s255),AT(80,27,,10),FULL,USE(Glo:ConfigPathBS),SKIP,TRN,FONT('Consolas'),READONLY,ALRT(MouseLeft2)
                LIST,AT(2,40),FULL,USE(?List:CfgAllQ),VSCROLL,VCR,FROM(CfgAllQ),FORMAT('120L(2)|M~Name~L(1)@s255@38L(2)|' & |
                        'M~Extension~C(0)@s13@36R(2)|M~Date~C(0)@d1@34R(2)|M~Time~C(0)@T4@42R(2)|M~Size~C(0)@n13@50L(2)|' & |
                        'M~Folder~L(1)@s64@#7#100L(2)|M~Path~L(1)@s255@')
            END
            TAB('CfgTree'),USE(?Tab:ConfigTree),TIP('AppData SoftVelocity Config Tree')
                COMBO(@s16),AT(5,28,70,9),USE(Glo:ConfigSub,, ?Glo:ConfigSub:2),SKIP,VSCROLL,DROP(9),FROM(CfgSubsQ)
                ENTRY(@s255),AT(80,27,,10),FULL,USE(Glo:ConfigPathBS,, ?Glo:Cfg2),SKIP,TRN,FONT('Consolas'),READONLY, |
                        ALRT(MouseLeft2)
                LIST,AT(2,40),FULL,USE(?List:CfgTreeQ),VSCROLL,VCR,FROM(CfgTreeQ),FORMAT('135L(2)|MT~Name~L(1)@s255@38L(' & |
                        '2)|M~Extension~C(0)@s13@33R(2)|M~Date~C(0)@d1b@32R(2)|M~Time~C(0)@T4b@42R(2)|M~Size~C(0)@n13b@5' & |
                        '0L(2)|M~Folder~L(1)@s64@#8#100L(2)|M~Path~L(1)@s255@')
            END
            TAB('Lists'),USE(?Tab:Lists)
                STRING('Lists Queue with LIST From(Queue) &Ref and Property'),AT(10,28,,10),USE(?Lists_FYI),TRN
                LIST,AT(1,42),FULL,USE(?List:ListsQ),VSCROLL,FONT(,10),VCR,FROM(ListsQ),FORMAT('28C|M~List<13,10>FEQ~@n4' & |
                        '@80L(2)|M~FEQ Name~@s32@50R(3)|M~Queue Ref<13,10>Prop 7A23~C(0)@n11@50R(3)|M~Queue Ref<13><10>7' & |
                        'A23 Hex~C(0)@s9@28C|M~Path<13,10>FEQ~@n4@80L(2)|M~Dir Path~@s255@')
            END
        END
    END
Explore         STRING(260)
ListSortAllQ    ListSortColClass 
ListSortCfgAllQ ListSortColClass 
    CODE
    OPEN(FilesWindow)
    ManifestFixup()  
    ?SHEET1{PROP:TabSheetStyle}=2 
    COMPILE('**END**', _IFDef_CBWndPreview_)
       WndPreviewCls.Init()
    !end of COMPILE('**END**', _IFDef_CBWndPreview_)  
    
    DO Load_ListsQ_Rtn  
    IF EXISTS(Glo:ClarionRoot) THEN POST(EVENT:Accepted,?Glo:ClarionRoot).
    ACCEPT
       CASE ACCEPTED()
       OF ?Glo:ClarionRoot
            X=LenFastClip(Glo:ClarionRoot)
            IF X AND Glo:ClarionRoot[X]='\' THEN Glo:ClarionRoot[X]=''.
            IF ~EXISTS(Glo:ClarionRoot) THEN BEEP ; SELECT(?Glo:ClarionRoot) ; CYCLE .
            0{PROP:Text}='Clarion Folders: ' & Glo:ClarionRoot
            X=INSTRING('\',Glo:ClarionRoot,-1,SIZE(Glo:ClarionRoot))
            Glo:ClarionFolder=Upper1(SUB(Glo:ClarionRoot,X+1,99)) 
            DO Load_DirsQ_Rtn ; DISPLAY
       OF ?Glo:ConfigSub OROF ?Glo:ConfigSub:2
            Glo:ConfigPathBS = Glo:ConfigAppDataBS & CLIP(Glo:ConfigSub) &'\'
            IF ~EXISTS(Glo:ConfigPathBS) THEN 
               Message('Path does not exist: ' & Glo:ConfigPathBS)
               SELECT(?) ; BEEP ; CYCLE
            END
            LoadAllQ_TreeQ(Glo:ConfigPathBS,Glo:ConfigSub,CfgAllQ,CfgTreeQ) 
            ListSortCfgAllQ.InitSortCol(0,0)
            DISPLAY         
       END
       CASE EVENT()
       OF EVENT:AlertKey 
          IF KEYCODE()=MouseLeft2 THEN
             Explore=CONTENTS(?)
             IF Explore[2:3]=':\' THEN
                SETCURSOR(CURSOR:Wait) ; DISABLE(?) ; DISPLAY  !Try to give visual feedback something is happening
                RUN('Explorer.exe /n,"' & CLIP(Explore) &'"')
                SLEEP(0) ; SLEEP(250) ; SLEEP(0) ; SETCURSOR() ; ENABLE(?) ; DISPLAY
             END
          END          
       OF EVENT:HeaderPressed 
          CASE FIELD()
          OF ?List:AllQ     ; ListSortAllQ.TakeHeaderPressed()
          OF ?List:CfgAllQ  ; ListSortCfgAllQ.TakeHeaderPressed() 
          ELSE
              ListsQ.ListFEQ = FIELD()
              GET(ListsQ,ListsQ.ListFEQ)
              IF ~ERRORCODE() THEN 
                 ListsQ.ListSortCls.TakeHeaderPressed()   !Class should Register for Event?
              END
          END
       OF EVENT:NewSelection
          IF KEYCODE()<>MouseLeft2 THEN CYCLE.
          CASE FIELD() 
          OF ?List:AllQ  ; GET(AllQ, CHOICE(?List:AllQ))   ; IF ERRORCODE() THEN CYCLE.
                           Explore=CLIP(AllQ:PathBS) & |
                                        AllQ:Name 
          OF ?List:TreeQ ; GET(TreeQ, CHOICE(?List:TreeQ)) ; IF ERRORCODE() THEN CYCLE.
                           Explore=CLIP(TreeQ:PathBS) & |
                                        CHOOSE(~BAND(TreeQ:Attrib,ff_:DIRECTORY), TreeQ:Name,'') 
          ELSE
             ListsQ.ListFEQ = FIELD()
             GET(ListsQ,ListsQ.ListFEQ)                     ; IF ERRORCODE() THEN CYCLE.
             GET(ListsQ.FileQRef, CHOICE(ListsQ.ListFEQ))   ; IF ERRORCODE() THEN CYCLE.
             Explore=ListsQ.DirPathBS & CLIP(ListsQ.FileQRef.Name)
          END 
          SETCURSOR(CURSOR:Wait) ; DISABLE(?) ; DISPLAY  !Try to give visual feedback something is happening
          RUN('Explorer.exe /select,"' & CLIP(Explore) &'"') 
          SLEEP(0) ; SLEEP(250) ; SLEEP(0) ; SETCURSOR() ; ENABLE(?) ; DISPLAY
       END
    END !ACCEPT
    CLOSE(FilesWindow)
!-----------------------------------------
Load_ListsQ_Rtn ROUTINE
    DATA
F   LONG
C   LONG
Msg  STRING(32)
DirFormat  ANY
L   LONG,DIM(4)
E   LONG,DIM(4)
Q   LONG
CwVer STRING(4)
    CODE
    DirFormat=?List:DirBinQ{PROP:Format}
    GETPOSITION(?List:DirBinQ,L[1],L[2],L[3],L[4])
    GETPOSITION(?Path_Bin,E[1],E[2],E[3],E[4])   !ENTRY above LIST
    F=0
    LOOP ; F=0{PROP:NextField,F} ; IF F=0 THEN BREAK.
        Msg=F{PROP:Msg}
        CASE F{PROP:Type}
        OF CREATE:list
           IF Msg[1]<>'\' THEN CYCLE. ! MSG('\LibSrc\Win') on LIST is subfolder
           CLEAR(ListsQ)
           ListsQ.ListFEQ = F
           ListsQ.FEQName = ClaFieldName(F)
!Example: Code can use PROP: to get FROM(Q) &Queue Reference so does not need to be in code, i.e. can add LIST without Code           
           Q=ListFromQProp(F) 
                IF Q=0 THEN 
                    Message('Load_ListsQ_Rtn for |LIST ' & F &' "'& ListsQ.FEQName &'" |ListFromQProp() returned zero')
                    CYCLE
                END
           ListsQ.FromQRefNum=Q        
           ListsQ.FromQHex=Hex8(Q)
           ListsQ.FromQRef &= (Q)
           ListsQ.FileQRef &= (Q)
           ListsQ.ListSortCls &= NEW(ListSortColClass)
           SortClassInit(ListsQ.ListSortCls,F,ListsQ.FromQRef)
           ListsQ.DirPathBS &= NEW(PSTRING(256))
           ADD(ListsQ,ListsQ.ListFEQ)

           SETPOSITION(F,L[1],L[2],L[3],L[4])     !LIST move all to same spot 
           F{PROP:NoWidth}=1 ; F{PROP:NoHeight}=1 ; F{PROP:Full}=1 
           F{PROP:VScroll}=1 ; F{PROP:VCR}=1
           F{PROP:Format}=DirFormat
           
        END !Case Type LIST
    END
    !Clone template: BOX,AT(1,28,,10),FULL,USE(?Box_Bin). Setup up the color look with Gradient 
    ?Box_Bin{PROP:GradientType}=GradientTypes:Horizontal
    ?Box_Bin{PROP:GradientFromColor}=COLOR:GradientInactiveCaption  !Light color
    ?Box_Bin{PROP:GradientToColor}  =COLOR:InactiveCaption          !to Dark color
    !Clone template: ENTRY(@s255),AT(0,27,,10),FULL,USE(?Path_Bin),SKIP,TRN,LEFT(3),FONT('Consolas'),READONLY
    ?Path_Bin{PROP:FontColor}=COLOR:InactiveCaptionText
    ?List:DirBinQ{'PathEntryFEQ'}=?Path_Bin   !How List finds ENTRY with Path
    !Create the ENTRY control to show the Path above the LIST, plus blue Box across
    LOOP Q=1 TO RECORDS(ListsQ)
        GET(ListsQ,Q)
        F = ListsQ.ListFEQ
        IF F=?List:DirBinQ THEN
           C = ?Path_Bin
        ELSE 
           C=CLONE(0, ?Box_Bin, F{PROP:Parent} )   !BOX with Color
           UNHIDE(C)           
           C=CLONE(0, ?Path_Bin, F{PROP:Parent} )  !ENTRY with Path          
           UNHIDE(C)
        END
        C{PROP:Alrt,255}=MouseLeft2    !ALRT not cloned
        C{PROP:Use}=ListsQ.DirPathBS
        ListsQ.DirPathFEQ=C
        F{'PathEntryFEQ'}=C  !Not used but a way a List could find ENTRY without LIsts Queue
        PUT(ListsQ)
    END
    SortClassInit(ListSortAllQ,?List:ALLQ,AllQ)

    Glo:ClarionRoot='C:\Clarion'                !Load ClarionQ of Install Root Folders from Registry 
    FREE(ClarionQ) ;  CLEAR(ClarionQ)
    GetClarionRoots(ClarionQ, ClaQ:Root, ClaQ:Clarion, ClaQ:VersionNo)
    SORT(ClarionQ, -ClaQ:VersionNo, -ClaQ:Root)
    GET(ClarionQ, 1)
    IF ~ERRORCODE() THEN Glo:ClarionRoot=ClaQ:Root.

    Glo:ConfigAppDataBS = GetConfigPathBS()     !Load Config Folder Queue from AppData SoftVelocity
    Glo:ConfigPathBS = Glo:ConfigAppDataBS
    DIRECTORY(CfgAllQ, Glo:ConfigAppDataBS &'*.*',ff_:NORMAL+ff_:DIRECTORY)
    DirQNormalize(CfgAllQ)
    SORT(CfgAllQ, CfgAllQ:Name)
    LOOP Q=RECORDS(CfgAllQ) TO 1 BY -1
        GET(CfgAllQ,Q)
        IF CfgAllQ:Name='.' OR CfgAllQ:Name='..' THEN DELETE(CfgAllQ) ; CYCLE.
        IF BAND(CfgAllQ:Attrib,ff_:DIRECTORY) THEN
           CfgSubQ:Folder = CfgAllQ:Name
           ADD(CfgSubsQ)
        END
        CfgAllQ:Folder='Clarion'
        CfgAllQ:PathBS=Glo:ConfigAppDataBS
        PUT(CfgAllQ)      
    END 
    SortClassInit(ListSortCfgAllQ,?List:CfgALLQ,CFgAllQ)
    C=CLONE(0, ?Box_Bin, ?List:CfgALLQ{PROP:Parent} ) ; UNHIDE(C)  !BOX with Color
    C=CLONE(0, ?Box_Bin, ?List:CfgTreeQ{PROP:Parent} ) ; UNHIDE(C)  !BOX with Color 
    !Color Combo Drop List like Tooltip so stands out from white list
    F=?Glo:ConfigSub{PROP:ListFEQ}   ; F{PROP:Color}=COLOR:InfoBackground ; F{PROP:FontColor}=COLOR:InfoText 
    F=?Glo:ConfigSub:2{PROP:ListFEQ} ; F{PROP:Color}=COLOR:InfoBackground ; F{PROP:FontColor}=COLOR:InfoText  
    EXIT
!-----------------------------------------
Load_DirsQ_Rtn ROUTINE    !Load all LISTs using Directory()
    LOOP QNdx=1 TO RECORDS(ListsQ)
        GET(ListsQ,QNdx)    
        DirQFromListMSG(ListsQ.ListFEQ, ListsQ.FileQRef, ListsQ.DirPathFEQ)
        ListsQ.DirPathDebug=ListsQ.DirPathBS   !  LIST can't handle &STRING
        PUT(ListsQ)
        ListsQ.ListSortCls.InitSortCol(5)
    END
    LoadAllQ_TreeQ(CLIP(Glo:ClarionRoot) &'\', Glo:ClarionFolder, AllQ, TreeQ)
    ListSortAllQ.InitSortCol(0,0)
!-----------------------------------------
DirQFromListMSG PROCEDURE(LONG ListFEQ, FILE:Queue FromDirQ, LONG PathEntryFEQ)
FullPathBS PSTRING(256)
PathFEQ    LONG
    CODE 
    FullPathBS = CLIP(Glo:ClarionRoot) & CLIP(ListFEQ{PROP:Msg})
    ListFEQ{'DirPathBS'}=FullPathBS   
    CHANGE(PathEntryFEQ,FullPathBS)  !Entry before LIST with Path  
    FREE(FromDirQ)  
    DIRECTORY(FromDirQ, FullPathBS & '*.*',ff_:NORMAL) 
    DirQNormalize(FromDirQ)
!-----------------------------------------    
DirQNormalize PROCEDURE(FILE:Queue DirQ) !Cap Name[1] and ShortName = EXT 
X       LONG,AUTO
QNdx    LONG,AUTO !DirQ  DirQ:Name  DirQ:ShortName(8.3)  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:SizeU  DirQ:Attrib                                                                                                                               DirQ:SizeU  ULONG,OVER(DirQ:Size) !Over 2GB will show negative w/o UNSigned SizeU
    CODE   
    LOOP QNdx = 1 TO RECORDS(DirQ)                    !Case Name
         GET(DirQ,QNdx)
         DirQ:Name[1]=UPPER(DirQ:Name[1])
         X=INSTRING('.',DirQ:Name,-1,SIZE(DirQ:Name))
         DirQ:ShortName=SUB(DirQ:Name,X+1,99)         !Put EXT in Short Name        
         IF LenFastClip(DirQ:ShortName) <= 3 THEN 
            DirQ:ShortName=UPPER(DirQ:ShortName)
         ELSE 
            DirQ:ShortName[1]=UPPER(DirQ:ShortName[1])
         END
         IF BAND(DirQ:Attrib,ff_:DIRECTORY) THEN DirQ:ShortName=' <DIR>'.
         PUT(DirQ)
    END !LOOP
    RETURN
!----------------------
GetClarionRoots PROCEDURE(QUEUE Cla_Queue, *STRING ClaQ_RootPath,*STRING ClaQ_Clarion,*DECIMAL ClaQ_VersioNo)
SVSubKeysQ  QUEUE,PRE(SVSubQ)      !Keys under 'SOFTWARE\SoftVelocity'
ClarionKey      STRING(16)         !Should be Clarion11 10 9.1  
            END
Ndx  LONG,AUTO
Root STRING(256)
SOFTWARE_SoftVelocity EQUATE('SOFTWARE\SoftVelocity')
    CODE
    GetRegSubKeys(REG_LOCAL_MACHINE, SOFTWARE_SoftVelocity, SVSubKeysQ) 
    LOOP Ndx=1 TO RECORDS(SVSubKeysQ)
        GET(SVSubKeysQ,Ndx)
        IF lower(SvSubQ:ClarionKey[1:7])<>'clarion' THEN CYCLE.  !Only take 'Clarion', SV could add other products.
        Root = GETREG(REG_LOCAL_MACHINE, SOFTWARE_SoftVelocity &'\'& SvSubQ:ClarionKey  ,'ROOT')
        IF ~Root THEN CYCLE.                !Blank indicates Root NOT found in Registry
        IF ~EXISTS(Root) THEN CYCLE.        !If Folder not on disk, then was deleted
        ClaQ_Clarion  = SvSubQ:ClarionKey    !E.g. Clarion11 
        ClaQ_VersioNo = DEFORMAT(SvSubQ:ClarionKey)    !E.g. 11 or 9.1
        ClaQ_RootPath = Root                 !E.g. C:\Clarion11  or C:\TopSpeed\Cw11
        ADD(Cla_Queue)
    END
    RETURN 
!---------------------- 
GetConfigPathBS  PROCEDURE()!,STRING !Return C:\Users\..\AppData\Roaming\SoftVelocity\Clarion\
cPath CSTRING(256)
CSIDL_APPDATA	   EQUATE(26)
    CODE
    IF SHGetFolderPathA(0, CSIDL_APPDATA, 0, 0, cPath) < 0 THEN !Returns HResult 
       cPath='C:\SHGetFolderPathA_Fail'
    END   
    RETURN cPath & '\SoftVelocity\Clarion\'
!----------------------
Upper1          PROCEDURE(*STRING S) 
    CODE
    S[1]=UPPER(S[1]) 
Upper1          PROCEDURE(STRING S)!,STRING
    CODE
    S[1]=UPPER(S[1])
    RETURN S
!----------------------
SortClassInit PROCEDURE(ListSortColClass SortCls, LONG ListFEQ, QUEUE FromQ)
    CODE
    SortCls.Init(FromQ, ListFEQ)        
   ! SortCls.InitColors(COLOR:HighlightText,COLOR:Highlight,COLOR:InfoText,COLOR:InfoBackground) 
    SortCls.InitColors(COLOR:INACTIVECAPTIONTEXT,COLOR:INACTIVECAPTION,COLOR:InfoText,COLOR:InfoBackground) 
   ! SortCls.InitColors(COLOR:White,COLOR:Blue,COLOR:InfoText,COLOR:InfoBackground)
   ! If Manifest and Windows 10 the Header background is always white. RTL needs to Own and draw colored background
   !  SortCls.InitColors(Color:Maroon,COLOR:White,COLOR:InfoText,COLOR:InfoBackground)
     ListFEQ{PROP:NoTheme}=1  !with Theme is invisible
    SortCls.InitSortCol(5)
    RETURN 
!-------------------
Hex8 PROCEDURE(LONG _Lng)!,STRING
LngAdj  LONG,AUTO,STATIC 
L       BYTE,DIM(4),OVER(LngAdj)
Hex     STRING('0123456789ABCDEF'),STATIC
HX      STRING(9),AUTO,STATIC 
  CODE
    LngAdj = BAND(BSHIFT(_Lng, -4),0F0F0F0Fh) + 01010101h
    HX[1]=HEX[L[4]] ; HX[3]=HEX[L[3]] ; HX[5]=HEX[L[2]] ; HX[7]=HEX[L[1]]
    LngAdj=BAND(_Lng,0F0F0F0Fh)  + 01010101h
    HX[2]=HEX[L[4]] ; HX[4]=HEX[L[3]] ; HX[6]=HEX[L[2]] ; HX[8]=HEX[L[1]] ; HX[9]='h'
    RETURN HX
!================================================== 
LoadAllQ_TreeQ   PROCEDURE(STRING Load4PathBS, STRING Load4Folder, AllQ _AllQ, TreeQ _TreeQ)
X       LONG,AUTO
QNdx    LONG,AUTO
Dirs2LoadQ      QUEUE,PRE(D2LoadQ)   !Directories to load, added to as they are found
Name                STRING(255) 
Folder              STRING(64)
Level               LONG             !D2LoadQ:Level
                END
Dirs2Ndx        LONG,AUTO          !Current Record in Dirs2LoadQ to load for GET(,Dirs2Ndx)
Dirs2AddSpot    LONG,AUTO          !For ADD(Dirs2Load2,Dirs2AddSpot) so dirs are inserted in a good order
Dir1Loading     STRING(255),AUTO   !Current directory loading (from Dirs2LoadQ)
Dir1Folder      STRING(64),AUTO    !Current Folder loading (from Dirs2LoadQ)
Dir1Level       LONG,AUTO          !Current Folder Level loading (from Dirs2LoadQ)
Dir1Q           QUEUE(FILE:Queue),PRE(Dir1Q).   !The current Dir (Dir1Loading) files
Dirs2Window WINDOW('Loading Directories'),AT(,,263,38),FONT('Segoe UI',10),GRAY,DOUBLE,TIMER(1)
       STRING(@s255),AT(8,8),USE(Dir1Loading)
       PROGRESS,USE(?Dirs2Progress),AT(8,21,248,8),RANGE(0,100),SMOOTH
     END
    CODE 
    FREE(_AllQ)
    FREE(_TreeQ)
    !--Recurse directories, load (*.*) files into _AllQ
    FREE(Dirs2LoadQ) ; FREE(Dir1Q)
    D2LoadQ:Name  =Load4PathBS  !CLIP(Glo:ClarionRoot) &'\'
    D2LoadQ:Folder=Load4Folder  !Glo:ClarionFolder
    D2LoadQ:Level=1
    ADD(Dirs2LoadQ)
    Dirs2Ndx=0
    OPEN(Dirs2Window)
    0{PROP:Text}='Loading: '& D2LoadQ:Name
    ACCEPT
      CASE EVENT()
      OF EVENT:Timer 
        Dirs2Ndx += 1
        GET(Dirs2LoadQ,Dirs2Ndx)
        IF ERRORCODE() THEN BREAK. 
        Dir1Loading=D2LoadQ:Name 
        Dir1Folder =D2LoadQ:Folder 
        Dir1Level  =D2LoadQ:Level
        Dirs2AddSpot=Dirs2Ndx + 1 

        CLEAR(_TreeQ)
        _TreeQ.Name   = Dir1Folder 
        _TreeQ.Attrib = ff_:DIRECTORY
        _TreeQ.Folder = Dir1Folder
        _TreeQ.Level  = Dir1Level
        _TreeQ.PathBS = Dir1Loading
        ADD(_TreeQ)

        DISPLAY
        DIRECTORY(Dir1Q,CLIP(Dir1Loading)&'*.*',ff_:NORMAL+ff_:DIRECTORY)
        DirQNormalize(Dir1Q)
        SORT(Dir1Q,Dir1Q:Name)
        LOOP 
            GET(Dir1Q,1)
            IF ERRORCODE() THEN BREAK.
            DELETE(Dir1Q)
            IF BAND(Dir1Q:Attrib,FF_:Directory)
               IF Dir1Q:Name='.' OR Dir1Q:Name='..' THEN CYCLE.
               IF UPPER(Dir1Q:Name)='BACKUP' THEN CYCLE.
               D2LoadQ:Name=CLIP(Dir1Loading) & CLIP(Dir1Q:Name) & '\'
               D2LoadQ:Folder=Dir1Q:Name
               D2LoadQ:Level =Dir1Level+1
               ADD(Dirs2LoadQ,Dirs2AddSpot)
               Dirs2AddSpot += 1
               CYCLE !Only put Files into _AllQ
            END
    
            _AllQ :=: Dir1Q
            _AllQ.folder = Dir1Folder
            _AllQ.PathBS = Dir1Loading
            _AllQ.Attrib = BAND(_AllQ.Attrib,BXOR(-1,ff_:ARCHIVE))
            ADD(_AllQ)
            CLEAR(_TreeQ)
            _TreeQ :=: _AllQ 
            _TreeQ.level  = Dir1Level + 1
            ADD(_TreeQ)            
        END
        ?Dirs2Progress{PROP:Progress}=Dirs2Ndx / RECORDS(Dirs2LoadQ) * 100
      END !CASE EVENT()
    END !LOOP thru Dirs2LoadQ    
    X=-1
    LOOP QNdx=RECORDS(_TreeQ) TO 1 BY -1   !Fold branches
        GET(_TreeQ,QNdx)
        IF _TreeQ.Level < X AND _TreeQ.Level > 1 THEN
            _TreeQ.Level *= -1
            PUT(_TreeQ)
        END
        X=ABS(_TreeQ.Level)
    END        
    CLOSE(Dirs2Window)
    RETURN

!==================================================
ManifestFixup PROCEDURE(BYTE SheetNoTheme=1, BYTE ColorAsWindow=0)
FEQ LONG,AUTO
  CODE   !Changes VistaManifest.TPW Template makes ...
  FEQ=0 
  LOOP   
      FEQ=0{PROP:NextField,FEQ} ; IF ~FEQ THEN BREAK. 
      CASE FEQ{PROP:Type}
      OF Create:sheet
         IF SheetNoTheme THEN FEQ{PROP:NoTheme}=1.     !%ForceSHEETNoTheme
      OF Create:List
         FEQ{PROP:NoTheme}=1     !Make headers gray
      OF Create:OPTION   OROF Create:GROUP OROF Create:RADIO 
      OROF Create:STRING OROF Create:CHECK OROF Create:PROMPT  
           IF ~FEQ{PROP:Parent} THEN CYCLE.
           IF ColorAsWindow THEN  !%ForceMakeColorXPManifest
              FEQ{PROP:Trn}=0     
              IF FEQ{PROP:Color}=COLOR:None THEN
                 FEQ{PROP:Color}=COLOR:Window
              END 
           ELSE                   !%ForceMakeTransparentXPManifest
               FEQ{PROP:Trn}=1
           END
      END  
  END
  RETURN  